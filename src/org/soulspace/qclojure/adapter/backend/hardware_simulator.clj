(ns org.soulspace.qclojure.adapter.backend.hardware-simulator
  "Noisy quantum simulator backend implementing realistic
  quantum computing device simulation with comprehensive noise modeling.

  This backend provides local simulation of quantum devices with noise
  using the domain layer's quantum state and circuit functionality.
  It serves as both a reference implementation and testing backend for
  quantum algorithms under realistic noise conditions.
 
  The simulator can be initialized with a device map or with max-qubits
  - max-qubits
  - native-gates
  - topology
  - noise-model

  This simulator models various types of quantum noise including:
  - Depolarizing noise using Kraus operators
  - Amplitude damping (T1 decay) modeling energy dissipation  
  - Phase damping (T2 dephasing) modeling pure dephasing
  - Readout errors with configurable bit-flip probabilities
  - Coherent errors and systematic rotation biases
  - Gate-specific noise parameters based on real device calibration
  - Comprehensive Amazon Braket quantum hardware noise models

  The noise model can be configured with parameters such as T1 and T2
  times, gate operation times, and noise strengths. It supports
  advanced noise configurations including correlated readout errors
  and coherent errors with specific rotation angles and axes.
  The simulator applies noise during gate operations and measurements,
  simulating realistic quantum device behavior.

  The noise model map has the following structure:
  ```clojure
  {:gate-noise {
    :h {:noise-type :depolarizing :noise-strength 0.01}
    :x {:noise-type :amplitude-damping :noise-strength 0.02}
    :cnot {:noise-type :phase-damping :noise-strength 0.03}
    ...}
   :readout-error {:prob-0-to-1 0.05 :prob-1-to-0 0.02}}
  ```
  
  Note: The simulator currently doesn't model crosstalk between qubits.

  The simulator supports asynchronous job management, allowing
  users to submit circuits and retrieve results later. It can be used
  for testing algorithms, circuit designs, and quantum operations
  without requiring access to actual quantum hardware.
   
  It also implements the CloudQuantumBackend protocol for mock cloud
  backend functionality, allowing it to be used in a cloud-like
  environment for testing purposes."
  (:require [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]
            [org.soulspace.qclojure.domain.channel :as channel]
            [org.soulspace.qclojure.domain.noise :as noise]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.application.hardware-optimization :as hwopt]))

;; Noisy simulator state management
(defonce simulator-state (atom {:job-counter 0
                                :active-jobs {}}))

;; Job record for noisy simulations
(defrecord NoisySimulatorJob
           [job-id circuit options status result created-at completed-at])

;;;
;;; Helper functions for job management
;;;
(defn- generate-noisy-job-id []
  (let [new-counter (:job-counter (swap! simulator-state update :job-counter inc))]
    (str "noisy_job_" new-counter "_" (System/currentTimeMillis))))

;;;
;;; Circuit execution 
;;;
(defn- gate-with-noise-applicator
  "Takes a noise model and returns a function that applies gate noise.
   
   Parameters:
   - noise-model: Noise model configuration map

   Returns: Function that applies a gate with noise to a quantum state"
  [noise-model]
  (fn [state gate]
    (let [clean-state (qc/apply-operation-to-state state gate)]
      (noise/apply-gate-noise clean-state gate noise-model))))

(defn- execute-single-noisy-shot
  "Execute a single shot of a noisy quantum circuit.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - noise-model: Noise model configuration
  - num-qubits: Number of qubits in the circuit
  
  Returns: Single measurement outcome as a basis string"
  ([circuit noise-model]
   (execute-single-noisy-shot circuit (qs/zero-state (:num-qubits circuit)) noise-model))
  ([circuit initial-state noise-model]
   (let [num-qubits (:num-qubits circuit)
         ;; Apply all gates with noise
         final-state (reduce (gate-with-noise-applicator noise-model)
                             initial-state
                             (:operations circuit))
         ; Final measurement with readout noise
         measurement (noise/apply-readout-noise final-state num-qubits noise-model)
         ]
     measurement)))

(defn- execute-noisy-circuit-simulation
  "Execute a noisy quantum circuit simulation with proper noise independence per shot.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - options: Execution options including shot count
  - noise-model: Noise model configuration
  
  Returns: Noisy simulation results"
  [circuit options noise-model]
  (try
    (let [start-time (System/currentTimeMillis)
          shots (get options :shots 1024)
          num-qubits (:num-qubits circuit)]

      ;; Execute each shot independently with fresh noise
      (loop [shot-count 0
             accumulated-results {}
             last-final-state nil]

        (if (>= shot-count shots)
          {:job-status :completed
           :measurement-results accumulated-results
           :final-state last-final-state  ; Return the final state from the last shot
           :noise-applied true
           :shots-executed shots
           :execution-time-ms (- (System/currentTimeMillis) start-time)}

          ;; TODO incorporate result spec, return state and measured bitstring from single shot

          ;; Execute single shot with fresh noise
          (let [outcome-bitstring (execute-single-noisy-shot circuit noise-model)
                ;; For the final state, we'll capture it from one of the shots
                ;; (Note: this is somewhat artificial since each shot has different noise)
                updated-results (update accumulated-results outcome-bitstring (fnil inc 0))]

            (recur (inc shot-count)
                   updated-results
                   (if (= shot-count (dec shots))
                     ;; Capture final state from last shot for compatibility
                     (loop [state (qs/zero-state num-qubits)
                            gates (:operations circuit)]
                       (if (empty? gates)
                         state
                         (recur (qc/apply-operation-to-state state (first gates))
                                (rest gates))))
                     last-final-state))))))

    (catch Exception e
      (.printStackTrace e)
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))

;;;
;;; Noise Model Management
;;;
(defn noise-model-for
  "Get the noise model for a specific platform.
  
  Parameters:
  - platform: Platform name (keyword)
  
  Returns: Noise model map or nil if not found"
  ([platform]
   (noise-model-for qb/devices platform))
  ([devices platform]
   (get-in devices [platform :noise-model])))

(defn all-noise-models
  "Get all available noise models.
  
  Returns: Map of platform names to noise models"
  ([]
   (all-noise-models qb/devices))
  ([devices]
   (into {} (map (fn [[k v]] [k (:noise-model v)]) devices))))

;;;
;;; Accessor functions for device properties
;;;
(defn max-qubits
  "Get the maximum number of qubits supported by the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Maximum qubit count (default: 16)"
  [backend]
  (or (get-in backend [:device :max-qubits]) 16))

(defn native-gates
  "Get the set of native gates supported by the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Set of native gate keywords (default: all QClojure gates)"
  [backend]
  (or (get-in backend [:device :native-gates]) opreg/native-simulator-gate-set))

(defn topology
  "Get the qubit topology of the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Topology map or default fully connected for 16 qubits"
  [backend]
  (get-in backend [:device :topology]))

(defn noise-model
  "Get the noise model of the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Noise model map or empty map if none defined"
  [backend]
  (or (get-in backend [:device :noise-model]) {}))

;;;
;;; Hardware Simulator Implementation
;;;
(defrecord QuantumHardwareSimulator [device config]
  qb/QuantumBackend

  (get-backend-info [this]
    {:backend-type :quantum-hardware-simulator
     :backend-name "Noisy QPU Simulator"
     :description "Realistic quantum simulator with comprehensive noise modeling"
     :max-qubits (max-qubits this)
     :native-gates (native-gates this)
     :topology (topology this)
     :noise-model (noise-model this)
     :config config
     :capabilities #{:topology-optimization :gate-cancellation
                     :gate-decomposition :circuit-transformation
                     :depolarizing-noise :amplitude-damping :phase-damping
                     :coherent-errors :readout-errors :decoherence-modeling}
     :version "1.0.0"})

  (get-supported-gates [this] (native-gates this))
  (is-available? [_this] true)

  (submit-circuit [this circuit options]
    (let [job-id (generate-noisy-job-id)
          max-qubits (max-qubits this)
          native-gates (native-gates this)
          topology (topology this)
          noise-model (noise-model this)
;;          ;; Optimize circuit for hardware
;;          optimized-circuit (hwopt/optimize circuit
;;                                            native-gates
;;                                            topology)
          job (->NoisySimulatorJob job-id circuit options :queued nil
                                   (System/currentTimeMillis) nil)]

      ;; Store job and start execution in background
      (swap! simulator-state assoc-in [:active-jobs job-id] job)

      ;; Execute immediately in future (async execution)
      (future
        (let [result (execute-noisy-circuit-simulation circuit options noise-model)
              completed-job (assoc job
                                   :status (:job-status result)
                                   :result result
                                   :completed-at (System/currentTimeMillis))]
          (swap! simulator-state assoc-in [:active-jobs job-id] completed-job)))

      job-id))

  (get-job-status [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (:status job)
      :not-found))

  (get-job-result [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (if (= (:status job) :completed)
        (assoc (:result job) :job-id job-id)
        {:job-id job-id
         :job-status (:status job)
         :error-message "Job not completed"})
      {:job-id job-id
       :job-status :not-found
       :error-message "Job not found"}))

  (cancel-job [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (if (#{:queued :running} (:status job))
        (do
          (swap! simulator-state assoc-in [:active-jobs job-id]
                 (assoc job :status :cancelled
                        :completed-at (System/currentTimeMillis)))
          :cancelled)
        :already-completed)
      :not-found))

  (get-queue-status [_this]
    (let [jobs (vals (:active-jobs @simulator-state))
          active-count (count (filter #(#{:queued :running} (:status %)) jobs))
          completed-count (count (filter #(= :completed (:status %)) jobs))]
      {:total-jobs (count jobs)
       :active-jobs active-count
       :completed-jobs completed-count}))

  ;; Add CloudQuantumBackend implementation
  qb/CloudQuantumBackend

  (authenticate [_this _credentials]
    {:status :authenticated
     :session-id (str "hardware_sim_session_" (System/currentTimeMillis))
     :expires-at (+ (System/currentTimeMillis) (* 24 60 60 1000))})

  (get-session-info [_this]
    {:status :authenticated
     :backend-type :quantum-hardware-simulator
     :session-id "hardware_sim_session_mock"
     :authenticated-at (System/currentTimeMillis)})

  (list-available-devices [this]
    [{:device-id "hardware-simulator-1"
      :device-name "Quantum Hardware Simulator"
      :device-status :online
      :backend-type :quantum-hardware-simulator
      :max-qubits (max-qubits this)
      :noise-model (noise-model this)}])

  (get-device-topology [this device-id]
    (let [max-qubits (max-qubits this)
          coupling-map (for [i (range max-qubits)
                             j (range max-qubits)
                             :when (not= i j)]
                         [i j])]
      {:device-id device-id
       :device-name "Quantum Hardware Simulator"
       :coupling-map coupling-map
       :max-qubits (max-qubits this)
       :noise-model (noise-model this)}))

  (get-calibration-data [this device-id]
    {:device-id device-id
     :timestamp (java.time.Instant/now)
     :noise-model (noise-model this)
     :coherence-times (vec (repeat (get config :max-qubits 20) 100000))})

  (estimate-cost [_this _circuit _options]
    {:total-cost 0.0
     :currency "USD"
     :cost-breakdown {:circuit-cost 0.0 :shot-cost 0.0 :device-cost 0.0}
     :estimated-credits 0})

  (batch-submit [this circuits options]
    (let [batch-id (str "noisy_batch_" (System/currentTimeMillis))
          job-ids (mapv #(qb/submit-circuit this % options) circuits)]
      (swap! simulator-state assoc-in [:active-jobs batch-id]
             {:batch-id batch-id
              :job-ids job-ids
              :status :queued
              :created-at (System/currentTimeMillis)})
      batch-id))

  (get-batch-status [this batch-job-id]
    (if-let [batch-info (get (:active-jobs @simulator-state) batch-job-id)]
      (let [job-ids (:job-ids batch-info)
            statuses (map #(qb/get-job-status this %) job-ids)
            status-counts (frequencies statuses)]
        {:batch-id batch-job-id
         :total-jobs (count job-ids)
         :status-counts status-counts
         :overall-status (cond
                           (every? #(= % :completed) statuses) :completed
                           (some #(= % :failed) statuses) :partial-failure
                           (some #(= % :running) statuses) :running
                           :else :queued)})
      {:batch-id batch-job-id :status :not-found}))

  (get-batch-results [this batch-job-id]
    (if-let [batch-info (get (:active-jobs @simulator-state) batch-job-id)]
      (let [job-ids (:job-ids batch-info)
            results (into {} (map (fn [job-id]
                                    [job-id (qb/get-job-result this job-id)])
                                  job-ids))]
        {:batch-id batch-job-id
         :results results
         :completed-jobs (count (filter #(= (:job-status %) :completed)
                                        (vals results)))
         :total-jobs (count job-ids)})
      {:batch-id batch-job-id :error-message "Batch not found"})))

;;;
;;; Factory functions for hardware simulators
;;;
(defn create-noisy-simulator
  "Create a local noisy quantum simulator with comprehensive noise modeling.
  
   Parameters:
   - noise-model: noise model configuration
   - config: Optional simulator configuration
  
   Returns: QuantumHardwareSimulator instance with noise model, full gate set
   and fully connected qubits"
  ([noise-model]
   (create-noisy-simulator noise-model {}))
  ([noise-model config]
   (->QuantumHardwareSimulator {:name "Noisy Simulator"
                                :noise-model noise-model}
                               config)))

(defn create-hardware-simulator
  "Create a hardware simulator for a specific device.
  
   Parameters:
   - device: Device map with max-qubits, native-gates, topology, noise-model
   - config: Optional simulator configuration
  
   Returns: QuantumHardwareSimulator instance"
  ([device]
   (create-hardware-simulator device {}))
  ([device config]
   (->QuantumHardwareSimulator device config)))

;;;
;;; Utility functions for hardware simulator state management
;;;
(defn reset-simulator-state!
  "Reset the hardware simulator state, clearing all jobs and counters.
  
  Returns: Updated state"
  []
  (reset! simulator-state {:job-counter 0 :active-jobs {}}))

(defn get-simulator-stats
  "Get statistics about the hardware simulator state.
  
  Returns: Map with job counts and statistics"
  []
  (let [current-state @simulator-state
        jobs (vals (:active-jobs current-state))
        completed-jobs (filter #(= (:status %) :completed) jobs)
        execution-times (keep #(let [start (:created-at %)
                                     end (:completed-at %)]
                                 (when (and start end)
                                   (- end start)))
                              completed-jobs)
        jobs-by-status (->> jobs
                            (group-by :status)
                            (map (fn [[status job-list]] [status (count job-list)]))
                            (into {}))
        active-jobs-count (count (filter #(#{:queued :running} (:status %)) jobs))
        oldest-job (when (seq jobs)
                     (apply min (map :created-at jobs)))
        newest-job (when (seq jobs)
                     (apply max (map :created-at jobs)))]

    {:total-jobs (count jobs)
     :completed-jobs (count completed-jobs)
     :average-execution-time (if (seq execution-times)
                               (/ (reduce + execution-times)
                                  (count execution-times))
                               0)
     :job-counter (:job-counter current-state)
     :jobs-by-status jobs-by-status
     :active-jobs-count active-jobs-count
     :oldest-job oldest-job
     :newest-job newest-job}))

(defn cleanup-completed-jobs!
  "Remove completed jobs older than specified time (in milliseconds).
  
  Parameters:
  - max-age-ms: Maximum age for completed jobs (default: 1 hour)
  
  Returns: Number of jobs cleaned up"
  ([]
   (cleanup-completed-jobs! (* 60 60 1000))) ; 1 hour default
  ([max-age-ms]
   (let [current-time (System/currentTimeMillis)
         cutoff-time (- current-time max-age-ms)
         current-jobs (:active-jobs @simulator-state)
         jobs-to-keep (into {}
                            (filter
                             (fn [[_job-id job]]
                               (or (not= (:status job) :completed)
                                   (> (:completed-at job 0) cutoff-time)))
                             current-jobs))
         removed-count (- (count current-jobs) (count jobs-to-keep))]
     (swap! simulator-state assoc :active-jobs jobs-to-keep)
     removed-count)))


(comment

  (require '[org.soulspace.qclojure.application.noise-analysis :as analysis])

  (noise-model-for :ibm-lagos)

  ;; Create advanced simulators with different noise characteristics
  (def ibm-sim (create-noisy-simulator (noise-model-for :ibm-lagos)))
  (keys ibm-sim)
  (:device ibm-sim)
  (max-qubits ibm-sim)
  (noise-model ibm-sim)
  (def rigetti-sim (create-noisy-simulator (noise-model-for :rigetti-aspen)))
  (def ideal-sim (create-noisy-simulator {}))

  ;; === Amazon Braket Hardware Models ===

  ;; IonQ trapped ion systems (high fidelity, slow gates)
  (def ionq-harmony-sim (create-noisy-simulator (noise-model-for :ionq-harmony)))
  (def ionq-aria-sim (create-noisy-simulator (noise-model-for :ionq-aria)))
  (def ionq-forte-sim (create-noisy-simulator (noise-model-for :ionq-forte)))

  ;; Rigetti superconducting systems (fast gates, moderate fidelity)
  (def rigetti-m3-sim (create-noisy-simulator (noise-model-for :rigetti-aspen-m3)))

  ;; Photonic and neutral atom systems (unique characteristics)
  (def xanadu-sim (create-noisy-simulator (noise-model-for :xanadu-x-series)))
  (def quera-sim (create-noisy-simulator (noise-model-for :quera-aquila)))

  ;; Test Bell circuit across all platforms
  (def bell-circuit
    (-> (qc/create-circuit 2 "Bell State")
        (qc/h-gate 0)
        (qc/cnot-gate 0 1)))

  (execute-single-noisy-shot bell-circuit (noise-model-for :ibm-lagos))
  (execute-noisy-circuit-simulation bell-circuit {:shots 1000} (noise-model-for :ibm-lagos))

  ;; Compare all Amazon Braket platforms
  (def all-models (all-noise-models))
  (def bell-comparison (analysis/compare-hardware-platforms bell-circuit all-models))

  (println "=== Amazon Braket Platform Comparison (Bell Circuit) ===")
  (doseq [[platform data] (sort-by (comp :estimated-fidelity second) > bell-comparison)]
    (println (format "%s (% s):"
                     (name platform)
                     (name (:platform-type data))))
    (println (format "  Fidelity: %.4f (%.2f%% error)"
                     (:estimated-fidelity data)
                     (* 100 (:total-estimated-error data))))
    (println (format "  T1: %s μs, T2: %s μs"
                     (get-in data [:coherence-times :t1-time])
                     (get-in data [:coherence-times :t2-time])))
    (println (format "  Readout errors: 0→1: %.3f, 1→0: %.3f"
                     (get-in data [:readout-fidelity :prob-0-to-1])
                     (get-in data [:readout-fidelity :prob-1-to-0]))))

  ;; Test more complex circuit to see platform differences
  (def grover-circuit
    (-> (qc/create-circuit 3 "3-qubit Grover")
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)  ; Superposition
        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2)      ; Oracle simulation
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)  ; Diffusion
        (qc/z-gate 0) (qc/z-gate 1) (qc/z-gate 2)
        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2)
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)))

  (def grover-comparison (analysis/compare-hardware-platforms grover-circuit all-models))

  (println "\n=== Platform Comparison (Complex Grover Circuit) ===")
  (doseq [[platform data] (sort-by (comp :estimated-fidelity second) > grover-comparison)]
    (println (format "%s: %.4f fidelity (%.1f%% total error)"
                     (name platform)
                     (:estimated-fidelity data)
                     (* 100 (:total-estimated-error data)))))

  ;; Individual noise channel testing
  (def test-state (qs/plus-state))

  ;; Depolarizing noise
  (def depol-ops (channel/depolarizing-kraus-operators 0.1))
  (def simulator-state (channel/apply-quantum-channel test-state depol-ops 0))
  (println "\nDepolarizing noise result:" (:state-vector simulator-state))

  ;; Amplitude damping
  (def amp-ops (channel/amplitude-damping-kraus-operators 0.1))
  (def damped-state (channel/apply-quantum-channel test-state amp-ops 0))
  (println "Amplitude damping result:" (:state-vector damped-state))

  ;; Decoherence parameters for different platforms
  (def ionq-decoherence (channel/calculate-decoherence-params 15000.0 7000.0 40000.0))
  (def rigetti-decoherence (channel/calculate-decoherence-params 45.0 35.0 200.0))

  (println "\nDecoherence comparison:")
  (println "  IonQ Aria (T1=15ms, T2=7ms, gate=40μs):" ionq-decoherence)
  (println "  Rigetti M3 (T1=45μs, T2=35μs, gate=200ns):" rigetti-decoherence))
