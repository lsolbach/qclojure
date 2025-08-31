(ns org.soulspace.qclojure.adapter.backend.noisy-simulator
  "Advanced noisy quantum simulator backend implementing realistic
  quantum computing device simulation with comprehensive noise modeling.

  This backend provides local simulation of quantum devices with noise
  using the domain layer's quantum state and circuit functionality.
  It serves as both a reference implementation and testing backend for
  quantum algorithms under realistic noise conditions.
 
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
  (:require [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.channel :as channel]
            [org.soulspace.qclojure.domain.noise :as noise]))

;; Noisy simulator state management
(defonce state (atom {:job-counter 0
                      :active-jobs {}}))

;; Job record for noisy simulations
(defrecord NoisySimulatorJob
           [job-id circuit options noise-model status result created-at completed-at])

;; Helper functions for job management
(defn- generate-noisy-job-id []
  (let [new-counter (:job-counter (swap! state update :job-counter inc))]
    (str "noisy_job_" new-counter "_" (System/currentTimeMillis))))

;; Advanced noise model specifications (defined in application.noise namespace)
;; Using specs from org.soulspace.qclojure.application.noise

;; Quantum channel functions are now directly called from domain.channel namespace
;; This provides clean separation without unnecessary delegation layer

#_(defn- execute-single-noisy-shot
    "Execute a single shot of a noisy quantum circuit.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - noise-model: Noise model configuration
  - num-qubits: Number of qubits in the circuit
  
  Returns: Single measurement outcome as a basis string"
    [circuit noise-model num-qubits]
    (loop [current-state (qs/zero-state num-qubits)
           remaining-gates (:operations circuit)]

      (if (empty? remaining-gates)
        ;; Final measurement with readout noise  
        (let [clean-outcome (:outcome (qs/measure-state current-state))
              clean-bitstring (qs/basis-string clean-outcome num-qubits)

              ;; Apply readout noise if configured
              final-bitstring (if-let [readout-config (:readout-error noise-model)]
                                (let [{:keys [prob-0-to-1 prob-1-to-0]} readout-config]
                                  (apply str
                                         (map-indexed
                                          (fn [_qubit-idx bit]
                                            (let [flip-prob (case bit
                                                              \0 prob-0-to-1
                                                              \1 prob-1-to-0)]
                                              (if (< (rand) flip-prob)
                                                (case bit \0 "1" \1 "0" bit)
                                                bit)))
                                          clean-bitstring)))
                                clean-bitstring)]
          final-bitstring)

        ;; Apply gate with noise
        (let [gate (first remaining-gates)
              gate-type (:operation-type gate)
              target-qubit (get-in gate [:operation-params :target] 0)

              ;; Apply clean gate operation first
              post-gate-state (qc/apply-operation-to-state current-state gate)

              ;; Apply noise based on gate-specific configuration
              final-gate-state (if-let [gate-noise-config (get-in noise-model [:gate-noise gate-type])]
                                 ;; Apply noise after the gate operation
                                 (let [{:keys [noise-type noise-strength t1-time t2-time gate-time]} gate-noise-config]
                                   (case noise-type
                                     :depolarizing
                                     (let [kraus-ops (channel/depolarizing-kraus-operators noise-strength)]
                                       (channel/apply-quantum-channel post-gate-state kraus-ops target-qubit))

                                     :amplitude-damping
                                     (let [decoherence (if (and t1-time gate-time)
                                                         (channel/calculate-decoherence-params t1-time (or t2-time (* t1-time 2)) gate-time)
                                                         {:gamma-1 noise-strength :gamma-2 0})
                                           kraus-ops (channel/amplitude-damping-kraus-operators (:gamma-1 decoherence))]
                                       (channel/apply-quantum-channel post-gate-state kraus-ops target-qubit))

                                     :phase-damping
                                     (let [decoherence (if (and t2-time gate-time)
                                                         (channel/calculate-decoherence-params (or t1-time (* t2-time 2)) t2-time gate-time)
                                                         {:gamma-1 0 :gamma-2 noise-strength})
                                           kraus-ops (channel/phase-damping-kraus-operators (:gamma-2 decoherence))]
                                       (channel/apply-quantum-channel post-gate-state kraus-ops target-qubit))

                                     :coherent
                                     (let [coherent-config (get gate-noise-config :coherent-error {:rotation-angle 0.01 :rotation-axis :z})
                                           kraus-op (channel/coherent-error-kraus-operator
                                                     (:rotation-angle coherent-config)
                                                     (:rotation-axis coherent-config))]
                                       (channel/apply-single-qubit-kraus-operator post-gate-state kraus-op target-qubit))

                                     ;; Default: no noise
                                     post-gate-state))
                                 ;; No noise configuration for this gate type
                                 post-gate-state)]

          (recur final-gate-state (rest remaining-gates))))))

(defn- execute-single-noisy-shot
  "Execute a single shot of a noisy quantum circuit.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - noise-model: Noise model configuration
  - num-qubits: Number of qubits in the circuit
  
  Returns: Single measurement outcome as a basis string"
  [circuit noise-model num-qubits]
  (loop [current-state (qs/zero-state num-qubits)
         remaining-gates (:operations circuit)]

    (if (empty? remaining-gates)
      ;; Final measurement with readout noise  
      (noise/apply-readout-noise current-state num-qubits noise-model)

      ;; Apply gate with noise
      (let [gate (first remaining-gates)
            final-gate-state (noise/apply-gate-noise current-state gate noise-model)]
        (recur final-gate-state (rest remaining-gates))))))

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

          ;; Execute single shot with fresh noise
          (let [outcome-bitstring (execute-single-noisy-shot circuit noise-model num-qubits)
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
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))

;;
;; Noise Model Management
;;
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


;;
;; Noisy Simulator Implementation
;;
(deftype LocalNoisyQuantumSimulator [noise-model config]
  qb/QuantumBackend

  (get-backend-info [_this]
    {:backend-type :advanced-noisy-simulator
     :backend-name "Advanced Noisy Quantum Simulator"
     :description "Realistic quantum simulator with comprehensive noise modeling"
     :noise-model noise-model
     :config config
     :capabilities #{:depolarizing-noise :amplitude-damping :phase-damping
                     :coherent-errors :readout-errors :decoherence-modeling}
     :version "1.0.0"})

  (get-supported-gates [_this] #{:x :y :z :h :cnot :rx :ry :rz})
  (is-available? [_this] true)

  (submit-circuit [_this circuit options]
    (let [job-id (generate-noisy-job-id)
          job (->NoisySimulatorJob job-id circuit options noise-model :queued nil
                                   (System/currentTimeMillis) nil)]

      ;; Store job and start execution in background
      (swap! state assoc-in [:active-jobs job-id] job)

      ;; Execute immediately in future (async execution)
      (future
        (let [result (execute-noisy-circuit-simulation circuit options noise-model)
              completed-job (assoc job
                                   :status (:job-status result)
                                   :result result
                                   :completed-at (System/currentTimeMillis))]
          (swap! state assoc-in [:active-jobs job-id] completed-job)))

      job-id))

  (get-job-status [_this job-id]
    (if-let [job (get (:active-jobs @state) job-id)]
      (:status job)
      :not-found))

  (get-job-result [_this job-id]
    (if-let [job (get (:active-jobs @state) job-id)]
      (if (= (:status job) :completed)
        (assoc (:result job) :job-id job-id)
        {:job-id job-id
         :job-status (:status job)
         :error-message "Job not completed"})
      {:job-id job-id
       :job-status :not-found
       :error-message "Job not found"}))

  (cancel-job [_this job-id]
    (if-let [job (get (:active-jobs @state) job-id)]
      (if (#{:queued :running} (:status job))
        (do
          (swap! state assoc-in [:active-jobs job-id]
                 (assoc job :status :cancelled
                        :completed-at (System/currentTimeMillis)))
          :cancelled)
        :already-completed)
      :not-found))

  (get-queue-status [_this]
    (let [jobs (vals (:active-jobs @state))
          active-count (count (filter #(#{:queued :running} (:status %)) jobs))
          completed-count (count (filter #(= :completed (:status %)) jobs))]
      {:total-jobs (count jobs)
       :active-jobs active-count
       :completed-jobs completed-count}))

  ;; Add CloudQuantumBackend implementation
  qb/CloudQuantumBackend

  (authenticate [_this _credentials]
    {:status :authenticated
     :session-id (str "noisy_sim_session_" (System/currentTimeMillis))
     :expires-at (+ (System/currentTimeMillis) (* 24 60 60 1000))})

  (get-session-info [_this]
    {:status :authenticated
     :backend-type :advanced-noisy-simulator
     :session-id "noisy_sim_session_mock"
     :authenticated-at (System/currentTimeMillis)})

  (list-available-devices [_this]
    [{:device-id "noisy-simulator-1"
      :device-name "Advanced Noisy Quantum Simulator"
      :device-status :online
      :max-qubits (get config :max-qubits 20)
      :backend-type :advanced-noisy-simulator
      :noise-model noise-model}])

  (get-device-topology [_this device-id]
    (let [max-qubits (get config :max-qubits 20)
          coupling-map (for [i (range max-qubits)
                             j (range max-qubits)
                             :when (not= i j)]
                         [i j])]
      {:device-id device-id
       :device-name "Advanced Noisy Quantum Simulator"
       :coupling-map coupling-map
       :max-qubits max-qubits
       :noise-model noise-model}))

  (get-calibration-data [_this device-id]
    {:device-id device-id
     :timestamp (java.time.Instant/now)
     :noise-model noise-model
     :coherence-times (vec (repeat (get config :max-qubits 20) 100000))})

  (estimate-cost [_this _circuit _options]
    {:total-cost 0.0
     :currency "USD"
     :cost-breakdown {:circuit-cost 0.0 :shot-cost 0.0 :device-cost 0.0}
     :estimated-credits 0})

  (batch-submit [this circuits options]
    (let [batch-id (str "noisy_batch_" (System/currentTimeMillis))
          job-ids (mapv #(qb/submit-circuit this % options) circuits)]
      (swap! state assoc-in [:active-jobs batch-id]
             {:batch-id batch-id
              :job-ids job-ids
              :status :queued
              :created-at (System/currentTimeMillis)})
      batch-id))

  (get-batch-status [this batch-job-id]
    (if-let [batch-info (get (:active-jobs @state) batch-job-id)]
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
    (if-let [batch-info (get (:active-jobs @state) batch-job-id)]
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

;; Factory functions for advanced simulators
(defn create-noisy-simulator
  "Create a local noisy quantum simulator with comprehensive noise modeling.
  
  Parameters:
  - noise-model: Advanced noise model configuration
  - config: Optional simulator configuration
  
  Returns: LocalNoisyQuantumSimulator instance"
  ([noise-model]
   (create-noisy-simulator noise-model {}))
  ([noise-model config]
   (->LocalNoisyQuantumSimulator noise-model config)))

;; Utility functions for noisy simulator state management
(defn reset-simulator-state!
  "Reset the noisy simulator state, clearing all jobs and counters.
  
  Returns: Updated state"
  []
  (reset! state {:job-counter 0 :active-jobs {}}))

(defn get-simulator-stats
  "Get statistics about the noisy simulator state.
  
  Returns: Map with job counts and statistics"
  []
  (let [current-state @state
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
         current-jobs (:active-jobs @state)
         jobs-to-keep (into {}
                            (filter
                             (fn [[_job-id job]]
                               (or (not= (:status job) :completed)
                                   (> (:completed-at job 0) cutoff-time)))
                             current-jobs))
         removed-count (- (count current-jobs) (count jobs-to-keep))]
     (swap! state assoc :active-jobs jobs-to-keep)
     removed-count)))


(comment

  (require '[org.soulspace.qclojure.application.noise-analysis :as napp])

  (noise-model-for :ibm-lagos)

  ;; Create advanced simulators with different noise characteristics
  (def ibm-sim (create-noisy-simulator (noise-model-for :ibm-lagos-noise)))
  (def rigetti-sim (create-noisy-simulator (noise-model-for :rigetti-aspen-noise)))
  (def ideal-sim (create-noisy-simulator {}))

  ;; === Amazon Braket Hardware Models ===

  ;; IonQ trapped ion systems (high fidelity, slow gates)
  (def ionq-harmony-sim (create-noisy-simulator (noise-model-for :ionq-harmony-noise)))
  (def ionq-aria-sim (create-noisy-simulator (noise-model-for :ionq-aria-noise)))
  (def ionq-forte-sim (create-noisy-simulator (noise-model-for :ionq-forte-noise)))

  ;; Rigetti superconducting systems (fast gates, moderate fidelity)
  (def rigetti-m3-sim (create-noisy-simulator (noise-model-for :rigetti-aspen-m3-noise)))

  ;; Photonic and neutral atom systems (unique characteristics)
  (def xanadu-sim (create-noisy-simulator (noise-model-for :xanadu-x-series-noise)))
  (def quera-sim (create-noisy-simulator (noise-model-for :quera-aquila-noise)))

  ;; Test Bell circuit across all platforms
  (def bell-circuit
    (-> (qc/create-circuit 2 "Bell State")
        (qc/h-gate 0)
        (qc/cnot-gate 0 1)))

  ;; Compare all Amazon Braket platforms
  (def all-models (all-noise-models))
  (def bell-comparison (napp/compare-hardware-platforms bell-circuit all-models))

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

  (def grover-comparison (napp/compare-hardware-platforms grover-circuit all-models))

  (println "\n=== Platform Comparison (Complex Grover Circuit) ===")
  (doseq [[platform data] (sort-by (comp :estimated-fidelity second) > grover-comparison)]
    (println (format "%s: %.4f fidelity (%.1f%% total error)"
                     (name platform)
                     (:estimated-fidelity data)
                     (* 100 (:total-estimated-error data)))))

  ;; Platform-specific insights
  (println "\n=== Platform Characteristics ===")
  (println "Trapped Ion (IonQ):")
  (println "  - Highest fidelity for complex circuits")
  (println "  - Slower gate times (μs scale)")
  (println "  - All-to-all connectivity")
  (println "  - Best for: Deep circuits, high-fidelity algorithms")

  (println "\nSuperconducting (Rigetti, IBM):")
  (println "  - Fast gate times (ns scale)")
  (println "  - Limited connectivity")
  (println "  - Good for: NISQ algorithms, near-term applications")

  (println "\nPhotonic (Xanadu):")
  (println "  - Continuous variable operations")
  (println "  - High optical losses")
  (println "  - Good for: Specific algorithms like Gaussian boson sampling")

  (println "\nNeutral Atom (QuEra):")
  (println "  - Programmable connectivity")
  (println "  - Unique Rydberg interactions")
  (println "  - Good for: Optimization, many-body physics")

  ;; Individual noise channel testing
  (def test-state (qs/plus-state))

  ;; Depolarizing noise
  (def depol-ops (channel/depolarizing-kraus-operators 0.1))
  (def state (channel/apply-quantum-channel test-state depol-ops 0))
  (println "\nDepolarizing noise result:" (:state-vector state))

  ;; Amplitude damping
  (def amp-ops (channel/amplitude-damping-kraus-operators 0.1))
  (def damped-state (channel/apply-quantum-channel test-state amp-ops 0))
  (println "Amplitude damping result:" (:state-vector damped-state))

  ;; Decoherence parameters for different platforms
  (def ionq-decoherence (channel/calculate-decoherence-params 15000.0 7000.0 40000.0))
  (def rigetti-decoherence (channel/calculate-decoherence-params 45.0 35.0 200.0))

  (println "\nDecoherence comparison:")
  (println "  IonQ Aria (T1=15ms, T2=7ms, gate=40μs):" ionq-decoherence)
  (println "  Rigetti M3 (T1=45μs, T2=35μs, gate=200ns):" rigetti-decoherence)
  )

;; Advanced noise modeling capabilities:
;; 1. Realistic Amazon Braket device parameters
;; 2. Platform-specific noise characteristics
;; 3. Gate-specific noise strengths and timings
;; 4. Multiple noise channels (depolarizing, amplitude/phase damping, coherent errors)
;; 5. Readout errors with platform-specific rates
;; 6. Circuit fidelity estimation and platform comparison
;; 7. Real device calibration data integration
;; 8. Cross-platform benchmarking capabilities
  

