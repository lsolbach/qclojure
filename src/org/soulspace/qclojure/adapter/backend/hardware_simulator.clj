(ns org.soulspace.qclojure.adapter.backend.hardware-simulator
  "Quantum hardware simulator backend implementation with realistic noise modeling.
   
   This backend simulates quantum circuits on a virtual quantum device,
   incorporating various noise models to mimic real quantum hardware behavior.
   It supports asynchronous job execution, job tracking, and comprehensive
   result extraction including state trajectories and density matrix calculations.
   The backend adheres to the QuantumBackend protocol, ensuring compatibility
   with the broader QClojure framework.
   
   The simulator backend supports predefined quantum devices with specific topologies
   and noise characteristics, as well as custom configurations.
   Noise models can include gate errors, readout errors, and decoherence effects,
   allowing for realistic simulation of quantum algorithms under noisy conditions.
   
   The simulator backend is stateful, managing job submissions and their execution status.
   It also tracks the current device and allows for device selection.

   Key Features:
   - Realistic noise modeling (depolarizing, amplitude damping, phase damping, etc.)
   - Asynchronous job submission and tracking
   - Comprehensive result extraction (measurement results, expectation values, density matrices)
   - Support for predefined quantum devices and custom configurations
   - Integration with QClojure's optimization and circuit transformation pipelines
   - Adherence to the QuantumBackend protocol for seamless integration
   "
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.result :as result]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]
            [org.soulspace.qclojure.domain.noise :as noise]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.application.hardware-optimization :as hwopt]
            [org.soulspace.qclojure.domain.topology :as topo]
            [org.soulspace.qclojure.domain.device :as device]))

;; Predefined quantum devices
(def device-list
  (edn/read-string
   (slurp (io/resource "simulator-devices.edn"))))

(def device-map
  (into {} (map backend/device-entry device-list)))

;;;
;;; Hardware simulator state management
;;;
(defonce simulator-state
  (atom {:job-counter 0
         :active-jobs {}
         :devices device-list
         :current-device (first device-list)}))

;;
;; Job record for noisy simulations
;;
(defrecord HardwareSimulatorJob
           [job-id circuit options status result created-at completed-at])

;;
;; Helper functions for job management
;;
(defn- generate-job-id []
  (let [new-counter (:job-counter (swap! simulator-state update :job-counter inc))]
    (str "job_" new-counter "_" (System/currentTimeMillis))))

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
    (let [clean-state (circuit/apply-operation-to-state state gate)]
      (noise/apply-gate-noise clean-state gate noise-model))))

(defn execute-single-noisy-shot-with-trajectory
  "Execute a single shot of a noisy quantum circuit, returning both outcome and final state.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - initial-state: Initial quantum state (optional, defaults to |00...0⟩)
  - noise-model: Noise model configuration
  
  Returns: Map containing :outcome (measurement bitstring) and :final-state (quantum state)"
  ([circuit noise-model]
   (execute-single-noisy-shot-with-trajectory circuit (state/zero-state (:num-qubits circuit)) noise-model))
  ([circuit initial-state noise-model]
   (let [num-qubits (:num-qubits circuit)
         ;; Apply all gates with noise
         final-state (reduce (gate-with-noise-applicator noise-model)
                             initial-state
                             (:operations circuit))
         ;; Final measurement with readout noise
         measurement (noise/apply-readout-noise final-state num-qubits noise-model)]
     {:outcome measurement
      :final-state final-state})))

(defn execute-circuit-simulation-with-trajectories
  "Execute a quantum circuit simulation collecting state trajectories.
  
  This enhanced version supports comprehensive result extraction similar to 
  the ideal simulator, including expectation values, variances, probabilities,
  and other quantum mechanical observables computed from the noisy simulation.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - initial-state: Initial quantum state for simulation
  - options: Execution options including shot count, trajectory collection settings and result specs
  - noise-model: Noise model configuration
  
  Returns: Simulation results including trajectory-based density matrix and extracted results"
  [circuit initial-state options noise-model]
  (try
    (let [start-time (System/currentTimeMillis)
          shots (get options :shots 1024)
          max-trajectories (get options :max-trajectories 100)
          result-specs (:result-specs options)] ; Extract result specifications

      ;; TODO check result specs/extraction

      ;; Execute each shot independently with fresh noise
      (loop [shot-count 0
             accumulated-results {}
             trajectories []
             last-final-state nil]

        (if (>= shot-count shots)
          ;; All shots completed, compile final results
          (let [base-result {:measurement-results accumulated-results
                             :final-state last-final-state}
                ;; Add trajectory-based results if collected and apply result extraction
                enhanced-result (if (seq trajectories)
                                  (let [density-matrix-result (state/trajectory-to-density-matrix trajectories)
                                        density-matrix (:density-matrix density-matrix-result)]
                                    (assoc base-result
                                           :trajectories trajectories
                                           :trajectory-count (count trajectories)
                                           :density-matrix density-matrix
                                           :density-matrix-trace (:trace density-matrix-result)
                                           :trajectory-weights (:weights density-matrix-result)))
                                  base-result)]

            ;; Apply result extraction if result specs are provided
            (if result-specs
              (let [extracted-results (result/extract-noisy-results enhanced-result result-specs circuit)]
                {:job-status :completed
                 :circuit circuit
                 :circuit-metadata {:circuit-depth (circuit/circuit-depth circuit)
                                    :circuit-operation-count (circuit/circuit-operation-count circuit)
                                    :circuit-gate-count (circuit/circuit-gate-count circuit)}
                 :shots-executed shots
                 :execution-time-ms (- (System/currentTimeMillis) start-time)
                 :results (merge enhanced-result extracted-results)})
              {:job-status :completed
               :circuit circuit
               :circuit-metadata {:circuit-depth (circuit/circuit-depth circuit)
                                  :circuit-operation-count (circuit/circuit-operation-count circuit)
                                  :circuit-gate-count (circuit/circuit-gate-count circuit)}
               :shots-executed shots
               :execution-time-ms (- (System/currentTimeMillis) start-time)
               :results enhanced-result}))

          ;; Execute single shot with fresh noise
          (let [shot-result (execute-single-noisy-shot-with-trajectory circuit initial-state noise-model)
                outcome-bitstring (:outcome shot-result)
                final-state (:final-state shot-result)
                updated-results (update accumulated-results outcome-bitstring (fnil inc 0))

                ;; Collect trajectory if enabled and under limit
                updated-trajectories (if (< (count trajectories) max-trajectories)
                                       (conj trajectories final-state)
                                       trajectories)]

            (recur (inc shot-count)
                   updated-results
                   updated-trajectories
                   final-state)))))

    (catch Exception e
      (.printStackTrace e)
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))

(defn execute-circuit
  "Execute a noisy quantum circuit with comprehensive result extraction.
  
  This function provides a high-level interface similar to the ideal simulator
  but with realistic noise modeling. It automatically enables trajectory 
  collection when density matrix-based calculations are needed.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - initial-state: Initial quantum state (optional, defaults to |00...0⟩)
  - options: Execution options including:
    - :shots - Number of measurement shots (default: 1024)
    - :result-specs - Map specifying what results to extract
    - :noise-model - Noise model configuration (optional)
    - :collect-trajectories - Enable trajectory collection (auto-detected from result specs)
    - :max-trajectories - Maximum trajectories to collect (default: 100)
  
  Returns: Comprehensive results with both raw simulation data and extracted observables
  
  Examples:
    (execute-circuit bell-circuit
      {:shots 1000
       :result-specs {:measurement {:shots 1000}
                      :expectation {:observables [obs/pauli-z obs/pauli-x]}
                      :density-matrix true}})
    
    (execute-circuit vqe-circuit qs/|00⟩ 
      {:shots 2000
       :result-specs {:hamiltonian {:hamiltonian h2-hamiltonian}
                      :state-vector true}
       :noise-model {:depolarizing-rate 0.01}})"
  [circuit device options]
  (let [initial-state (or (:initial-state options)
                          (state/zero-state (:num-qubits circuit)))
        result-specs (:result-specs options)
        noise-model (or (:noise-model device) {})

        ;; Auto-enable trajectory collection if density matrix operations are needed
        needs-trajectories? (or (:collect-trajectories options)
                                (:density-matrix result-specs)
                                (:hamiltonian result-specs)
                                (:expectation result-specs))

        enhanced-options (cond-> options
                           needs-trajectories? (assoc :collect-trajectories true)
                           (not (:max-trajectories options)) (assoc :max-trajectories 100))]

    (execute-circuit-simulation-with-trajectories circuit initial-state enhanced-options noise-model)))

;;;
;;; Accessor functions for device properties
;;;
(defn max-qubits
  "Get the maximum number of qubits supported by the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Maximum qubit count (default: 16)"
  [backend]
  (or (get-in backend [:config :max-qubits]) 16))

(defn native-gates
  "Get the set of native gates supported by the backend.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Set of native gate keywords (default: all QClojure gates)"
  [device]
  (or (get device :native-gates) opreg/native-simulator-gate-set))

(defn coupling
  "Get the qubit coupling of the device.
  
   Parameters:
   - device: Device map

   Returns: Qubit coupling as vector of vectors"
  ([device]
   (let [coupling (:coupling device)]
     (if coupling
       coupling
       (topo/fully-connected-coupling (:num-qubits device))))))

;;;
;;; Hardware Simulator Implementation
;;;
(defrecord QuantumHardwareSimulator [config]
  backend/QuantumBackend

  (backend-info [_this]
    {:backend-type :hardware-simulator
     :backend-name "Hardware Simulator"
     :devices (:devices @simulator-state)
     :device (:current-device @simulator-state)
     :config config
     :capabilities #{:multi-device}})

  (device [_this] (:current-device @simulator-state))

  (available? [_this] true)

  (submit-circuit [_this circuit options]
    (let [device (:current-device @simulator-state)
          job-id (generate-job-id)]
      (try
        ;; Optimize circuit for hardware
        (let [hw-circuit (hwopt/optimize
                          {:circuit circuit
                           :device device
                           :options (merge {:optimize-gates? (get options :optimize-gates? true)
                                            :optimize-qubits? (get options :optimize-qubits? true)
                                            :optimize-topology? (get options :optimize-topology? false)
                                            :transform-operations? (get options :transform-operations? true)
                                            :max-iterations (get options :max-iterations 100)}
                                           options)})
              job (->HardwareSimulatorJob job-id hw-circuit options :queued nil
                                          (System/currentTimeMillis) nil)]

          ;; Store job and start execution in background
          (swap! simulator-state assoc-in [:active-jobs job-id] job)

          ;; Execute immediately in future (async execution)
          (future
            (let [result (execute-circuit circuit device options)
                  completed-job (assoc job
                                       :status (:job-status result)
                                       :result result
                                       :completed-at (System/currentTimeMillis))]
              (swap! simulator-state assoc-in [:active-jobs job-id] completed-job)))
          job-id)

        (catch clojure.lang.ExceptionInfo e
          ;; Handle optimization errors (including empty circuits)
          (let [failed-job (->HardwareSimulatorJob
                            job-id
                            circuit
                            options
                            :failed
                            {:job-status :failed
                             :error-message (.getMessage e)}
                            (System/currentTimeMillis)
                            (System/currentTimeMillis))]
            ;; Store the failed job for tracking
            (swap! simulator-state assoc-in [:active-jobs job-id] failed-job)
            job-id)))))

  (job-status [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (:status job)
      :not-found))

  (job-result [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (cond
        (= (:status job) :completed)
        (assoc (:result job) :job-id job-id)

        (= (:status job) :failed)
        (:result job) ; Return the failure result directly

        :else
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

  (queue-status [_this]
    (let [jobs (vals (:active-jobs @simulator-state))
          active-count (count (filter #(#{:queued :running} (:status %)) jobs))
          completed-count (count (filter #(= :completed (:status %)) jobs))]
      {:total-jobs (count jobs)
       :active-jobs active-count
       :completed-jobs completed-count}))

  backend/MultiDeviceBackend
  (devices [_this]
    device-list)

  (select-device [_this device]
    (let [device (if (keyword? device)
                   (:device device-map)
                   device)]
      (swap! simulator-state assoc :current-device device)
      (:current-device @simulator-state))))

;;;
;;; Factory functions for hardware simulators
;;;
(defn create-hardware-simulator
  "Create a hardware simulator for a specific device.
  
   Parameters:
   - config: Optional simulator configuration
      - :max-qubits - Maximum number of qubits to simulate (default: 16)
   - device: Optional device map with max-qubits, native-gates, topology and/or coupling and noise-model
  
   Returns: QuantumHardwareSimulator instance"
  ([]
   (->QuantumHardwareSimulator {:max-qubits 16}))
  ([config]
   (->QuantumHardwareSimulator config))
  ([config device]
   (let [backend (->QuantumHardwareSimulator config)
         ;; add device to devices if not already present
         device (device/validate-device device)]
     (swap! simulator-state assoc :devices (conj (:devices @simulator-state) device))
     (backend/select-device backend device))))

;;;
;;; Utility functions for hardware simulator state management
;;;
(defn reset-simulator-state!
  "Reset the hardware simulator state, clearing all jobs and counters.
  
  Returns: Updated state"
  []
  (reset! simulator-state {:job-counter 0 :active-jobs {} :devices device-list
                           :current-device (first device-list)}))

(defn simulator-statistics
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
  ;; Create a hardware simulator
  )
