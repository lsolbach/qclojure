(ns org.soulspace.qclojure.adapter.backend.ideal-simulator
  "Local quantum simulator implementing the QuantumBackend protocol.
  
  This adapter provides a local simulation of quantum circuits using
  the domain layer's quantum state and circuit functionality. It serves
  as both a reference implementation and a testing backend.
   
  This simulator implements an ideal quantum computer without noise,
  simulating quantum gates and measurements using matrix operations.
   
  The simulator supports asynchronous job management, allowing
  users to submit circuits and retrieve results later. It can be used
  for testing algorithms, circuit designs, and quantum operations
  without requiring access to actual quantum hardware.
   
  It also implements the CloudQuantumBackend protocol for mock cloud
  backend functionality, allowing it to be used in a cloud-like
  environment for testing purposes."
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]))

;;;
;;; Specs for simulator
;;;
(s/def ::seed int?)

(s/def ::simulator-config
  (s/keys :opt-un [::num-qubits ::seed]))

;;
(def device
  {:id :ideal-simulator
   :name "Ideal Quantum Simulator"
   :provider "QClojure"
   :platform "Local"
   :technology :simulator
   :num-qubits 20  ; Limited by classical memory
   :topology :all-to-all
   :connectivity :full
   :native-gates opreg/native-simulator-gate-set
   :virtual-gates #{}
   :supported-operations opreg/native-simulator-gate-set
   :measurement-basis :any
   :noise-model {}
   :performance {:gate-fidelity 1.0
                 :readout-fidelity 1.0
                 :gate-times {:all 0}    ; Instantaneous
                 :simulation-time {:exponential-scaling true}}})

;;;
;;; Simulator state management
;;;
(defonce ^:private simulator-state (atom {:job-counter 0
                                          :active-jobs {}}))

;; Job state record
(defrecord SimulatorJob
  [job-id circuit options status result created-at completed-at])

;;;
;;; Helper functions
;;;
(defn- generate-job-id []
  (let [new-counter (:job-counter (swap! simulator-state update :job-counter inc))]
    (str "sim_job_" new-counter "_" (System/currentTimeMillis))))

(defn- execute-circuit
  "Execute a quantum circuit simulation.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - options: Execution options including shot count and result specification
  
  Returns: A map with the results of the simulation."
  [circuit options]
  (try
    (let [start-time (System/currentTimeMillis)
          num-qubits (:num-qubits circuit)
          result-specs (:result-specs options)

          ;; Create initial state and execute the circuit
          initial-state (if (:initial-state options)
                          (:initial-state options)
                          (state/zero-state num-qubits))
          results (circuit/execute-circuit circuit initial-state result-specs)]
      {:job-status :completed
       :results results
       :execution-time-ms (- (System/currentTimeMillis) start-time)})

    (catch Exception e
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))
;;;
;;; Simulator backend implementation
;;;
(defrecord LocalQuantumSimulator [config]
  backend/QuantumBackend

  (backend-info [_this]
    {:backend-type :simulator
     :backend-name "Local Ideal Quantum Simulator"
     :description "Local ideal simulator for quantum circuits using matrix operations"
     :backend-config config
     :max-qubits (get config :max-qubits 16)
     :capabilities #{:quantum-backend}
     :device device
     :version "1.0.0"})

  (device [_this]
    device)

  (available?
    ; Always available for local simulation
    [_this] true)

  (submit-circuit [_this circuit options]
    (let [job-id (generate-job-id)
          job (->SimulatorJob job-id circuit options :queued nil
                              (System/currentTimeMillis) nil)]

      ;; Store job and start execution in background
      (swap! simulator-state assoc-in [:active-jobs job-id] job)

      ;; Execute immediately (could be made async with future)
      (future
        (let [result (execute-circuit circuit options)
              completed-job (assoc job
                                   :status (:job-status result)
                                   :result result
                                   :completed-at (System/currentTimeMillis))]
          (swap! simulator-state assoc-in [:active-jobs job-id] completed-job)))
      job-id))

  (job-status [_this job-id]
    (if-let [job (get (:active-jobs @simulator-state) job-id)]
      (:status job)
      :not-found))

  (job-result [_this job-id]
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
        :cannot-cancel)
      :not-found))

  (queue-status [_this]
    (let [jobs (vals (:active-jobs @simulator-state))
          queued (count (filter #(= (:status %) :queued) jobs))
          running (count (filter #(= (:status %) :running) jobs))
          completed (count (filter #(= (:status %) :completed) jobs))]

      {:total-jobs (count jobs)
       :queued queued
       :running running
       :completed completed
       :backend-load 0.0 ; Simulator has no real load
       :estimated-wait-time 0})))

;;;
;;; Factory functions
;;;
(defn create-simulator
  "Create a new local quantum simulator backend.
  
  Parameters:
  - config: Optional configuration map
    - :max-qubits - Maximum number of qubits to simulate (default: 20)
    - :noise-model - Noise model for realistic simulation (not implemented)
    - :seed - Random seed for reproducible results
  
  Returns: LocalQuantumSimulator instance"
  ([]
   (create-simulator {}))
  ([config]
   {:pre [(map? config)]}
   (->LocalQuantumSimulator config)))

;;;
;;; Utility functions for testing and debugging
;;;
(defn reset-simulator-state!
  "Reset the simulator state, clearing all jobs.
  
  This is useful for testing and development."
  []
  (reset! simulator-state {:job-counter 0
                 :active-jobs {}}))

(defn get-simulator-stats
  "Get statistics about the simulator usage.
  
  Returns: Map with job statistics and performance metrics"
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
  ;; Create a simulator
  (def sim (create-simulator {:max-qubits 10}))
  
  ;; Check backend info
  (backend/backend-info sim)
  
  ;; Create a simple circuit
  (def bell-circuit 
    (-> (circuit/create-circuit 2 "Bell State")
        (circuit/h-gate 0)
        (circuit/cnot-gate 0 1)))
  
  ;; Execute synchronously
  (def result (backend/execute-circuit sim bell-circuit {:shots 1000}))
  
  ;; Analyze results
  (backend/analyze-measurement-results (:measurement-results result))
  
  ;; Execute asynchronously
  (def job-id (backend/execute-circuit-async sim bell-circuit))
  (backend/job-status sim job-id)
  (backend/job-result sim job-id)
  
  ;; Test with different algorithms
  (require '[org.soulspace.qclojure.application.algorithm.deutsch :as deutsch])
  
  ;; Deutsch algorithm
  (deutsch/deutsch-algorithm sim (fn [_] true))
  
  ;; Reset for clean testing
  (reset-simulator-state!)
  )
