(ns org.soulspace.qclojure.adapter.backend.simulator
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
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;; Specs for simulator
(s/def ::max-qubits pos-int?)
(s/def ::seed int?)

(s/def ::simulator-config
  (s/keys :opt-un [::max-qubits ::seed]))

;; Simulator state management
(defonce ^:private state (atom {:job-counter 0
                                :active-jobs {}}))

;; Job state record
(defrecord SimulatorJob
  [job-id circuit options status result created-at completed-at])

;; Helper functions
(defn- generate-job-id []
  (let [new-counter (:job-counter (swap! state update :job-counter inc))]
    (str "sim_job_" new-counter "_" (System/currentTimeMillis))))

;; Helper functions for measurement simulation
(defn- get-measurement-probabilities
  "Get measurement probabilities for all basis states.
  
  Parameters:
  - state: Quantum state
  
  Returns: Vector of probabilities for each basis state"
  [state]
  (let [num-states (count (:state-vector state))]
    (mapv #(qs/probability state %) (range num-states))))

(defn- measure-quantum-state
  "Perform measurement simulation on a quantum state.
  
  Parameters:
  - state: Quantum state
  - shots: Number of measurement shots
  - num-qubits: Number of qubits in the system
  
  Returns: Map of measurement outcomes to counts"
  [state shots num-qubits]
  (let [probabilities (get-measurement-probabilities state)
        ;; Convert to computational basis state labels
        basis-states (map #(qs/basis-string % num-qubits) 
                         (range (count probabilities)))
        outcomes (into {} (map vector basis-states probabilities))]
    
    ;; Simulate shot-by-shot measurement
    (loop [shots-remaining shots
           results {}]
      (if (zero? shots-remaining)
        results
        (let [;; Sample from probability distribution
              rand-val (rand)
              ;; Find which outcome this sample corresponds to
              outcome (loop [cumulative-prob 0.0
                           remaining-outcomes (seq outcomes)]
                       (if (empty? remaining-outcomes)
                         (first basis-states) ; fallback
                         (let [[outcome prob] (first remaining-outcomes)
                               new-cumulative (+ cumulative-prob prob)]
                           (if (< rand-val new-cumulative)
                             outcome
                             (recur new-cumulative (rest remaining-outcomes))))))]
          (recur (dec shots-remaining)
                (update results outcome (fnil inc 0))))))))

(defn- execute-circuit-simulation
  "Execute a quantum circuit simulation.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - options: Execution options including shot count
  
  Returns: Simulation results"
  [circuit options]
  (try
    (let [start-time (System/currentTimeMillis)
          num-qubits (:num-qubits circuit)
          shots (get options :shots 1024)

          ;; Create initial state and execute the circuit
          initial-state (if (:initial-state options)
                          (:initial-state options)
                          (qs/zero-state num-qubits))
          final-state (:final-state (qc/execute-circuit circuit initial-state))

          ;; Perform measurements
          measurement-results (measure-quantum-state
                               final-state
                               shots
                               num-qubits)]
      
      {:job-status :completed
       :measurement-results measurement-results
       :final-state final-state
       :execution-time-ms (- (System/currentTimeMillis) start-time)})

    (catch Exception e
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))

;; Simulator backend implementation
(deftype LocalQuantumSimulator [config]
  qb/QuantumBackend
  
  (get-backend-info [_this]
    {:backend-type :simulator
     :backend-name "Local Quantum Simulator"
     :description "Local simulator for quantum circuits using matrix operations"
     :backend-config config
     :max-qubits (get config :max-qubits 20)
     :capabilities #{:simulation :measurement :statevector :batch-execution}
     :supported-gates gr/native-simulator-gates
     :version "1.0.0"})
  
  (get-supported-gates [_this]
    ;; Return the supported gates from config or default to all simulator gates
    (get config :supported-gates gr/native-simulator-gates))
  
  (is-available? [_this]
    true) ; Simulator is always available
  
  (submit-circuit [_this circuit options]
    (let [job-id (generate-job-id)
          job (->SimulatorJob job-id circuit options :queued nil 
                             (System/currentTimeMillis) nil)]
      
      ;; Store job and start execution in background
      (swap! state assoc-in [:active-jobs job-id] job)
      
      ;; Execute immediately (could be made async with future)
      (future
        (let [result (execute-circuit-simulation circuit options)
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
        :cannot-cancel)
      :not-found))
  
  (get-queue-status [_this]
    (let [jobs (vals (:active-jobs @state))
          queued (count (filter #(= (:status %) :queued) jobs))
          running (count (filter #(= (:status %) :running) jobs))
          completed (count (filter #(= (:status %) :completed) jobs))]
      
      {:total-jobs (count jobs)
       :queued queued
       :running running
       :completed completed
       :backend-load 0.0 ; Simulator has no real load
       :estimated-wait-time 0}))
  
  ;; Mock cloud backend implementation for testing
  qb/CloudQuantumBackend
  
  (authenticate [_this _credentials]
    ;; Mock authentication - always succeeds for simulator
    {:status :authenticated
     :session-id (str "sim_session_" (System/currentTimeMillis))
     :expires-at (+ (System/currentTimeMillis) (* 24 60 60 1000))}) ; 24 hours
  
  (get-session-info [_this]
    ;; Mock session - always authenticated
    {:status :authenticated
     :backend-type :simulator
     :session-id "sim_session_mock"
     :authenticated-at (System/currentTimeMillis)})
  
  (list-available-devices [_this]
    ;; Mock device list for simulator
    [{:device-id "simulator-1"
      :device-name "Local Quantum Simulator"
      :device-status :online
      :max-qubits (get config :max-qubits 20)
      :backend-type :simulator}])
  
  (get-device-topology [_this device-id]
    ;; Mock topology - all-to-all connectivity for simulator
    (let [max-qubits (get config :max-qubits 20)
          coupling-map (for [i (range max-qubits)
                            j (range max-qubits)
                            :when (not= i j)]
                        [i j])]
      {:device-id device-id
       :device-name "Local Quantum Simulator"
       :coupling-map coupling-map
       :max-qubits max-qubits
       :gate-times {:hadamard 10 :cnot 20 :rotation 15}
       :gate-errors {:hadamard 0.001 :cnot 0.01 :rotation 0.005}}))
  
  (get-calibration-data [_this device-id]
    ;; Mock calibration data for simulator
    {:device-id device-id
     :timestamp (java.time.Instant/now)
     :gate-times {:hadamard 10 :cnot 20 :rotation 15}
     :gate-errors {:hadamard 0.001 :cnot 0.01 :rotation 0.005}
     :readout-errors (vec (repeat (get config :max-qubits 20) 0.02))
     :coherence-times (vec (repeat (get config :max-qubits 20) 100000))})
  
  (estimate-cost [_this _circuit _options]
    ;; Mock cost estimation - free for simulator
    {:total-cost 0.0
     :currency "USD"
     :cost-breakdown {:circuit-cost 0.0
                     :shot-cost 0.0
                     :device-cost 0.0}
     :estimated-credits 0})
  
  (batch-submit [this circuits options]
    ;; Submit each circuit individually and track as batch
    (let [batch-id (str "batch_" (System/currentTimeMillis))
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
      {:batch-id batch-job-id
       :status :not-found}))
  
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
      {:batch-id batch-job-id
       :error-message "Batch not found"})))

;; Factory functions
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

;; Utility functions for testing and debugging
(defn reset-simulator-state!
  "Reset the simulator state, clearing all jobs.
  
  This is useful for testing and development."
  []
  (reset! state {:job-counter 0
                 :active-jobs {}}))

(defn get-simulator-stats
  "Get statistics about the simulator usage.
  
  Returns: Map with job statistics and performance metrics"
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

(comment  
  ;; Create a simulator
  (def sim (create-simulator {:max-qubits 10}))
  
  ;; Check backend info
  (qb/get-backend-info sim)
  
  ;; Create a simple circuit
  (def bell-circuit 
    (-> (qc/create-circuit 2 "Bell State")
        (qc/h-gate 0)
        (qc/cnot-gate 0 1)))
  
  ;; Execute synchronously
  (def result (qb/execute-circuit sim bell-circuit {:shots 1000}))
  
  ;; Analyze results
  (qb/analyze-measurement-results (:measurement-results result))
  
  ;; Execute asynchronously
  (def job-id (qb/execute-circuit-async sim bell-circuit))
  (qb/get-job-status sim job-id)
  (qb/get-job-result sim job-id)
  
  ;; Test with different algorithms
  (require '[org.soulspace.qclojure.application.algorithm.deutsch :as deutsch])
  
  ;; Deutsch algorithm
  (deutsch/deutsch-algorithm sim (fn [_] true))
  
  ;; Reset for clean testing
  (reset-simulator-state!)
  )
