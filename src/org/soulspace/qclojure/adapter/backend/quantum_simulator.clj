(ns org.soulspace.qclojure.adapter.backend.quantum-simulator
  "Local quantum simulator implementing the QuantumBackend protocol.
  
  This adapter provides a local simulation of quantum circuits using
  the domain layer's quantum state and circuit functionality. It serves
  as both a reference implementation and a testing backend."
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.application.quantum-backend :as qb]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.gate-registry :as gr]))

;; Simulator state management
(defonce ^:private job-counter (atom 0))
(defonce ^:private active-jobs (atom {}))

;; Specs for simulator
(s/def ::simulator-config
  (s/keys :opt-un [::max-qubits ::noise-model ::seed]))

(s/def ::max-qubits pos-int?)
(s/def ::noise-model map?)
(s/def ::seed int?)

;; Job state record
(defrecord SimulatorJob
  [job-id circuit options status result created-at completed-at])

;; Helper functions
(defn- generate-job-id []
  (str "sim_job_" (swap! job-counter inc) "_" (System/currentTimeMillis)))

;; Helper functions for measurement simulation
(defn- index-to-basis-string
  "Convert a basis state index to its binary string representation.
  
  Parameters:
  - index: Integer index of the basis state  
  - num-qubits: Number of qubits in the system
  
  Returns: String representation like '00', '01', '10', '11' for 2 qubits"
  [index num-qubits]
  (let [binary-str (Integer/toBinaryString index)
        padding-needed (- num-qubits (count binary-str))
        padding (apply str (repeat padding-needed "0"))]
    (str padding binary-str)))

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
        ;; Convert to computational basis state strings
        basis-states (map #(index-to-basis-string % num-qubits) 
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
          initial-state (qs/zero-state num-qubits)
          final-state (qc/execute-circuit circuit initial-state)
          
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
     :capabilities #{:simulation :measurement :statevector}
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
      (swap! active-jobs assoc job-id job)
      
      ;; Execute immediately (could be made async with future)
      (future
        (let [result (execute-circuit-simulation circuit options)
              completed-job (assoc job 
                                  :status (:job-status result)
                                  :result result
                                  :completed-at (System/currentTimeMillis))]
          (swap! active-jobs assoc job-id completed-job)))
      
      job-id))
  
  (get-job-status [this job-id]
    (if-let [job (get @active-jobs job-id)]
      (:status job)
      :not-found))
  
  (get-job-result [this job-id]
    (if-let [job (get @active-jobs job-id)]
      (if (= (:status job) :completed)
        (assoc (:result job) :job-id job-id)
        {:job-id job-id
         :job-status (:status job)
         :error-message "Job not completed"})
      {:job-id job-id
       :job-status :not-found
       :error-message "Job not found"}))
  
  (cancel-job [this job-id]
    (if-let [job (get @active-jobs job-id)]
      (if (#{:queued :running} (:status job))
        (do
          (swap! active-jobs assoc job-id 
                (assoc job :status :cancelled 
                          :completed-at (System/currentTimeMillis)))
          :cancelled)
        :cannot-cancel)
      :not-found))
  
  (get-queue-status [this]
    (let [jobs (vals @active-jobs)
          queued (count (filter #(= (:status %) :queued) jobs))
          running (count (filter #(= (:status %) :running) jobs))
          completed (count (filter #(= (:status %) :completed) jobs))]
      
      {:total-jobs (count jobs)
       :queued queued
       :running running
       :completed completed
       :backend-load 0.0 ; Simulator has no real load
       :estimated-wait-time 0})))

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

(defn create-noisy-simulator
  "Create a quantum simulator with noise modeling.
  
  This is a placeholder for future noise model implementation.
  
  Parameters:
  - noise-config: Configuration for noise modeling
  
  Returns: LocalQuantumSimulator with noise"
  [noise-config]
  (create-simulator {:noise-model noise-config}))

;; Utility functions for testing and debugging
(defn reset-simulator-state!
  "Reset the simulator state, clearing all jobs.
  
  This is useful for testing and development."
  []
  (reset! active-jobs {})
  (reset! job-counter 0))

(defn get-simulator-stats
  "Get statistics about the simulator usage.
  
  Returns: Map with job statistics and performance metrics"
  []
  (let [jobs (vals @active-jobs)
        completed-jobs (filter #(= (:status %) :completed) jobs)
        execution-times (keep #(let [start (:created-at %)
                                     end (:completed-at %)]
                                 (when (and start end)
                                   (- end start)))
                             completed-jobs)]
    
    {:total-jobs (count jobs)
     :completed-jobs (count completed-jobs)
     :average-execution-time (if (seq execution-times)
                               (/ (reduce + execution-times) 
                                  (count execution-times))
                               0)
     :job-counter @job-counter}))

(comment
  ;; Example usage:
  
  ;; Create a simulator
  (def sim (create-simulator {:max-qubits 10}))
  
  ;; Check backend info
  (qb/get-backend-info sim)
  
  ;; Create a simple circuit
  (def bell-circuit 
    (-> (qc/create-circuit 2 "Bell State")
        (qc/add-gate :hadamard {:qubit-target 0})
        (qc/add-gate :cnot {:qubit-control 0 :qubit-target 1})))
  
  ;; Execute synchronously
  (def result (qb/execute-circuit sim bell-circuit {:shots 1000}))
  
  ;; Analyze results
  (qb/analyze-measurement-results (:measurement-results result))
  
  ;; Execute asynchronously
  (def job-id (qb/execute-circuit-async sim bell-circuit))
  (qb/get-job-status sim job-id)
  (qb/get-job-result sim job-id)
  
  ;; Test with different algorithms
  (require '[qclojure.application.quantum-algorithms :as qa])
  
  ;; Deutsch algorithm
  (def deutsch-circuit (:circuit (qa/deutsch-algorithm (fn [_] true))))
  (qb/execute-circuit sim deutsch-circuit)
  
  ;; Reset for clean testing
  (reset-simulator-state!)
  )
