(ns org.soulspace.qclojure.application.quantum-backend
  "Protocol and interface for quantum computing hardware backends.
  
  This namespace defines the protocol for connecting to and executing
  quantum circuits on real quantum hardware or simulators. It provides
  a clean abstraction layer that allows the application to work with
  different quantum computing providers and simulators."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.gate-registry :as gr]))

;; Specs for hardware interface
(s/def ::backend-type #{:simulator :hardware :cloud})
(s/def ::backend-name string?)
(s/def ::backend-config map?)
(s/def ::job-id string?)
(s/def ::job-status #{:queued :running :completed :failed :cancelled})
(s/def ::shots pos-int?)
(s/def ::measurement-results (s/map-of string? nat-int?))
(s/def ::supported-gates ::gr/gate-set)

(s/def ::backend-info
  (s/keys :req-un [::backend-type ::backend-name ::capabilities ::supported-gates]
          :opt-un [::backend-config ::description]))

(s/def ::execution-options
  (s/keys :opt-un [::shots ::backend-info]))

(s/def ::job-result
  (s/keys :req-un [::job-id ::job-status]
          :opt-un [::measurement-results ::error-message]))

;; Protocol for quantum hardware backends
(defprotocol QuantumBackend
  "Protocol for quantum computing hardware backends.
  
  This protocol defines the interface that all quantum backends must implement,
  whether they are simulators, cloud services, or local hardware."
  
  (get-backend-info [this]
    "Get information about this backend including type, capabilities, and configuration.")
  
  (get-supported-gates [this]
    "Get the set of quantum gates supported by this backend.
    
    Returns: A set of gate keywords indicating which gates this backend
    can execute natively or through decomposition. The gate names correspond
    to those defined in org.soulspace.qclojure.domain.gate-registry.")

  (is-available? [this]
    "Check if the backend is currently available for job submission.")
  
  (submit-circuit [this circuit options]
    "Submit a quantum circuit for execution.
    
    Parameters:
    - circuit: A quantum circuit from qclojure.domain.quantum-circuit
    - options: Execution options including shot count
    
    Returns: A job ID for tracking the execution.")
  
  (get-job-status [this job-id]
    "Get the current status of a submitted job.")
  
  (get-job-result [this job-id]
    "Get the results of a completed job.
    
    Returns: A map containing measurement results and statistics.")
  
  (cancel-job [this job-id]
    "Cancel a queued or running job.")
  
  (get-queue-status [this]
    "Get information about the current job queue."))

;; High-level execution functions
(defn execute-circuit
  "Execute a quantum circuit on the specified backend.
  
  This is a convenience function that handles the full workflow:
  submit circuit, wait for completion, and return results.
  
  Parameters:
  - backend: An implementation of QuantumBackend protocol
  - circuit: Quantum circuit to execute
  - options: Execution options (defaults: 1024 shots)
  
  Returns: Job result map with measurement outcomes"
  ([backend circuit]
   (execute-circuit backend circuit {:shots 1024}))
  ([backend circuit options]
   {:pre [(satisfies? QuantumBackend backend)
          (s/valid? ::qc/quantum-circuit circuit)
          (s/valid? ::execution-options options)]}
   
   (if-not (is-available? backend)
     {:job-status :failed
      :error-message "Backend is not available"}
     
     (let [job-id (submit-circuit backend circuit options)]
       (loop [max-retries 100
              retry-count 0]
         (if (>= retry-count max-retries)
           {:job-status :failed
            :error-message "Job execution timeout"}
           
           (let [status (get-job-status backend job-id)]
             (case status
               :completed (get-job-result backend job-id)
               :failed {:job-status :failed
                       :job-id job-id
                       :error-message "Job execution failed"}
               :cancelled {:job-status :cancelled
                          :job-id job-id}
               ;; Still running or queued, wait and retry
               (do
                 (Thread/sleep 100) ; Wait 100ms
                 (recur max-retries (inc retry-count)))))))))))

(defn execute-circuit-async
  "Execute a quantum circuit asynchronously.
  
  Returns immediately with a job ID. Use get-job-status and get-job-result
  to track progress and retrieve results.
  
  Parameters:
  - backend: An implementation of QuantumBackend protocol
  - circuit: Quantum circuit to execute
  - options: Execution options
  
  Returns: Job ID string"
  ([backend circuit]
   (execute-circuit-async backend circuit {:shots 1024}))
  ([backend circuit options]
   {:pre [(satisfies? QuantumBackend backend)
          (s/valid? ::qc/quantum-circuit circuit)
          (s/valid? ::execution-options options)]}
   
   (submit-circuit backend circuit options)))

(defn wait-for-job
  "Wait for a job to complete and return the result.
  
  Parameters:
  - backend: The backend where the job was submitted
  - job-id: Job identifier
  - timeout-ms: Maximum time to wait in milliseconds (default: 30000)
  
  Returns: Job result map"
  ([backend job-id]
   (wait-for-job backend job-id 30000))
  ([backend job-id timeout-ms]
   {:pre [(satisfies? QuantumBackend backend)
          (string? job-id)
          (pos-int? timeout-ms)]}
   
   (let [start-time (System/currentTimeMillis)
         max-time (+ start-time timeout-ms)]
     
     (loop []
       (let [current-time (System/currentTimeMillis)]
         (if (> current-time max-time)
           {:job-status :failed
            :error-message "Job wait timeout"}
           
           (let [status (get-job-status backend job-id)]
             (case status
               :completed (get-job-result backend job-id)
               :failed {:job-status :failed
                       :job-id job-id
                       :error-message "Job execution failed"}
               :cancelled {:job-status :cancelled
                          :job-id job-id}
               ;; Still running or queued, wait and retry
               (do
                 (Thread/sleep 100)
                 (recur))))))))))

;; Utility functions for working with measurement results
(defn analyze-measurement-results
  "Analyze measurement results and compute statistics.
  
  Parameters:
  - results: Map of measurement outcomes to counts
  
  Returns: Map with probabilities and statistics"
  [results]
  {:pre [(map? results)]}
  
  (let [total-shots (reduce + (vals results))
        probabilities (into {} 
                           (map (fn [[outcome count]]
                                  [outcome (/ count total-shots)]))
                           results)]
    
    {:total-shots total-shots
     :outcomes results
     :probabilities probabilities
     :num-outcomes (count results)
     :most-frequent (first (sort-by val > results))
     :entropy (- (reduce + (map #(let [p %] 
                                   (if (zero? p) 0 (* p (Math/log p))))
                               (vals probabilities))))}))

;; Gate support utility functions
(defn supports-gate?
  "Check if a backend supports a specific gate.
  
  Parameters:
  - backend: Backend implementing QuantumBackend protocol
  - gate: Gate keyword to check for support
  
  Returns: True if the gate is supported, false otherwise"
  [backend gate]
  {:pre [(satisfies? org.soulspace.qclojure.application.quantum-backend/QuantumBackend backend)
         (keyword? gate)]}
  (contains? (get-supported-gates backend) gate))

(defn supports-gates?
  "Check if a backend supports all gates in a set.
  
  Parameters:
  - backend: Backend implementing QuantumBackend protocol  
  - gates: Set or collection of gate keywords
  
  Returns: True if all gates are supported, false otherwise"
  [backend gates]
  {:pre [(satisfies? QuantumBackend backend)
         (coll? gates)]}
  (let [supported-gates (get-supported-gates backend)
        required-gates (set gates)]
    (clojure.set/subset? required-gates supported-gates)))

(defn get-unsupported-gates
  "Get the set of gates from a collection that are not supported by a backend.
  
  Parameters:
  - backend: Backend implementing QuantumBackend protocol
  - gates: Set or collection of gate keywords
  
  Returns: Set of unsupported gate keywords"
  [backend gates]
  {:pre [(satisfies? QuantumBackend backend)
         (coll? gates)]}
  (let [supported-gates (get-supported-gates backend)
        required-gates (set gates)]
    (clojure.set/difference required-gates supported-gates)))

(defn filter-backends-by-gate-support
  "Filter a collection of backends by their gate support.
  
  Parameters:
  - backends: Collection of backends implementing QuantumBackend protocol
  - required-gates: Set or collection of gate keywords that must be supported
  
  Returns: Collection of backends that support all required gates"
  [backends required-gates]
  {:pre [(coll? backends)
         (coll? required-gates)]}
  (filter #(supports-gates? % required-gates) backends))

(defn compare-gate-support
  "Compare gate support between two backends.
  
  Parameters:
  - backend1: First backend implementing QuantumBackend protocol
  - backend2: Second backend implementing QuantumBackend protocol
  
  Returns: Map with comparison results including shared, unique to each backend"
  [backend1 backend2]
  {:pre [(satisfies? QuantumBackend backend1)
         (satisfies? QuantumBackend backend2)]}
  (let [gates1 (get-supported-gates backend1)
        gates2 (get-supported-gates backend2)]
    {:backend1-gates gates1
     :backend2-gates gates2
     :shared-gates (set/intersection gates1 gates2)
     :backend1-only (set/difference gates1 gates2)
     :backend2-only (set/difference gates2 gates1)
     :total-unique-gates (set/union gates1 gates2)}))

(defn find-minimal-backend
  "Find the backend with the minimal gate set that still supports required gates.
  
  This is useful for finding the most constrained backend that can still
  execute a given circuit, which might be preferred for noise considerations.
  
  Parameters:
  - backends: Collection of backends implementing QuantumBackend protocol
  - required-gates: Set or collection of gate keywords that must be supported
  
  Returns: Backend with smallest supported gate set that includes all required gates,
           or nil if no backend supports all required gates"
  [backends required-gates]
  {:pre [(coll? backends)
         (coll? required-gates)]}
  (let [compatible-backends (filter-backends-by-gate-support backends required-gates)]
    (when (seq compatible-backends)
      (apply min-key #(count (get-supported-gates %)) compatible-backends))))

;; Circuit transformation for backend compatibility
(defn transform-circuit-for-backend
  "Transform a quantum circuit to use only gates supported by a given backend.
   
   This function delegates to the specialized circuit-transformer namespace.
   See org.soulspace.qclojure.application.circuit-transformer for details.
   
   Parameters:
   - circuit: Quantum circuit to transform
   - backend: Target backend for the transformation
   - options: Optional map with transformation options:
      :max-iterations - Maximum number of decomposition iterations (default: 100)
      :transform-unsupported? - Whether to transform unsupported gates (default: true)
   
   Returns:
   A map containing:
   - :quantum-circuit - The transformed circuit
   - :transformed-gates - Count of gates that were transformed
   - :unsupported-gates - List of gate types that couldn't be transformed"
  [circuit backend & [options]]
  (require '[org.soulspace.qclojure.application.circuit-transformer :as ct])
  ((resolve 'org.soulspace.qclojure.application.circuit-transformer/transform-circuit)
   circuit backend options))

;; Specs for utility functions
(s/fdef supports-gate?
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :gate keyword?)
  :ret boolean?)

(s/fdef supports-gates?
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :gates (s/coll-of keyword?))
  :ret boolean?)

(s/fdef get-unsupported-gates
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :gates (s/coll-of keyword?))
  :ret ::gr/gate-set)

(s/fdef transform-circuit-for-backend
  :args (s/cat :circuit ::qc/quantum-circuit
               :backend #(satisfies? QuantumBackend %)
               :options (s/? map?))
  :ret map?)

(comment
  ;; Example usage of the quantum backend protocol
  
  ;
  )
