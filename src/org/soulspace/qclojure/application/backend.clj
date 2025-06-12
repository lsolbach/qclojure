(ns org.soulspace.qclojure.application.backend
  "Protocol and interface for quantum computing hardware backends.
  
  This namespace defines the protocol for connecting to and executing
  quantum circuits on real quantum hardware or simulators. It provides
  a clean abstraction layer that allows the application to work with
  different quantum computing providers and simulators."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;; Specs for hardware interface
(s/def ::backend-type #{:simulator :hardware :cloud})
(s/def ::backend-name string?)
(s/def ::backend-config map?)
(s/def ::job-id string?)
(s/def ::job-status #{:queued :running :completed :failed :cancelled})
(s/def ::initial-state (s/nilable ::qs/quantum-state))
(s/def ::shots pos-int?)
(s/def ::measurement-results (s/map-of string? nat-int?))
(s/def ::supported-gates ::gr/operation-set)

;; Enhanced specs for cloud backends
(s/def ::authentication-token string?)
(s/def ::session-id string?)
(s/def ::credentials (s/keys :req-un [::username ::password] :opt-un [::token ::api-key]))
(s/def ::username string?)
(s/def ::password string?)
(s/def ::token string?)
(s/def ::api-key string?)

(s/def ::device-id string?)
(s/def ::device-name string?)
(s/def ::device-status #{:online :offline :maintenance :calibrating})
(s/def ::coupling-map (s/coll-of (s/tuple nat-int? nat-int?)))
(s/def ::gate-times (s/map-of keyword? pos?))
(s/def ::gate-errors (s/map-of keyword? (s/double-in :min 0.0 :max 1.0)))
(s/def ::readout-errors (s/coll-of (s/double-in :min 0.0 :max 1.0)))

(s/def ::device-topology
  (s/keys :req-un [::device-id ::device-name ::coupling-map]
          :opt-un [::gate-times ::gate-errors ::readout-errors ::max-qubits]))

(s/def ::calibration-data
  (s/keys :req-un [::device-id ::timestamp]
          :opt-un [::gate-times ::gate-errors ::readout-errors ::coherence-times]))

(s/def ::timestamp inst?)
(s/def ::coherence-times (s/coll-of pos?))

(s/def ::cost-estimate
  (s/keys :req-un [::total-cost ::currency]
          :opt-un [::cost-breakdown ::estimated-credits]))

(s/def ::total-cost number?)
(s/def ::currency string?)
(s/def ::cost-breakdown map?)
(s/def ::estimated-credits number?)

(s/def ::batch-job-id string?)
(s/def ::batch-results (s/map-of ::job-id ::job-result))

(s/def ::backend-info
  (s/keys :req-un [::backend-type ::backend-name ::capabilities ::supported-gates]
          :opt-un [::backend-config ::description ::max-qubits ::device-topology ::cost-per-shot]))

(s/def ::capabilities (s/coll-of keyword?))
(s/def ::max-qubits pos-int?)
(s/def ::cost-per-shot number?)

(s/def ::execution-options
  (s/keys :opt-un [::shots ::initial-state ::backend-info ::device-id ::priority]))

(s/def ::priority #{:low :normal :high})

(s/def ::job-result
  (s/keys :req-un [::job-id ::job-status]
          :opt-un [::measurement-results ::error-message ::execution-time-ms ::final-state]))

;; TODO add option to initialize backend with an initial state

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

;; Enhanced protocol for cloud quantum backends
(defprotocol CloudQuantumBackend
  "Extended protocol for cloud-based quantum computing backends.
  
  This protocol extends the basic QuantumBackend with cloud-specific
  functionality including authentication, device topology, cost estimation,
  and batch operations."
  
  (authenticate [this credentials]
    "Authenticate with the cloud quantum service.
    
    Parameters:
    - credentials: Map containing authentication information
      (e.g., {:username 'user' :password 'pass'} or {:api-key 'key'})
    
    Returns: Authentication token or session information")
  
  (get-session-info [this]
    "Get current session information including authentication status.")
  
  (list-available-devices [this]
    "List all quantum devices available on this backend.
    
    Returns: Collection of device information maps")
  
  (get-device-topology [this device-id]
    "Get the topology and coupling map for a specific device.
    
    Parameters:
    - device-id: Identifier for the quantum device
    
    Returns: Device topology information including coupling map")
  
  (get-calibration-data [this device-id]
    "Get calibration data for a specific device.
    
    Parameters:
    - device-id: Identifier for the quantum device
    
    Returns: Current calibration data including gate errors and coherence times")
  
  (estimate-cost [this circuit options]
    "Estimate the cost of executing a circuit.
    
    Parameters:
    - circuit: Quantum circuit to estimate cost for
    - options: Execution options including shots and device
    
    Returns: Cost estimation information")
  
  (batch-submit [this circuits options]
    "Submit multiple circuits as a batch job.
    
    Parameters:
    - circuits: Collection of quantum circuits
    - options: Execution options applied to all circuits
    
    Returns: Batch job ID for tracking all submissions")
  
  (get-batch-status [this batch-job-id]
    "Get status of a batch job.
    
    Parameters:
    - batch-job-id: Identifier for the batch job
    
    Returns: Status information for all jobs in the batch")
  
  (get-batch-results [this batch-job-id]
    "Get results from a completed batch job.
    
    Parameters:
    - batch-job-id: Identifier for the batch job
    
    Returns: Map of individual job results"))

;; High-level execution functions
(defn execute-circuit
  "Execute a quantum circuit on the specified backend.
  
  This is a convenience function that handles the full workflow:
  submit circuit, wait for completion, and return results.
  
  Parameters:
  - backend: An implementation of QuantumBackend protocol
  - circuit: Quantum circuit to execute
  - options: Execution options (defaults: {:shots 512})

  Returns: Job result map with measurement outcomes"
  ([backend circuit]
   (execute-circuit backend circuit {:shots 512}))
  ([backend circuit options]
   {:pre [(satisfies? QuantumBackend backend)
          (s/valid? ::qc/quantum-circuit circuit)
          (s/valid? ::execution-options options)]}
   
   (if-not (is-available? backend)
     {:job-status :failed
      :error-message "Backend is not available"}
     
     (let [job-id (submit-circuit backend circuit options)]
       ;(println "Waiting for job" job-id "to complete...")
       (loop [max-retries 2000
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

;; Cloud backend utility functions
(defn cloud-backend?
  "Check if a backend is a cloud backend.
  
  Parameters:
  - backend: Backend to check
  
  Returns: True if the backend implements CloudQuantumBackend protocol"
  [backend]
  (satisfies? CloudQuantumBackend backend))

(defn authenticated?
  "Check if a cloud backend is currently authenticated.
  
  Parameters:
  - backend: Cloud backend implementing CloudQuantumBackend protocol
  
  Returns: True if authenticated, false otherwise"
  [backend]
  {:pre [(cloud-backend? backend)]}
  (try
    (let [session-info (get-session-info backend)]
      (and session-info 
           (not= (:status session-info) :unauthenticated)))
    (catch Exception _
      false)))

(defn ensure-authenticated
  "Ensure a cloud backend is authenticated, prompting for credentials if needed.
  
  Parameters:
  - backend: Cloud backend implementing CloudQuantumBackend protocol
  - credentials: Optional credentials map
  
  Returns: True if authentication successful, false otherwise"
  ([backend]
   (ensure-authenticated backend nil))
  ([backend credentials]
   {:pre [(cloud-backend? backend)]}
   (if (authenticated? backend)
     true
     (when credentials
       (try
         (authenticate backend credentials)
         true
         (catch Exception _
           false))))))

(defn find-best-device
  "Find the best available device for a given circuit.
  
  Considers factors like device availability, topology compatibility,
  and calibration quality.
  
  Parameters:
  - backend: Cloud backend implementing CloudQuantumBackend protocol
  - circuit: Quantum circuit to execute
  - options: Options map with preferences:
    - :prefer-online - Prefer online devices (default: true)
    - :min-qubits - Minimum number of qubits required
    - :max-error-rate - Maximum acceptable gate error rate
  
  Returns: Device ID of the best matching device, or nil if none suitable"
  [backend circuit & [options]]
  {:pre [(cloud-backend? backend)
         (s/valid? ::qc/quantum-circuit circuit)]}
  (let [opts (merge {:prefer-online true} options)
        devices (list-available-devices backend)
        circuit-qubits (:num-qubits circuit)
        min-qubits (or (:min-qubits opts) circuit-qubits)]
    
    (->> devices
         ;; Filter by basic requirements
         (filter (fn [device]
                   (and (>= (get device :max-qubits 0) min-qubits)
                        (if (:prefer-online opts)
                          (= (:device-status device) :online)
                          true))))
         ;; Score devices by quality
         (map (fn [device]
                (let [topology (get-device-topology backend (:device-id device))
                      calibration (get-calibration-data backend (:device-id device))
                      error-rate (get-in calibration [:gate-errors :average] 0.01)
                      connectivity-score (count (:coupling-map topology))]
                  (assoc device 
                         :quality-score (- connectivity-score error-rate)
                         :error-rate error-rate))))
         ;; Filter by error rate if specified
         (filter (fn [device]
                   (if-let [max-error (:max-error-rate opts)]
                     (<= (:error-rate device) max-error)
                     true)))
         ;; Sort by quality score
         (sort-by :quality-score >)
         ;; Return best device ID
         first
         :device-id)))

(defn estimate-execution-cost
  "Estimate the total cost of executing circuits on a cloud backend.
  
  Parameters:
  - backend: Cloud backend implementing CloudQuantumBackend protocol
  - circuits: Single circuit or collection of circuits
  - options: Execution options
  
  Returns: Cost estimation map"
  [backend circuits options]
  {:pre [(cloud-backend? backend)]}
  (let [circuit-list (if (sequential? circuits) circuits [circuits])
        estimates (map #(estimate-cost backend % options) circuit-list)
        total-cost (reduce + (map :total-cost estimates))]
    
    {:total-cost total-cost
     :currency (:currency (first estimates))
     :individual-estimates estimates
     :circuit-count (count circuit-list)}))

;; Gate support utility functions
(defn supports-gate?
  "Check if a backend supports a specific gate.
  
  Parameters:
  - backend: Backend implementing QuantumBackend protocol
  - gate: Gate keyword to check for support
  
  Returns: True if the gate is supported, false otherwise"
  [backend gate]
  {:pre [(satisfies? org.soulspace.qclojure.application.backend/QuantumBackend backend)
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
   
   Parameters:
   - circuit: Quantum circuit to transform
   - backend: Target backend for the transformation
   - options: Optional map with transformation options:
      :max-iterations - Maximum number of decomposition iterations (default: 100)
      :transform-unsupported? - Whether to transform unsupported gates (default: true)
   
   Returns:
   A map containing:
   - :quantum-circuit - The transformed circuit
   - :transformed-operation-count - Count of gates that were transformed
   - :unsupported-gates - List of gate types that couldn't be transformed"
  [circuit backend & [options]]
  (ct/transform-circuit circuit (get-supported-gates backend) options))

;; Specs for utility functions
(s/fdef supports-operation?
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :operation keyword?)
  :ret boolean?)

(s/fdef supports-operations?
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :operations (s/coll-of keyword?))
  :ret boolean?)

(s/fdef get-unsupported-operations
  :args (s/cat :backend #(satisfies? QuantumBackend %)
               :operations (s/coll-of keyword?))
  :ret ::gr/operation-set)

(s/fdef transform-circuit-for-backend
  :args (s/cat :circuit ::qc/quantum-circuit
               :backend #(satisfies? QuantumBackend %)
               :options (s/? map?))
  :ret map?)

(comment
  ;; Example usage of the quantum backend protocol
  
  ;
  )
