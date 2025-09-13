(ns org.soulspace.qclojure.application.backend
  "Protocol and interface for quantum computing hardware backends.
  
  This namespace defines the protocols for connecting to and executing
  quantum circuits on real quantum hardware or simulators. It provides
  a clean abstraction layer that allows the application to work with
  different quantum computing providers and simulators."
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.result :as result]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.device :as device]
            [org.soulspace.qclojure.application.hardware-optimization :as hw-opt]
            [org.soulspace.qclojure.domain.operation-registry :as op-reg]))

;;;
;;; Specs for backend protocol
;;;

;;
;; Specs for general backend protocol
;;
(s/def ::backend-type #{:simulator :hardware :cloud})
(s/def ::backend-name string?)
(s/def ::backend-config map?)
(s/def ::job-id string?)
(s/def ::job-status #{:queued :running :completed :failed :cancelled})
(s/def ::initial-state (s/nilable ::state/state))
(s/def ::shots pos-int?)
(s/def ::measurement-results (s/map-of string? nat-int?))

;;
;; Enhanced specs for cloud backends
;;
(s/def ::authentication-token string?)
(s/def ::session-id string?)
(s/def ::username string?)
(s/def ::password string?)
(s/def ::token string?)
(s/def ::api-key string?)
(s/def ::credentials (s/keys :req-un [::username ::password] :opt-un [::token ::api-key]))

(s/def ::batch-job-id string?)
(s/def ::batch-results (s/map-of ::job-id ::job-result))

(s/def ::backend-info
  (s/keys :req-un [::backend-type
                   ::backend-name
                   ::capabilities]
          :opt-un [::backend-config
                   ::description]))

(s/def ::capabilities (s/coll-of keyword?))

;;
;; Specs for job submission and results
;;
(s/def ::execution-options
  (s/keys :opt-un [::shots
                   ::initial-state
                   ::result/result-specs]))

(s/def ::job-result
  (s/keys :req-un [::job-id ::job-status]
          :opt-un [::measurement-results ::error-message
                   ::execution-time-ms ::final-state]))

;;;
;;; Protocol for quantum computing backends (simulators, hardware, cloud services)
;;;
(defprotocol QuantumBackend
  "Protocol for quantum computing hardware backends.
  
  This protocol defines the interface that all quantum backends must implement,
  whether they are simulators, cloud services, or local hardware."

  (backend-info [this]
    "Get information about this backend including type, capabilities, and configuration.")

  (device [this]
    "Return the current device.
     
     Returns: Device map")

  (available? [this]
    "Check if the device is currently available for job submission.")
  
  (submit-circuit [this circuit options]
    "Submit a quantum circuit for execution.
    
    Parameters:
    - circuit: A quantum circuit from qclojure.domain.quantum-circuit
    - options: Execution options including shot count and result spec
    
    Returns: A job ID for tracking the execution.")

  (job-status [this job-id]
    "Get the current status of a submitted job.")

  (job-result [this job-id]
    "Get the results of a completed job.
    
    Returns: A map containing measurement results and statistics.")

  (cancel-job [this job-id]
    "Cancel a queued or running job.")

  (queue-status [this]
    "Get information about the current job queue.")
  ;
  )

;;;
;;; Protocol for multiple device backends
;;;
(defprotocol MultiDeviceBackend
  "Protocol for backends that support multiple quantum devices.
  
  This protocol extends the basic QuantumBackend with functionality
  for managing and selecting among multiple devices."

  (devices [this]
    "List all quantum devices available on this backend.
    
    Returns: Collection of device maps")

  (select-device [this device]
    "Select the device to use for job submissions.")
  ;
  )

;;;
;;; Protocol for batched job submissions
;;;
(defprotocol BatchJobBackend 
  "Protocol for backends that support batched job submissions.
    
   This protocol extends the basic QuantumBackend with functionality
   for submitting and managing batches of quantum circuits." 
  
  (batch-submit [this circuits options] [this circuits device options]
    "Submit multiple circuits as a batch job.
    
    Parameters:
    - circuits: Collection of quantum circuits
    - options: Execution options applied to all circuits
    
    Returns: Batch job ID for tracking all submissions")

  (batch-status [this batch-job-id]
    "Get status of a batch job.
    
    Parameters:
    - batch-job-id: Identifier for the batch job
    
    Returns: Status information for all jobs in the batch")

  (batch-results [this batch-job-id]
    "Get results from a completed batch job.
    
    Parameters:
    - batch-job-id: Identifier for the batch job
    
    Returns: Map of individual job results"))

;;;
;;; Protocol for cloud quantum backends
;;;
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

  (session-info [this]
    "Get current session information including authentication status.")

  (estimate-cost [this circuit options] [this circuit device options]
    "Estimate the cost of executing a circuit.
    
    Parameters:
    - circuit: Quantum circuit to estimate cost for
    - options: Execution options including shots and device
    
    Returns: Cost estimation information"))

;;;
;;; High-level execution functions
;;;
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
   (execute-circuit backend circuit{:shots 512}))
  ([backend circuit options]
   {:pre [(satisfies? QuantumBackend backend)
          (s/valid? ::circuit/circuit circuit)
          (s/valid? ::execution-options options)]}

   (if-not (available? backend)
     {:job-status :failed
      :error-message "Backend is not available"}

     (let [job-id (submit-circuit backend circuit options)]
       ;(println "Waiting for job" job-id "to complete...")
       (loop [max-retries 2000
              retry-count 0]
         (if (>= retry-count max-retries)
           {:job-status :failed
            :error-message "Job execution timeout"}

           (let [status (job-status backend job-id)]
             (case status
               :completed (job-result backend job-id)
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
          (s/valid? ::circuit/circuit circuit)
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

           (let [status (job-status backend job-id)]
             (case status
               :completed (job-result backend job-id)
               :failed {:job-status :failed
                        :job-id job-id
                        :error-message "Job execution failed"}
               :cancelled {:job-status :cancelled
                           :job-id job-id}
               ;; Still running or queued, wait and retry
               (do
                 (Thread/sleep 100)
                 (recur))))))))))

;;;
;;; Utility functions for working with measurement results
;;;
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

;;;
;;; Cloud backend utility functions
;;;
(defn cloud-backend?
  "Check if a backend is a cloud backend.
  
  Parameters:
  - backend: Backend to check
  
  Returns: True if the backend implements CloudQuantumBackend protocol"
  [backend]
  (satisfies? CloudQuantumBackend backend))
