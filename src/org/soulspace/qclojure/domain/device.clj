(ns org.soulspace.qclojure.domain.device
  "Device-specific configurations and utilities for quantum circuits.
   
   This namespace provides functions to manage device configurations,
   validate circuits against device constraints, and handle device-specific
   optimizations."
  (:require [clojure.set :as set] 
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.operation-registry :as op-reg]
            [org.soulspace.qclojure.domain.topology :as topo]) 
  )

;;;
;;; Device Specs
;;;
(s/def ::device-id string?)
(s/def ::device-name string?)

(s/def ::max-qubits pos-int?)
(s/def ::native-gates ::op-reg/operation-set)

;;;
;;; Device Validation
;;;
(defn validate-circuit
  "Validate a quantum circuit against device constraints.
   
   Parameters:
   - circuit: Quantum circuit to validate
   - supported-operations: Set of natively supported operations
   - coupling: Qubit coupling for hardware topology (optional)
   
   Returns:
   Validation result map containing:
   - :all-gates-supported? - Boolean indicating if all gates are supported
   - :unsupported-gates - List of unsupported gate types (if any)
   - :topology-valid? - Boolean indicating if circuit respects topology (if coupling provided)
   - :topology-issues - List of topology issues (if any)"
  [device circuit]
  (let [supported-operations (:supported-operations device)
        gate-types (set (map :operation-type (:operations circuit)))
        unsupported-gates (remove #(contains? supported-operations %) gate-types)
        all-gates-supported? (empty? unsupported-gates)]
    {:all-gates-supported? all-gates-supported?
     :unsupported-gates (vec unsupported-gates)}))

