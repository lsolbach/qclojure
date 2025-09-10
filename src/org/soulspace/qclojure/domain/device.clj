(ns org.soulspace.qclojure.domain.device
  "Device-specific configurations and utilities for quantum circuits.
   
   This namespace provides functions to manage device configurations,
   validate circuits against device constraints, and handle device-specific
   optimizations."
  (:require [clojure.set :as set]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.gate-optimization :as go]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]
            [org.soulspace.qclojure.domain.topology :as topo]) 
  )

;;;
;;; Device Specs
;;;


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
  [circuit supported-operations & [coupling]]
  (let [gate-types (set (map :operation-type (:operations circuit)))
        unsupported-gates (remove #(contains? supported-operations %) gate-types)
        all-gates-supported? (empty? unsupported-gates)
        topology-result (if coupling
                          (topo/validate-topology circuit coupling)
                          {:topology-valid? true :topology-issues []})]
    {:all-gates-supported? all-gates-supported?
     :unsupported-gates (vec unsupported-gates)
     :topology-valid? (:topology-valid? topology-result)
     :topology-issues (:topology-issues topology-result)}))

