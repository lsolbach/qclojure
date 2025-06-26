(ns org.soulspace.qclojure.domain.circuit-composition
  (:require [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.spec.alpha :as s]))

;; Circuit composition and transformation
;; Helper function for circuit composition
(defn update-operation-params
  "Update operation parameters based on a qubit mapping function.
  
  This function handles all types of quantum operations (gates and measurements) 
  and updates any qubit indices in their parameters based on the provided mapping function.
  
  Parameters:
  - operation: The quantum operation to update
  - mapping-fn: Function that takes a qubit index and returns a new index
  
  Returns:
  Updated operation with remapped qubit indices"
  [operation mapping-fn]
  (if-let [params (:operation-params operation)]
    (let [updated-params
          (reduce-kv (fn [m k v]
                       (cond
                         ;; Check if this is a qubit index parameter
                         (#{:target :control :qubit1 :qubit2 :control1 :control2 :target1 :target2} k)
                         (assoc m k (mapping-fn v))

                         ;; Handle measurement qubits vector
                         (= k :measurement-qubits)
                         (assoc m k (mapv mapping-fn v))

                         ;; Keep other parameters as they are
                         :else
                         (assoc m k v)))
                     {}
                     params)]
      (assoc operation :operation-params updated-params))
    operation))

(defn extend-circuit
  "Extend a quantum circuit to support a larger number of qubits.
  
  This function creates a new circuit with the specified number of qubits
  while preserving all the operations of the original circuit. The original 
  circuit operations will apply to the same qubits as before.
  
  With the optional qubit-mapping parameter, you can specify a function
  to map original qubit indices to new indices in the extended circuit.
  
  Parameters:
  - circuit: Original quantum circuit to extend
  - new-num-qubits: New total number of qubits (must be >= original)
  - qubit-mapping: (Optional) Function that maps original qubit indices to new indices
  
  Returns:
  A new circuit with increased qubit count and remapped qubit operations if specified
  
  Examples:
  (extend-circuit (h-gate (create-circuit 1) 0) 3)
  ;=> 3-qubit circuit with Hadamard gate on qubit 0
  
  ;; Shift all qubits by 2 positions  
  (extend-circuit (h-gate (create-circuit 1) 0) 3 #(+ % 2))
  ;=> 3-qubit circuit with Hadamard gate on qubit 2"
  [circuit new-num-qubits & {:keys [qubit-mapping] :or {qubit-mapping identity}}]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)
         (>= new-num-qubits (:num-qubits circuit))]}

  ;; Only update operation parameters if the qubit mapping is not identity
  (let [operations (if (= qubit-mapping identity)
                     (:operations circuit)
                     (mapv #(update-operation-params % qubit-mapping) (:operations circuit)))]
    (-> circuit
        (assoc :num-qubits new-num-qubits)
        (assoc :operations operations)
        (update :name #(str % (when % " ") "(extended to " new-num-qubits " qubits)")))))

(defn compose-circuits
  "Compose two quantum circuits sequentially.
  
  Creates a new circuit that applies the first circuit followed by the second.
  If the circuits have different numbers of qubits, the one with fewer qubits
  will be automatically extended to match the larger one.
  
  With optional parameters, you can control how the circuits are composed:
  - qubit-mapping: Function to map circuit2's qubit indices to the combined circuit
  - offset: Simple integer offset for circuit2's qubits (shorthand for adding offset)
  - control-qubits-only: Boolean flag to indicate circuit2 should only operate on the
    first n qubits of circuit1 where n is the number of qubits in circuit2
  
  Parameters:
  - circuit1: First quantum circuit to execute
  - circuit2: Second quantum circuit to execute after the first
  - options: (Optional) Map with composition options:
    - :qubit-mapping - Function mapping circuit2's qubit indices
    - :offset - Integer to add to circuit2's qubit indices
    - :control-qubits-only - Boolean indicating circuit2 operates on control qubits only
  
  Returns:
  New quantum circuit containing all operations from circuit1 followed by all operations from circuit2
  
  Examples:
  (compose-circuits (h-gate (create-circuit 1) 0) (x-gate (create-circuit 1) 0))
  ;=> Circuit that applies H then X on qubit 0
  
  (compose-circuits (h-gate (create-circuit 1) 0) (cnot-gate (create-circuit 2) 0 1))
  ;=> 2-qubit circuit that applies H on qubit 0, then CNOT from qubit 0 to 1
  
  ;; With offset, apply second circuit starting at qubit 3
  (compose-circuits (create-circuit 5) (h-gate (create-circuit 2) 0) {:offset 3})
  ;=> 5-qubit circuit with H gate on qubit 3"
  [circuit1 circuit2 & [options]]
  {:pre [(s/valid? ::qc/quantum-circuit circuit1)
         (s/valid? ::qc/quantum-circuit circuit2)]}
  (let [{:keys [qubit-mapping offset control-qubits-only]} (or options {})
        num-qubits-1 (:num-qubits circuit1)
        num-qubits-2 (:num-qubits circuit2)
        max-qubits (max num-qubits-1 num-qubits-2)

        ;; Determine the appropriate qubit mapping function
        mapping-fn (cond
                     ;; Explicit mapping function provided
                     (fn? qubit-mapping)
                     qubit-mapping

                     ;; Simple offset provided
                     (integer? offset)
                     #(+ % offset)

                     ;; Circuit2 operates on control qubits only (for algorithms like Shor's)
                     control-qubits-only
                     identity

                     ;; Default - identity mapping
                     :else
                     identity)

        ;; Apply the mapping function to circuit2's operations
        mapped-operations-2 (if (= mapping-fn identity)
                              (:operations circuit2)
                              (mapv #(update-operation-params % mapping-fn)
                                    (:operations circuit2)))

        ;; Extend circuit1 if needed
        target-qubits (if (< num-qubits-1 max-qubits)
                        max-qubits
                        num-qubits-1)

        ;; Calculate proper name for the composed circuit
        circuit-name (str (get circuit1 :name "Circuit1") " + "
                          (get circuit2 :name "Circuit2")
                          (when offset (str " (offset " offset ")"))
                          (when control-qubits-only " (control qubits only)"))]

    (-> circuit1
        (assoc :num-qubits target-qubits)
        (update :operations #(into % mapped-operations-2))
        (assoc :name circuit-name))))

