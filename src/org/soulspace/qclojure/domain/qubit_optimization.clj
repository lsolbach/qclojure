(ns org.soulspace.qclojure.domain.qubit-optimization
  "Functions for analyzing and optimizing qubit usage in quantum circuits."
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.set :as set]))

;;
;; Qubit Optimization Functions
;;
(defn- extract-qubit-ids
  "Extract all qubit IDs used by a operation.
  
  Parameters:
  - operation: operation map with operation-type and operation-params
  
  Returns:
  Set of qubit IDs used by this operation"
  [operation]
  (let [params (:operation-params operation)
        ;; Common qubit parameter names - only these should be treated as qubit IDs
        qubit-param-keys #{:target :control :control1 :control2 :target1 :target2 :swap1 :swap2 :qubit1 :qubit2}]
    (into #{}
          (comp (filter (fn [[k v]]
                          (and (contains? qubit-param-keys k)
                               (number? v))))
                (map second))
          params)))

(defn analyze-qubit-usage
  "Analyze which qubits are actually used in a circuit.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Map containing:
  - :used-qubits - Set of qubit IDs that are actually used
  - :total-qubits - Total number of qubits declared in circuit
  - :unused-qubits - Set of qubit IDs that are declared but unused
  - :max-qubit-id - Highest qubit ID used
  - :qubit-usage-efficiency - Ratio of used qubits to total qubits"
  [circuit]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)]}

  (let [operations (:operations circuit)
        total-qubits (:num-qubits circuit)

        ;; Extract all qubit IDs used in operations
        used-qubits (reduce (fn [acc operation]
                              (into acc (extract-qubit-ids operation)))
                            #{}
                            operations)

        ;; Calculate unused qubits
        all-qubits (set (range total-qubits))
        unused-qubits (set/difference all-qubits used-qubits)

        ;; Find the maximum qubit ID actually used
        max-qubit-id (if (empty? used-qubits) -1 (apply max used-qubits))

        ;; Calculate efficiency
        efficiency (if (zero? total-qubits)
                     0.0
                     (/ (count used-qubits) (double total-qubits)))]

    {:used-qubits used-qubits
     :total-qubits total-qubits
     :unused-qubits unused-qubits
     :max-qubit-id max-qubit-id
     :qubit-usage-efficiency efficiency}))

(defn- create-qubit-mapping
  "Create a mapping from old qubit IDs to new compact qubit IDs.
  
  Parameters:
  - used-qubits: Set of qubit IDs that are actually used
  
  Returns:
  Map from old qubit ID to new qubit ID"
  [used-qubits]
  (let [sorted-qubits (sort used-qubits)]
    (into {}
          (map-indexed (fn [new-id old-id]
                         [old-id new-id])
                       sorted-qubits))))

(defn- remap-operation-qubits
  "Remap qubit IDs in a operation according to a qubit mapping.
  
  Parameters:
  - operation: operation map to remap
  - qubit-mapping: Map from old qubit ID to new qubit ID
  
  Returns:
  operation map with remapped qubit IDs"
  [operation qubit-mapping]
  (let [params (:operation-params operation)
        ;; Only remap parameters that are qubit IDs
        qubit-param-keys #{:target :control :control1 :control2 :target1 :target2 :swap1 :swap2 :qubit1 :qubit2}
        remapped-params (into {}
                              (map (fn [[param-key param-value]]
                                     [param-key
                                      (if (and (contains? qubit-param-keys param-key)
                                               (number? param-value))
                                        (get qubit-mapping param-value param-value)
                                        param-value)])
                                   params))]
    (assoc operation :operation-params remapped-params)))

(defn optimize-qubit-usage
  "Optimize a circuit to use the minimum number of qubits.
  
  This function compacts qubit IDs to eliminate gaps and unused qubits,
  reducing the total number of qubits required for the circuit.
  
  Parameters:
  - circuit: Quantum circuit to optimize
  
  Returns:
  Map containing:
  - :quantum-circuit - Circuit with optimized qubit usage
  - :qubit-mapping - Map from old qubit IDs to new qubit IDs
  - :qubits-saved - Number of qubits saved by optimization
  - :original-qubits - Original number of qubits
  - :optimized-qubits - Final number of qubits after optimization
  
  Example:
  ;; Circuit using qubits [0, 2, 5] out of 6 total qubits
  ;; After optimization: uses qubits [0, 1, 2] out of 3 total qubits
  (optimize-qubit-usage circuit)
  ;=> {:quantum-circuit <optimized-circuit>, :qubit-mapping {0 0, 2 1, 5 2}, 
  ;    :qubits-saved 3, :original-qubits 6, :optimized-qubits 3}"
  [circuit]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)]}

  (let [usage-analysis (analyze-qubit-usage circuit)
        used-qubits (:used-qubits usage-analysis)
        original-qubits (:num-qubits circuit)

        ;; Create mapping from old qubit IDs to compact new IDs
        qubit-mapping (create-qubit-mapping used-qubits)
        optimized-qubits (count used-qubits)

        ;; Remap all operations to use the new qubit IDs
        optimized-operations (mapv #(remap-operation-qubits % qubit-mapping)
                                   (:operations circuit))

        ;; Create optimized circuit
        optimized-circuit (assoc circuit
                                 :num-qubits optimized-qubits
                                 :operations optimized-operations)
        qubits-saved (- original-qubits optimized-qubits)]

    {:quantum-circuit optimized-circuit
     :qubit-mapping qubit-mapping
     :qubits-saved qubits-saved
     :original-qubits original-qubits
     :optimized-qubits optimized-qubits}))

