(ns org.soulspace.qclojure.domain.qubit-optimization
  "Functions for analyzing and optimizing qubit usage in quantum circuits.
   
   This namespace provides utilities to analyze which qubits are actually used
   in a quantum circuit and to optimize the circuit by remapping qubit IDs to
   eliminate gaps and unused qubits.
   
   Qubit Optimization:
   - Analyze qubit usage efficiency in a circuit
   - Remap qubit IDs to minimize total qubit count
   - Ensure circuit validity after optimization"
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as circuit]))

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
  {:pre [(s/valid? ::circuit/circuit circuit)]}

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
   - ctx: Map containing:
     - :circuit - Quantum circuit to optimize
     - :options - Map with options:
        :optimize-qubits? - Whether to perform qubit optimization (default: false)
   
   Returns:
   Updated context with:
   - :circuit - Optimized circuit with remapped qubit IDs
   - :qubit-mapping - Map from old qubit IDs to new qubit IDs
   
   Throws an exception if the optimization results in an empty circuit.
   
   Example:
   (optimize-qubit-usage
    {:circuit my-circuit
     :options {:optimize-qubits? true}})
   
   ;=> {:circuit <optimized-circuit>, :qubit-mapping {0 0, 2 1, 3 2}}
   "
  [ctx]
  {:pre [(s/valid? ::circuit/circuit (:circuit ctx))]}
  (if-not (get-in ctx [:options :optimize-qubits?])
    ; No optimization requested, return original context
    ctx
    ; Perform optimization and return updated context
    (let [circuit (:circuit ctx)
          usage-analysis (analyze-qubit-usage circuit)
          used-qubits (:used-qubits usage-analysis)

          ;; Create mapping from old qubit IDs to compact new IDs
          qubit-mapping (create-qubit-mapping used-qubits)
          optimized-qubits (count used-qubits)

          ;; Remap all operations to use the new qubit IDs
          optimized-operations (mapv #(remap-operation-qubits % qubit-mapping)
                                     (:operations circuit))

          ;; Create optimized circuit
          optimized-circuit (assoc circuit
                                   :num-qubits optimized-qubits
                                   :operations optimized-operations)]
      (if (circuit/empty-circuit? optimized-circuit)
        (throw (ex-info "Optimization resulted in an empty circuit"
                        {:original-circuit circuit
                         :optimized-circuit optimized-circuit}))
        (assoc ctx :circuit optimized-circuit :qubit-mapping qubit-mapping)))))

(comment
  ;; Empty circuit, but no optimization requested, should return original circuit
  (optimize-qubit-usage
   {:circuit (circuit/create-circuit 1)})

  ;; Empty circuit, qubit will get optimized away and the circuit is invalid
  (try (optimize-qubit-usage
        {:circuit (circuit/create-circuit 1)
         :options {:optimize-qubits? true}})
       (catch Exception e
         (println "Caught exception:" (.getMessage e))
         (println "Data" (ex-data e))))

  ;
  )
