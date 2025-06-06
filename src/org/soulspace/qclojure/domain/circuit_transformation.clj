(ns org.soulspace.qclojure.domain.circuit-transformation
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming operations not supported
   by the backend into equivalent sequences of supported operations."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.operation-registry :as gr]
            [org.soulspace.qclojure.application.backend :as qb]))

;; Specs
(s/def ::transformation-result
  (s/keys :req-un [::qc/quantum-circuit]
          :opt-un [::transformed-operations ::unsupported-operations]))

(defn- can-be-fully-decomposed?
  "Check if a operation type can be fully decomposed into supported operations.
  
  Parameters:
  - operation-type: Type of operation to check for decomposition
  - supported-operations: Set of operation types supported by the backend
  - visited: Set of operation types already visited (to prevent infinite recursion)
  
  Returns:
  Boolean indicating whether the operation can be fully decomposed into supported operations"
  [operation-type supported-operations visited]
  ;; Already visited this operation in recursion? Return false to break cycles
  (if (contains? visited operation-type)
    false
    ;; Is the operation directly supported? Return true
    (if (contains? supported-operations operation-type)
      true
      ;; Not directly supported, try decomposition
      (let [decomposition (gr/get-gate-dependencies operation-type)]
        (if (empty? decomposition)
          ;; No decomposition available
          false
          ;; Check if ALL operations in decomposition can be fully decomposed
          (every? #(can-be-fully-decomposed? % supported-operations (conj visited operation-type))
                  decomposition))))))

(defn- decompose-operation
  "Decompose a operation into a sequence of more primitive operations.
  
  Parameters:
  - operation: operation map to decompose
  - supported-operations: Set of operation types supported

  Returns:
  Vector of operation maps representing the decomposition, or the original operation 
  if no decomposition is available or if it cannot be fully decomposed to supported operations"
  [operation supported-operations]
  (let [operation-type (:operation-type operation)
        operation-params (:operation-params operation)
        decomposition (gr/get-gate-dependencies operation-type)]

    ;; If the operation is already supported, no need to decompose
    (if (contains? supported-operations operation-type)
      [operation]
      
      ;; If no decomposition available OR cannot be fully decomposed, return original
      ;; This breaks potential infinite loops
      (if (or (empty? decomposition)
              (not (every? #(can-be-fully-decomposed? % supported-operations #{}) decomposition)))
        [operation]
        
        ;; Create a sequence of operations based on decomposition
        (mapv (fn [sub-operation-type]
                (cond
                  ;; Handle different operation types and parameters based on the original operation
                  (= sub-operation-type :rx) 
                  {:operation-type :rx 
                   :operation-params (select-keys operation-params [:target :angle])}
                  
                  (= sub-operation-type :ry)
                  {:operation-type :ry
                   :operation-params (select-keys operation-params [:target :angle])}
                  
                  (= sub-operation-type :rz)
                  {:operation-type :rz
                   :operation-params (select-keys operation-params [:target :angle])}
                  
                  (= sub-operation-type :x)
                  {:operation-type :x
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :y)
                  {:operation-type :y
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :z)
                  {:operation-type :z
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :h)
                  {:operation-type :h
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :s)
                  {:operation-type :s
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :t)
                  {:operation-type :t
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :cnot)
                  {:operation-type :cnot
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :cx)
                  {:operation-type :cx
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :cz)
                  {:operation-type :cz
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :cy)
                  {:operation-type :cy
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :s-dag)
                  {:operation-type :s-dag
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :t-dag)
                  {:operation-type :t-dag
                   :operation-params (select-keys operation-params [:target])}
                  
                  (= sub-operation-type :swap)
                  {:operation-type :swap
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :iswap)
                  {:operation-type :iswap
                   :operation-params (select-keys operation-params [:control :target])}
                  
                  (= sub-operation-type :phase)
                  {:operation-type :phase
                   :operation-params (select-keys operation-params [:target :angle])}
                  
                  (= sub-operation-type :crx)
                  {:operation-type :crx
                   :operation-params (select-keys operation-params [:control :target :angle])}
                  
                  (= sub-operation-type :cry)
                  {:operation-type :cry
                   :operation-params (select-keys operation-params [:control :target :angle])}
                  
                  (= sub-operation-type :crz)
                  {:operation-type :crz
                   :operation-params (select-keys operation-params [:control :target :angle])}
                  
                  (= sub-operation-type :toffoli)
                  {:operation-type :toffoli
                   :operation-params (select-keys operation-params [:control1 :control2 :target])}
                  
                  (= sub-operation-type :fredkin)
                  {:operation-type :fredkin
                   :operation-params (select-keys operation-params [:control :target1 :target2])}
                  
                  ;; Default case for unknown operation types
                  :else {:operation-type sub-operation-type
                         :operation-params operation-params}))
              decomposition)))))

(defn- transform-operations
  "Transform the operations in a circuit to use only supported operations.

  Parameters:
  - operations: Original vector of operation maps
  - supported-operations: Set of operation types supported
  - max-iterations: Maximum decomposition iterations to prevent infinite loops
  
  Returns:
  A vector of transformed operations that are all supported or
  operations that couldn't be further decomposed"
  [operations supported-operations max-iterations]
  (loop [current-operations operations
         iteration 0
         processed-operations #{}]  ;; Track operations we've already tried to decompose
    (if (>= iteration max-iterations)
      ;; Safety check to prevent infinite loops
      (throw (ex-info "Maximum iterations reached during circuit transformation" 
                      {:operations current-operations
                       :iteration iteration}))
      
      (let [;; Find any operations that need decomposition
            needs-decomposition? (fn [operation] 
                                   (and (not (contains? processed-operations (:operation-type operation)))
                                        (not (contains? supported-operations (:operation-type operation)))))
            unsupported (filterv needs-decomposition? current-operations)]
        
        (if (empty? unsupported)
          ;; All operations are either supported or can't be decomposed further
          current-operations
          
          ;; Replace the first unsupported operation with its decomposition
          (let [unsupported-operation (first unsupported)
                operation-type (:operation-type unsupported-operation)
                unsupported-index (.indexOf current-operations unsupported-operation)
                decomposed-operations (decompose-operation unsupported-operation supported-operations)
                
                ;; Check if the operation was actually decomposed
                was-decomposed? (not= [unsupported-operation] decomposed-operations)
                
                ;; If the operation wasn't decomposed, mark it as processed so we don't try again
                new-processed-operations (if was-decomposed?
                                      processed-operations
                                      (conj processed-operations operation-type))
                
                ;; Create new operations vector with decomposition replacing original operation
                new-operations (into []
                                (concat 
                                 (subvec current-operations 0 unsupported-index)
                                 decomposed-operations
                                 (subvec current-operations (inc unsupported-index))))]
            
            (recur new-operations (inc iteration) new-processed-operations)))))))

(defn transform-circuit
  "Transform a quantum circuit to use only operations supported by a given backend.

  This function takes a quantum circuit and the supported operations, and returns a new circuit
  where all operations have been decomposed into the supported operations.
  
  Parameters:
  - circuit: Quantum circuit to transform
  - supported-operations: Set of operation types supported
  - options: Optional map with transformation options:
      :max-iterations - Maximum number of decomposition iterations (default: 100)
      :transform-unsupported? - Whether to transform unsupported operations (default: true)
  
  Returns:
  A map containing:
  - :quantum-circuit - The transformed circuit
  - :transformed-operations - Count of operations that were transformed
  - :unsupported-operations - List of operation types that couldn't be transformed
  
  Example:
  (transform-circuit my-circuit #{:h :x :cnot} {:max-iterations 50})
  ;=> {:quantum-circuit <transformed-circuit>, :transformed-operations 3, :unsupported-operations []}"
  ([circuit supported-operations]
   (transform-circuit circuit supported-operations {}))
  
  ([circuit supported-operations options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)]}
   
   (let [max-iterations (get options :max-iterations 100)
         transform-unsupported? (get options :transform-unsupported? true)
         
         original-operations (:operations circuit)
         original-operation-count (count original-operations)
         
         ;; Apply transformation
         transformed-operations (if transform-unsupported?
                             (transform-operations original-operations supported-operations max-iterations)
                             original-operations)
         
         ;; Create new circuit with transformed operations
         transformed-circuit (assoc circuit :operations transformed-operations)
         
         ;; Calculate stats for return value
         new-types (frequencies (map :operation-type transformed-operations))
         remaining-unsupported (into [] 
                                     (filter #(not (contains? supported-operations %)) 
                                             (keys new-types)))]
     
     {:quantum-circuit transformed-circuit
      :transformed-operations (- (count transformed-operations) original-operation-count)
      :unsupported-operations remaining-unsupported})))

(defn get-transformation-summary
  "Get a human-readable summary of a circuit transformation.
  
  Parameters:
  - transformation-result: Result from transform-circuit
  
  Returns:
  String with transformation summary"
  [transformation-result]
  (let [circuit (:quantum-circuit transformation-result)
        transformed (:transformed-operations transformation-result)
        unsupported (:unsupported-operations transformation-result)]
    (str "Circuit transformation summary:\n"
         "- Final operation count: " (count (:operations circuit)) "\n"
         "- operations transformed: " transformed "\n"
         "- Unsupported operations: " (if (empty? unsupported) "None" (pr-str unsupported)))))

;; Circuit Optimization Functions

(defn- extract-qubit-ids
  "Extract all qubit IDs used by a operation.
  
  Parameters:
  - operation: operation map with operation-type and operation-params
  
  Returns:
  Set of qubit IDs used by this operation"
  [operation]
  (let [params (:operation-params operation)
        ;; Common qubit parameter names - only these should be treated as qubit IDs
        qubit-param-keys #{:target :control :control1 :control2 :target1 :target2 :swap1 :swap2}]
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
        qubit-param-keys #{:target :control :control1 :control2 :target1 :target2 :swap1 :swap2}
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

(defn optimize
  "Comprehensive circuit optimization for specific supported operations.
  
  This function combines multiple optimization strategies:
  1. Transform operations to supported equivalents
  2. Optimize qubit usage to minimize qubit count
  3. Optionally apply operation sequence optimizations
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - supported-operations: Set of supported operations for optimization
  - options: Optional map with optimization options:
      :optimize-qubits? - Whether to optimize qubit usage (default: true)
      :transform-operations? - Whether to transform unsupported operations (default: true)
      :max-iterations - Maximum decomposition iterations (default: 100)
  
  Returns:
  Map containing:
  - :quantum-circuit - The fully optimized circuit
  - :transformation-result - Result from operation transformation
  - :qubit-optimization-result - Result from qubit optimization (if enabled)
  - :optimization-summary - Human-readable summary of all optimizations
  
  Example:
  (optimize my-circuit #{:h :x :z :cnot} {:optimize-qubits? true})
  ;=> {:quantum-circuit <optimized-circuit>, 
  ;    :transformation-result {...}, 
  ;    :qubit-optimization-result {...},
  ;    :optimization-summary \"...\"}"
  ([circuit supported-operations]
   (optimize circuit supported-operations {}))
  
  ([circuit supported-operations options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)]}
   
   (let [optimize-qubits? (get options :optimize-qubits? true)
         transform-operations? (get options :transform-operations? true)
         
         ;; Step 1: Transform operations to supported equivalents
         transformation-result (if transform-operations?
                                 (transform-circuit circuit supported-operations options)
                                 ;; Even if not transforming, we should identify unsupported operations
                                 (let [operation-types (map :operation-type (:operation circuit))
                                       unsupported (filterv #(not (contains? supported-operations %)) operation-types)
                                       unique-unsupported (vec (distinct unsupported))]
                                   {:quantum-circuit circuit
                                    :transformed-operations 0
                                    :unsupported-operations unique-unsupported}))
         
         transformed-circuit (:quantum-circuit transformation-result)
         
         ;; Step 2: Optimize qubit usage
         qubit-optimization-result (if optimize-qubits?
                                     (optimize-qubit-usage transformed-circuit)
                                     {:quantum-circuit transformed-circuit
                                      :qubit-mapping {}
                                      :qubits-saved 0
                                      :original-qubits (:num-qubits transformed-circuit)
                                      :optimized-qubits (:num-qubits transformed-circuit)})
         
         final-circuit (:quantum-circuit qubit-optimization-result)
         
         ;; Generate comprehensive summary
         summary (str "Circuit optimization summary:\n"
                      "- Original qubits: " (:num-qubits circuit) "\n"
                      "- Final qubits: " (:num-qubits final-circuit) "\n"
                      "- Qubits saved: " (:qubits-saved qubit-optimization-result) "\n"
                      "- Original operation: " (count (:operations circuit)) "\n"
                      "- Final operation: " (count (:operations final-circuit)) "\n"
                      "- Operations transformed: " (:transformed-operation transformation-result) "\n"
                      "- Unsupported operation: " (if (empty? (:unsupported-operation transformation-result))
                                                "None"
                                                (pr-str (:unsupported-operation transformation-result))))]
     
     {:quantum-circuit final-circuit
      :transformation-result transformation-result
      :qubit-optimization-result qubit-optimization-result
      :optimization-summary summary})))

;; Existing transform-circuit and related functions continue below...
