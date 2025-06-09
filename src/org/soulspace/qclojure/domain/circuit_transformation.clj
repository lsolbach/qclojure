(ns org.soulspace.qclojure.domain.circuit-transformation
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming operations not supported
   by the backend into equivalent sequences of supported operations."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;; Specs
(s/def ::transformation-result
  (s/keys :req-un [::qc/quantum-circuit]
          :opt-un [::transformed-operation-count ::unsupported-operations]))

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
  - :transformed-operation-count - Count of operations that were transformed
  - :unsupported-operations - List of operation types that couldn't be transformed
  
  Example:
  (transform-circuit my-circuit #{:h :x :cnot} {:max-iterations 50})
  ;=> {:quantum-circuit <transformed-circuit>, :transformed-operation-count 3, :unsupported-operations []}"
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
      :transformed-operation-count (- (count transformed-operations) original-operation-count)
      :unsupported-operations remaining-unsupported})))

(defn get-transformation-summary
  "Get a human-readable summary of a circuit transformation.
  
  Parameters:
  - transformation-result: Result from transform-circuit
  
  Returns:
  String with transformation summary"
  [transformation-result]
  (let [circuit (:quantum-circuit transformation-result)
        transformed (:transformed-operation-count transformation-result)
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
                                    :transformed-operation-count 0
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

;; Hardware Topology Creation Functions

(defn create-linear-topology
  "Create a linear hardware topology where qubits are connected in a line.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for linear topology"
  [num-qubits]
  (cond
    (< num-qubits 1) []
    (= num-qubits 1) [[]]
    :else
    (vec (for [i (range num-qubits)]
           (vec (cond
                  (= i 0) [1]                     ; First qubit connects to second
                  (= i (dec num-qubits)) [(dec i)] ; Last qubit connects to second-to-last
                  :else [(dec i) (inc i)]))))))   ; Middle qubits connect to neighbors

(defn create-ring-topology
  "Create a ring hardware topology where qubits are connected in a circle.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for ring topology"
  [num-qubits]
  (cond
    (< num-qubits 3) (create-linear-topology num-qubits) ; Ring needs at least 3 qubits
    :else
    (vec (for [i (range num-qubits)]
           (let [prev (mod (dec i) num-qubits)
                 next (mod (inc i) num-qubits)]
             [prev next])))))

(defn create-star-topology
  "Create a star hardware topology with one central qubit connected to all others.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for star topology"
  [num-qubits]
  (cond
    (< num-qubits 1) []
    (= num-qubits 1) [[]]
    :else
    (vec (cons (vec (range 1 num-qubits))  ; Center qubit (0) connects to all others
               (repeat (dec num-qubits) [0]))))) ; All other qubits connect only to center

(defn create-grid-topology
  "Create a grid hardware topology with qubits arranged in a rectangular grid.
  
  Parameters:
  - rows: Number of rows in the grid
  - cols: Number of columns in the grid
  
  Returns:
  Vector of vectors representing adjacency list for grid topology"
  [rows cols]
  (let [num-qubits (* rows cols)]
    (vec (for [i (range num-qubits)]
           (let [row (quot i cols)
                 col (mod i cols)]
             (vec (concat
                   ;; Up neighbor
                   (when (> row 0) [(- i cols)])
                   ;; Down neighbor  
                   (when (< row (dec rows)) [(+ i cols)])
                   ;; Left neighbor
                   (when (> col 0) [(dec i)])
                   ;; Right neighbor
                   (when (< col (dec cols)) [(inc i)]))))))))

;; Hardware Topology Optimization Functions

(defn validate-topology
  "Validate that a hardware topology is well-formed.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  
  Returns:
  Boolean indicating if topology is valid"
  [topology]
  (and (vector? topology)
       (every? vector? topology)
       ;; Check that no qubit connects to itself
       (every? (fn [qubit-id]
                 (let [neighbors (get topology qubit-id)]
                   (not (some #(= % qubit-id) neighbors))))
               (range (count topology)))
       ;; Check that connections are symmetric
       (every? (fn [qubit-id]
                 (let [neighbors (get topology qubit-id)]
                   (every? (fn [neighbor]
                             (and (< neighbor (count topology))
                                  (some #(= % qubit-id) (get topology neighbor))))
                           neighbors)))
               (range (count topology)))))

(defn calculate-distance-matrix
  "Calculate shortest path distances between all pairs of qubits in topology.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  
  Returns:
  2D vector where element [i][j] is the shortest distance from qubit i to qubit j"
  [topology]
  (let [num-qubits (count topology)]
    (letfn [(bfs-distance [start end]
              (if (= start end)
                0
                (loop [queue [[start 0]]
                       visited #{start}]
                  (if (empty? queue)
                    Integer/MAX_VALUE ; No path found
                    (let [[current dist] (first queue)
                          rest-queue (rest queue)]
                      (if (= current end)
                        dist
                        (let [neighbors (get topology current)
                              new-nodes (filter #(not (contains? visited %)) neighbors)
                              new-queue (concat rest-queue (map #(vector % (inc dist)) new-nodes))
                              new-visited (into visited new-nodes)]
                          (recur new-queue new-visited))))))))]
      
      (mapv (fn [i]
              (mapv (fn [j]
                      (bfs-distance i j))
                    (range num-qubits)))
            (range num-qubits)))))

(defn extract-two-qubit-operations
  "Extract all two-qubit operations from a circuit.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Vector of maps containing :control and :target qubit pairs"
  [circuit]
  (let [operations (:operations circuit)]
    (->> operations
         (filter (fn [op]
                   (let [params (:operation-params op)]
                     ;; Check for two-qubit operations
                     (or (and (:control params) (:target params))
                         (and (:control1 params) (:target params))
                         (and (:target1 params) (:target2 params))
                         (and (:swap1 params) (:swap2 params))))))
         (map (fn [op]
                (let [params (:operation-params op)]
                  (cond
                    ;; Standard control-target operations (CNOT, CZ, etc.)
                    (and (:control params) (:target params))
                    {:control (:control params) :target (:target params) :operation-type (:operation-type op)}
                    
                    ;; Toffoli gate (control1 as primary control)
                    (and (:control1 params) (:target params))
                    {:control (:control1 params) :target (:target params) :operation-type (:operation-type op)}
                    
                    ;; Fredkin gate 
                    (and (:target1 params) (:target2 params))
                    {:control (:target1 params) :target (:target2 params) :operation-type (:operation-type op)}
                    
                    ;; SWAP operations
                    (and (:swap1 params) (:swap2 params))
                    {:control (:swap1 params) :target (:swap2 params) :operation-type (:operation-type op)})))))))

(defn calculate-mapping-cost
  "Calculate the cost of a logical-to-physical qubit mapping.
  
  Parameters:
  - two-qubit-ops: Vector of two-qubit operations with :control and :target
  - mapping: Map from logical qubit to physical qubit
  - distance-matrix: 2D vector of distances between physical qubits
  
  Returns:
  Total cost (sum of distances for all two-qubit operations)"
  [two-qubit-ops mapping distance-matrix]
  (reduce (fn [total-cost op]
            (let [logical-control (:control op)
                  logical-target (:target op)
                  physical-control (get mapping logical-control)
                  physical-target (get mapping logical-target)]
              (if (and physical-control physical-target)
                (+ total-cost (get-in distance-matrix [physical-control physical-target]))
                ;; If mapping is incomplete, return very high cost
                Integer/MAX_VALUE)))
          0
          two-qubit-ops))

(defn find-shortest-path
  "Find shortest path between two qubits in the topology.
  
  Parameters:
  - topology: Hardware topology
  - start: Starting qubit
  - end: Ending qubit
  
  Returns:
  Vector of qubits representing the path from start to end"
  [topology start end]
  (if (= start end)
    [start]
    (loop [queue [[start [start]]]
           visited #{start}]
      (if (empty? queue)
        nil ; No path found
        (let [[current path] (first queue)
              rest-queue (rest queue)]
          (if (= current end)
            path
            (let [neighbors (get topology current)
                  new-nodes (filter #(not (contains? visited %)) neighbors)
                  new-queue (concat rest-queue 
                                    (map #(vector % (conj path %)) new-nodes))
                  new-visited (into visited new-nodes)]
              (recur new-queue new-visited))))))))

(defn generate-swap-operations
  "Generate SWAP operations to route a qubit from start to end position.
  
  Parameters:
  - path: Vector of qubits representing routing path
  - target-qubit: The logical qubit that needs to be moved
  
  Returns:
  Vector of SWAP operation maps"
  [path target-qubit]
  (if (<= (count path) 2)
    [] ; No SWAPs needed for adjacent qubits
    (let [swap-pairs (partition 2 1 path)] ; Create adjacent pairs
      (mapv (fn [[q1 q2]]
              {:operation-type :swap
               :operation-params {:target1 q1 :target2 q2}
               :routing-info {:moves-qubit target-qubit :from q1 :to q2}})
            swap-pairs))))

(defn find-optimal-mapping
  "Find an optimal mapping from logical qubits to physical qubits using a greedy approach.
  
  Parameters:
  - When called with 3 args [circuit topology distance-matrix]:
    - circuit: Quantum circuit to optimize
    - topology: Hardware topology
    - distance-matrix: Precomputed distance matrix
  - When called with 3 args [two-qubit-ops num-physical-qubits distance-matrix] (legacy):
    - two-qubit-ops: Vector of two-qubit operations
    - num-physical-qubits: Number of physical qubits available
    - distance-matrix: Precomputed distance matrix
  
  Returns:
  Map from logical qubit to physical qubit"
  [arg1 arg2 arg3]
  (let [;; Detect which signature is being used
        [two-qubit-ops num-logical-qubits num-physical-qubits distance-matrix] 
        (if (map? arg1) ; First arg is circuit
          [(extract-two-qubit-operations arg1) (:num-qubits arg1) (count arg2) arg3]
          [arg1 (count (set (concat (map :control arg1) (map :target arg1)))) arg2 arg3])]
    
    (when (> num-logical-qubits num-physical-qubits)
      (throw (ex-info "Circuit requires more qubits than available in topology"
                      {:logical-qubits num-logical-qubits
                       :physical-qubits num-physical-qubits})))
    
    ;; Simple greedy approach: try different starting offsets
    (if (<= num-logical-qubits 8) ; Manageable for small circuits
      (let [logical-qubits (range num-logical-qubits)
            all-mappings (for [offset (range num-physical-qubits)]
                           (zipmap logical-qubits
                                   (map #(mod (+ % offset) num-physical-qubits)
                                        logical-qubits)))
            best-mapping (first (sort-by #(calculate-mapping-cost two-qubit-ops % distance-matrix)
                                         all-mappings))]
        best-mapping)
      ;; For larger circuits, use identity mapping as placeholder
      (zipmap (range num-logical-qubits) (range num-logical-qubits)))))

(defn optimize-for-topology
  "Optimize a quantum circuit for a specific hardware topology.
  
  This function performs topology-aware optimization by:
  1. Finding an optimal mapping from logical to physical qubits
  2. Inserting SWAP operations when needed for routing
  3. Minimizing the total cost of the circuit on the given topology
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - topology: Hardware topology as vector of vectors (adjacency list)
  - options: Optional map with optimization options:
      :insert-swaps? - Whether to insert SWAP operations for routing (default: true)
      :optimize-mapping? - Whether to optimize qubit mapping (default: true)
  
  Returns:
  Map containing:
  - :quantum-circuit - The topology-optimized circuit
  - :logical-to-physical - Map from logical qubit to physical qubit
  - :physical-to-logical - Map from physical qubit to logical qubit  
  - :swap-count - Number of SWAP operations inserted
  - :total-cost - Total routing cost of the optimized circuit
  - :topology-summary - Human-readable summary of topology optimization
  
  Example:
  ;; Linear topology for 5 qubits: 0-1-2-3-4
  (def linear-topology [[1] [0 2] [1 3] [2 4] [3]])
  (optimize-for-topology my-circuit linear-topology)
  ;=> {:quantum-circuit <optimized-circuit>, :logical-to-physical {0 1, 1 2, 2 3}, ...}"
  ([circuit topology]
   (optimize-for-topology circuit topology {}))
  
  ([circuit topology options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)
          (validate-topology topology)]}
   
   (let [optimize-mapping? (get options :optimize-mapping? true)
         
         ;; Calculate distance matrix for topology
         distance-matrix (calculate-distance-matrix topology)
         
         ;; Find optimal qubit mapping
         logical-to-physical (if optimize-mapping?
                               (find-optimal-mapping circuit topology distance-matrix)
                               ;; Use identity mapping if optimization disabled
                               (zipmap (range (:num-qubits circuit)) 
                                       (range (:num-qubits circuit))))
         
         ;; Create reverse mapping
         physical-to-logical (into {} (map (fn [[l p]] [p l]) logical-to-physical))
         
         ;; Apply the mapping to the circuit operations
         mapped-operations (mapv #(update-operation-params % 
                                                           (fn [qubit-id] 
                                                             (get logical-to-physical qubit-id qubit-id)))
                                 (:operations circuit))
         
         ;; Insert SWAP operations for non-adjacent two-qubit operations
         insert-swaps? (get options :insert-swaps? true)
         [final-operations swap-count] (if insert-swaps?
                                         (let [result-operations (atom [])
                                               total-swaps (atom 0)
                                               ;; Track current physical positions of logical qubits
                                               current-mapping (atom logical-to-physical)]
                                           ;; Process each operation in the circuit in order
                                           (doseq [op (:operations circuit)]
                                             (let [params (:operation-params op)]
                                               (if (and (:control params) (:target params)) ; Two-qubit operation
                                                 (let [logical-control (:control params)
                                                       logical-target (:target params)
                                                       physical-control (get @current-mapping logical-control)
                                                       physical-target (get @current-mapping logical-target)
                                                       distance (get-in distance-matrix [physical-control physical-target])]
                                                   (if (> distance 1) ; Not adjacent, need SWAPs
                                                     ;; Find where to move the control qubit to be adjacent to target
                                                     (let [path (find-shortest-path topology physical-control physical-target)
                                                           ;; Move control to the position adjacent to target (second-to-last in path)
                                                           adjacent-position (nth path (- (count path) 2))
                                                           ;; Generate SWAPs to move from current control position to adjacent position
                                                           swap-path (find-shortest-path topology physical-control adjacent-position)
                                                           swap-ops (if (> (count swap-path) 2)
                                                                      (generate-swap-operations swap-path logical-control)
                                                                      ;; For adjacent moves, manually create the SWAP
                                                                      (if (= (count swap-path) 2)
                                                                        [{:operation-type :swap
                                                                          :operation-params {:target1 (first swap-path) 
                                                                                           :target2 (second swap-path)}
                                                                          :routing-info {:moves-qubit logical-control 
                                                                                       :from (first swap-path) 
                                                                                       :to (second swap-path)}}]
                                                                        []))]
                                                       ;; Add SWAP operations FIRST
                                                       (swap! total-swaps + (count swap-ops))
                                                       (swap! result-operations into swap-ops)
                                                       ;; After SWAPs, control is at adjacent position
                                                       (let [final-op {:operation-type (:operation-type op)
                                                                      :operation-params {:control adjacent-position
                                                                                       :target physical-target}}]
                                                         (swap! result-operations conj final-op)))
                                                     ;; Adjacent qubits, no SWAPs needed
                                                     (let [mapped-op {:operation-type (:operation-type op)
                                                                     :operation-params {:control physical-control
                                                                                      :target physical-target}}]
                                                       (swap! result-operations conj mapped-op))))
                                                 ;; Single-qubit operation - map directly
                                                 (let [mapped-op (update-operation-params op 
                                                                                          (fn [qubit-id] 
                                                                                            (get @current-mapping qubit-id qubit-id)))]
                                                   (swap! result-operations conj mapped-op)))))
                                           [@result-operations @total-swaps])
                                         [mapped-operations 0])
         
         ;; Calculate total cost using the original logical operations
         original-two-qubit-ops (extract-two-qubit-operations circuit)
         total-cost (calculate-mapping-cost original-two-qubit-ops 
                                            logical-to-physical
                                            distance-matrix)
         
         ;; Create optimized circuit
         optimized-circuit (assoc circuit :operations final-operations)
         
         ;; Generate summary
         topology-summary (str "Topology optimization summary:\n"
                               "- Hardware qubits: " (count topology) "\n"
                               "- Logical qubits: " (:num-qubits circuit) "\n"
                               "- Qubit mapping: " logical-to-physical "\n"
                               "- SWAP operations added: " swap-count "\n"
                               "- Total routing cost: " total-cost)]
     
     {:quantum-circuit optimized-circuit
      :logical-to-physical logical-to-physical
      :physical-to-logical physical-to-logical
      :swap-count swap-count
      :total-cost total-cost
      :topology-summary topology-summary})))

;; Topology Analysis and Utility Functions

(defn analyze-topology-connectivity
  "Analyze the connectivity properties of a hardware topology.
  
  Parameters:
  - topology: Hardware topology as vector of vectors
  
  Returns:
  Map containing topology analysis:
  - :num-qubits - Total number of qubits
  - :total-edges - Total number of edges (connections)
  - :avg-degree - Average degree (connections per qubit)
  - :max-degree - Maximum degree
  - :min-degree - Minimum degree
  - :diameter - Maximum shortest path distance between any two qubits
  - :is-connected - Whether the topology is fully connected"
  [topology]
  (let [num-qubits (count topology)
        degrees (mapv count topology)
        total-edges (/ (reduce + degrees) 2) ; Each edge counted twice
        avg-degree (/ (reduce + degrees) (double num-qubits)) ; Total degree sum divided by qubits
        max-degree (apply max degrees)
        min-degree (apply min degrees)
        
        ;; Calculate diameter using distance matrix
        distance-matrix (calculate-distance-matrix topology)
        all-distances (for [i (range num-qubits)
                            j (range num-qubits)
                            :when (not= i j)]
                        (get-in distance-matrix [i j]))
        diameter (if (some #(= % Integer/MAX_VALUE) all-distances)
                   Integer/MAX_VALUE ; Disconnected
                   (apply max all-distances))
        is-connected (not= diameter Integer/MAX_VALUE)]
    
    {:num-qubits num-qubits
     :total-edges total-edges
     :avg-degree avg-degree
     :max-degree max-degree
     :min-degree min-degree
     :diameter diameter
     :is-connected is-connected}))

(defn get-topology-info
  "Get human-readable information about a topology.
  
  Parameters:
  - topology: Hardware topology
  - name: Optional name for the topology
  
  Returns:
  String with topology information"
  ([topology]
   (get-topology-info topology "Topology"))
  ([topology name]
   (let [analysis (analyze-topology-connectivity topology)]
     (str name " Analysis:\n"
          "- Qubits: " (:num-qubits analysis) "\n"
          "- Edges: " (:total-edges analysis) "\n"
          "- Average degree: " (format "%.2f" (:avg-degree analysis)) "\n"
          "- Degree range: " (:min-degree analysis) "-" (:max-degree analysis) "\n"
          "- Diameter: " (if (= (:diameter analysis) Integer/MAX_VALUE) 
                           "∞ (disconnected)" 
                           (:diameter analysis)) "\n"
          "- Connected: " (:is-connected analysis)))))

(defn compare-topologies
  "Compare multiple hardware topologies for a given circuit.
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - topologies: Map of topology-name to topology
  
  Returns:
  Vector of maps sorted by total cost, each containing:
  - :topology-name - Name of the topology
  - :total-cost - Total routing cost
  - :swap-count - Number of SWAP operations needed
  - :logical-to-physical - Optimal qubit mapping"
  [circuit topologies]
  (->> topologies
       (map (fn [[name topology]]
              (let [result (optimize-for-topology circuit topology)]
                {:topology-name name
                 :total-cost (:total-cost result)
                 :swap-count (:swap-count result)
                 :logical-to-physical (:logical-to-physical result)})))
       (sort-by :total-cost)
       vec))

;; Rich comment block for REPL experimentation
(comment
  ;; Example usage of topology optimization
  
  ;; Create a test circuit
  (def bell-circuit 
    {:num-qubits 2
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :cnot :operation-params {:control 0 :target 1}}]})
  
  ;; Test with different topologies
  (def linear-2 (create-linear-topology 2))
  (def result (optimize-for-topology bell-circuit linear-2))
  (println (:topology-summary result))
  
  ;; Compare multiple topologies
  (def topologies {"Linear-5" (create-linear-topology 5)
                   "Ring-5" (create-ring-topology 5)
                   "Star-5" (create-star-topology 5)})
  
  (doseq [[name topo] topologies]
    (println (get-topology-info topo name)))
  
  ;; Analyze a complex circuit
  (def complex-circuit 
    {:num-qubits 4
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :cnot :operation-params {:control 0 :target 1}}
                  {:operation-type :cnot :operation-params {:control 1 :target 2}}
                  {:operation-type :cnot :operation-params {:control 2 :target 3}}
                  {:operation-type :cnot :operation-params {:control 0 :target 3}}]})
  
  (def comparison (compare-topologies complex-circuit topologies))
  (doseq [[name result] comparison]
    (println (str name ": cost=" (:total-cost result))))
  
  ;; Test IBM-like topologies
  (def ibm-5q-linear [[1] [0 2] [1 3] [2 4] [3]])
  (def ibm-5q-t [[1] [0 2 3] [1 4] [1] [2]])
  
  (println "IBM 5-qubit linear:" (get-topology-info ibm-5q-linear "IBM Linear"))
  (println "IBM 5-qubit T:" (get-topology-info ibm-5q-t "IBM T-shape"))
  )