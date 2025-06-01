(ns org.soulspace.qclojure.application.circuit-transformer
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming gates not supported
   by the backend into equivalent sequences of supported gates."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.gate-registry :as gr]
            [org.soulspace.qclojure.application.quantum-backend :as qb]))

;; Specs
(s/def ::transformation-result
  (s/keys :req-un [::qc/quantum-circuit]
          :opt-un [::transformed-gates ::unsupported-gates]))

(defn- can-be-fully-decomposed?
  "Check if a gate type can be fully decomposed into supported gates.
  
  Parameters:
  - gate-type: Type of gate to check for decomposition
  - supported-gates: Set of gate types supported by the backend
  - visited: Set of gate types already visited (to prevent infinite recursion)
  
  Returns:
  Boolean indicating whether the gate can be fully decomposed into supported gates"
  [gate-type supported-gates visited]
  ;; Already visited this gate in recursion? Return false to break cycles
  (if (contains? visited gate-type)
    false
    ;; Is the gate directly supported? Return true
    (if (contains? supported-gates gate-type)
      true
      ;; Not directly supported, try decomposition
      (let [decomposition (gr/get-gate-dependencies gate-type)]
        (if (empty? decomposition)
          ;; No decomposition available
          false
          ;; Check if ALL gates in decomposition can be fully decomposed
          (every? #(can-be-fully-decomposed? % supported-gates (conj visited gate-type))
                  decomposition))))))

(defn- decompose-gate
  "Decompose a gate into a sequence of more primitive gates.
  
  Parameters:
  - gate: Gate map to decompose
  - backend: Backend to target for decomposition
  
  Returns:
  Vector of gate maps representing the decomposition, or the original gate 
  if no decomposition is available or if it cannot be fully decomposed to supported gates"
  [gate backend]
  (let [gate-type (:gate-type gate)
        gate-params (:gate-params gate)
        decomposition (gr/get-gate-dependencies gate-type)
        supported-gates (qb/get-supported-gates backend)]
    
    ;; If the gate is already supported, no need to decompose
    (if (contains? supported-gates gate-type)
      [gate]
      
      ;; If no decomposition available OR cannot be fully decomposed, return original
      ;; This breaks potential infinite loops
      (if (or (empty? decomposition)
              (not (every? #(can-be-fully-decomposed? % supported-gates #{}) decomposition)))
        [gate]
        
        ;; Create a sequence of gates based on decomposition
        (mapv (fn [sub-gate-type]
                (cond
                  ;; Handle different gate types and parameters based on the original gate
                  (= sub-gate-type :rx) 
                  {:gate-type :rx 
                   :gate-params (select-keys gate-params [:target :angle])}
                  
                  (= sub-gate-type :ry)
                  {:gate-type :ry
                   :gate-params (select-keys gate-params [:target :angle])}
                  
                  (= sub-gate-type :rz)
                  {:gate-type :rz
                   :gate-params (select-keys gate-params [:target :angle])}
                  
                  (= sub-gate-type :x)
                  {:gate-type :x
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :y)
                  {:gate-type :y
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :z)
                  {:gate-type :z
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :h)
                  {:gate-type :h
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :s)
                  {:gate-type :s
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :t)
                  {:gate-type :t
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :cnot)
                  {:gate-type :cnot
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :cx)
                  {:gate-type :cx
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :cz)
                  {:gate-type :cz
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :cy)
                  {:gate-type :cy
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :s-dag)
                  {:gate-type :s-dag
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :t-dag)
                  {:gate-type :t-dag
                   :gate-params (select-keys gate-params [:target])}
                  
                  (= sub-gate-type :swap)
                  {:gate-type :swap
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :iswap)
                  {:gate-type :iswap
                   :gate-params (select-keys gate-params [:control :target])}
                  
                  (= sub-gate-type :phase)
                  {:gate-type :phase
                   :gate-params (select-keys gate-params [:target :angle])}
                  
                  (= sub-gate-type :crx)
                  {:gate-type :crx
                   :gate-params (select-keys gate-params [:control :target :angle])}
                  
                  (= sub-gate-type :cry)
                  {:gate-type :cry
                   :gate-params (select-keys gate-params [:control :target :angle])}
                  
                  (= sub-gate-type :crz)
                  {:gate-type :crz
                   :gate-params (select-keys gate-params [:control :target :angle])}
                  
                  (= sub-gate-type :toffoli)
                  {:gate-type :toffoli
                   :gate-params (select-keys gate-params [:control1 :control2 :target])}
                  
                  (= sub-gate-type :fredkin)
                  {:gate-type :fredkin
                   :gate-params (select-keys gate-params [:control :target1 :target2])}
                  
                  ;; Default case for unknown gate types
                  :else {:gate-type sub-gate-type
                         :gate-params gate-params}))
              decomposition)))))

(defn- transform-gates
  "Transform the gates in a circuit to use only backend-supported gates.
  
  Parameters:
  - gates: Original vector of gate maps
  - backend: Quantum backend that defines supported gates
  - max-iterations: Maximum decomposition iterations to prevent infinite loops
  
  Returns:
  A vector of transformed gates that are all supported by the backend or
  gates that couldn't be further decomposed"
  [gates backend max-iterations]
  (loop [current-gates gates
         iteration 0
         processed-gates #{}]  ;; Track gates we've already tried to decompose
    (if (>= iteration max-iterations)
      ;; Safety check to prevent infinite loops
      (throw (ex-info "Maximum iterations reached during circuit transformation" 
                      {:gates current-gates
                       :iteration iteration}))
      
      (let [supported-gates (qb/get-supported-gates backend)
            ;; Find any gates that need decomposition
            needs-decomposition? (fn [gate] 
                                   (and (not (contains? processed-gates (:gate-type gate)))
                                        (not (contains? supported-gates (:gate-type gate)))))
            unsupported (filterv needs-decomposition? current-gates)]
        
        (if (empty? unsupported)
          ;; All gates are either supported or can't be decomposed further
          current-gates
          
          ;; Replace the first unsupported gate with its decomposition
          (let [unsupported-gate (first unsupported)
                gate-type (:gate-type unsupported-gate)
                unsupported-index (.indexOf current-gates unsupported-gate)
                decomposed-gates (decompose-gate unsupported-gate backend)
                
                ;; Check if the gate was actually decomposed
                was-decomposed? (not= [unsupported-gate] decomposed-gates)
                
                ;; If the gate wasn't decomposed, mark it as processed so we don't try again
                new-processed-gates (if was-decomposed?
                                      processed-gates
                                      (conj processed-gates gate-type))
                
                ;; Create new gates vector with decomposition replacing original gate
                new-gates (into []
                                (concat 
                                 (subvec current-gates 0 unsupported-index)
                                 decomposed-gates
                                 (subvec current-gates (inc unsupported-index))))]
            
            (recur new-gates (inc iteration) new-processed-gates)))))))

(defn transform-circuit
  "Transform a quantum circuit to use only gates supported by a given backend.
  
  This function takes a quantum circuit and a backend, and returns a new circuit
  where all gates have been decomposed into gates supported by the backend.
  
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
  - :unsupported-gates - List of gate types that couldn't be transformed
  
  Example:
  (transform-circuit my-circuit backend)
  ;=> {:quantum-circuit <transformed-circuit>, :transformed-gates 3, :unsupported-gates []}"
  ([circuit backend]
   (transform-circuit circuit backend {}))
  
  ([circuit backend options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)
          (satisfies? org.soulspace.qclojure.application.quantum-backend/QuantumBackend backend)]}
   
   (let [max-iterations (get options :max-iterations 100)
         transform-unsupported? (get options :transform-unsupported? true)
         
         original-gates (:gates circuit)
         original-gate-count (count original-gates)
         
         ;; Apply transformation
         transformed-gates (if transform-unsupported?
                             (transform-gates original-gates backend max-iterations)
                             original-gates)
         
         ;; Create new circuit with transformed gates
         transformed-circuit (assoc circuit :gates transformed-gates)
         
         ;; Calculate stats for return value
         new-types (frequencies (map :gate-type transformed-gates))
         supported-gates (qb/get-supported-gates backend)
         remaining-unsupported (into [] 
                                     (filter #(not (contains? supported-gates %)) 
                                             (keys new-types)))]
     
     {:quantum-circuit transformed-circuit
      :transformed-gates (- (count transformed-gates) original-gate-count)
      :unsupported-gates remaining-unsupported})))

(defn get-transformation-summary
  "Get a human-readable summary of a circuit transformation.
  
  Parameters:
  - transformation-result: Result from transform-circuit
  
  Returns:
  String with transformation summary"
  [transformation-result]
  (let [circuit (:quantum-circuit transformation-result)
        transformed (:transformed-gates transformation-result)
        unsupported (:unsupported-gates transformation-result)]
    (str "Circuit transformation summary:\n"
         "- Final gate count: " (count (:gates circuit)) "\n"
         "- Gates transformed: " transformed "\n"
         "- Unsupported gates: " (if (empty? unsupported) "None" (pr-str unsupported)))))

;; Circuit Optimization Functions

(defn- extract-qubit-ids
  "Extract all qubit IDs used by a gate.
  
  Parameters:
  - gate: Gate map with gate-type and gate-params
  
  Returns:
  Set of qubit IDs used by this gate"
  [gate]
  (let [params (:gate-params gate)
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
  
  (let [gates (:gates circuit)
        total-qubits (:num-qubits circuit)
        
        ;; Extract all qubit IDs used in gates
        used-qubits (reduce (fn [acc gate]
                              (into acc (extract-qubit-ids gate)))
                            #{}
                            gates)
        
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

(defn- remap-gate-qubits
  "Remap qubit IDs in a gate according to a qubit mapping.
  
  Parameters:
  - gate: Gate map to remap
  - qubit-mapping: Map from old qubit ID to new qubit ID
  
  Returns:
  Gate map with remapped qubit IDs"
  [gate qubit-mapping]
  (let [params (:gate-params gate)
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
    (assoc gate :gate-params remapped-params)))

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
        
        ;; Remap all gates to use the new qubit IDs
        optimized-gates (mapv #(remap-gate-qubits % qubit-mapping)
                              (:gates circuit))
        
        ;; Create optimized circuit
        optimized-circuit (assoc circuit
                                 :num-qubits optimized-qubits
                                 :gates optimized-gates)
        
        qubits-saved (- original-qubits optimized-qubits)]
    
    {:quantum-circuit optimized-circuit
     :qubit-mapping qubit-mapping
     :qubits-saved qubits-saved
     :original-qubits original-qubits
     :optimized-qubits optimized-qubits}))

(defn optimize-for-backend
  "Comprehensive circuit optimization for a specific backend.
  
  This function combines multiple optimization strategies:
  1. Transform gates to backend-supported equivalents
  2. Optimize qubit usage to minimize qubit count
  3. Optionally apply gate sequence optimizations
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - backend: Target backend for optimization
  - options: Optional map with optimization options:
      :optimize-qubits? - Whether to optimize qubit usage (default: true)
      :transform-gates? - Whether to transform unsupported gates (default: true)
      :max-iterations - Maximum decomposition iterations (default: 100)
  
  Returns:
  Map containing:
  - :quantum-circuit - The fully optimized circuit
  - :transformation-result - Result from gate transformation
  - :qubit-optimization-result - Result from qubit optimization (if enabled)
  - :optimization-summary - Human-readable summary of all optimizations
  
  Example:
  (optimize-for-backend my-circuit backend {:optimize-qubits? true})
  ;=> {:quantum-circuit <optimized-circuit>, 
  ;    :transformation-result {...}, 
  ;    :qubit-optimization-result {...},
  ;    :optimization-summary \"...\"}"
  ([circuit backend]
   (optimize-for-backend circuit backend {}))
  
  ([circuit backend options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)
          (satisfies? org.soulspace.qclojure.application.quantum-backend/QuantumBackend backend)]}
   
   (let [optimize-qubits? (get options :optimize-qubits? true)
         transform-gates? (get options :transform-gates? true)
         
         ;; Step 1: Transform gates to backend-supported equivalents
         transformation-result (if transform-gates?
                                 (transform-circuit circuit backend options)
                                 ;; Even if not transforming, we should identify unsupported gates
                                 (let [supported-gates (qb/get-supported-gates backend)
                                       gate-types (map :gate-type (:gates circuit))
                                       unsupported (filterv #(not (contains? supported-gates %)) gate-types)
                                       unique-unsupported (vec (distinct unsupported))]
                                   {:quantum-circuit circuit
                                    :transformed-gates 0
                                    :unsupported-gates unique-unsupported}))
         
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
                      "- Original gates: " (count (:gates circuit)) "\n"
                      "- Final gates: " (count (:gates final-circuit)) "\n"
                      "- Gates transformed: " (:transformed-gates transformation-result) "\n"
                      "- Unsupported gates: " (if (empty? (:unsupported-gates transformation-result))
                                                "None"
                                                (pr-str (:unsupported-gates transformation-result))))]
     
     {:quantum-circuit final-circuit
      :transformation-result transformation-result
      :qubit-optimization-result qubit-optimization-result
      :optimization-summary summary})))

;; Existing transform-circuit and related functions continue below...
