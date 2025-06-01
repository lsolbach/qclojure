(ns org.soulspace.qclojure.application.circuit-transformer
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming gates not supported
   by the backend into equivalent sequences of supported gates."
  (:require [clojure.spec.alpha :as s]
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

(comment
  ;; Example usage
  ;; Note: quantum-circuit is already required at the namespace level
  (require '[org.soulspace.qclojure.adapter.backend.quantum-simulator :as sim])
  
  ;; Create a circuit and a backend
  (def my-circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/y-gate 1)
                      (qc/cnot-gate 0 1)))
                      
  (def simulator (sim/create-simulator {:supported-gates #{:h :x :cnot}}))
  
  ;; Transform the circuit
  (def result (transform-circuit my-circuit simulator))
  
  ;; Show transformation summary
  (println (get-transformation-summary result))
  
  ;; Access the transformed circuit
  (def transformed-circuit (:quantum-circuit result))
)
