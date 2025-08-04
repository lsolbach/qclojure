(ns org.soulspace.qclojure.domain.hamiltonian
  "Core Hamiltonian representation and manipulation for quantum systems.
  
  This namespace provides the fundamental building blocks for working with
  quantum Hamiltonians in the VQE algorithm and other quantum algorithms.
  
  Key Responsibilities:
  - Pauli term creation and validation
  - Hamiltonian expectation value calculations  
  - Measurement grouping for hardware efficiency
  - Commuting term analysis
  
  Design Principles:
  - Pure functions with no side effects
  - Hardware-agnostic mathematical operations
  - Composable building blocks for higher-level algorithms"
(:require [clojure.spec.alpha :as s]
          [org.soulspace.qclojure.domain.observables :as obs]))

;;
;; Specs for Hamiltonian components
;;
(s/def ::pauli-term 
  (s/keys :req-un [::coefficient ::pauli-string]))

(s/def ::coefficient number?)
(s/def ::pauli-string string?)

(s/def ::hamiltonian 
  (s/coll-of ::pauli-term))

;;
;; Hamiltonian Representation and Manipulation
;;
(defn pauli-term
  "Create a Pauli term with coefficient and Pauli string.
  
  Parameters:
  - coefficient: Real coefficient for the term
  - pauli-string: String like 'XYZZ' representing tensor product of Pauli operators
  
  Returns:
  Map representing a single term in the Hamiltonian"
  [coefficient pauli-string]
  {:pre [(number? coefficient) (string? pauli-string)
         (every? #{\I \X \Y \Z} pauli-string)]}
  {:coefficient coefficient
   :pauli-string pauli-string})

(defn validate-hamiltonian
  "Validate that a Hamiltonian is properly formed.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  
  Returns:
  Boolean indicating validity"
  [hamiltonian]
  (and (coll? hamiltonian)
       (every? #(s/valid? ::pauli-term %) hamiltonian)
       (let [string-lengths (map #(count (:pauli-string %)) hamiltonian)]
         (or (empty? string-lengths) ; empty Hamiltonian is valid
             (apply = string-lengths)))))

;;
;; Expectation Value Calculation
;;
(defn pauli-string-expectation
  "Calculate expectation value of a single Pauli string.
  
  Parameters:
  - pauli-string: String like 'XYZZ' representing Pauli operators
  - quantum-state: Quantum state to measure
  
  Returns:
  Real expectation value"
  [pauli-string quantum-state]
  {:pre [(string? pauli-string) (map? quantum-state)]}
  (let [observable (obs/pauli-string->observable pauli-string)]
    (obs/expectation-value observable quantum-state)))

(defn hamiltonian-expectation
  "Calculate expectation value of a Hamiltonian.
  
  ⟨H⟩ = Σᵢ cᵢ ⟨Pᵢ⟩ where cᵢ are coefficients and Pᵢ are Pauli strings.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  - quantum-state: Quantum state to measure
  
  Returns:
  Real expectation value (energy)"
  [hamiltonian quantum-state]
  {:pre [(validate-hamiltonian hamiltonian) (map? quantum-state)]}
  (reduce + (map (fn [term]
                   (let [coeff (:coefficient term)
                         pauli-str (:pauli-string term)]
                     (* coeff (pauli-string-expectation pauli-str quantum-state))))
                 hamiltonian)))

(defn group-commuting-terms
  "Group Hamiltonian terms that can be measured simultaneously.
  
  Terms that commute can be measured in the same quantum circuit execution,
  reducing the number of measurements needed.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  
  Returns:
  Vector of groups, where each group is a collection of commuting terms"
  [hamiltonian]
  {:pre [(validate-hamiltonian hamiltonian)]}
  ;; Simple greedy grouping algorithm
  ;; More sophisticated algorithms exist (e.g., graph coloring)
  (loop [ungrouped hamiltonian
         groups []]
    (if (empty? ungrouped)
      groups
      (let [current-term (first ungrouped)
            current-string (:pauli-string current-term)
            ;; Find all terms that commute with current term
            commuting (filter (fn [term]
                                (let [other-string (:pauli-string term)]
                                  ;; Two Pauli strings commute if they have an even number
                                  ;; of positions where both have non-commuting Paulis
                                  (even? (count (filter (fn [[p1 p2]]
                                                          (and (not= p1 \I) (not= p2 \I)
                                                               (not= p1 p2)))
                                                        (map vector current-string other-string))))))
                              ungrouped)
            remaining (remove (set commuting) ungrouped)]
        (recur remaining (conj groups commuting))))))

(defn group-pauli-terms-by-measurement-basis
  "Group Pauli terms by their required measurement basis for hardware execution.
  
  This function is essential for efficient quantum hardware execution, as different
  Pauli operators require different measurement bases:
  - Z operators: measured directly in computational basis
  - X operators: require H rotation before measurement
  - Y operators: require S†H rotation before measurement
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  
  Returns:
  Map with measurement basis as key and list of compatible terms as value
  Format: {:z [terms...] :x [terms...] :y [terms...] :mixed [terms...]}"
  [hamiltonian]
  {:pre [(validate-hamiltonian hamiltonian)]}
  (group-by (fn [term]
              (let [pauli-str (:pauli-string term)
                    unique-paulis (set (remove #{\I} pauli-str))]
                (cond
                  (empty? unique-paulis) :identity  ; All identity
                  (= unique-paulis #{\Z}) :z        ; Only Z operators
                  (= unique-paulis #{\X}) :x        ; Only X operators  
                  (= unique-paulis #{\Y}) :y        ; Only Y operators
                  :else :mixed)))                   ; Mixed operators (need separate measurement)
            hamiltonian))
