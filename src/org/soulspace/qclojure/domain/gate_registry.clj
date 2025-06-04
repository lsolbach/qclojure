(ns org.soulspace.qclojure.domain.gate-registry
  "Gate registry and catalog for quantum backends.
  
  This namespace provides a comprehensive catalog of quantum gates
  that can be supported by different quantum backends. Backends can
  reference this registry to declare their supported gate sets."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(def gate-types #{:single-qubit
                  :two-qubit
                  :multi-qubit
                  :parametric})

(def gate-params #{:target :target1 :target2 :control :control1 :control2 :angle})

;; Specs for gate definitions
(s/def ::gate-id keyword?)
(s/def ::gate-type gate-types)
(s/def ::parameter-count nat-int?)
(s/def ::description string?)
(s/def ::native-gate? boolean?)
(s/def ::decomposition (s/coll-of ::gate-id :kind vector?))

(s/def ::gate-definition
  (s/keys :req-un [::gate-id ::gate-type ::description]
          :opt-un [::parameter-count ::native-gate? ::decomposition]))

(s/def ::gate-set (s/coll-of ::gate-id :kind set?))

;; Comprehensive gate catalog
(def gate-catalog
  "Complete catalog of quantum gates with their properties."
  {;; Single-qubit Pauli gates
   :x {:gate-id :x
       :gate-name "X"
       :gate-type :single-qubit
       :description "Pauli-X (NOT) gate - bit flip operation"
       :native-gate? false
       :decomposition [:h :cnot :h]}  ;; X = HZH = H(controlled-Z)H

   :y {:gate-id :y
       :gate-name "Y"
       :gate-type :single-qubit
       :description "Pauli-Y gate - bit flip + phase flip operation"
       :native-gate? false
       :decomposition [:t :t :t :h :t :h]}  ;; Y can be decomposed to H and T gates

   :z {:gate-id :z
       :gate-name "Z"
       :gate-type :single-qubit
       :description "Pauli-Z gate - phase flip operation"
       :native-gate? false
       :decomposition [:t :t :t :t]}  ;; Z = T^4 (four T gates)

   ;; Hadamard gate
   :h {:gate-id :h
       :gate-name "H"
       :gate-type :single-qubit
       :description "Hadamard gate - creates superposition"
       :native-gate? true}

   ;; Phase gates
   :s {:gate-id :s
       :gate-name "S"
       :gate-type :single-qubit
       :description "S gate (π/2 phase gate)"
       :native-gate? false
       :decomposition [:rz]}

   :s-dag {:gate-id :s-dag
           :gate-name "S†"
           :gate-type :single-qubit
           :description "S† gate (−π/2 phase gate)"
           :native-gate? false
           :decomposition [:rz]}

   :t {:gate-id :t
       :gate-name "T"
       :gate-type :single-qubit
       :description "T gate (π/4 phase gate)"
       :native-gate? false
       :decomposition [:rz]}

   :t-dag {:gate-id :t-dag
           :gate-name "T†"
           :gate-type :single-qubit
           :description "T† gate (−π/4 phase gate)"
           :native-gate? false
           :decomposition [:rz]}

   ;; Parametric rotation gates
   :rx {:gate-id :rx
        :gate-name "RX"
        :gate-type :parametric
        :parameter-count 1
        :description "Rotation around X-axis"
        :native-gate? false
        :decomposition [:h :t :h]}    ;; RX can be built from H-T-H sequence

   :ry {:gate-id :ry
        :gate-name "RY"
        :gate-type :parametric
        :parameter-count 1
        :description "Rotation around Y-axis"
        :native-gate? false
        :decomposition [:rx :rz]}

   :rz {:gate-id :rz
        :gate-name "RZ"
        :gate-type :parametric
        :parameter-count 1
        :description "Rotation around Z-axis"
        :native-gate? false
        :decomposition [:h :cnot :t :cnot :h]}  ;; RZ can be implemented with universal gates

   :phase {:gate-id :phase
           :gate-name "Phase"
           :gate-type :parametric
           :parameter-count 1
           :parameters [:theta]
           :description "Arbitrary phase gate"
           :native-gate? false
           :decomposition [:rz]}

   ;; Two-qubit gates
   :cnot {:gate-id :cnot
          :gate-name "CNOT"
          :gate-type :two-qubit
          :description "Controlled-NOT gate"
          :native-gate? true}

   :cx {:gate-id :cx
        :gate-name "CX"
        :gate-type :two-qubit
        :description "Controlled-X gate (alias for CNOT)"
        :native-gate? true
        :decomposition [:cnot]}

   :cz {:gate-id :cz
        :gate-name "CZ"
        :gate-type :two-qubit
        :description "Controlled-Z gate"
        :native-gate? false
        :decomposition [:cnot :h]}

   :cy {:gate-id :cy
        :gate-name "CY"
        :gate-type :two-qubit
        :description "Controlled-Y gate"
        :native-gate? false
        :decomposition [:cnot :ry :rx]}

   :swap {:gate-id :swap
          :gate-name "SWAP"
          :gate-type :two-qubit
          :description "SWAP gate - exchanges two qubits"
          :native-gate? false
          :decomposition [:cnot]}

   :iswap {:gate-id :iswap
           :gate-name "iSWAP"
           :gate-type :two-qubit
           :description "iSWAP gate - swap with phase"
           :native-gate? false
           :decomposition [:cnot :rz]}

   ;; Parametric two-qubit gates
   :crx {:gate-id :crx
         :gate-name "CRX"
         :gate-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around X-axis"
         :native-gate? false
         :decomposition [:cnot :rx]}

   :cry {:gate-id :cry
         :gate-name "CRY"
         :gate-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Y-axis"
         :native-gate? false
         :decomposition [:cnot :ry]}

   :crz {:gate-id :crz
         :gate-name "CRZ"
         :gate-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Z-axis"
         :native-gate? false
         :decomposition [:cnot :rz]}

   ;; Multi-qubit gates
   :toffoli {:gate-id :toffoli
             :gate-name "Toffoli"
             :gate-type :multi-qubit
             :description "Toffoli gate (CCX) - doubly controlled NOT"
             :native-gate? false
             :decomposition [:cnot :h :t]}

   :fredkin {:gate-id :fredkin
             :gate-name "Fredkin"
             :gate-type :multi-qubit
             :description "Fredkin gate (CSWAP) - controlled SWAP"
             :native-gate? false
             :decomposition [:cnot :toffoli]}})

;; Predefined gate sets for common hardware
(def universal-gate-set
  "Universal gate set sufficient for any quantum computation."
  #{:h :t :cnot})

(def basic-gate-set
  "Basic gate set with common single and two-qubit gates."
  #{:x :y :z :h :s :t :cnot :swap})

(def parametric-gate-set
  "Gate set with parametric rotations."
  #{:rx :ry :rz :cnot :phase})

(def native-simulator-gates
  "Gates typically supported natively by quantum simulators."
  #{:x :y :z :h :s :s-dag :t :t-dag :rx :ry :rz :phase
    :cnot :cx :cz :cy :swap :crx :cry :crz :toffoli :fredkin})

(def superconducting-hardware-gates
  "Typical gate set for superconducting quantum processors."
  #{:x :z :h :s :t :rx :ry :rz :cnot})

(def trapped-ion-hardware-gates
  "Typical gate set for trapped ion quantum processors."
  #{:x :y :z :h :rx :ry :rz :cnot :swap})

;; Utility functions
(defn get-gate-info
  "Get detailed information about a specific gate.
  
  Parameters:
  - gate-name: Keyword identifying the gate
  
  Returns: Gate definition map or nil if not found"
  [gate-name]
  (get gate-catalog gate-name))

(defn get-gates-by-type
  "Get all gates of a specific type.
  
  Parameters:
  - gate-type: One of :single-qubit, :two-qubit, :multi-qubit, :parametric
  
  Returns: Set of gate names"
  [gate-type]
  (->> gate-catalog
       (filter #(= gate-type (:gate-type (val %))))
       (map key)
       (into #{})))

(defn get-native-gates
  "Get all gates marked as native (hardware-implementable).
  
  Returns: Set of gate names that are typically native to hardware"
  []
  (->> gate-catalog
       (filter #(:native-gate? (val %)))
       (map key)
       (into #{})))

(defn validate-gate-set
  "Validate that all gates in a set are known.
  
  Parameters:
  - gate-set: Set of gate keywords
  
  Returns: True if all gates are known, false otherwise"
  [gate-set]
  (set/subset? gate-set (into #{} (keys gate-catalog))))

(defn get-gate-dependencies
  "Get the decomposition dependencies for a gate.
  
  Parameters:
  - gate-name: Keyword identifying the gate
  
  Returns: Vector of gate names this gate decomposes into, or empty if native"
  [gate-name]
  (get-in gate-catalog [gate-name :decomposition] []))

(defn expand-gate-set
  "Expand a gate set to include all decomposition dependencies.
  
  This function takes a set of gates and recursively adds all gates
  that are needed to implement them through decomposition.
  
  Parameters:
  - gate-set: Set of gate keywords
  
  Returns: Expanded set including all dependencies"
  [gate-set]
  (loop [expanded gate-set
         to-process gate-set]
    (if (empty? to-process)
      expanded
      (let [current-gate (first to-process)
            remaining (rest to-process)
            dependencies (get-gate-dependencies current-gate)
            new-deps (set/difference (into #{} dependencies) expanded)]
        (recur (set/union expanded new-deps)
               (concat remaining new-deps))))))

(defn minimal-native-set
  "Find the minimal set of native gates needed to implement a gate set.
  
  Parameters:
  - gate-set: Set of gate keywords
  
  Returns: Minimal set of native gates needed"
  [gate-set]
  (let [expanded (expand-gate-set gate-set)
        native-gates (get-native-gates)]
    (set/intersection expanded native-gates)))

;; Specs for validation
(s/fdef get-gate-info
  :args (s/cat :gate-id ::gate-id)
  :ret (s/nilable ::gate-definition))

(s/fdef get-gates-by-type
  :args (s/cat :gate-type ::gate-type)
  :ret ::gate-set)

(s/fdef validate-gate-set
  :args (s/cat :gate-set ::gate-set)
  :ret boolean?)

(s/fdef expand-gate-set
  :args (s/cat :gate-set ::gate-set)
  :ret ::gate-set)

(comment
  
  
  ;
  )