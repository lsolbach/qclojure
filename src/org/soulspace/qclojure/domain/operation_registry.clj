(ns org.soulspace.qclojure.domain.operation-registry
  "Operation registry and catalog for quantum backends.
  
  This namespace provides a comprehensive catalog of operations,
  quantum gates and measurements, that can be supported by
  different quantum backends. Backends can reference this registry
  to declare their supported operation sets."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(def operation-kinds
  "Set of operation kinds."
  #{:gate :measurement})

(def operation-types
  "Set of operation types."
  #{:single-qubit
    :two-qubit
    :multi-qubit
    :parametric})

(def operation-params
  "Set of operation parameters."
  #{:target :target1 :target2 :control :control1 :control2
    :angle})

;; Specs for operation definitions
(s/def ::operation-kind keyword?)
(s/def ::operation-id keyword?)
(s/def ::operation-type operation-types)
(s/def ::parameter-count nat-int?)
(s/def ::description string?)
(s/def ::native-gate? boolean?)
(s/def ::decomposition (s/coll-of ::operation-id :kind vector?))

(s/def ::operation-definition
  (s/keys :req-un [::operation-id ::operation-type ::description]
          :opt-un [::parameter-count ::native-gate? ::decomposition]))

(s/def ::operation-set (s/coll-of ::operation-id :kind set?))

;; Comprehensive operation catalog
(def operation-catalog
  "Complete catalog of operations with their properties.
   Operations can be gates or measurements, with various types and parameters."
  {;; Single-qubit Pauli operations
   :x {:operation-kind :gate
       :operation-id :x
       :operation-name "X"
       :operation-type :single-qubit
       :description "Pauli-X (NOT) gate - bit flip operation"
       :native-gate? false
       :decomposition [:h :cnot :h]}  ;; X = HZH = H(controlled-Z)H

   :y {:operation-kind :gate
       :operation-id :y
       :operation-name "Y"
       :operation-type :single-qubit
       :description "Pauli-Y gate - bit flip + phase flip operation"
       :native-gate? false
       :decomposition [:t :t :t :h :t :h]}  ;; Y can be decomposed to H and T gates

   :z {:operation-kind :gate
       :operation-id :z
       :operation-name "Z"
       :operation-type :single-qubit
       :description "Pauli-Z gate - phase flip operation"
       :native-gate? false
       :decomposition [:t :t :t :t]}  ;; Z = T^4 (four T gates)

   ;; Hadamard gate
   :h {:operation-kind :gate
       :operation-id :h
       :operation-name "H"
       :operation-type :single-qubit
       :description "Hadamard gate - creates superposition"
       :native-gate? true}

   ;; Phase gates
   :s {:operation-kind :gate
       :operation-id :s
       :operation-name "S"
       :operation-type :single-qubit
       :description "S gate (π/2 phase gate)"
       :native-gate? false
       :decomposition [:rz]}

   :s-dag {:operation-kind :gate
           :operation-id :s-dag
           :operation-name "S†"
           :operation-type :single-qubit
           :description "S† gate (−π/2 phase gate)"
           :native-gate? false
           :decomposition [:rz]}

   :t {:operation-kind :gate
       :operation-id :t
       :operation-name "T"
       :operation-type :single-qubit
       :description "T gate (π/4 phase gate)"
       :native-gate? false
       :decomposition [:rz]}

   :t-dag {:operation-kind :gate
           :operation-id :t-dag
           :operation-name "T†"
           :operation-type :single-qubit
           :description "T† gate (−π/4 phase gate)"
           :native-gate? false
           :decomposition [:rz]}

   ;; Parametric rotation gates
   :rx {:operation-kind :gate
        :operation-id :rx
        :operation-name "RX"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around X-axis"
        :native-gate? false
        :decomposition [:h :t :h]}    ;; RX can be built from H-T-H sequence

   :ry {:operation-kind :gate
        :operation-id :ry
        :operation-name "RY"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around Y-axis"
        :native-gate? false
        :decomposition [:rx :rz]}

   :rz {:operation-kind :gate
        :operation-id :rz
        :operation-name "RZ"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around Z-axis"
        :native-gate? false
        :decomposition [:h :cnot :t :cnot :h]}  ;; RZ can be implemented with universal gates

   :phase {:operation-kind :gate
           :operation-id :phase
           :operation-name "Phase"
           :operation-type :parametric
           :parameter-count 1
           :parameters [:theta]
           :description "Arbitrary phase gate"
           :native-gate? false
           :decomposition [:rz]}

   ;; Two-qubit gates
   :cnot {:operation-kind :gate
          :operation-id :cnot
          :operation-name "CNOT"
          :operation-type :two-qubit
          :description "Controlled-NOT gate"
          :native-gate? true}

   :cz {:operation-kind :gate
        :operation-id :cz
        :operation-name "CZ"
        :operation-type :two-qubit
        :description "Controlled-Z gate"
        :native-gate? false
        :decomposition [:cnot :h]}

   :cy {:operation-kind :gate
        :operation-id :cy
        :operation-name "CY"
        :operation-type :two-qubit
        :description "Controlled-Y gate"
        :native-gate? false
        :decomposition [:cnot :ry :rx]}

   :swap {:operation-kind :gate
          :operation-id :swap
          :operation-name "SWAP"
          :operation-type :two-qubit
          :description "SWAP gate - exchanges two qubits"
          :native-gate? false
          :decomposition [:cnot]}

   :iswap {:operation-kind :gate
           :operation-id :iswap
           :operation-name "iSWAP"
           :operation-type :two-qubit
           :description "iSWAP gate - swap with phase"
           :native-gate? false
           :decomposition [:cnot :rz]}

   ;; Parametric two-qubit gates
   :crx {:operation-kind :gate
         :operation-id :crx
         :operation-name "CRX"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around X-axis"
         :native-gate? false
         :decomposition [:cnot :rx]}

   :cry {:operation-kind :gate
         :operation-id :cry
         :operation-name "CRY"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Y-axis"
         :native-gate? false
         :decomposition [:cnot :ry]}

   :crz {:operation-kind :gate
         :operation-id :crz
         :operation-name "CRZ"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Z-axis"
         :native-gate? false
         :decomposition [:cnot :rz]}

   ;; Multi-qubit gates
   :toffoli {:operation-kind :gate
             :operation-id :toffoli
             :operation-name "Toffoli"
             :operation-type :multi-qubit
             :description "Toffoli gate (CCX) - doubly controlled NOT"
             :native-gate? false
             :decomposition [:cnot :h :t]}

   :fredkin {:operation-kind :gate
             :operation-id :fredkin
             :operation-name "Fredkin"
             :operation-type :multi-qubit
             :description "Fredkin gate (CSWAP) - controlled SWAP"
             :native-gate? false
             :decomposition [:cnot :toffoli]}
   
   ;; Measurement operations
   ;; TODO add measurement ops
   })

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
;; Gate alias system
(def gate-aliases
  "Map of gate aliases to their canonical names.
  
  This allows backends and users to reference gates by common alternative names.
  All aliases resolve to the canonical gate name used in the gate catalog."
  {:not :x           ; NOT gate is X gate
   :bit-flip :x      ; Bit flip is X gate
   :phase-flip :z    ; Phase flip is Z gate
   :cx :cnot         ; CX is CNOT gate
   :ccx :toffoli     ; CCX is Toffoli gate
   :ccnot :toffoli   ; CCNOT is Toffoli gate
   :cswap :fredkin   ; CSWAP is Fredkin gate
   :p :phase         ; P is often used for phase gate
   :u1 :phase        ; U1 is a phase gate in some systems
   :sdg :s-dag       ; Common abbreviation for S-dagger
   :tdg :t-dag       ; Common abbreviation for T-dagger
   })

(defn resolve-gate-alias
  "Resolve a gate name through any aliases.
  
  Parameters:
  - gate-name: Keyword that might be an alias
  
  Returns: Canonical gate name
  
  Example:
  (resolve-gate-alias :not) ;=> :x
  (resolve-gate-alias :phase-flip) ;=> :z
  (resolve-gate-alias :h) ;=> :h (no alias)"
  [gate-name]
  (get gate-aliases gate-name gate-name))

(defn get-gate-info-with-alias
  "Get gate information, resolving aliases first.
  
  Parameters:
  - gate-name: Keyword identifying the gate (may be an alias)
  
  Returns: Gate definition map or nil if not found
  
  Example:
  (get-gate-info-with-alias :not) ;=> Returns X gate info
  (get-gate-info-with-alias :phase-flip) ;=> Returns Z gate info"
  [gate-name]
  (get operation-catalog (resolve-gate-alias gate-name)))

(defn validate-gate-set
  "Validate a gate set, resolving aliases first.
  
  Parameters:
  - gate-set: Set of gate keywords (may include aliases)
  
  Returns: True if all gates are known (after alias resolution)
  
  Example:
  (validate-gate-set #{:not :phase-flip :h}) ;=> true"
  [gate-set]
  (let [resolved-gates (into #{} (map resolve-gate-alias gate-set))
        known-gates (into #{} (keys operation-catalog))]
    (set/subset? resolved-gates known-gates)))

(defn normalize-gate-set
  "Normalize a gate set by resolving all aliases to canonical names.
  
  Parameters:
  - gate-set: Set of gate keywords (may include aliases)
  
  Returns: Set with all aliases resolved to canonical names
  
  Example:
  (normalize-gate-set #{:not :phase-flip :h}) ;=> #{:x :z :h}"
  [gate-set]
  (into #{} (map resolve-gate-alias gate-set)))


;; Update existing functions to use alias resolution
(defn get-gate-info
  "Get detailed information about a specific gate.
  
  Parameters:
  - gate-name: Keyword identifying the gate (aliases supported)
  
  Returns: Gate definition map or nil if not found"
  [gate-name]
  (get-gate-info-with-alias gate-name))


(defn get-gates-by-type
  "Get all gates of a specific type.
  
  Parameters:
  - gate-type: One of :single-qubit, :two-qubit, :multi-qubit, :parametric
  
  Returns: Set of gate names"
  [gate-type]
  (->> operation-catalog
       (filter #(= gate-type (:operation-type (val %))))
       (map key)
       (into #{})))

(defn get-native-gates
  "Get all gates marked as native (hardware-implementable).
  
  Returns: Set of gate names that are typically native to hardware"
  []
  (->> operation-catalog
       (filter #(:native-gate? (val %)))
       (map key)
       (into #{})))

(defn get-gate-dependencies
  "Get the decomposition dependencies for a gate.
  
  Parameters:
  - gate-name: Keyword identifying the gate
  
  Returns: Vector of gate names this gate decomposes into, or empty if native"
  [gate-name]
  (get-in operation-catalog [gate-name :decomposition] []))

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
  :args (s/cat :operation-id ::operation-id)
  :ret (s/nilable ::operation-definition))

(s/fdef get-gates-by-type
  :args (s/cat :operation-type ::operation-type)
  :ret ::operation-set)

(s/fdef validate-gate-set
  :args (s/cat :operation-set ::operation-set)
  :ret boolean?)

(s/fdef expand-gate-set
  :args (s/cat :operation-set ::operation-set)
  :ret ::operation-set)

(comment
  
  
  ;
  )