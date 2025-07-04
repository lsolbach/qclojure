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
;; Enhanced decomposition system supporting multiple backends and parameterization
(s/def ::decomposition-spec 
  (s/or :simple-vector (s/coll-of ::operation-id :kind vector?)
        :backend-map (s/map-of keyword? (s/coll-of any?))
        :function-map (s/map-of keyword? fn?)))

(s/def ::decomposition ::decomposition-spec)

(s/def ::operation-definition
  (s/keys :req-un [::operation-id ::operation-type ::description]
          :opt-un [::parameter-count ::decomposition]))

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
       :decomposition {:universal [:ry]  ; RY(π) = X
                      :parametric-fn (fn [_] [[:ry Math/PI]])
                      :rz-only [:rz :h :rz :h]}}  ; For RZ+H basis

   :y {:operation-kind :gate
       :operation-id :y
       :operation-name "Y"
       :operation-type :single-qubit
       :description "Pauli-Y gate - bit flip + phase flip operation"
       :decomposition {:universal [:s :x :s-dag]  ; S-X-S† = Y
                      :parametric-fn (fn [_] [[:rz (/ Math/PI 2)] [:ry Math/PI] [:rz (/ Math/PI -2)]])
                      :t-basis [:t :t :x :t :t :t :t]}}  ; Using T gates

   :z {:operation-kind :gate
       :operation-id :z
       :operation-name "Z"
       :operation-type :single-qubit
       :description "Pauli-Z gate - phase flip operation"
       :decomposition {:universal [:t :t :t :t]  ; T^4 = Z (correct!)
                      :parametric-fn (fn [_] [[:rz Math/PI]])
                      :s-basis [:s :s]}}  ; S^2 = Z

   ;; Hadamard gate
   :h {:operation-kind :gate
       :operation-id :h
       :operation-name "H"
       :operation-type :single-qubit
       :description "Hadamard gate - creates superposition"}

   ;; Phase gates
   :s {:operation-kind :gate
       :operation-id :s
       :operation-name "S"
       :operation-type :single-qubit
       :description "S gate (π/2 phase gate)"
       :decomposition {:universal [:t :t]                       ; T^2 = S
                      :parametric-fn (fn [_] [[:rz (/ Math/PI 2)]])}}

   :s-dag {:operation-kind :gate
           :operation-id :s-dag
           :operation-name "S†"
           :operation-type :single-qubit
           :description "S† gate (−π/2 phase gate)"
           :decomposition {:universal [:t-dag :t-dag]
                          :parametric-fn (fn [_] [[:rz (/ Math/PI -2)]])}}

   :t {:operation-kind :gate
       :operation-id :t
       :operation-name "T"
       :operation-type :single-qubit
       :description "T gate (π/4 phase gate)"}

   :t-dag {:operation-kind :gate
           :operation-id :t-dag
           :operation-name "T†"
           :operation-type :single-qubit
           :description "T† gate (−π/4 phase gate)"
           :decomposition {:parametric-fn (fn [_] [[:rz (/ Math/PI -4)]])}}

   ;; Parametric rotation gates
   :rx {:operation-kind :gate
        :operation-id :rx
        :operation-name "RX"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around X-axis"
        :decomposition {:universal-fn (fn [theta] [[:h] [:rz theta] [:h]])
                       :ry-rz-fn (fn [theta] [[:ry (/ Math/PI 2)] [:rz theta] [:ry (/ Math/PI -2)]])}}

   :ry {:operation-kind :gate
        :operation-id :ry
        :operation-name "RY"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around Y-axis"
        :decomposition {:universal-fn (fn [theta] [[:rx (/ Math/PI 2)] [:rz theta] [:rx (/ Math/PI -2)]])}}

   :rz {:operation-kind :gate
        :operation-id :rz
        :operation-name "RZ"
        :operation-type :parametric
        :parameter-count 1
        :description "Rotation around Z-axis"}

   :phase {:operation-kind :gate
           :operation-id :phase
           :operation-name "Phase"
           :operation-type :parametric
           :parameter-count 1
           :parameters [:theta]
           :description "Arbitrary phase gate"
           :decomposition [:rz]}

   ;; Two-qubit gates
   :cnot {:operation-kind :gate
          :operation-id :cnot
          :operation-name "CNOT"
          :operation-type :two-qubit
          :description "Controlled-NOT gate"}

   :cz {:operation-kind :gate
        :operation-id :cz
        :operation-name "CZ"
        :operation-type :two-qubit
        :description "Controlled-Z gate"
        :decomposition {:cnot-h [[:h :target] [:cnot :control :target] [:h :target]]}}

   :cy {:operation-kind :gate
        :operation-id :cy
        :operation-name "CY"
        :operation-type :two-qubit
        :description "Controlled-Y gate"
        :decomposition {:cnot-s [[:s-dag :target] [:cnot :control :target] [:s :target]]}}

   :swap {:operation-kind :gate
          :operation-id :swap
          :operation-name "SWAP"
          :operation-type :two-qubit
          :description "SWAP gate - exchanges two qubits"
          :decomposition {:cnot [[:cnot :target1 :target2] 
                                [:cnot :target2 :target1] 
                                [:cnot :target1 :target2]]
                         :cz-h [{:gate :h :target :target2}
                                {:gate :cz :control :target1 :target :target2}
                                {:gate :h :target :target2}
                                {:gate :h :target :target1}
                                {:gate :cz :control :target2 :target :target1}
                                {:gate :h :target :target1}
                                {:gate :h :target :target2}
                                {:gate :cz :control :target1 :target :target2}
                                {:gate :h :target :target2}]
                         :braket-rigetti [{:gate :h :target :target2}
                                         {:gate :cz :control :target1 :target :target2}
                                         {:gate :h :target :target2}
                                         {:gate :h :target :target1}
                                         {:gate :cz :control :target2 :target :target1}
                                         {:gate :h :target :target1}
                                         {:gate :h :target :target2}
                                         {:gate :cz :control :target1 :target :target2}
                                         {:gate :h :target :target2}]}}

   :iswap {:operation-kind :gate
           :operation-id :iswap
           :operation-name "iSWAP"
           :operation-type :two-qubit
           :description "iSWAP gate - swap with phase"
           :decomposition {:cnot-s [[:s :target1] [:s :target2] [:h :target1] [:cnot :target1 :target2] 
                                   [:cnot :target2 :target1] [:h :target2]]}}

   ;; Parametric two-qubit gates
   :crx {:operation-kind :gate
         :operation-id :crx
         :operation-name "CRX"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around X-axis"
         :decomposition [:cnot :rx]}

   :cry {:operation-kind :gate
         :operation-id :cry
         :operation-name "CRY"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Y-axis"
         :decomposition [:cnot :ry]}

   :crz {:operation-kind :gate
         :operation-id :crz
         :operation-name "CRZ"
         :operation-type :parametric
         :parameter-count 1
         :parameters [:theta]
         :description "Controlled rotation around Z-axis"
         :decomposition [:cnot :rz]}

   ;; Multi-qubit gates
   :toffoli {:operation-kind :gate
             :operation-id :toffoli
             :operation-name "Toffoli"
             :operation-type :multi-qubit
             :description "Toffoli gate (CCX) - doubly controlled NOT"
             :decomposition {:cnot-t [[:h :target]
                                     [:cnot :control2 :target]
                                     [:t-dag :target]
                                     [:cnot :control1 :target]
                                     [:t :target]
                                     [:cnot :control2 :target]
                                     [:t-dag :target]
                                     [:cnot :control1 :target]
                                     [:t :control2]
                                     [:t :target]
                                     [:cnot :control1 :control2]
                                     [:h :target]
                                     [:t :control1]
                                     [:t-dag :control2]
                                     [:cnot :control1 :control2]]}}

   :fredkin {:operation-kind :gate
             :operation-id :fredkin
             :operation-name "Fredkin"
             :operation-type :multi-qubit
             :description "Fredkin gate (CSWAP) - controlled SWAP"
             :decomposition {:cnot-toffoli [[:cnot :target2 :target1] 
                                           [:toffoli :control :target1 :target2] 
                                           [:cnot :target2 :target1]]}}

   ;; Rydberg gates - Specific to neutral atom quantum processors
   :rydberg-cz {:operation-kind :gate
                :operation-id :rydberg-cz
                :operation-name "Rydberg CZ"
                :operation-type :two-qubit
                :description "Rydberg blockade-based controlled-Z gate"
                :hardware-specific :neutral-atom
                :decomposition {:standard [:cz]}}

   :rydberg-cphase {:operation-kind :gate
                    :operation-id :rydberg-cphase
                    :operation-name "Rydberg CPhase"
                    :operation-type :parametric
                    :parameter-count 1
                    :parameters [:phi]
                    :description "Rydberg controlled phase gate with arbitrary phase"
                    :hardware-specific :neutral-atom
                    :decomposition {:standard-fn (fn [phi] [[:crz phi]])}}

   :rydberg-blockade {:operation-kind :gate
                      :operation-id :rydberg-blockade
                      :operation-name "Rydberg Blockade"
                      :operation-type :multi-qubit
                      :parameter-count 1
                      :parameters [:phi]
                      :description "Multi-qubit Rydberg blockade gate"
                      :hardware-specific :neutral-atom}

   ;; Global gates - Applied to all qubits simultaneously
   :global-rx {:operation-kind :gate
               :operation-id :global-rx
               :operation-name "Global RX"
               :operation-type :global-parametric
               :parameter-count 1
               :parameters [:theta]
               :description "Global X rotation applied to all qubits"
               :hardware-specific :neutral-atom
               :decomposition {:single-qubit-fn (fn [n theta] 
                                                  (for [i (range n)] [:rx theta :target i]))}}

   :global-ry {:operation-kind :gate
               :operation-id :global-ry
               :operation-name "Global RY"
               :operation-type :global-parametric
               :parameter-count 1
               :parameters [:theta]
               :description "Global Y rotation applied to all qubits"
               :hardware-specific :neutral-atom
               :decomposition {:single-qubit-fn (fn [n theta] 
                                                  (for [i (range n)] [:ry theta :target i]))}}

   :global-rz {:operation-kind :gate
               :operation-id :global-rz
               :operation-name "Global RZ"
               :operation-type :global-parametric
               :parameter-count 1
               :parameters [:theta]
               :description "Global Z rotation applied to all qubits"
               :hardware-specific :neutral-atom
               :decomposition {:single-qubit-fn (fn [n theta] 
                                                  (for [i (range n)] [:rz theta :target i]))}}

   :global-h {:operation-kind :gate
              :operation-id :global-h
              :operation-name "Global H"
              :operation-type :global
              :description "Global Hadamard gate applied to all qubits"
              :hardware-specific :neutral-atom
              :decomposition {:single-qubit-fn (fn [n] 
                                                 (for [i (range n)] [:h :target i]))}}

   :global-x {:operation-kind :gate
              :operation-id :global-x
              :operation-name "Global X"
              :operation-type :global
              :description "Global X gate applied to all qubits"
              :hardware-specific :neutral-atom
              :decomposition {:single-qubit-fn (fn [n] 
                                                 (for [i (range n)] [:x :target i]))}}

   :global-y {:operation-kind :gate
              :operation-id :global-y
              :operation-name "Global Y"
              :operation-type :global
              :description "Global Y gate applied to all qubits"
              :hardware-specific :neutral-atom
              :decomposition {:single-qubit-fn (fn [n] 
                                                 (for [i (range n)] [:y :target i]))}}

   :global-z {:operation-kind :gate
              :operation-id :global-z
              :operation-name "Global Z"
              :operation-type :global
              :description "Global Z gate applied to all qubits"
              :hardware-specific :neutral-atom
              :decomposition {:single-qubit-fn (fn [n] 
                                                 (for [i (range n)] [:z :target i]))}}
   
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

(def neutral-atom-hardware-gates
  "Typical gate set for neutral atom quantum processors."
  #{:x :y :z :h :rx :ry :rz :rydberg-cz :rydberg-cphase :rydberg-blockade
    :global-x :global-y :global-z :global-h :global-rx :global-ry :global-rz})

(def neutral-atom-global-gates
  "Global gate subset for neutral atom quantum processors."
  #{:global-x :global-y :global-z :global-h :global-rx :global-ry :global-rz})

(def neutral-atom-rydberg-gates
  "Rydberg-specific gate subset for neutral atom quantum processors."
  #{:rydberg-cz :rydberg-cphase :rydberg-blockade})

;; Hardware-specific gate sets (Amazon Braket examples)
(def braket-ionq-gates
  "Native gates for IonQ devices on Amazon Braket"
  #{:rx :ry :rz :cnot})

(def braket-rigetti-gates  
  "Native gates for Rigetti devices on Amazon Braket"
  #{:i :rx :ry :rz :cz :h})

(def braket-simulator-gates
  "Gates supported by Braket simulators"
  #{:i :x :y :z :h :s :s-dag :t :t-dag 
    :rx :ry :rz :cnot :cx :cz :cy :swap
    :iswap :crx :cry :crz :toffoli :fredkin})

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

;;; Hardware-specific utility functions

(defn get-native-gates-for-hardware
  "Get native gates for a specific hardware platform.
  
  Parameters:
  - hardware-key: Keyword identifying the hardware platform
  
  Returns: Set of gate names native to that hardware, or nil if unknown
  
  Example:
  (get-native-gates-for-hardware :braket-ionq) ;=> #{:rx :ry :rz :cnot}"
  [hardware-key]
  (case hardware-key
    :braket-ionq braket-ionq-gates
    :braket-rigetti braket-rigetti-gates
    :braket-simulator braket-simulator-gates
    :superconducting superconducting-hardware-gates
    :trapped-ion trapped-ion-hardware-gates
    :universal universal-gate-set
    :basic basic-gate-set
    :parametric parametric-gate-set
    :native-simulator native-simulator-gates
    nil))

(defn get-gate-dependencies
  "Get the decomposition dependencies for a gate.
  
  Parameters:
  - gate-name: Keyword identifying the gate
  
  Returns: Vector of gate names this gate decomposes into, or empty if native"
  [gate-name]
  (let [decomp (get-in operation-catalog [gate-name :decomposition])]
    (cond
      ;; If it's a map, try parametric first (for more fundamental gates), then universal
      (map? decomp) 
      (let [universal (:universal decomp)
            parametric-fn (:parametric-fn decomp)
            ;; Try parametric decomposition first for more fundamental gates
            parametric-deps (when parametric-fn
                              (try
                                (let [dummy-params {:target 0}
                                      param-result (parametric-fn dummy-params)]
                                  ;; Extract gate keywords from parametric result
                                  ;; Handle both [[:gate angle]] and [:gate] formats
                                  (mapv #(if (vector? %) (first %) %) param-result))
                                (catch Exception _ nil)))]
        (or parametric-deps universal []))
      ;; If it's a vector, return it
      (vector? decomp) decomp
      ;; Otherwise empty
      :else [])))

;; Enhanced decomposition functions
#_(defn get-decomposition-for-target
  "Get decomposition for a specific target gate set.
  
  Parameters:
  - gate-name: Gate to decompose
  - target-set: Target hardware gate set keyword or set of gates
  - params: Optional parameters for parametric gates
  
  Returns: Vector of decomposed gates or nil if not possible"
  [gate-name target-set & [params]]
  (let [gate-info (get operation-catalog gate-name)
        decompositions (:decomposition gate-info)
        target-gates (if (keyword? target-set)
                       (case target-set
                         :braket-ionq braket-ionq-gates
                         :braket-rigetti braket-rigetti-gates
                         :braket-simulator braket-simulator-gates
                         :superconducting superconducting-hardware-gates
                         :trapped-ion trapped-ion-hardware-gates
                         :universal universal-gate-set
                         #{})
                       target-set)]
    (cond
      ;; Gate is already in target set
      (contains? target-gates gate-name) 
      [[gate-name params]]
      
      ;; No decomposition available
      (nil? decompositions)
      nil
      
      ;; Try target-specific function decomposition
      (and params (keyword? target-set) (get decompositions (keyword (str (name target-set) "-fn"))))
      ((get decompositions (keyword (str (name target-set) "-fn"))) (first params))
      
      ;; Try parametric function decomposition
      (and params (:parametric-fn decompositions))
      ((:parametric-fn decompositions) (first params))
      
      ;; Try universal function decomposition  
      (and params (:universal-fn decompositions))
      ((:universal-fn decompositions) (first params))
      
      ;; Try specific target decomposition
      (and (keyword? target-set) (get decompositions target-set))
      (get decompositions target-set)
      
      ;; Try universal decomposition
      (:universal decompositions)
      (:universal decompositions)
      
      ;; If decomposition is just a vector, return it
      (vector? decompositions)
      decompositions
      
      ;; No decomposition available
      :else nil)))

#_(defn decompose-circuit-for-hardware
  "Decompose a quantum circuit for specific hardware.
  
  Parameters:
  - circuit: Vector of [gate-name & params] operations
  - target-gates: Set of native gates for target hardware or keyword
  
  Returns: Decomposed circuit or throws if decomposition impossible"
  [circuit target-gates]
  (mapcat (fn [[gate-name & params]]
            (let [decomp (get-decomposition-for-target gate-name target-gates params)]
              (if decomp
                decomp
                (throw (ex-info "Cannot decompose gate for target hardware"
                               {:gate gate-name
                                :target-gates target-gates})))))
          circuit))

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

(defn minimal-native-set-for-hardware
  "Find the minimal set of native gates needed to implement a gate set on specific hardware.
  
  Parameters:
  - gate-set: Set of gate keywords  
  - hardware-key: Keyword identifying the target hardware platform
  
  Returns: Minimal set of native gates needed for that hardware, or empty set if hardware unknown
  
  Example:
  (minimal-native-set-for-hardware #{:x :y :z} :braket-ionq) ;=> #{:rx :ry :rz}"
  [gate-set hardware-key]
  (let [expanded (expand-gate-set gate-set)
        native-gates (get-native-gates-for-hardware hardware-key)]
    (if native-gates
      (set/intersection expanded native-gates)
      #{})))

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

(s/fdef get-native-gates-for-hardware
  :args (s/cat :hardware-key keyword?)
  :ret (s/nilable ::operation-set))

(s/fdef minimal-native-set-for-hardware
  :args (s/cat :operation-set ::operation-set :hardware-key keyword?)
  :ret ::operation-set)

(comment
  ;; Check what gates are native on specific hardware:
  (get-native-gates-for-hardware :braket-ionq)
  ;=> #{:rx :ry :rz :cnot}
  
  (get-native-gates-for-hardware :braket-rigetti)  
  ;=> #{:i :rx :ry :rz :cz :h}
  
  ;; Find minimal native gates needed for a circuit on specific hardware:
  (minimal-native-set-for-hardware #{:x :y :z :h} :braket-ionq)
  ;=> #{:ry}  ; X,Y,Z can be decomposed to RY rotations
  
  (minimal-native-set-for-hardware #{:x :y :z :h} :braket-rigetti)
  ;=> #{:h :ry}  ; H is native on Rigetti, need RY for Pauli gates
  
  ;; Check gate support correctly:
  (contains? (get-native-gates-for-hardware :braket-ionq) :h)     ;=> false
  (contains? (get-native-gates-for-hardware :braket-rigetti) :h)  ;=> true
  
  ;
  )