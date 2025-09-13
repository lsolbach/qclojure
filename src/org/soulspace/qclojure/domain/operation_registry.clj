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
    :parametric
    :global
    :global-parametric})

(def operation-params
  "Set of operation parameters."
  #{:target :target1 :target2 :control :control1 :control2
    :angle :theta :phi})

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
  (s/keys :req-un [::operation-kind ::operation-id ::operation-type ::description]
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

   ;; Identity gate
   :i {:operation-kind :gate
       :operation-id :i
       :operation-name "I"
       :operation-type :single-qubit
       :description "Identity gate - leaves qubit state unchanged"
       :decomposition {:universal []  ; Identity requires no operations
                      :parametric-fn (fn [_] [])
                      :nop true}}  ; This is a no-operation gate

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
   :measure {:operation-kind :measurement
             :operation-id :measure
             :operation-name "Measure"
             :operation-type :single-qubit
             :description "Computational basis measurement (Z-basis) - collapses qubit to |0⟩ or |1⟩"}
   })

;; Predefined gate sets for common hardware
(def universal-gate-set
  "Universal gate set sufficient for any quantum computation."
  #{:h :t :cnot})

(def basic-gate-set
  "Basic gate set with common single and two-qubit gates."
  #{:i :x :y :z :h :s :t :cnot :swap})

(def parametric-gate-set
  "Gate set with parametric rotations."
  #{:rx :ry :rz :cnot :phase})

; TODO move to sim backends?
(def native-simulator-gate-set
  "Gates typically supported natively by quantum simulators."
  #{:i :x :y :z :h :s :s-dag :t :t-dag :rx :ry :rz :phase
    :cnot :cz :cy :swap :crx :cry :crz :toffoli :fredkin})

;; Utility functions
;; Gate alias system
(def gate-aliases
  "Map of gate aliases to their canonical names.
  
  This allows backends and users to reference gates by common alternative names.
  All aliases resolve to the canonical gate name used in the gate catalog."
  {:not :x           ; NOT gate is X gate
   :bit-flip :x      ; Bit flip is X gate
   :phase-flip :z    ; Phase flip is Z gate
   :id :i            ; Identity gate alias for QASM2
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

(comment
  
  ;
  )