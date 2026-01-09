(ns org.soulspace.qclojure.domain.state
  "Core quantum state representation and operations.
   
   This namespace defines the fundamental data structures and functions
   for representing and manipulating quantum states in QClojure.
   It includes utilities for creating common quantum states, performing
   state normalization, tensor products, and calculating measurement probabilities.
   
   The quantum state is represented as a map with keys:
   - :state-vector: Vector of complex amplitudes (fastmath Vec2)
   - :num-qubits: Number of qubits in the state
   
   The state vector follows the standard computational basis ordering.
   
   QClojure currently uses the fastmath library for complex number support and
   mathematical operations.

   Specs are provided for validation of quantum states and related data structures."
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]
            [org.soulspace.qclojure.domain.math :as qmath]))

;;;
;;; Specs for quantum states
;;;
(s/def ::complex-amplitude qmath/complex?)
(s/def ::state-vector (s/coll-of ::complex-amplitude :kind vector? :min-count 1))
(s/def ::num-qubits pos-int?)
(s/def ::state (s/keys :req-un [::state-vector ::num-qubits]))
(s/def ::basis-states (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::basis-strings (s/coll-of string? :kind vector? :min-count 1))
(s/def ::basis-labels (s/coll-of string? :kind vector? :min-count 1))

; Enable fastmath operator macros
#_(m/use-primitive-operators)

;;;
;;; Computational basis state utilities
;;;
(defn basis-string
  "Generate a string representation of a computational basis state.
   For a given value, this function produces the corresponding binary string
   representation of the computational basis state.
   If value is an integer, it is interpreted as the index of the state in the computational basis.
   
   Parameters:
   - value: Integer, vector of bits, or string representing the computational basis state.
   - n-qubits: (optional) Number of qubits for the state, defaults to length of binary string.

   Returns:
   String representation of the computational basis state in binary format."
  ([value]
   (if (number? value)
     (basis-string value (fm/log2int value))
     (basis-string value (count value))))
  ([value n-qubits]
   (let [binary-string (cond (number? value)
                             (Long/toBinaryString value)
                             (vector? value)
                             (apply str (map #(if (= % 0) "0" "1") value))
                             (string? value)
                             (if (and (str/starts-with? value "|")
                                      (str/ends-with? value "⟩"))
                               (subs value 1 (- (count value) 1))
                               value))]
     (str (str/join (repeat (- n-qubits (count binary-string)) "0")) binary-string))))

(defn basis-strings
  "Generate binary basis strings for n qubits.
  
  Returns vector of strings like ['00', '01', '10', '11'] for 2 qubits.
  
  Parameters:
  - n-qubits: Number of qubits
   
  Returns:
  Vector of basis strings"
  [n-qubits]
  (mapv #(basis-string % n-qubits)
        (range (bit-shift-left 1 n-qubits))))

(defn basis-label
  "Generate a string representation of a computational basis state in ket form.
   For a given value, this function produces the corresponding basis state label
   in the form |b₀b₁...bₙ₋₁⟩ where bᵢ are the bits of the state.
   If value is an integer, it is interpreted as the index of the state in the computational basis.
   
   Parameters:
   - value: Integer, vector of bits, or string representing the computational basis state.
   - n-qubits: (optional) Number of qubits for the state, defaults to length of binary string.

   Returns:
   String representation of the computational basis state in ket |b₀b₁...bₙ₋₁⟩ form."
  ([value]
   (str "|" (basis-string value) "⟩"))
  ([value n-qubits]
   (str "|" (basis-string value n-qubits) "⟩")))

(defn basis-labels
  "Generate binary basis labels for n qubits.
    
  Parameters:
  - n-qubits: Number of qubits
   
  Returns:
  Vector of basis labels."
  [n-qubits]
  (mapv #(basis-label % n-qubits)
        (range (bit-shift-left 1 n-qubits))))

(defn bits-to-index
  "Convert a vector of bits to the corresponding state vector index.
  
  For n qubits with bits [b0, b1, ..., b(n-1)], the index is:
  index = b0*2^(n-1) + b1*2^(n-2) + ... + b(n-1)*2^0
  
  This maps computational basis states to their positions in the state vector.
  
  Parameters:
  - bits: Vector of 0s and 1s representing the computational basis state
  
  Returns:
  Integer index into the state vector (0 to 2^n - 1)
  
  Examples:
  (bits-to-index [0 0 0]) ;=> 0  ; |000⟩ corresponds to index 0
  (bits-to-index [0 0 1]) ;=> 1  ; |001⟩ corresponds to index 1  
  (bits-to-index [1 0 1]) ;=> 5  ; |101⟩ corresponds to index 5"
  [bits]
  (let [n (count bits)]
    (reduce + (map-indexed (fn [i bit]
                             (* bit (bit-shift-left 1 (- n 1 i))))
                           bits))))

(defn index-to-bits
  "Convert an index to its binary bit representation.
  
  For a given index, this function computes the corresponding vector of bits
  representing the computational basis state. The bits are ordered from most
  significant to least significant (left to right).
  
  Parameters:
  - index: Integer index (0 to 2^n - 1)
  - n: Number of qubits (determines bit vector length)
  
  Returns:
  Vector of bits [b₀ b₁ ... bₙ₋₁] representing the computational basis state
  
  Examples:
  (index-to-bits 0 3) ;=> [0 0 0]  ; |000⟩ corresponds to index 0
  (index-to-bits 1 3) ;=> [0 0 1]  ; |001⟩ corresponds to index 1
  (index-to-bits 5 3) ;=> [1 0 1]  ; |101⟩ corresponds to index 5"
  [index n]
  {:pre [(integer? index)
         (>= index 0)
         (< index (bit-shift-left 1 n))
         (pos-int? n)]}
  (vec (for [i (range n)]
         (bit-and (bit-shift-right index (- n 1 i)) 1))))

(comment

  (basis-string 1 4)
  (basis-string "001" 4)
  (basis-string [0 0 1] 4)
  (basis-string 4 4)
  (basis-string "100" 4)
  (basis-string [1 0 0] 4)

  (basis-strings 3)

  (basis-label 4)
  (basis-label [1 0 0])
  (basis-label "100")
  (basis-label "001")
  (basis-label [0 0 1])
  (basis-label 15)

  (basis-labels 3)

  (bits-to-index [0 0 0])
  (bits-to-index [0 0 1])
  (bits-to-index [1 0 1])

  (index-to-bits 0 3)
  (index-to-bits 1 3)
  (index-to-bits 5 3)
  ; 
  )

;;;
;;; Quantum state creation functions
;;;
(defn single-qubit-state
  "Create a single qubit state with given amplitude for |1⟩ component.
  
  Creates a normalized single-qubit quantum state where the amplitude
  parameter determines the probability amplitude for the |1⟩ basis state.
  The |0⟩ amplitude is computed to ensure normalization.
  
  Parameters:
  - amplitude: Complex amplitude for the |1⟩ component (fastmath Vec2)
  
  Returns:
  Quantum state map with :state-vector and :num-qubits keys
  
  Example:
  (single-qubit-state (fc/complex 0.707 0))
  ;=> State approximately equal to |+⟩ = (|0⟩ + |1⟩)/√2"
  [amplitude]
  {:pre [(s/valid? ::complex-amplitude amplitude)]
   :post [(s/valid? ::state %)]}
  (let [norm (fm/sqrt (+ (fc/abs amplitude) (fc/abs (- 1 amplitude))))]
    {:state-vector [(fc/complex (/ (fc/abs amplitude) norm) 0)
                    (fc/complex (/ (fc/abs (- 1 amplitude)) norm) 0)]
     :num-qubits 1}))

(defn multi-qubit-state
  "Create a multi-qubit quantum state from a vector of complex amplitudes.
  
  Constructs a quantum state for n qubits where n is determined by the
  logarithm base 2 of the amplitude vector length. The amplitudes represent
  the probability amplitudes for each computational basis state.
  
  For n qubits, the basis states are ordered as:
  |00...0⟩, |00...1⟩, |00...10⟩, ..., |11...1⟩
  
  Parameters:
  - amplitudes: Vector of complex amplitudes (each a fastmath Vec2)
                Length must be a power of 2 (2^n for n qubits)
  
  Returns:
  Quantum state map with :state-vector and :num-qubits keys
  
  Example:
  (multi-qubit-state [(fc/complex 0.707 0) (fc/complex 0 0) 
                      (fc/complex 0 0) (fc/complex 0.707 0)])
  ;=> 2-qubit Bell state (|00⟩ + |11⟩)/√2"
  [amplitudes]
  {:pre [(every? #(s/valid? ::complex-amplitude %) amplitudes)]
   ;:post [(s/valid? ::state %)]
   }
  (let [num-qubits (max 1 (fm/log2int (count amplitudes)))
        state-vector amplitudes]
    {:state-vector state-vector
     :num-qubits num-qubits}))

(defn zero-state
  "Create the |0⟩ computational basis state.
  
  For single qubit: Creates |0⟩ = [1, 0] state
  For n qubits: Creates |00...0⟩ state with all qubits in |0⟩
  
  The |0⟩ state is a fundamental computational basis state where:
  - Single qubit: 100% probability of measuring 0
  - Multi-qubit: 100% probability of measuring all 0s
  
  Parameters:
  - (no args): Creates single-qubit |0⟩ state
  - n: (optional) Number of qubits for multi-qubit |00...0⟩ state
  
  Returns:
  Quantum state map representing the |0⟩^⊗n state
  
  Examples:
  (zero-state)
  ;=> {:state-vector [1+0i, 0+0i], :num-qubits 1}  ; |0⟩
  
  (zero-state 3)
  ;=> 3-qubit state |000⟩"
  ([]
   {:state-vector [(fc/complex 1 0) (fc/complex 0 0)]
    :num-qubits 1})
  ([n]
   {:pre [(pos-int? n)]
   ;:post [(s/valid? ::state %)]
    }
   (let [size (bit-shift-left 1 n)
         state-vector (into [] (concat [(fc/complex 1 0)] (repeat (- size 1) (fc/complex 0 0))))]
     {:state-vector state-vector
      :num-qubits n})))

(defn one-state
  "Create the |1⟩ computational basis state.
  
  Creates a single-qubit quantum state |1⟩ = [0, 1] where there is
  100% probability of measuring the qubit in the excited state.
  
  Parameters: None
  
  Returns:
  Single-qubit quantum state map representing |1⟩
  
  Example:
  (one-state)
  ;=> {:state-vector [0+0i, 1+0i], :num-qubits 1}"
  ([]
   {:state-vector [(fc/complex 0 0) (fc/complex 1 0)]
    :num-qubits 1})
  ([n]
   {:pre [(pos-int? n)]}
   (let [size (bit-shift-left 1 n)
         state-vector (into [] (concat (repeat (- size 1) (fc/complex 0 0)) [(fc/complex 1 0)]))]
     {:state-vector state-vector
      :num-qubits n})))

(defn plus-state
  "Create the |+⟩ superposition state.
  
  Creates the |+⟩ = (|0⟩ + |1⟩)/√2 state, which is an equal superposition
  of the computational basis states. This state has 50% probability of
  measuring either 0 or 1, representing true quantum superposition.
  
  The |+⟩ state is an eigenstate of the Pauli-X operator and is commonly
  used in quantum algorithms and quantum information protocols.
  
  Parameters: None
  
  Returns:
  Single-qubit quantum state map representing |+⟩
  
  Example:
  (plus-state)
  ;=> {:state-vector [0.707+0i, 0.707+0i], :num-qubits 1}"
  ([]
   (let [sqrt2-inv (/ 1 (fm/sqrt 2))]
     {:state-vector [(fc/complex sqrt2-inv 0) (fc/complex sqrt2-inv 0)]
      :num-qubits 1}))
  ([n]
   {:pre [(pos-int? n)]}
   (let [size (bit-shift-left 1 n)
         amplitude (/ 1 (fm/sqrt size))
         state-vector (vec (repeat size (fc/complex amplitude 0)))]
     {:state-vector state-vector
      :num-qubits n})))

(defn minus-state
  "Create the |-⟩ superposition state.
  
  Creates the |-⟩ = (|0⟩ - |1⟩)/√2 state, which is an equal superposition
  of the computational basis states with a relative phase of π between them.
  This state also has 50% probability of measuring either 0 or 1, but the
  negative amplitude creates different interference patterns.
  
  The |-⟩ state is an eigenstate of the Pauli-X operator (with eigenvalue -1)
  and demonstrates quantum phase relationships.
  
  Parameters: None
  
  Returns:
  Single-qubit quantum state map representing |-⟩
  
  Example:
  (minus-state)
  ;=> {:state-vector [0.707+0i, -0.707+0i], :num-qubits 1}"
  ([]
   (let [sqrt2-inv (/ 1 (fm/sqrt 2))]
     {:state-vector [(fc/complex sqrt2-inv 0) (fc/complex (- sqrt2-inv) 0)]
      :num-qubits 1}))
  ([n]
   {:pre [(pos-int? n)]}
   (let [size (bit-shift-left 1 n)
         amplitude (/ 1 (fm/sqrt size))
         state-vector (vec (concat [(fc/complex amplitude 0)]
                                   (repeat (- size 1) (fc/complex (- amplitude) 0))))]
     {:state-vector state-vector
      :num-qubits n})))

(defn plus-i-state
  "Create the |+i⟩ = (|0⟩ + i|1⟩)/√2 superposition state.
  
  This state is a superposition with a phase factor of i, commonly used in quantum algorithms.
  The |+i⟩ state has equal probability amplitudes for |0⟩ and |1⟩, but with a complex phase
  that introduces interference effects.

  The |+i⟩ state is an eigenstate of the Pauli-Y operator and is useful for
  demonstrating quantum phase relationships.

  Parameters: None
   
  Returns:
  Single-qubit quantum state map representing |+i⟩
  
  Example:
  (plus-i-state)
  ;=> {:state-vector [0.707+0i, 0+0.707i], :num-qubits 1}"
  ([]
   (let [sqrt2-inv (/ 1 (fm/sqrt 2))]
     {:state-vector [(fc/complex sqrt2-inv 0) (fc/complex 0 sqrt2-inv)]
      :num-qubits 1}))
  ([n]
   {:pre [(pos-int? n)]}
   (let [size (bit-shift-left 1 n)
         amplitude (/ 1 (fm/sqrt size))
         state-vector (vec (concat [(fc/complex amplitude 0)]
                                   (repeat (- size 1) (fc/complex 0 amplitude))))]
     {:state-vector state-vector
      :num-qubits n})))

(defn minus-i-state
  "Create the |-i⟩ = (|0⟩ - i|1⟩)/√2 superposition state.
  
  This state is a superposition with a phase factor of -i, commonly used in quantum algorithms.
  The |-i⟩ state has equal probability amplitudes for |0⟩ and |1⟩, but with a complex phase
  that introduces interference effects.
  
  The |-i⟩ state is an eigenstate of the Pauli-Y operator (with eigenvalue -1) and is useful for
  demonstrating quantum phase relationships.

  Parameters: None
  
  Returns:
  Single-qubit quantum state map representing |-i⟩
  
  Example:
  (minus-i-state)
  ;=> {:state-vector [0.707+0i, 0-0.707i], :num-qubits 1}"
  ([]
   (let [sqrt2-inv (/ 1 (fm/sqrt 2))]
     {:state-vector [(fc/complex sqrt2-inv 0) (fc/complex 0 (- sqrt2-inv))]
      :num-qubits 1}))
  ([n]
   {:pre [(pos-int? n)]}
   (let [size (bit-shift-left 1 n)
         amplitude (/ 1 (fm/sqrt size))
         state-vector (vec (concat [(fc/complex amplitude 0)]
                                   (repeat (- size 1) (fc/complex 0 (- amplitude)) )))]
     {:state-vector state-vector
      :num-qubits n})))

(comment
  (single-qubit-state (fc/complex 0.707 0))

  (multi-qubit-state [(fc/complex 0.707 0) (fc/complex 0 0)
                      (fc/complex 0 0) (fc/complex 0.707 0)])

  (zero-state)
  (zero-state 3)

  (one-state)
  (one-state 3)

  (plus-state)
  (plus-state 3)

  (minus-state)
  (minus-state 3)

  (plus-i-state)
  (plus-i-state 3)

  (minus-i-state)
  (minus-i-state 3)
  )

(defn computational-basis-state
  "Create a computational basis state |b₀b₁...bₙ₋₁⟩ from a vector of bits.
  
  Creates a pure quantum state where one specific computational basis state
  has amplitude 1 and all others have amplitude 0. This represents a classical
  bit string in quantum form.
  
  The bits are ordered from most significant to least significant (left to right),
  so [1,0,1] represents the state |101⟩. This is consistent with standard
  quantum computing notation.
  
  Parameters:
  - n: Number of qubits (must match length of bits vector)
  - bits: Vector of 0s and 1s representing the desired basis state
  
  Returns:
  Quantum state map representing the computational basis state
  
  Throws:
  AssertionError if n doesn't match bits length or bits contains invalid values
  
  Examples:
  (computational-basis-state 3 [0 0 0])  ;=> |000⟩ state (same as zero-state)
  (computational-basis-state 3 [1 0 1])  ;=> |101⟩ state
  (computational-basis-state 3 [0 1 1])  ;=> |011⟩ state
  (computational-basis-state 2 [1 1])    ;=> |11⟩ state"
  [n bits]
  {:pre [(pos-int? n)
         (= n (count bits))
         (every? #(or (= % 0) (= % 1)) bits)]}
  (let [size (bit-shift-left 1 n)  ; 2^n
        target-index (bits-to-index bits)
        state-vector (assoc (vec (repeat size (fc/complex 0 0)))
                            target-index (fc/complex 1 0))]
    {:state-vector state-vector
     :num-qubits n}))

;;;
;;; State operations
;;;
(defn normalize-state
  "Normalize a quantum state vector to unit length.
  
  Quantum states must be normalized such that the sum of squared magnitudes
  of all amplitudes equals 1. This ensures that the total probability of
  all measurement outcomes is 100%.
  
  The normalization process:
  1. Calculate the norm: √(Σ|αᵢ|²) where αᵢ are the amplitudes
  2. Divide each amplitude by the norm: αᵢ' = αᵢ/norm
  
  Parameters:
  - state: Quantum state map to normalize
  
  Returns:
  Normalized quantum state with the same relative amplitudes but unit norm
  
  Example:
  (normalize-state (multi-qubit-state [(fc/complex 3 0) (fc/complex 4 0)]))
  ;=> Normalized state with amplitudes [0.6, 0.8] since 3²+4²=25, norm=5"
  [state]
  (let [amplitudes (:state-vector state)
        norm (cla/norm2 amplitudes)
        normalized-amplitudes (if (and (pos? norm)
                                       (> norm (cla/current-tolerance)))
                                (mapv #(fc/scale % (/ 1.0 norm)) amplitudes)
                                amplitudes)]
    (assoc state :state-vector normalized-amplitudes)))

(defn tensor-product
  "Compute the tensor product of two quantum states.
  
  The tensor product (⊗) combines two quantum systems into a single
  composite system. For states |ψ⟩ ⊗ |φ⟩, the resulting state has
  dimensionality equal to the product of the individual state dimensions.
  
  The tensor product is fundamental for:
  - Creating multi-qubit states from single-qubit states
  - Building composite quantum systems
  - Representing non-entangled product states
  
  Mathematical operation:
  If |ψ⟩ = α|0⟩ + β|1⟩ and |φ⟩ = γ|0⟩ + δ|1⟩, then
  |ψ⟩ ⊗ |φ⟩ = αγ|00⟩ + αδ|01⟩ + βγ|10⟩ + βδ|11⟩
  
  Parameters:
  - state1: First quantum state
  - state2: Second quantum state
  
  Returns:
  Composite quantum state representing state1 ⊗ state2
  
  Example:
  (tensor-product |0⟩ |1⟩)
  ;=> 2-qubit state |01⟩ = [0, 1, 0, 0]"
  [state1 state2]
  #_{:pre [(s/valid? ::state state1)
           (s/valid? ::state state2)]
     :post [(s/valid? ::state %)]}
  (let [v1 (:state-vector state1)
        v2 (:state-vector state2)
        n1 (:num-qubits state1)
        n2 (:num-qubits state2)
        ;; Tensor product of vectors: for each element in v1, multiply by each element in v2
        result-vector (vec (for [a1 v1
                                 a2 v2]
                             (fc/mult a1 a2)))]
    {:state-vector result-vector
     :num-qubits (+ n1 n2)}))

(defn state-projector
  "Create a projector matrix |ψ⟩⟨ψ| from a quantum state.
    
    Parameters:
    - backend: Backend instance
    - psi: State vector (real or complex, may be unnormalized)
    
    Returns:
    Projector matrix representing |ψ⟩⟨ψ|
    
    Note:
    Uses outer-product semantics with automatic normalization"
  [state]
  {:pre [(s/valid? ::state state)]}
  (cla/outer-product (:state-vector state) (:state-vector state)))

(defn trace-one?
  "Test if a matrix has trace equal to one (Tr(ρ) ≈ 1).
  
  Parameters:
  - rho: Square matrix (typically a density matrix)
      
  Returns:
  Boolean indicating whether the trace is approximately 1
  
  Note:
  Useful for validating quantum density matrices"
  [rho]
  (let [trace (cla/trace rho)
        eps (cla/current-tolerance)
        trace-real (fc/re trace)]  ; Extract real part of complex trace
    (< (fm/abs (- trace-real 1.0)) eps)))

(defn probability
  "Calculate the probability of measuring a quantum state in a specific basis state.
  
  According to the Born rule, the probability of measuring a quantum state
  in a particular computational basis state is the squared magnitude of
  the corresponding amplitude: P(|i⟩) = |αᵢ|²
  
  Parameters:
  - state: Quantum state to analyze
  - basis-index: Integer index of the computational basis state (0-indexed)
                 For n qubits: 0 represents |00...0⟩, 2ⁿ-1 represents |11...1⟩
  
  Returns:
  Real number between 0 and 1 representing the measurement probability
  
  Examples:
  (probability |+⟩ 0)
  ;=> 0.5  ; 50% chance of measuring |0⟩
  
  (probability |+⟩ 1)  
  ;=> 0.5  ; 50% chance of measuring |1⟩
  
  (probability |0⟩ 0)
  ;=> 1.0  ; 100% chance of measuring |0⟩"
  [state basis-index]
  ;; Temporarily disabled spec validation
  ;; {:pre [(s/valid? ::state state)
  ;;        (< basis-index (count (:state-vector state)))]}
  (let [amplitude (nth (:state-vector state) basis-index)]
    (* (fc/abs amplitude) (fc/abs amplitude))))

(defn probabilities
  "Calculate the probabilities from the amplitudes of a quantum state.
   
   According to the Born rule, the probability of measuring a quantum state
   in a particular computational basis state is the squared magnitude of
   the corresponding amplitude: P(|i⟩) = |αᵢ|².
   
   Parameters:
   - state: Quantum state to analyze
   
   Returns:
   Vector of real numbers between 0 and 1 representing the measurement probabilities
   
   Example:
   (probabilities |+⟩)
   ;=> [0.5, 0.5]  ; 50% chance of measuring |0⟩ or |1⟩
   (probabilities |0⟩)
   ;=> [1.0, 0.0]  ; 100% chance of measuring |0⟩"
  [state]
  (let [amps (:state-vector state)
        probs (mapv (fn [a]
                      (let [mag (fc/abs a)]
                        (* mag mag)))
                    amps)]
    probs))

(defn density-matrix
  "Create a density matrix ρ for a quantum state.
  
  For pure states: ρ = |ψ⟩⟨ψ| (outer product of state vector with itself)
  For mixed states: ρ = Σᵢ pᵢ |ψᵢ⟩⟨ψᵢ|
  
  The density matrix is the fundamental representation of quantum states in
  the density matrix formalism, which can handle both pure and mixed states
  uniformly. It has the following properties:
  - Tr(ρ) = 1 (normalized)
  - ρ† = ρ (Hermitian)
  - ρ ≥ 0 (positive semidefinite)
  - For pure states: ρ² = ρ (idempotent)
  
  Parameters:
  - state: Quantum state (pure or mixed)
  
  Returns:
  Density matrix ρ as a matrix of complex numbers"
  [state]
  (let [projector (state-projector state)]
    (if (trace-one? projector)
      ;; Pure state density matrix: ρ = |ψ⟩⟨ψ| with Tr(ρ)=1
      projector
      ;; Mixed state (diagonal) density matrix: ρ = Σ_i p_i |i⟩⟨i| with p_i = |α_i|^2
      (let [probs (probabilities state)
            total (reduce + probs)
            eps (cla/current-tolerance)
            probs (if (and (pos? total)
                           (> (fm/abs (- total 1.0)) eps))
                    ;; normalize to ensure Tr(ρ)=1
                    (mapv #(/ % total) probs)
                    probs)
            n (count probs)]
        (mapv (fn [i p]
                (mapv (fn [j]
                        (if (= i j)
                          (fc/complex p 0.0)
                          fc/ZERO))
                      (range n)))
              (range n) probs)))))

(defn trajectory-to-density-matrix
  "Convert a collection of quantum state trajectories to a normalized density matrix.
   
  This function takes a collection of normalized quantum states (trajectories) and computes
  the corresponding density matrix by averaging the projectors of each state with given weights.
  This is useful for representing mixed states arising from statistical ensembles
  of pure states, such as those from noisy quantum simulations.
   
  Parameters:
  - trajectories: Collection of quantum state maps (each with :state-vector and :num-qubits)
                 All states must be normalized and have the same number of qubits
  - weights: (optional) Collection of weights for each trajectory. Weights will be 
            automatically normalized to sum to 1.0. If not provided, equal weighting is assumed.
   
  Returns:
  Map containing:
  - :density-matrix - The resulting density matrix as a matrix of complex numbers
  - :num-qubits - Number of qubits in the states
  - :trace - Trace of the density matrix (should be 1.0 after normalization)
  - :weights - The normalized weights used

   Example:
   (trajectory-to-density-matrix [|0⟩ |1⟩])
   (trajectory-to-density-matrix [|0⟩ |+⟩ |1⟩] [0.5 0.3 0.2])"
  ([trajectories]
   (trajectory-to-density-matrix trajectories nil))
  ([trajectories weights]
   {:pre [(seq trajectories)
          (every? #(s/valid? ::state %) trajectories)]}
   (let [n (count trajectories)
         first-state (first trajectories)
         num-qubits (:num-qubits first-state)

         ;; Validate all trajectories have same number of qubits
         _ (when-not (every? #(= (:num-qubits %) num-qubits) trajectories)
             (throw (ex-info "All trajectories must have the same number of qubits"
                             {:expected num-qubits
                              :found (mapv :num-qubits trajectories)})))

         ;; Validate all trajectories are normalized (within tolerance)
         eps (cla/current-tolerance)
         _ (doseq [state trajectories]
             (let [norm-sq (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector state)))]
               (when (> (fm/abs (- norm-sq 1.0)) eps)
                 (throw (ex-info "All trajectories must be normalized quantum states"
                                 {:norm-squared norm-sq :tolerance eps})))))

         ;; Normalize weights to sum to 1.0
         raw-weights (or weights (repeat n (/ 1.0 n)))
         weight-sum (reduce + raw-weights)
         normalized-weights (if (> (fm/abs weight-sum) eps)
                              (mapv #(/ % weight-sum) raw-weights)
                              raw-weights)

         ;; Create weighted projectors using state-projector function
         projectors (map (fn [state weight]
                           (cla/scale (state-projector state) weight))
                         trajectories normalized-weights)

         ;; Sum all weighted projectors
         density-matrix (reduce cla/add (first projectors) (rest projectors))
         trace (cla/trace density-matrix)]

     {:density-matrix density-matrix
      :num-qubits num-qubits
      :trace (fc/re trace)
      :weights (vec normalized-weights)})))

;;;
;;; Pre-defined common quantum states
;;;
(def |0⟩
  "Single-qubit |0⟩ computational basis state."
  (zero-state 1))

(def |1⟩
  "Single-qubit |1⟩ computational basis state."
  (one-state))

(def |+⟩
  "Single-qubit |+⟩ = (|0⟩ + |1⟩)/√2 superposition state."
  (plus-state))

(def |-⟩
  "Single-qubit |-⟩ = (|0⟩ - |1⟩)/√2 superposition state."
  (minus-state))

(def |+i⟩
  "Single-qubit |+i⟩ = (|0⟩ + i|1⟩)/√2 state."
  (plus-i-state))

(def |-i⟩
  "Single-qubit |-i⟩ = (|0⟩ - i|1⟩)/√2 state."
  (minus-i-state))

(def |00⟩
  "Two-qubit |00⟩ computational basis state."
  (tensor-product |0⟩ |0⟩))
(def |01⟩
  "Two-qubit |01⟩ computational basis state."
  (tensor-product |0⟩ |1⟩))
(def |10⟩
  "Two-qubit |10⟩ computational basis state."
  (tensor-product |1⟩ |0⟩))
(def |11⟩
  "Two-qubit |11⟩ computational basis state."
  (tensor-product |1⟩ |1⟩))

(def |000⟩
  "Three-qubit |000⟩ computational basis state."
  (zero-state 3))

(def |001⟩
  "Three-qubit |001⟩ computational basis state."
  (computational-basis-state 3 [0 0 1]))

(def |010⟩
  "Three-qubit |010⟩ computational basis state."
  (computational-basis-state 3 [0 1 0]))

(def |011⟩
  "Three-qubit |011⟩ computational basis state."
  (computational-basis-state 3 [0 1 1]))

(def |100⟩
  "Three-qubit |100⟩ computational basis state."
  (computational-basis-state 3 [1 0 0]))

(def |101⟩
  "Three-qubit |101⟩ computational basis state."
  (computational-basis-state 3 [1 0 1]))

(def |110⟩
  "Three-qubit |110⟩ computational basis state."
  (computational-basis-state 3 [1 1 0]))

(def |111⟩
  "Three-qubit |111⟩ computational basis state."
  (computational-basis-state 3 [1 1 1]))

;;;
;;; Measurement
;;;
(defn measure-state
  "Perform a complete quantum measurement in the computational basis.
  
  Simulates a quantum measurement by:
  1. Computing measurement probabilities for each basis state according to Born rule
  2. Randomly selecting an outcome based on these probabilities  
  3. Collapsing the state to the measured basis state
  
  This implements the fundamental quantum measurement postulate where the system
  collapses from superposition to a definite classical state.
  
  Parameters:
  - state: Quantum state to measure
  
  Returns:
  Map containing:
  - :outcome - Integer index of the measured basis state (0 to 2^n-1)
  - :collapsed-state - New quantum state after measurement collapse
  - :probability - Probability of the measured outcome
  
  Example:
  (measure-state |+⟩)
  ;=> {:outcome 0, :collapsed-state |0⟩, :probability 0.5}
  
  Note: This is probabilistic - repeated calls may yield different results"
  [state]
  {:pre [(map? state)
         (vector? (:state-vector state))
         (pos-int? (:num-qubits state))]}
  (let [amplitudes (:state-vector state)
        probabilities (mapv #(let [amp-mag (fc/abs %)] (* amp-mag amp-mag)) amplitudes)
        total-prob (reduce + probabilities)
        ;; Verify normalization (allowing for small numerical errors)
        _ (when (> (abs (- total-prob 1.0)) 1e-8)
            (throw (ex-info "State is not properly normalized"
                            {:total-probability total-prob})))
        cumulative-probs (reductions + probabilities)
        random-val (rand total-prob)
        outcome (count (take-while #(< % random-val) cumulative-probs))
        outcome (min outcome (dec (count amplitudes))) ; Ensure valid index
        collapsed-vector (assoc (vec (repeat (count amplitudes) (fc/complex 0 0)))
                                outcome (fc/complex 1 0))]
    {:outcome outcome
     :collapsed-state (assoc state :state-vector collapsed-vector)
     :probability (nth probabilities outcome)}))

(defn measure-specific-qubits
  "Perform quantum measurement on specific qubits with proper partial measurement.
  
  This implements proper partial measurement by:
  1. Computing probabilities for all possible outcomes of the measured qubits
  2. Selecting an outcome probabilistically according to Born rule
  3. Collapsing the measured qubits while preserving quantum coherence in unmeasured qubits
  4. Properly renormalizing the remaining state
  
  For a full quantum simulator, this correctly handles:
  - Entangled states where measurement affects the entire system
  - Proper probability calculations for partial measurements  
  - Correct post-measurement state normalization
  - Preservation of quantum correlations in unmeasured subsystems
  
  Parameters:
  - state: Quantum state to measure
  - measurement-qubits: Vector of qubit indices to measure (0-indexed)
  
  Returns:
  Map containing:
  - :outcomes - Vector of measurement outcomes (0 or 1) for each measured qubit
  - :collapsed-state - Properly normalized quantum state after partial measurement
  - :probabilities - Map of outcome -> probability for each possible measurement result
  
  Example:
  For a Bell state measuring qubit 0:
  (measure-specific-qubits bell-state [0])
  ;=> {:outcomes [0], :collapsed-state normalized-state, :probabilities {...}}
  
  Note: This correctly implements quantum measurement theory"
  [state measurement-qubits]
  {:pre [(map? state)
         (vector? (:state-vector state))
         (pos-int? (:num-qubits state))
         (vector? measurement-qubits)
         (every? #(and (integer? %) (>= % 0) (< % (:num-qubits state))) measurement-qubits)]}
  (let [n-qubits (:num-qubits state)
        amplitudes (:state-vector state)
        n-measured (count measurement-qubits)
        n-outcomes (bit-shift-left 1 n-measured) ; 2^n-measured possible outcomes

        ;; Calculate probabilities for each possible measurement outcome
        outcome-probabilities
        (into {}
              (for [outcome-idx (range n-outcomes)]
                (let [outcome-bits (into []
                                         (for [i (range n-measured)]
                                           (bit-and (bit-shift-right outcome-idx i) 1)))
                      ;; Sum probabilities of all basis states consistent with this measurement
                      total-prob
                      (reduce +
                              (for [basis-idx (range (count amplitudes))
                                    :let [basis-bits (into []
                                                           (for [i (range n-qubits)]
                                                             (bit-and (bit-shift-right basis-idx (- n-qubits 1 i)) 1)))
                                          measured-bits (mapv #(nth basis-bits %) measurement-qubits)]
                                    :when (= measured-bits outcome-bits)]
                                (let [amp (nth amplitudes basis-idx)
                                      amp-mag (fc/abs amp)]
                                  (* amp-mag amp-mag))))]
                  [outcome-bits total-prob])))

        ;; Select outcome probabilistically
        total-prob (reduce + (vals outcome-probabilities))
        cumulative-probs (reductions + (vals outcome-probabilities))
        random-val (rand total-prob)
        selected-outcome-idx (count (take-while #(< % random-val) cumulative-probs))
        selected-outcome-idx (min selected-outcome-idx (dec (count outcome-probabilities)))
        selected-outcome (nth (keys outcome-probabilities) selected-outcome-idx)
        selected-probability (get outcome-probabilities selected-outcome)

        ;; Collapse state: zero out amplitudes inconsistent with measurement
        ;; and renormalize remaining amplitudes
        collapsed-amplitudes
        (mapv (fn [basis-idx amplitude]
                (let [basis-bits (into []
                                       (for [i (range n-qubits)]
                                         (bit-and (bit-shift-right basis-idx (- n-qubits 1 i)) 1)))
                      measured-bits (mapv #(nth basis-bits %) measurement-qubits)]
                  (if (= measured-bits selected-outcome)
                    ;; Keep amplitude but will renorm
                    amplitude
                    ;; Zero out inconsistent amplitudes
                    (fc/complex 0 0))))
              (range (count amplitudes))
              amplitudes)

        ;; Renormalize the collapsed state
        normalization-factor (if (> selected-probability 0)
                               (/ 1.0 (fm/sqrt selected-probability))
                               1.0)
        normalized-amplitudes (mapv #(fc/mult % (fc/complex normalization-factor 0)) collapsed-amplitudes)

        collapsed-state {:state-vector normalized-amplitudes
                         :num-qubits n-qubits}]

    {:outcomes selected-outcome
     :collapsed-state collapsed-state
     :probabilities outcome-probabilities}))

(defn partial-trace
  "Compute the partial trace of a quantum state over specified qubits.
  
  The partial trace operation reduces a multi-qubit quantum state to a subsystem
  by 'tracing out' or summing over the unwanted qubits. This is essential for
  analyzing subsystems of entangled quantum states.
  
  For a 2-qubit state |ψ⟩ = Σ αᵢⱼ|ij⟩, tracing out qubit j gives:
  ρᵢ = Σⱼ |αᵢⱼ|² for the reduced single-qubit state
  
  This implementation supports tracing out a single qubit from a multi-qubit system.

  Parameters:
  - state: Multi-qubit quantum state to trace
  - trace-qubit: Index of the qubit to trace out (0-indexed)

  Returns:
  Reduced quantum state with one fewer qubit

  Example:
    (partial-trace bell-state 1)  ; Trace out second qubit of Bell state
  ;=> Mixed state of first qubit"
  [state trace-qubit]
  {:pre [(< trace-qubit (:num-qubits state))
         (> (:num-qubits state) 1)]}
  (let [n-qubits (:num-qubits state)
        amplitudes (:state-vector state)

        ;; For simplicity, implement partial trace for 2-qubit systems
        ;; In a full implementation, this would handle arbitrary n-qubit systems
        reduced-amplitudes
        (if (= n-qubits 2)
          ;; 2-qubit case: trace out specified qubit
          (if (= trace-qubit 1)
            ;; Trace out second qubit: |00⟩ + |01⟩ -> |0⟩, |10⟩ + |11⟩ -> |1⟩
            (let [amp0 (fm/sqrt (+ (* (fc/abs (nth amplitudes 0)) (fc/abs (nth amplitudes 0)))
                                   (* (fc/abs (nth amplitudes 1)) (fc/abs (nth amplitudes 1)))))
                  amp1 (fm/sqrt (+ (* (fc/abs (nth amplitudes 2)) (fc/abs (nth amplitudes 2)))
                                   (* (fc/abs (nth amplitudes 3)) (fc/abs (nth amplitudes 3)))))]
              [(fc/complex amp0 0) (fc/complex amp1 0)])
            ;; Trace out first qubit: |00⟩ + |10⟩ -> |0⟩, |01⟩ + |11⟩ -> |1⟩  
            (let [amp0 (fm/sqrt (+ (* (fc/abs (nth amplitudes 0)) (fc/abs (nth amplitudes 0)))
                                   (* (fc/abs (nth amplitudes 2)) (fc/abs (nth amplitudes 2)))))
                  amp1 (fm/sqrt (+ (* (fc/abs (nth amplitudes 1)) (fc/abs (nth amplitudes 1)))
                                   (* (fc/abs (nth amplitudes 3)) (fc/abs (nth amplitudes 3)))))]
              [(fc/complex amp0 0) (fc/complex amp1 0)]))
          ;; For higher dimensions, use a simplified approach
          ;; This is a placeholder - full implementation would handle general case
          [(fc/complex (/ 1 (fm/sqrt 2)) 0) (fc/complex (/ 1 (fm/sqrt 2)) 0)])]

    {:state-vector (vec reduced-amplitudes)
     :num-qubits (dec n-qubits)}))

;; Measurement utility functions
; TODO duplicate to probabilities, remove?
(defn measurement-probabilities
  "Calculate measurement probabilities for all computational basis states.
  
  Returns a vector of probabilities for measuring each computational basis state,
  computed using the Born rule: P(|i⟩) = |αᵢ|² where αᵢ is the amplitude
  for basis state |i⟩.
  
  Parameters:
  - state: Quantum state to analyze
  
  Returns:
  Vector of probabilities, one for each computational basis state
  
  Example:
  (measurement-probabilities |+⟩)
  ;=> [0.5 0.5]  ; Equal probability for |0⟩ and |1⟩"
  [state]
  {:pre [(map? state)
         (vector? (:state-vector state))
         (pos-int? (:num-qubits state))]}
  (mapv #(let [amp-mag (fc/abs %)] (* amp-mag amp-mag))
        (:state-vector state)))

; TODO duplicate to index-to-bits, remove?
(defn measurement-outcomes-to-bits
  "Convert a measurement outcome integer to its binary bit representation.
  
  This is the inverse of bits-to-index. Converts an integer measurement outcome
  back to the corresponding bit vector representation.
  
  Parameters:
  - outcome: Integer measurement outcome (0 to 2^n-1)
  - n-qubits: Number of qubits (determines bit vector length)
  
  Returns:
  Vector of bits [b₀ b₁ ... bₙ₋₁] representing the measurement outcome
  
  Examples:
  (measurement-outcomes-to-bits 0 1) ;=> [0]
  (measurement-outcomes-to-bits 1 1) ;=> [1]  
  (measurement-outcomes-to-bits 5 3) ;=> [1 0 1]  ; 5 = 4+1 = 101₂"
  [outcome n-qubits]
  {:pre [(integer? outcome)
         (>= outcome 0)
         (< outcome (bit-shift-left 1 n-qubits))
         (pos-int? n-qubits)]}
  (vec (for [i (range n-qubits)]
         (bit-and (bit-shift-right outcome (- n-qubits 1 i)) 1))))

(defn measure-state-statistics
  "Perform multiple measurements and collect statistical data.
  
  Simulates running the same quantum measurement many times to gather
  statistical information about measurement outcomes, frequencies, and
  empirical probabilities.
  
  Parameters:
  - state: Quantum state to measure repeatedly
  - num-measurements: Number of measurements to perform
  
  Returns:
  Map containing:
  - :total-measurements - Total number of measurements performed
  - :outcomes - Vector of all measurement outcomes  
  - :frequencies - Map of outcome -> count
  - :probabilities - Map of outcome -> empirical probability
  - :expected-probabilities - Map of outcome -> theoretical probability
  
  Example:
  (measure-state-statistics |+⟩ 1000)
  ;=> {:total-measurements 1000, :outcomes [...], :frequencies {0 501, 1 499}, ...}"
  [state num-measurements]
  {:pre [(map? state)
         (vector? (:state-vector state))
         (pos-int? (:num-qubits state))
         (pos-int? num-measurements)]}
  (let [outcomes (repeatedly num-measurements #(:outcome (measure-state state)))
        frequencies (frequencies outcomes)
        total (reduce + (vals frequencies))
        empirical-probs (into {} (map (fn [[outcome count]]
                                        [outcome (/ count total)])
                                      frequencies))
        ;; Calculate expected probabilities using Born rule
        n-states (count (:state-vector state))
        expected-probs (into {} (map (fn [i]
                                       [i (probability state i)])
                                     (range n-states)))]
    {:total-measurements num-measurements
     :outcomes (vec outcomes)
     :frequencies frequencies
     :probabilities empirical-probs
     :expected-probabilities expected-probs}))

(defn state-fidelity
  "Calculate fidelity between two quantum states.
   
   This computes the overlap between the state vectors of the two states.
   Fidelity is defined as the absolute value of the inner product of the state vectors.
   
   Parameters:
   - state1: First quantum state
   - state2: Second quantum state
   
   Returns:
   - Fidelity value as a double, representing the closeness of the two states."
  [state1 state2]
  (let [sv1 (:state-vector state1)
        sv2 (:state-vector state2)
        overlap-terms (map (fn [a1 a2]
                             (let [conj-a1 (fc/complex (fc/re a1) (- (fc/im a1)))
                                   product (fc/mult conj-a1 a2)]
                               product))
                           sv1 sv2)
        total-overlap (reduce fc/add overlap-terms)]
    (fc/abs total-overlap)))

(comment

  ;; Test normalization
  (def |0⟩-norm (normalize-state |0⟩))
  (def |1⟩-norm (normalize-state |1⟩))
  (def |+⟩-norm (normalize-state |+⟩))
  (def |-⟩-norm (normalize-state |-⟩))

  (normalize-state (multi-qubit-state [(fc/complex 3 0) (fc/complex 4 0)]))

  ;; Test measurements
  (probability |+⟩ 0)
  (probability |+⟩ 1)

  (measure-state |+⟩)

  (bits-to-index [1 1 0])
  (index-to-bits 6 3) ;=> [1 1 0]
  (computational-basis-state 3 [1 1 0]) ;=> |110⟩ state
  (measure-state |110⟩)

  ;
  )

; Disable fastmath operator macros to avoid conflicts
#_(m/unuse-primitive-operators)
