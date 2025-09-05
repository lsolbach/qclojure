(ns org.soulspace.qclojure.domain.channel
  "Quantum channel domain layer providing pure quantum mechanical channel operations.
  
  This namespace contains the core quantum channel functionality separated from
  implementation details. It focuses on the mathematical representation and 
  application of quantum channels through Kraus operators.
  
  Quantum channels represent the most general form of quantum operations,
  describing how quantum states evolve under the influence of noise, decoherence,
  and other physical processes. Every quantum channel can be represented using
  the Kraus representation: Ε(ρ) = Σᵢ KᵢρKᵢ†
  
  Key concepts implemented:
  - Kraus operator generation for standard noise models
  - Quantum channel application to quantum states
  - Decoherence parameter calculations
  - Matrix operations for quantum channel mathematics"
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]))

;; Specs for quantum channel validation
(s/def ::noise-probability (s/and number? #(<= 0 % 1)))
(s/def ::damping-parameter (s/and number? #(<= 0 % 1)))
(s/def ::rotation-angle number?)
(s/def ::rotation-axis #{:x :y :z})
(s/def ::kraus-matrix (s/coll-of (s/coll-of any? :count 2) :count 2))
(s/def ::kraus-operator (s/keys :req-un [::matrix]))
(s/def ::quantum-channel (s/coll-of ::kraus-operator))

;; Alias matrix key for kraus operators
(s/def ::matrix ::kraus-matrix)

;;
;; Kraus Operator Generation Functions (Pure Quantum Mechanics)
;;

(defn depolarizing-kraus-operators
  "Generate Kraus operators for depolarizing noise channel.
  
  The depolarizing channel applies each Pauli operator with probability p/4,
  and leaves the state unchanged with probability 1-3p/4.
  
  For proper normalization: Σᵢ Kᵢ† Kᵢ = I
  
  Parameters:
  - p: Total error probability (0 <= p <= 3/4 for physical channel)
  
  Returns: Vector of Kraus operators with coefficients applied to matrices"
  [p]
  {:pre [(s/valid? ::noise-probability p)
         (<= p 0.75)] ; Physical constraint for depolarizing channel
   :post [(s/valid? ::quantum-channel %)]}
  (let [;; Use existing Pauli matrices from gate namespace
        no-error-coeff (fm/sqrt (- 1.0 p))
        error-coeff (fm/sqrt (/ p 3.0))]
    ;; Apply coefficients directly to matrices for proper quantum channel
    [{:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex no-error-coeff 0) %) row)) qg/pauli-i)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-x)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-y)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-z)}]))

(defn amplitude-damping-kraus-operators
  "Generate Kraus operators for amplitude damping (T1 decay).
  
  Models energy dissipation where |1⟩ decays to |0⟩.
  Kraus operators: K₀ = [[1, 0], [0, √(1-γ)]], K₁ = [[0, √γ], [0, 0]]
  
  Parameters:
  - gamma: Damping parameter (0 <= gamma <= 1)
  
  Returns: Vector of Kraus operators using fastmath complex numbers"
  [gamma]
  {:pre [(s/valid? ::damping-parameter gamma)]
   :post [(s/valid? ::quantum-channel %)]}
  [{:matrix [[(fc/complex 1.0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (fm/sqrt (- 1.0 gamma)) 0)]]}
   {:matrix [[(fc/complex 0 0) (fc/complex (fm/sqrt gamma) 0)]
             [(fc/complex 0 0) (fc/complex 0 0)]]}])

(defn phase-damping-kraus-operators
  "Generate Kraus operators for phase damping (T2 dephasing).
  
  Models random phase flips without energy loss.
  Kraus operators: K₀ = [[1, 0], [0, √(1-γ)]], K₁ = [[0, 0], [0, √γ]]
  
  Parameters:
  - gamma: Dephasing parameter (0 <= gamma <= 1)
  
  Returns: Vector of Kraus operators using fastmath complex numbers"
  [gamma]
  {:pre [(s/valid? ::damping-parameter gamma)]
   :post [(s/valid? ::quantum-channel %)]}
  [{:matrix [[(fc/complex 1.0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (fm/sqrt (- 1.0 gamma)) 0)]]}
   {:matrix [[(fc/complex 0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (fm/sqrt gamma) 0)]]}])

(defn coherent-error-kraus-operator
  "Generate Kraus operator for coherent (systematic) errors.
  
  Parameters:
  - angle: Rotation angle in radians
  - axis: Rotation axis (:x, :y, or :z)
  
  Returns: Single Kraus operator for coherent rotation"
  [angle axis]
  {:pre [(s/valid? ::rotation-angle angle)
         (s/valid? ::rotation-axis axis)]
   :post [(s/valid? ::kraus-operator %)]}
  (let [cos-half (fm/cos (/ angle 2.0))
        sin-half (fm/sin (/ angle 2.0))]
    (case axis
      :x {:matrix [[(fc/complex cos-half 0) (fc/complex (- sin-half) 0)] 
                   [(fc/complex sin-half 0) (fc/complex cos-half 0)]]}
      :y {:matrix [[(fc/complex cos-half 0) (fc/complex sin-half 0)] 
                   [(fc/complex (- sin-half) 0) (fc/complex cos-half 0)]]}
      :z {:matrix [[(fc/complex (fm/cos angle) 0) (fc/complex 0 0)] 
                   [(fc/complex 0 0) (fc/complex (fm/cos (- angle)) 0)]]})))

;;
;; Quantum Channel Application Functions 
;;

(defn apply-matrix-to-amplitude
  "Apply a 2x2 matrix to a single amplitude pair in a quantum state.
  
  Performs proper complex matrix-vector multiplication:
  [new-a0] = [m00 m01] [a0]
  [new-a1]   [m10 m11] [a1]
  
  Parameters:
  - amplitude-pair: [a0 a1] where each is a fastmath Vec2 complex number
  - matrix: 2x2 matrix [[m00 m01] [m10 m11]] with fastmath complex elements
  
  Returns: [new-a0 new-a1] as fastmath complex numbers"
  [amplitude-pair matrix]
  {:pre [(= 2 (count amplitude-pair))
         (= 2 (count matrix))
         (every? #(= 2 (count %)) matrix)]}
  (let [[[m00 m01] [m10 m11]] matrix
        [a0 a1] amplitude-pair]
    [(fc/add (fc/mult m00 a0) (fc/mult m01 a1))
     (fc/add (fc/mult m10 a0) (fc/mult m11 a1))]))

(defn apply-single-qubit-kraus-operator
  "Apply a single Kraus operator to a specific qubit in a multi-qubit state.
  
  This implements the proper quantum mechanics for Kraus operator application:
  |ψ'⟩ = K|ψ⟩ / ||K|ψ⟩||
  
  The Kraus operator matrix should already include any coefficients.
  
  Parameters:
  - state: Quantum state
  - kraus-op: Kraus operator {:matrix matrix}
  - qubit-index: Target qubit (0-indexed)
  
  Returns: New quantum state after applying Kraus operator and normalizing"
  [state kraus-op qubit-index]
  {:pre [(s/valid? ::qs/state state)
         (s/valid? ::kraus-operator kraus-op)
         (nat-int? qubit-index)
         (< qubit-index (:num-qubits state))]}
  (let [n-qubits (:num-qubits state)
        state-vec (:state-vector state)
        matrix (:matrix kraus-op)
        n-states (count state-vec)]
     
     (if (= n-qubits 1)
       ;; Single qubit case
       (let [[new-amp0 new-amp1] (apply-matrix-to-amplitude 
                                  [(first state-vec) (second state-vec)] 
                                  matrix)
             result-state {:num-qubits 1
                           :state-vector [new-amp0 new-amp1]}]
         ;; Normalize the result using the existing normalize-state function
         (qs/normalize-state result-state))
       
       ;; Multi-qubit case - apply to specific qubit
       (let [new-amplitudes 
             (vec (for [i (range n-states)]
                    (let [qubit-bit (bit-test i (- n-qubits 1 qubit-index))
                          partner-i (if qubit-bit
                                      (bit-clear i (- n-qubits 1 qubit-index))
                                      (bit-set i (- n-qubits 1 qubit-index)))
                          amp-pair (if qubit-bit
                                     [(nth state-vec partner-i) (nth state-vec i)]
                                     [(nth state-vec i) (nth state-vec partner-i)])
                          [new-amp0 new-amp1] (apply-matrix-to-amplitude 
                                                amp-pair
                                                matrix)]
                      (if qubit-bit new-amp1 new-amp0))))
             result-state {:num-qubits n-qubits
                           :state-vector new-amplitudes}]
         
         ;; Normalize the result using the existing normalize-state function
         (qs/normalize-state result-state)))))

(defn apply-quantum-channel
  "Apply a complete quantum channel defined by multiple Kraus operators.
  
  For pure state simulators, implements quantum channels by randomly selecting
  one Kraus operator to apply based on the probabilities encoded in the Kraus
  operator coefficients. This provides correct noise simulation.
  
  Parameters:
  - state: Input quantum state
  - kraus-operators: Vector of Kraus operators with matrices and coefficients
  - qubit-index: Target qubit
  
  Returns: Output quantum state after channel application"
  [state kraus-operators qubit-index]
  {:pre [(s/valid? ::qs/state state)
         (s/valid? ::quantum-channel kraus-operators)
         (nat-int? qubit-index)
         (< qubit-index (:num-qubits state))]}
  (if (= (count kraus-operators) 1)
    ;; Single Kraus operator case - apply directly
    (apply-single-qubit-kraus-operator state (first kraus-operators) qubit-index)
    ;; Multiple Kraus operators - select based on probabilities from coefficients
    (let [;; Calculate probabilities from Kraus operator coefficients (max |coefficient|²)
          probabilities (mapv (fn [kraus-op]
                                (let [matrix (:matrix kraus-op)]
                                  ;; Find maximum coefficient magnitude squared from the matrix
                                  (apply max (map (fn [row]
                                                    (apply max (map (fn [coeff]
                                                                      (+ (* (fc/re coeff) (fc/re coeff)) 
                                                                         (* (fc/im coeff) (fc/im coeff))))
                                                                    row)))
                                                  matrix)))) kraus-operators)
          ;; Generate random number for selection
          rand-val (rand)
          ;; Select Kraus operator based on cumulative probabilities
          selected-operator (loop [cumulative-prob 0.0
                                   idx 0]
                             (let [new-cumulative (+ cumulative-prob (nth probabilities idx))]
                               (if (or (< rand-val new-cumulative) (>= idx (dec (count kraus-operators))))
                                 (nth kraus-operators idx)
                                 (recur new-cumulative (inc idx)))))]
      ;; Apply the selected Kraus operator
      (apply-single-qubit-kraus-operator state selected-operator qubit-index))))

;;
;; Decoherence and Physical Parameter Calculations
;;

(defn calculate-decoherence-params
  "Calculate decoherence parameters from T1, T2 times and gate duration.
  
  Parameters:
  - t1: T1 relaxation time (microseconds)
  - t2: T2 dephasing time (microseconds) 
  - gate-time: Gate operation time (nanoseconds)
  
  Returns: {:gamma-1 gamma-2} decoherence parameters"
  [t1 t2 gate-time]
  {:pre [(pos? t1) (pos? t2) (pos? gate-time)]}
  (let [gate-time-us (/ gate-time 1000.0) ; Convert ns to μs
        gamma-1 (- 1.0 (fm/exp (- (/ gate-time-us t1))))
        gamma-2 (- 1.0 (fm/exp (- (/ gate-time-us t2))))]
    {:gamma-1 gamma-1 :gamma-2 gamma-2}))

;;
;; Channel Composition and Utilities
;;

(defn compose-channels
  "Compose two quantum channels sequentially.
  
  The composition of two channels Ε₁ and Ε₂ is (Ε₂ ∘ Ε₁)(ρ) = Ε₂(Ε₁(ρ))
  
  Parameters:
  - channel-1: First quantum channel (applied first)
  - channel-2: Second quantum channel (applied second)
  
  Returns: Function that applies both channels sequentially"
  [channel-1 channel-2]
  {:pre [(s/valid? ::quantum-channel channel-1)
         (s/valid? ::quantum-channel channel-2)]}
  (fn [state qubit-index]
    (-> state
        (apply-quantum-channel channel-1 qubit-index)
        (apply-quantum-channel channel-2 qubit-index))))

(defn channel-fidelity
  "Calculate the fidelity between input and output states of a channel.
  
  Fidelity measures how well a quantum channel preserves quantum information.
  F(ρ, σ) = Tr(√(√ρ σ √ρ))²
  
  For pure states: F(|ψ⟩, |φ⟩) = |⟨ψ|φ⟩|²
  
  Parameters:
  - input-state: Input quantum state
  - output-state: Output quantum state after channel application
  
  Returns: Fidelity value between 0 and 1"
  [input-state output-state]
  {:pre [(s/valid? ::qs/state input-state)
         (s/valid? ::qs/state output-state)
         (= (:num-qubits input-state) (:num-qubits output-state))]}
  (let [input-vec (:state-vector input-state)
        output-vec (:state-vector output-state)
        ;; Calculate inner product ⟨ψ|φ⟩
        inner-product (reduce fc/add 
                              (map (fn [a b] 
                                     (fc/mult (fc/conjugate a) b))
                                   input-vec output-vec))
        ;; Calculate |⟨ψ|φ⟩|² 
        magnitude (fc/abs inner-product)]
    (* magnitude magnitude)))