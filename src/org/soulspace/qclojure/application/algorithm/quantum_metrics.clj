(ns org.soulspace.qclojure.application.algorithm.quantum-metrics
  "Quantum metrics and distance measures for quantum states and circuits.
   
   This namespace provides algorithms for measuring similarity and overlap
   between quantum states using various quantum metrics:
   
   - SWAP Test: Measures the inner product between two quantum states
   - Quantum Fisher Information Matrix (QFIM): Characterizes parameter sensitivity
   - Classical Fisher Information Matrix (CFIM): Characterizes measurement sensitivity
   
   These metrics are fundamental for quantum state tomography, quantum metrology,
   variational quantum algorithms, and quantum machine learning applications."
  (:require [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]
            [org.soulspace.qclojure.application.backend :as backend]))

;; TODO implement SWAP Tests
(defn swap-test-circuit
  "Constructs a SWAP test circuit for measuring overlap between two quantum states.
  
  The SWAP test is a quantum algorithm that uses an ancilla qubit and a controlled-SWAP
  (Fredkin) gate to measure the inner product |⟨ψ|φ⟩|² between two quantum states.
  
  Circuit structure:
  1. Ancilla qubit (qubit 0) is initialized to |0⟩ and Hadamard applied
  2. State 1 (|ψ⟩) occupies qubits 1 to n
  3. State 2 (|φ⟩) occupies qubits n+1 to 2n
  4. Controlled-SWAP (Fredkin) gates swap corresponding qubits of states 1 and 2,
     controlled by the ancilla
  5. Final Hadamard applied to ancilla
  6. Measure ancilla qubit
  
  The probability of measuring |0⟩ on the ancilla is: P(0) = (1 + |⟨ψ|φ⟩|²)/2
  From this, we can extract: |⟨ψ|φ⟩|² = 2·P(0) - 1
  
  Parameters:
  - n-qubits: Number of qubits in each state to compare (positive integer)
  
  Returns:
  A quantum circuit that performs the SWAP test. The circuit has 2n+1 qubits total:
  - Qubit 0: Ancilla qubit for measurement
  - Qubits 1 to n: First quantum state |ψ⟩
  - Qubits n+1 to 2n: Second quantum state |φ⟩
  
  Usage:
  To use this circuit, prepare the initial state as |0⟩⊗|ψ⟩⊗|φ⟩ where |ψ⟩ and |φ⟩
  are the states to compare. Execute the circuit and measure the ancilla qubit.
  
  Example:
  (def swap-test (swap-test-circuit 2))  ; Compare two 2-qubit states
  ; Prepare initial state |0⟩⊗|ψ⟩⊗|φ⟩ (5 qubits total)
  ; Execute circuit and measure ancilla (qubit 0)
  ; If P(0) ≈ 1, states are identical; if P(0) ≈ 0.5, states are orthogonal
  
  References:
  - Nielsen & Chuang, \"Quantum Computation and Quantum Information\", Section 9.3
  - Buhrman, Cleve, Watrous, de Wolf, \"Quantum Fingerprinting\" (2001)"
  [n-qubits]
  {:pre [(pos-int? n-qubits)]}
  (let [total-qubits (inc (* 2 n-qubits))  ; 1 ancilla + 2*n-qubits for the two states
        ancilla 0
        state1-start 1
        state2-start (inc n-qubits)]
    (-> (circuit/create-circuit total-qubits
                                "SWAP Test"
                                (str "Measures overlap between two " n-qubits "-qubit states"))
        ;; Step 1: Apply Hadamard to ancilla to create superposition
        (circuit/h-gate ancilla)

        ;; Step 2: Apply controlled-SWAP (Fredkin) gates for each qubit pair
        ;; Swap qubit i from state1 with qubit i from state2, controlled by ancilla
        (#(reduce (fn [circ i]
                    (let [qubit1 (+ state1-start i)
                          qubit2 (+ state2-start i)]
                      (circuit/fredkin-gate circ ancilla qubit1 qubit2)))
                  %
                  (range n-qubits)))

        ;; Step 3: Apply final Hadamard to ancilla to complete interference
        (circuit/h-gate ancilla)

        ;; Step 4: Measure the ancilla qubit
        (circuit/measure-operation [ancilla]))))

(defn quantum-fisher-information-matrix
  "Calculates the Quantum Fisher Information Matrix (QFIM) for a parameterized quantum state.
  
  The QFIM characterizes the distinguishability of quantum states under parameter variations.
  It is defined as:
  
  F_ij = 4 · Re[⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩ - ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩]
  
  The QFIM provides the optimal metric for parameter updates in quantum optimization,
  and is used in quantum metrology to achieve the quantum Cramér-Rao bound for
  parameter estimation.
  
  This implementation uses the parameter shift rule to compute state derivatives
  efficiently, requiring 2N circuit evaluations where N is the number of parameters.
  
  Use Cases:
  - Quantum Natural Gradient optimization (faster convergence than standard gradients)
  - Quantum Metrology (optimal parameter estimation bounds)
  - Variational Quantum Algorithms (VQE, QAOA optimization)
  - Quantum Machine Learning (training quantum neural networks)
  
  Parameters:
  - ansatz-fn: Function that creates a quantum circuit from parameters.
                Takes parameter vector, returns circuit.
  - backend: Quantum backend for circuit execution (simulator with state vector access).
  - parameters: Current parameter vector (vector of numbers).
  - options: Optional map with:
    - :shift - Parameter shift for derivatives (default: π/2)
    - :parallel? - Use parallel computation (default: false)
    - Backend execution options: :shots, :initial-state, :result-specs
  
  Returns:
  Quantum Fisher Information Matrix as a vector of vectors (NxN matrix where N = parameter count).
  The matrix is real-valued, symmetric, and positive semi-definite.
  
  Example:
  ```clojure
  (defn my-ansatz [params]
    (-> (circuit/create-circuit 2 \"Ansatz\")
        (circuit/ry-gate 0 (nth params 0))
        (circuit/ry-gate 1 (nth params 1))
        (circuit/cnot-gate 0 1)))
  
  (def qfim (quantum-fisher-information-matrix 
              my-ansatz 
              simulator 
              [0.5 1.0]
              {:shift (/ Math/PI 2)}))
  ;=> [[4.0 0.0] [0.0 4.0]]  ; For independent rotations
  ```
  
  References:
  - Nielsen & Chuang, \"Quantum Computation and Quantum Information\", Chapter 9
  - Stokes et al., \"Quantum Natural Gradient\", Quantum 4, 269 (2020)
  - Liu et al., \"Efficient Learning of Quantum States\", Nat. Phys. 17, 1349 (2021)"
  [ansatz-fn backend parameters & {:keys [shift parallel?]
                                   :or {shift (/ fm/PI 2)
                                        parallel? false}
                                   :as options}]
  {:pre [(fn? ansatz-fn)
         (vector? parameters)
         (pos? (count parameters))]}

  (let [n (count parameters)
        ;; Extract backend execution options (remove QFIM-specific options)
        backend-options (dissoc options :shift :parallel?)

        ;; Get current state |ψ(θ)⟩
        current-circuit (ansatz-fn parameters)
        current-result (backend/execute-circuit backend current-circuit backend-options)
        current-state (if (= (:job-status current-result) :completed)
                        (get-in current-result [:results :final-state])
                        ;; Fallback for synchronous execution
                        (circuit/execute-circuit current-circuit
                                                 (state/zero-state (:num-qubits current-circuit))))

        ;; Helper to compute state derivative using parameter shift
        compute-derivative
        (fn [param-idx]
          (let [params-plus (assoc parameters param-idx
                                   (+ (nth parameters param-idx) shift))
                params-minus (assoc parameters param-idx
                                    (- (nth parameters param-idx) shift))

                circuit-plus (ansatz-fn params-plus)
                circuit-minus (ansatz-fn params-minus)

                result-plus (backend/execute-circuit backend circuit-plus backend-options)
                result-minus (backend/execute-circuit backend circuit-minus backend-options)

                state-plus (if (= (:job-status result-plus) :completed)
                             (get-in result-plus [:results :final-state])
                             (circuit/execute-circuit circuit-plus
                                                      (state/zero-state (:num-qubits circuit-plus))))

                state-minus (if (= (:job-status result-minus) :completed)
                              (get-in result-minus [:results :final-state])
                              (circuit/execute-circuit circuit-minus
                                                       (state/zero-state (:num-qubits circuit-minus))))

                ;; Compute derivative: |∂ψ/∂θ⟩ ≈ (|ψ(θ+s)⟩ - |ψ(θ-s)⟩) / (2 sin(s))
                sv-plus (:state-vector state-plus)
                sv-minus (:state-vector state-minus)
                scale-factor (/ 1.0 (* 2.0 (Math/sin shift)))]

            {:state-vector
             (mapv (fn [amp-plus amp-minus]
                     (fc/scale (fc/sub amp-plus amp-minus) scale-factor))
                   sv-plus sv-minus)
             :num-qubits (:num-qubits state-plus)}))

        ;; Compute all state derivatives
        state-derivatives (if parallel?
                            (vec (pmap compute-derivative (range n)))
                            (mapv compute-derivative (range n)))]

    ;; Compute Fisher Information Matrix elements
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (let [deriv-i (:state-vector (nth state-derivatives i))
                        deriv-j (:state-vector (nth state-derivatives j))
                        state-vec (:state-vector current-state)

                        ;; ⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩
                        inner-deriv-deriv (cla/inner-product deriv-i deriv-j)

                        ;; ⟨∂ψ/∂θᵢ|ψ⟩
                        inner-deriv-i-state (cla/inner-product deriv-i state-vec)

                        ;; ⟨ψ|∂ψ/∂θⱼ⟩ = ⟨∂ψ/∂θⱼ|ψ⟩*
                        inner-deriv-j-state (cla/inner-product deriv-j state-vec)
                        inner-state-deriv-j (fc/conjugate inner-deriv-j-state)

                        ;; ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩
                        product-terms (fc/mult inner-deriv-i-state inner-state-deriv-j)

                        ;; F_ij = 4 · Re[⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩ - ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩]
                        fisher-element (* 4.0 (fc/re (fc/sub inner-deriv-deriv product-terms)))]

                    fisher-element)))))))

(defn classical-fisher-information-matrix
  "Calculates the Classical Fisher Information Matrix (CFIM) from measurement data.
  
  The CFIM quantifies parameter sensitivity using measurement outcome probabilities
  instead of quantum state vectors. It is defined as:
  
  F_ij = Σ_x [1/p(x|θ)] · [∂p(x|θ)/∂θᵢ] · [∂p(x|θ)/∂θⱼ]
  
  where x ranges over all possible measurement outcomes and p(x|θ) is the
  probability of observing outcome x given parameters θ.
  
  The CFIM is suitable for real quantum hardware (QPUs) where only measurement
  frequencies are available, not full state vectors. It provides a lower bound
  on the Quantum Fisher Information (QFIM ≥ CFIM).
  
  Use Cases:
  - Parameter optimization on real quantum hardware
  - Natural gradient methods when state vector access is unavailable
  - Quantum metrology with shot-limited measurements
  - Variational algorithms on cloud-based QPUs (IBM, Rigetti, IonQ, etc.)
  
  Parameters:
  - ansatz-fn: Function that creates a quantum circuit from parameters.
                Takes parameter vector, returns circuit with measurements.
  - backend: Quantum backend for circuit execution (real QPU or shot-based simulator).
  - parameters: Current parameter vector (vector of numbers).
  - options: Map with:
    - :shots - Number of measurements (required, e.g., 8192)
    - :shift - Parameter shift for probability derivatives (default: π/2)
    - :parallel? - Use parallel computation (default: false)
    - :epsilon - Regularization for zero probabilities (default: 1e-10)
    - Additional backend execution options
  
  Returns:
  Classical Fisher Information Matrix as a vector of vectors (NxN matrix).
  The matrix is real-valued, symmetric, and positive semi-definite.
  
  Example:
  ```clojure
  (defn my-ansatz [params]
    (-> (circuit/create-circuit 2 \"Ansatz\")
        (circuit/ry-gate 0 (nth params 0))
        (circuit/ry-gate 1 (nth params 1))
        (circuit/cnot-gate 0 1)
        (circuit/measure-all-operation)))
  
  (def cfim (classical-fisher-information-matrix 
              my-ansatz 
              qpu-backend 
              [0.5 1.0]
              {:shots 8192}))
  ;=> [[1.95 0.02] [0.02 1.98]]  ; With shot noise
  ```
  
  References:
  - Stokes et al., \"Quantum Natural Gradient\", Quantum 4, 269 (2020)
  - Cerezo et al., \"Variational Quantum Algorithms\", Nat. Rev. Phys. 3, 625 (2021)
  - Liu et al., \"Parameter Estimation on Quantum Hardware\", arXiv:2107.09737"
  [ansatz-fn backend parameters & {:keys [shots shift parallel? epsilon]
                                   :or {shift (/ fm/PI 2)
                                        parallel? false
                                        epsilon 1e-10}
                                   :as options}]
  {:pre [(fn? ansatz-fn)
         (vector? parameters)
         (pos? (count parameters))
         (or shots (contains? options :shots))]}

  (let [n (count parameters)
        ;; Extract backend execution options and prepare result specs for measurements
        backend-options (-> options
                            (dissoc :shift :parallel? :epsilon)
                            (assoc :result-specs {:measurements {:shots shots}}))

        ;; Helper to get measurement probabilities for given parameters
        get-probabilities
        (fn [params]
          (let [circuit (ansatz-fn params)
                result (backend/execute-circuit backend circuit backend-options)]
            (if (= (:job-status result) :completed)
              ;; Extract empirical probabilities from measurement results
              (get-in result [:results :measurement-results :empirical-probabilities] {})
              ;; Fallback: empty probabilities
              {})))

        ;; Get current probabilities p(x|θ)
        current-probs (get-probabilities parameters)

        ;; Compute probability derivatives ∂p(x|θ)/∂θᵢ using parameter shift
        compute-prob-derivative
        (fn [param-idx]
          (let [params-plus (assoc parameters param-idx
                                   (+ (nth parameters param-idx) shift))
                params-minus (assoc parameters param-idx
                                    (- (nth parameters param-idx) shift))

                probs-plus (get-probabilities params-plus)
                probs-minus (get-probabilities params-minus)

                ;; Get all unique outcomes from all three probability distributions
                all-outcomes (set (concat (keys current-probs)
                                          (keys probs-plus)
                                          (keys probs-minus)))

                ;; Compute derivative for each outcome:
                ;; ∂p(x|θ)/∂θᵢ ≈ [p(x|θ+s) - p(x|θ-s)] / (2·sin(s))
                scale-factor (/ 1.0 (* 2.0 (Math/sin shift)))]

            (into {} (map (fn [outcome]
                            (let [p-plus (get probs-plus outcome 0.0)
                                  p-minus (get probs-minus outcome 0.0)]
                              [outcome (* (- p-plus p-minus) scale-factor)]))
                          all-outcomes))))

        ;; Compute all probability derivatives
        prob-derivatives (if parallel?
                           (vec (pmap compute-prob-derivative (range n)))
                           (mapv compute-prob-derivative (range n)))]

    ;; Compute Classical Fisher Information Matrix elements
    ;; F_ij = Σ_x [1/p(x|θ)] · [∂p(x|θ)/∂θᵢ] · [∂p(x|θ)/∂θⱼ]
    ;; Only sum over outcomes where p(x|θ) > epsilon to avoid division by zero
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (let [deriv-i (nth prob-derivatives i)
                        deriv-j (nth prob-derivatives j)]

                    ;; Sum over all measurement outcomes with non-zero probability
                    (reduce + 0.0
                            (keep (fn [[outcome p]]
                                    (when (> p epsilon)
                                      (let [dp-di (get deriv-i outcome 0.0)
                                            dp-dj (get deriv-j outcome 0.0)]
                                        ;; F_ij contribution: (1/p) · ∂p/∂θᵢ · ∂p/∂θⱼ
                                        (/ (* dp-di dp-dj) p))))
                                  current-probs)))))))))
