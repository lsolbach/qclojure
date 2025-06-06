(ns org.soulspace.qclojure.domain.gate
  "Quantum gate operations for quantum state manipulation"
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]))

;; Specs for quantum gates
(s/def ::gate-matrix (s/coll-of (s/coll-of ::qs/complex-amplitude :kind vector?) :kind vector?))
(s/def ::qubit-index nat-int?)
(s/def ::control-qubit ::qubit-index)
(s/def ::target-qubit ::qubit-index)

; Enable fastmath operator macros
#_(m/use-primitive-operators)

;; High-level gate application functions - Convenient wrappers for common gatesns for quantum gates
(defn matrix-vector-mult
  "Multiply a ctor using fastmath complex numbers.
  
  Performs standard matrix-vector multiplication where each element
  of the result vector is the dot product of the corresponding matrix
  row with the input vector. All operations use fastmath complex arithmetic.
  
  This is the fundamental operation for applying quantum gates to quantum
  states, where the gate matrix transforms the state vector.
  
  Parameters:
  - matrix: 2D vector of vectors containing fastmath complex numbers
  - vector: 1D vector of fastmath complex numbers
  
  Returns:
  Vector of fastmath complex numbers representing the matrix-vector product
  
  Example:
  (matrix-vector-mult pauli-x [(fc/complex 1 0) (fc/complex 0 0)])
  ;=> [(fc/complex 0 0) (fc/complex 1 0)]  ; |0⟩ → |1⟩"
  [matrix vector]
  (mapv (fn [row]
          (reduce fc/add (fc/complex 0 0)
                  (map fc/mult row vector)))
        matrix))

(defn tensor-product-matrix
  "Compute the tensor product of two matrices.
  
  The tensor product (Kronecker product) of matrices is fundamental for
  constructing multi-qubit gate operations from single-qubit gates.
  
  For matrices A (m×n) and B (p×q), the tensor product A⊗B is an (mp×nq) matrix where:
  A⊗B[i*p+k, j*q+l] = A[i,j] * B[k,l]
  
  This operation allows us to:
  - Apply single-qubit gates to specific qubits in multi-qubit systems
  - Build controlled gates from basic gates
  - Construct composite quantum operations
  
  Parameters:
  - matrix1: First matrix (2D vector of fastmath complex numbers)
  - matrix2: Second matrix (2D vector of fastmath complex numbers)
  
  Returns:
  2D vector representing the tensor product matrix1 ⊗ matrix2
  
  Example:
  (tensor-product-matrix pauli-x pauli-i)
  ;=> 4×4 matrix representing X⊗I gate for 2-qubit system"
  [matrix1 matrix2]
  (let [rows1 (count matrix1)
        cols1 (count (first matrix1))
        rows2 (count matrix2)
        cols2 (count (first matrix2))]
    (vec (for [i1 (range rows1)
               i2 (range rows2)]
           (vec (for [j1 (range cols1)
                      j2 (range cols2)]
                  (fc/mult (get-in matrix1 [i1 j1])
                           (get-in matrix2 [i2 j2]))))))))

;; Basic single-qubit gates - Fundamental quantum gate matrices
(def pauli-i
  "Identity gate matrix: I = [[1,0], [0,1]]
  
  The identity gate leaves quantum states unchanged. It's used as a placeholder
  in multi-qubit operations and as a building block for more complex gates.
  
  Matrix representation:
  |0⟩ → |0⟩ : I|0⟩ = |0⟩
  |1⟩ → |1⟩ : I|1⟩ = |1⟩"
  [[(fc/complex 1 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex 1 0)]])

(def pauli-x
  "Pauli-X (NOT) gate matrix: X = [[0,1], [1,0]]
  
  The Pauli-X gate is the quantum equivalent of a classical NOT gate.
  It flips the computational basis states:
  
  X|0⟩ = |1⟩  (bit flip: 0 → 1)
  X|1⟩ = |0⟩  (bit flip: 1 → 0)
  
  The X gate rotates the qubit by π radians around the X-axis of the Bloch sphere.
  It's also known as the bit-flip gate and is one of the three Pauli matrices."
  [[(fc/complex 0 0) (fc/complex 1 0)]
   [(fc/complex 1 0) (fc/complex 0 0)]])

(def pauli-y
  "Pauli-Y gate matrix: Y = [[0,-i], [i,0]]
  
  The Pauli-Y gate applies both bit flip and phase flip operations:
  
  Y|0⟩ = i|1⟩   (bit flip + phase: 0 → i·1)
  Y|1⟩ = -i|0⟩  (bit flip + phase: 1 → -i·0)
  
  The Y gate rotates the qubit by π radians around the Y-axis of the Bloch sphere.
  It combines the effects of X and Z gates with specific complex phases."
  [[(fc/complex 0 0) (fc/complex 0 -1)]
   [(fc/complex 0 1) (fc/complex 0 0)]])

(def pauli-z
  "Pauli-Z gate matrix: Z = [[1,0], [0,-1]]
  
  The Pauli-Z gate applies a phase flip to the |1⟩ state:
  
  Z|0⟩ = |0⟩   (no change to |0⟩)
  Z|1⟩ = -|1⟩  (phase flip: adds π phase to |1⟩)
  
  The Z gate rotates the qubit by π radians around the Z-axis of the Bloch sphere.
  It's also known as the phase-flip gate and preserves computational basis amplitudes
  while changing relative phases."
  [[(fc/complex 1 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex -1 0)]])

(def hadamard
  "Hadamard gate matrix: H = (1/√2)[[1,1], [1,-1]]
  
  The Hadamard gate creates equal superposition states and is fundamental
  to quantum computing. It transforms computational basis states into
  superposition states and vice versa:
  
  H|0⟩ = (|0⟩ + |1⟩)/√2 = |+⟩  (creates superposition from |0⟩)
  H|1⟩ = (|0⟩ - |1⟩)/√2 = |-⟩  (creates superposition from |1⟩)
  H|+⟩ = |0⟩              (collapses superposition to |0⟩)
  H|-⟩ = |1⟩              (collapses superposition to |1⟩)
  
  The Hadamard gate is self-inverse: H² = I, and represents a rotation
  by π around the axis (X+Z)/√2 on the Bloch sphere."
  (let [sqrt2-inv (/ 1 (Math/sqrt 2))]
    [[(fc/complex sqrt2-inv 0) (fc/complex sqrt2-inv 0)]
     [(fc/complex sqrt2-inv 0) (fc/complex (- sqrt2-inv) 0)]]))

;; Phase gates - Gates that apply phase rotations
(defn phase-gate
  "Create a phase gate with arbitrary phase angle φ.
  
  The phase gate applies a phase rotation only to the |1⟩ component:
  
  P(φ)|0⟩ = |0⟩         (|0⟩ unchanged)
  P(φ)|1⟩ = e^(iφ)|1⟩   (adds phase φ to |1⟩)
  
  Phase gates are crucial for:
  - Implementing quantum algorithms requiring phase relationships
  - Building controlled rotation gates
  - Creating arbitrary single-qubit rotations
  
  Parameters:
  - phi: Phase angle in radians
  
  Returns:
  2×2 matrix representing the phase gate P(φ)
  
  Examples:
  (phase-gate 0)      ; Identity gate
  (phase-gate π)      ; Z gate  
  (phase-gate π/2)    ; S gate
  (phase-gate π/4)    ; T gate"
  [phi]
  [[(fc/complex 1 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex (m/cos phi) (m/sin phi))]])

(def s-gate
  "S gate (phase gate with π/2 phase): S = [[1,0], [0,i]]
  
  The S gate applies a π/2 phase rotation to the |1⟩ state:
  
  S|0⟩ = |0⟩    (no change to |0⟩)
  S|1⟩ = i|1⟩   (adds π/2 phase to |1⟩)
  
  The S gate is equivalent to √Z and is fundamental for building
  more complex quantum gates. It's often used in quantum algorithms
  requiring precise phase control."
  (phase-gate (/ m/PI 2)))

(def s-dag-gate
  "S-dagger gate (phase gate with -π/2 phase): S† = [[1,0], [0,-i]]
  
  The S† gate applies a -π/2 phase rotation to the |1⟩ state:
  
  S†|0⟩ = |0⟩    (no change to |0⟩)
  S†|1⟩ = -i|1⟩  (adds -π/2 phase to |1⟩)
  
  The S† gate is the Hermitian adjoint (conjugate transpose) of the S gate,
  and the inverse gate to S: S†·S = I. It's used in various quantum algorithms
  and is essential for implementing fault-tolerant quantum computation."
  (phase-gate (/ m/PI -2)))

(def t-gate
  "T gate (phase gate with π/4 phase): T = [[1,0], [0,e^(iπ/4)]]
  
  The T gate applies a π/4 phase rotation to the |1⟩ state:
  
  T|0⟩ = |0⟩           (no change to |0⟩)
  T|1⟩ = e^(iπ/4)|1⟩   (adds π/4 phase to |1⟩)
  
  The T gate is equivalent to ⁴√Z and is crucial for universal quantum
  computation. Together with Hadamard and CNOT gates, it forms a
  universal gate set capable of approximating any quantum computation."
  (phase-gate (/ m/PI 4)))

(def t-dag-gate
  "T-dagger gate (phase gate with -π/4 phase): T† = [[1,0], [0,e^(-iπ/4)]]
  
  The T† gate applies a -π/4 phase rotation to the |1⟩ state:
  
  T†|0⟩ = |0⟩            (no change to |0⟩)
  T†|1⟩ = e^(-iπ/4)|1⟩   (adds -π/4 phase to |1⟩)
  
  The T† gate is the Hermitian adjoint (conjugate transpose) of the T gate,
  and the inverse gate to T: T†·T = I. It's essential in fault-tolerant quantum
  computing and appears in many gate decompositions for advanced quantum circuits."
  (phase-gate (/ m/PI -4)))

;; Rotation gates - Parameterized gates for arbitrary rotations
(defn rx-gate
  "Rotation around X-axis gate: RX(θ) = cos(θ/2)I - i·sin(θ/2)X
  
  The RX gate rotates a qubit by angle θ around the X-axis of the Bloch sphere.
  This creates arbitrary superposition states and is fundamental for quantum
  algorithm implementation.
  
  Matrix form:
  RX(θ) = [[cos(θ/2), -i·sin(θ/2)]
           [-i·sin(θ/2), cos(θ/2)]]
  
  Special cases:
  - RX(0) = I (identity)
  - RX(π) = -iX (Pauli-X with global phase)
  - RX(π/2) = (I - iX)/√2 (half X rotation)
  
  Parameters:
  - theta: Rotation angle in radians
  
  Returns:
  2×2 matrix representing the RX(θ) rotation gate
  
  Example:
  (rx-gate (/ Math/PI 2))  ; 90° rotation around X-axis"
  [theta]
  (let [cos-half (m/cos (/ theta 2))
        sin-half (m/sin (/ theta 2))]
    [[(fc/complex cos-half 0) (fc/complex 0 (- sin-half))]
     [(fc/complex 0 (- sin-half)) (fc/complex cos-half 0)]]))

(defn ry-gate
  "Rotation around Y-axis gate: RY(θ) = cos(θ/2)I - i·sin(θ/2)Y
  
  The RY gate rotates a qubit by angle θ around the Y-axis of the Bloch sphere.
  This rotation changes both the real amplitudes of |0⟩ and |1⟩ components
  without introducing complex phases.
  
  Matrix form:
  RY(θ) = [[cos(θ/2), -sin(θ/2)]
           [sin(θ/2),  cos(θ/2)]]
  
  Special cases:
  - RY(0) = I (identity)
  - RY(π) = -iY (Pauli-Y with global phase)
  - RY(π/2) transforms |0⟩ → (|0⟩ + |1⟩)/√2
  - RY(-π/2) transforms |0⟩ → (|0⟩ - |1⟩)/√2
  
  Parameters:
  - theta: Rotation angle in radians
  
  Returns:
  2×2 matrix representing the RY(θ) rotation gate
  
  Example:
  (ry-gate Math/PI)  ; 180° rotation around Y-axis"
  [theta]
  (let [cos-half (m/cos (/ theta 2))
        sin-half (m/sin (/ theta 2))]
    [[(fc/complex cos-half 0) (fc/complex (- sin-half) 0)]
     [(fc/complex sin-half 0) (fc/complex cos-half 0)]]))

(defn rz-gate
  "Rotation around Z-axis gate: RZ(θ) = e^(-iθ/2)|0⟩⟨0| + e^(iθ/2)|1⟩⟨1|
  
  The RZ gate applies phase rotations to the computational basis states.
  It rotates a qubit by angle θ around the Z-axis of the Bloch sphere
  without changing the magnitudes of the amplitudes.
  
  Matrix form:
  RZ(θ) = [[e^(-iθ/2), 0]
           [0, e^(iθ/2)]]
  
  Special cases:
  - RZ(0) = I (identity, up to global phase)
  - RZ(π) = -iZ (Pauli-Z with global phase)
  - RZ(π/2) = -iS (S gate with global phase)
  - RZ(π/4) = -iT (T gate with global phase)
  
  Parameters:
  - theta: Rotation angle in radians
  
  Returns:
  2×2 matrix representing the RZ(θ) rotation gate
  
  Example:
  (rz-gate (/ Math/PI 4))  ; 45° phase rotation"
  [theta]
  (let [exp-neg (fc/complex (m/cos (/ theta -2)) (m/sin (/ theta -2)))
        exp-pos (fc/complex (m/cos (/ theta 2)) (m/sin (/ theta 2)))]
    [[exp-neg (fc/complex 0 0)]
     [(fc/complex 0 0) exp-pos]]))

(defn cnot-gate
  "CNOT (Controlled-NOT) gate matrix for 2-qubit systems.
  
  The CNOT gate is a fundamental two-qubit gate that flips the target qubit
  if and only if the control qubit is in state |1⟩. It's the quantum
  equivalent of the classical XOR gate and is essential for creating
  quantum entanglement.
  
  Truth table for computational basis states:
  |00⟩ → |00⟩  (control=0: no change)
  |01⟩ → |01⟩  (control=0: no change)
  |10⟩ → |11⟩  (control=1: flip target)
  |11⟩ → |10⟩  (control=1: flip target)
  
  Matrix representation (4×4 for 2-qubit system):
  CNOT = [[1,0,0,0],
          [0,1,0,0],
          [0,0,0,1],
          [0,0,1,0]]
  
  The CNOT gate is universal for classical computation and, together
  with single-qubit gates, forms a universal set for quantum computation.
  
  Parameters: None (assumes control=qubit 0, target=qubit 1)
  
  Returns:
  4×4 matrix representing the CNOT gate
  
  Example:
  (matrix-vector-mult (cnot-gate) [1 0 0 0])  ; |00⟩ → |00⟩
  (matrix-vector-mult (cnot-gate) [0 0 1 0])  ; |10⟩ → |11⟩"
  []
  [[(fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0)]
   [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0)]])

;; Gate application functions - Functions to apply gates to quantum states
(defn expand-gate-to-n-qubits
  "Expand a single-qubit gate to operate on an n-qubit system at a specific position.
  
  When applying a single-qubit gate to one qubit in a multi-qubit system,
  we need to expand the 2×2 gate matrix to a 2ⁿ×2ⁿ matrix using tensor products
  with identity matrices for the other qubits.
  
  For n qubits, if we want to apply gate G to qubit i:
  - Qubits 0 to i-1: tensor with identity matrices
  - Qubit i: tensor with gate matrix G  
  - Qubits i+1 to n-1: tensor with identity matrices
  
  Parameters:
  - gate-matrix: 2×2 single-qubit gate matrix
  - n: Total number of qubits in the system
  - target-qubit: Index of the qubit to apply the gate to (0-indexed)
  
  Returns:
  2ⁿ×2ⁿ matrix representing the gate applied to the specified qubit
  
  Example:
  (expand-gate-to-n-qubits pauli-x 2 0)
  ;=> 4×4 matrix for X⊗I (X gate on qubit 0, identity on qubit 1)"
  [gate-matrix n target-qubit]
  (let [identity-2x2 [[(fc/complex 1 0) (fc/complex 0 0)]
                      [(fc/complex 0 0) (fc/complex 1 0)]]]
    (reduce tensor-product-matrix
            (for [i (range n)]
              (if (= i target-qubit)
                gate-matrix
                identity-2x2)))))

(defn apply-single-qubit-gate
  "Apply a single-qubit gate to a quantum state at a specified qubit index.
  
  This is the core function for applying quantum gates to quantum states.
  It handles both single-qubit and multi-qubit systems:
  
  - For 1-qubit systems: Direct matrix-vector multiplication
  - For n-qubit systems: Expands the gate using tensor products and applies
  
  The function preserves the quantum state structure while transforming
  the amplitudes according to the gate operation.
  
  Parameters:
  - gate-matrix: 2×2 matrix representing the single-qubit gate
  - state: Quantum state to apply the gate to
  - qubit-index: Index of the target qubit (0-indexed)
  
  Returns:
  New quantum state after applying the gate
  
  Throws:
  AssertionError if qubit-index is out of bounds
  
  Examples:
  (apply-single-qubit-gate pauli-x |0⟩ 0)
  ;=> |1⟩ state
  
  (apply-single-qubit-gate hadamard (qs/zero-state 2) 0)
  ;=> 2-qubit state with H applied to first qubit"
  [gate-matrix state qubit-index]
  {:pre [(< qubit-index (:num-qubits state))]}
  (let [n (:num-qubits state)
        state-vector (:state-vector state)]
    (if (= n 1)
      ;; Single qubit case - direct application
      {:state-vector (matrix-vector-mult gate-matrix state-vector)
       :num-qubits 1}
      ;; Multi-qubit case - need to expand gate with identity matrices
      (let [expanded-gate (expand-gate-to-n-qubits gate-matrix n qubit-index)]
        {:state-vector (matrix-vector-mult expanded-gate state-vector)
         :num-qubits n}))))

;; Controlled gate operations - Two-qubit gates with control logic
(defn controlled-gate
  "Create a controlled version of a single-qubit gate.
  
  Constructs a 4×4 matrix for a controlled gate where the gate operation
  is applied to the target qubit only when the control qubit is |1⟩.
  
  For a 2-qubit controlled gate:
  - Control qubit = 0: Target gate is not applied (identity on target)
  - Control qubit = 1: Target gate is applied
  
  Parameters:
  - gate-matrix: 2×2 matrix of the single-qubit gate to be controlled
  
  Returns:
  4×4 matrix representing the controlled version of the input gate
  
  Example:
  (controlled-gate pauli-x)  ; Creates controlled-X (CNOT) gate
  
  Note: This is a simplified 2-qubit implementation"
  [gate-matrix]
  [[(fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0)]
   [(fc/complex 0 0) (fc/complex 0 0) (first (first gate-matrix)) (first (second gate-matrix))]
   [(fc/complex 0 0) (fc/complex 0 0) (second (first gate-matrix)) (second (second gate-matrix))]])

(defn apply-controlled-gate
  "Apply a controlled gate to a multi-qubit quantum state.
  
  This function implements controlled quantum gates for arbitrary n-qubit systems.
  A controlled gate applies the specified single-qubit gate to the target qubit
  only when the control qubit is in state |1⟩.
  
  The implementation works by transforming the state vector directly:
  - For each basis state, check the control qubit
  - If control=0: leave amplitude unchanged
  - If control=1: apply the gate transformation to target qubit
  
  This allows CNOT and other controlled gates to work with arbitrary
  multi-qubit systems, enabling complex quantum circuits like GHZ states.
  
  Parameters:
  - state: Quantum state to transform (n qubits)
  - control: Index of the control qubit (0-indexed)
  - target: Index of the target qubit (0-indexed)
  - gate-matrix: 2×2 matrix of the gate to apply when control=1
  
  Returns:
  New quantum state with controlled gate applied
  
  Example:
  (apply-controlled-gate (qs/zero-state 3) 0 2 pauli-x)
  ;=> Applies CNOT from qubit 0 to qubit 2 in 3-qubit system"
  [state control target gate-matrix]
  (let [n (:num-qubits state)
        state-vector (:state-vector state)
        size (int (Math/pow 2 n))
        new-vector (vec (repeat size (fc/complex 0 0)))]
    ;; Transform each basis state according to controlled gate logic
    (loop [i 0
           result new-vector]
      (if (>= i size)
        {:state-vector result :num-qubits n}
        (let [amplitude (nth state-vector i)
              control-bit (bit-test i (- n 1 control))]
          (if control-bit
            ;; Control is 1: apply gate to target qubit
            (let [target-bit (bit-test i (- n 1 target))
                  flipped-i (if target-bit
                              (bit-clear i (- n 1 target))
                              (bit-set i (- n 1 target)))
                  gate-elem-00 (get-in gate-matrix [0 0])
                  gate-elem-01 (get-in gate-matrix [0 1])
                  gate-elem-10 (get-in gate-matrix [1 0])
                  gate-elem-11 (get-in gate-matrix [1 1])]
              (if target-bit
                ;; Target was |1⟩: contribute to both |0⟩ and |1⟩ outcomes
                (recur (inc i)
                       (-> result
                           (update flipped-i fc/add (fc/mult amplitude gate-elem-10))
                           (update i fc/add (fc/mult amplitude gate-elem-11))))
                ;; Target was |0⟩: contribute to both |0⟩ and |1⟩ outcomes
                (recur (inc i)
                       (-> result
                           (update i fc/add (fc/mult amplitude gate-elem-00))
                           (update flipped-i fc/add (fc/mult amplitude gate-elem-01))))))
            ;; Control is 0: identity operation
            (recur (inc i)
                   (update result i fc/add amplitude))))))))

;; High-level gate application functions - Convenient wrappers for common gates
(defn x-gate
  "Apply Pauli-X (NOT) gate to a quantum state.
  
  Convenience function that applies the Pauli-X gate to either a single-qubit
  state or a specific qubit in a multi-qubit state. The X gate performs a
  bit flip operation.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - qubit-index: (optional) Index of target qubit for multi-qubit states (default: 0)
  
  Returns:
  New quantum state with X gate applied
  
  Examples:
  (x-gate |0⟩)     ;=> |1⟩
  (x-gate |1⟩)     ;=> |0⟩
  (x-gate (qs/zero-state 2) 1)  ;=> X applied to second qubit"
  ([state] (apply-single-qubit-gate pauli-x state 0))
  ([state qubit-index] (apply-single-qubit-gate pauli-x state qubit-index)))

(defn y-gate
  "Apply Pauli-Y gate to a quantum state.
  
  Convenience function that applies the Pauli-Y gate to either a single-qubit
  state or a specific qubit in a multi-qubit state. The Y gate performs both
  bit flip and phase flip operations.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - qubit-index: (optional) Index of target qubit for multi-qubit states (default: 0)
  
  Returns:
  New quantum state with Y gate applied
  
  Examples:
  (y-gate |0⟩)     ;=> i|1⟩
  (y-gate |1⟩)     ;=> -i|0⟩
  (y-gate (qs/zero-state 2) 1)  ;=> Y applied to second qubit"
  ([state] (apply-single-qubit-gate pauli-y state 0))
  ([state qubit-index] (apply-single-qubit-gate pauli-y state qubit-index)))

(defn z-gate
  "Apply Pauli-Z gate to a quantum state.
  
  Convenience function that applies the Pauli-Z gate to either a single-qubit
  state or a specific qubit in a multi-qubit state. The Z gate performs a
  phase flip operation without changing computational basis probabilities.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - qubit-index: (optional) Index of target qubit for multi-qubit states (default: 0)
  
  Returns:
  New quantum state with Z gate applied
  
  Examples:
  (z-gate |0⟩)     ;=> |0⟩ (no change)
  (z-gate |1⟩)     ;=> -|1⟩ (phase flip)
  (z-gate (qs/zero-state 2) 1)  ;=> Z applied to second qubit"
  ([state] (apply-single-qubit-gate pauli-z state 0))
  ([state qubit-index] (apply-single-qubit-gate pauli-z state qubit-index)))

(defn h-gate
  "Apply Hadamard gate to a quantum state.
  
  Convenience function that applies the Hadamard gate to either a single-qubit
  state or a specific qubit in a multi-qubit state. The Hadamard gate creates
  or destroys superposition states and is fundamental to quantum algorithms.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - qubit-index: (optional) Index of target qubit for multi-qubit states (default: 0)
  
  Returns:
  New quantum state with Hadamard gate applied
  
  Examples:
  (h-gate |0⟩)     ;=> |+⟩ = (|0⟩ + |1⟩)/√2
  (h-gate |1⟩)     ;=> |-⟩ = (|0⟩ - |1⟩)/√2
  (h-gate |+⟩)     ;=> |0⟩
  (h-gate (qs/zero-state 2) 1)  ;=> H applied to second qubit"
  ([state] (apply-single-qubit-gate hadamard state 0))
  ([state qubit-index] (apply-single-qubit-gate hadamard state qubit-index)))

(defn cnot
  "Apply CNOT gate to a multi-qubit quantum state.
  
  Convenience function that applies the CNOT (Controlled-NOT) gate to a
  quantum state. The CNOT gate is fundamental for creating quantum
  entanglement and implementing quantum algorithms.
  
  Default implementation assumes:
  - Control qubit: index 0 (first qubit)
  - Target qubit: index 1 (second qubit)
  
  For multi-qubit systems, this creates a controlled-X gate where the
  control and target qubits are specified, and all other qubits are
  unaffected (identity operation).
  
  Parameters:
  - state: Quantum state to apply the gate to (≥2 qubits)
  - control: (optional) Index of control qubit (default: 0)
  - target: (optional) Index of target qubit (default: 1)
  
  Returns:
  New quantum state with CNOT gate applied
  
  Throws:
  AssertionError if state has fewer than 2 qubits
  
  Examples:
  (cnot-gate (qs/tensor-product |0⟩ |0⟩))  ;=> |00⟩ (no change)
  (cnot-gate (qs/tensor-product |1⟩ |0⟩))  ;=> |11⟩ (target flipped)
  (cnot-gate (qs/zero-state 3) 0 2)        ;=> CNOT with control=0, target=2"
  ([state]
   (cnot state 0 1))
  ([state control target]
   {:pre [(>= (:num-qubits state) 2)
          (not= control target)
          (< control (:num-qubits state))
          (< target (:num-qubits state))]}
   (let [n (:num-qubits state)
         state-vector (:state-vector state)]
     (if (and (= n 2) (= control 0) (= target 1))
       ;; 2-qubit case with standard control=0, target=1: use direct CNOT matrix
       {:state-vector (matrix-vector-mult (cnot-gate) state-vector)
        :num-qubits 2}
       ;; All other cases: apply controlled operation
       (apply-controlled-gate state control target pauli-x)))))

;; Controlled rotation gate functions for QFT and other algorithms
(defn controlled-rx
  "Apply controlled RX gate to a multi-qubit quantum state.
  
  Applies RX rotation to the target qubit when the control qubit is |1⟩.
  
  Parameters:
  - state: Quantum state to transform (≥2 qubits)
  - control: Index of control qubit (0-indexed)
  - target: Index of target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  New quantum state with controlled RX gate applied"
  [state control target angle]
  (let [rx-matrix (rx-gate angle)]
    (apply-controlled-gate state control target rx-matrix)))

(defn controlled-ry
  "Apply controlled RY gate to a multi-qubit quantum state.
  
  Applies RY rotation to the target qubit when the control qubit is |1⟩.
  
  Parameters:
  - state: Quantum state to transform (≥2 qubits)
  - control: Index of control qubit (0-indexed)
  - target: Index of target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  New quantum state with controlled RY gate applied"
  [state control target angle]
  (let [ry-matrix (ry-gate angle)]
    (apply-controlled-gate state control target ry-matrix)))

(defn controlled-rz
  "Apply controlled RZ gate to a multi-qubit quantum state.
  
  Applies RZ rotation to the target qubit when the control qubit is |1⟩.
  This is essential for implementing the Quantum Fourier Transform.
  
  Parameters:
  - state: Quantum state to transform (≥2 qubits)
  - control: Index of control qubit (0-indexed)
  - target: Index of target qubit (0-indexed)  
  - angle: Rotation angle in radians
  
  Returns:
  New quantum state with controlled RZ gate applied
  
  Example:
  (controlled-rz (qs/zero-state 3) 0 2 (/ Math/PI 2))
  ;=> Applies controlled RZ(π/2) from qubit 0 to qubit 2"
  [state control target angle]
  (let [rz-matrix (rz-gate angle)]
    (apply-controlled-gate state control target rz-matrix)))

(defn controlled-z
  "Apply a controlled-Z gate to a quantum state.
  
  The controlled-Z gate applies a Z gate (phase flip) to the target qubit
  when the control qubit is in state |1⟩. It's symmetric between control
  and target qubits.
  
  Parameters:
  - state: Quantum state to modify
  - control-idx: Index of the control qubit
  - target-idx: Index of the target qubit
  
  Returns:
  New quantum state after applying the controlled-Z gate"
  [state control target]
  (apply-controlled-gate state control target pauli-z))

(defn controlled-y
  "Apply a controlled-Y gate to a quantum state.
  
  The controlled-Y gate applies a Y gate (bit and phase flip) to the target
  qubit when the control qubit is in state |1⟩.
  
  Parameters:
  - state: Quantum state to modify
  - control-idx: Index of the control qubit
  - target-idx: Index of the target qubit
  
  Returns:
  New quantum state after applying the controlled-Y gate"
  [state control target]
  (apply-controlled-gate state control target pauli-y))

;; SWAP gate implementation
(defn swap-gate
  "Apply a SWAP gate to exchange two qubits in a quantum state.
  
  The SWAP gate exchanges the states of two qubits. It can be decomposed
  into three CNOT gates: CNOT(i,j), CNOT(j,i), CNOT(i,j).
  
  For efficiency, this implementation directly swaps the amplitudes
  corresponding to the exchanged qubits in the state vector.
  
  Parameters:
  - state: Quantum state to apply the SWAP to
  - qubit1: Index of the first qubit (0-indexed)
  - qubit2: Index of the second qubit (0-indexed)
  
  Returns:
  New quantum state with qubits swapped
  
  Example:
  (swap-gate (qs/computational-basis-state 3 [1 0 1]) 0 2)
  ;=> Swaps qubits 0 and 2, resulting in state |1⟩ ⊗ |0⟩ ⊗ |1⟩ → |1⟩ ⊗ |0⟩ ⊗ |1⟩"
  [state qubit1 qubit2]
  {:pre [(s/valid? ::qs/quantum-state state)
         (nat-int? qubit1)
         (nat-int? qubit2)
         (not= qubit1 qubit2)
         (< qubit1 (:num-qubits state))
         (< qubit2 (:num-qubits state))]}
  (let [n (:num-qubits state)
        state-vec (:state-vector state)
        num-states (count state-vec)]

    {:num-qubits n
     :state-vector
     (vec (for [i (range num-states)]
            ;; For each basis state index i, determine which index j 
            ;; it should be swapped with based on qubit positions
            (let [bit1 (bit-and (bit-shift-right i qubit1) 1)
                  bit2 (bit-and (bit-shift-right i qubit2) 1)]
              ;; If the bits are the same, no swap needed
              (if (= bit1 bit2)
                (nth state-vec i)
                ;; Otherwise, compute the swapped index
                (let [j (-> i
                            (bit-and (bit-not (bit-shift-left 1 qubit1)))  ; Clear bit qubit1
                            (bit-and (bit-not (bit-shift-left 1 qubit2)))  ; Clear bit qubit2
                            (bit-or (bit-shift-left bit2 qubit1))          ; Set bit qubit1 to bit2
                            (bit-or (bit-shift-left bit1 qubit2)))]        ; Set bit qubit2 to bit1
                  (nth state-vec j))))))}))

(defn iswap-gate
  "Apply an iSWAP gate to a quantum state.
  
  The iSWAP gate is similar to the SWAP gate but also applies an i phase 
  to the swapped states. It exchanges |01⟩ and |10⟩ with an additional i phase,
  while leaving |00⟩ and |11⟩ unchanged.
  
  Matrix representation (in computational basis |00⟩, |01⟩, |10⟩, |11⟩):
  iSWAP = [[1, 0, 0, 0],
           [0, 0, i, 0],
           [0, i, 0, 0],
           [0, 0, 0, 1]]
  
  This gate is native to some superconducting quantum processors and used
  in various quantum algorithms and error correction techniques.
  
  Parameters:
  - state: Quantum state to apply the iSWAP to
  - qubit1: Index of the first qubit (0-indexed)
  - qubit2: Index of the second qubit (0-indexed)
  
  Returns:
  New quantum state after applying iSWAP
  
  Example:
  (iswap-gate (qs/computational-basis-state 2 [0 1]) 0 1)
  ;=> Transforms |01⟩ → i|10⟩"
  [state qubit1 qubit2]
  {:pre [(s/valid? ::qs/quantum-state state)
         (nat-int? qubit1)
         (nat-int? qubit2)
         (not= qubit1 qubit2)
         (< qubit1 (:num-qubits state))
         (< qubit2 (:num-qubits state))]}
  (let [n (:num-qubits state)
        state-vec (:state-vector state)
        num-states (count state-vec)
        i-complex (fc/complex 0 1)]  ; i = sqrt(-1)

    {:num-qubits n
     :state-vector
     (vec (for [i (range num-states)]
            (let [bit1 (bit-and (bit-shift-right i qubit1) 1)
                  bit2 (bit-and (bit-shift-right i qubit2) 1)]
              (if (= bit1 bit2)
                ;; |00⟩ and |11⟩ unchanged
                (nth state-vec i)
                ;; |01⟩ → i|10⟩, |10⟩ → i|01⟩
                (let [j (-> i
                            (bit-and (bit-not (bit-shift-left 1 qubit1)))  ; Clear bit qubit1
                            (bit-and (bit-not (bit-shift-left 1 qubit2)))  ; Clear bit qubit2
                            (bit-or (bit-shift-left bit2 qubit1))          ; Set bit qubit1 to bit2
                            (bit-or (bit-shift-left bit1 qubit2)))]        ; Set bit qubit2 to bit1
                  (fc/mult i-complex (nth state-vec j)))))))}))

;; Multi-controlled gates
(defn toffoli-gate
  "Apply a Toffoli (CCNOT) gate to a quantum state.
  
  The Toffoli gate is a three-qubit gate that applies an X (NOT) to the target
  qubit if and only if both control qubits are in state |1⟩. It's also known 
  as the controlled-controlled-X (CCX) gate.
  
  Truth table:
  |000⟩ → |000⟩
  |001⟩ → |001⟩
  |010⟩ → |010⟩
  |011⟩ → |011⟩
  |100⟩ → |100⟩
  |101⟩ → |101⟩
  |110⟩ → |111⟩ (flip target only when both controls are 1)
  |111⟩ → |110⟩ (flip target only when both controls are 1)
  
  The Toffoli gate is universal for classical reversible computation and is an
  important building block for quantum error correction and quantum algorithms.
  
  Parameters:
  - state: Quantum state to apply the Toffoli to
  - control1: Index of the first control qubit (0-indexed)
  - control2: Index of the second control qubit (0-indexed)
  - target: Index of the target qubit (0-indexed)
  
  Returns:
  New quantum state after applying Toffoli gate
  
  Example:
  (toffoli-gate (qs/computational-basis-state 3 [1 1 0]) 0 1 2)
  ;=> Transforms |110⟩ → |111⟩"
  [state control1 control2 target]
  {:pre [(s/valid? ::qs/quantum-state state)
         (nat-int? control1)
         (nat-int? control2)
         (nat-int? target)
         (distinct? control1 control2 target)
         (< control1 (:num-qubits state))
         (< control2 (:num-qubits state))
         (< target (:num-qubits state))]}
  (let [n (:num-qubits state)
        state-vector (:state-vector state)
        size (int (Math/pow 2 n))
        new-vector (vec (repeat size (fc/complex 0 0)))]
    ;; Transform each basis state according to Toffoli gate logic
    (loop [i 0
           result new-vector]
      (if (>= i size)
        {:state-vector result :num-qubits n}
        (let [amplitude (nth state-vector i)
              control1-bit (bit-test i (- n 1 control1))
              control2-bit (bit-test i (- n 1 control2))]
          (if (and control1-bit control2-bit)
            ;; Both controls are 1: apply X to target
            (let [target-bit (bit-test i (- n 1 target))
                  flipped-i (if target-bit
                              (bit-clear i (- n 1 target))
                              (bit-set i (- n 1 target)))]
              (recur (inc i)
                     (update result flipped-i fc/add amplitude)))
            ;; At least one control is 0: no change
            (recur (inc i)
                   (update result i fc/add amplitude))))))))

(defn fredkin-gate
  "Apply a Fredkin (CSWAP) gate to a quantum state.
  
  The Fredkin gate is a three-qubit gate that swaps the second and third qubits
  if and only if the first control qubit is in state |1⟩. It's also known as 
  the controlled-SWAP gate.
  
  Truth table:
  |000⟩ → |000⟩
  |001⟩ → |001⟩
  |010⟩ → |010⟩
  |011⟩ → |011⟩
  |100⟩ → |100⟩
  |101⟩ → |110⟩ (swap 2nd and 3rd qubits)
  |110⟩ → |101⟩ (swap 2nd and 3rd qubits)
  |111⟩ → |111⟩
  
  The Fredkin gate is universal for classical reversible computation and
  conserves the number of 1s in the system (Hamming weight preserving).
  
  Parameters:
  - state: Quantum state to apply the Fredkin to
  - control: Index of the control qubit (0-indexed)
  - target1: Index of the first target qubit (0-indexed)
  - target2: Index of the second target qubit (0-indexed)
  
  Returns:
  New quantum state after applying Fredkin gate
  
  Example:
  (fredkin-gate (qs/computational-basis-state 3 [1 0 1]) 0 1 2)
  ;=> Transforms |101⟩ → |110⟩"
  [state control target1 target2]
  {:pre [(s/valid? ::qs/quantum-state state)
         (nat-int? control)
         (nat-int? target1)
         (nat-int? target2)
         (distinct? control target1 target2)
         (< control (:num-qubits state))
         (< target1 (:num-qubits state))
         (< target2 (:num-qubits state))]}
  (let [n (:num-qubits state)
        state-vector (:state-vector state)
        size (int (Math/pow 2 n))
        new-vector (vec (repeat size (fc/complex 0 0)))]
    ;; Transform each basis state according to Fredkin gate logic
    (loop [i 0
           result new-vector]
      (if (>= i size)
        {:state-vector result :num-qubits n}
        (let [amplitude (nth state-vector i)
              control-bit (bit-test i (- n 1 control))]
          (if control-bit
            ;; Control is 1: swap target1 and target2
            (let [target1-bit (bit-test i (- n 1 target1))
                  target2-bit (bit-test i (- n 1 target2))]
              (if (= target1-bit target2-bit)
                ;; If both targets have same value, no change
                (recur (inc i) (update result i fc/add amplitude))
                ;; Otherwise, swap the bits
                (let [swapped-i (-> i
                                    (bit-and (bit-not (bit-shift-left 1 (- n 1 target1))))
                                    (bit-and (bit-not (bit-shift-left 1 (- n 1 target2))))
                                    (bit-or (bit-shift-left target2-bit (- n 1 target1)))
                                    (bit-or (bit-shift-left target1-bit (- n 1 target2))))]
                  (recur (inc i) (update result swapped-i fc/add amplitude)))))
            ;; Control is 0: no change
            (recur (inc i) (update result i fc/add amplitude))))))))

; Disable fastmath operator macros to avoid conflicts
#_(m/unuse-primitive-operators)
