(ns org.soulspace.qclojure.domain.quantum-circuit
  "Quantum circuit representation and execution"
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-gate :as qg]))

;; Specs for quantum circuits
(s/def ::gate-type keyword?)
(s/def ::qubit-target nat-int?)
(s/def ::qubit-control nat-int?)
(s/def ::angle number?)
(s/def ::gate-params (s/keys :opt-un [::qubit-target ::qubit-control ::angle]))

(s/def ::quantum-gate
  (s/keys :req-un [::gate-type]
          :opt-un [::gate-params]))

(s/def ::quantum-circuit
  (s/keys :req-un [::gates ::num-qubits]
          :opt-un [::name ::description]))

(s/def ::gates (s/coll-of ::quantum-gate :kind vector?))
(s/def ::num-qubits pos-int?)
(s/def ::name string?)
(s/def ::description string?)

;; Circuit creation functions
(defn create-circuit
  "Create a new quantum circuit.
  
  A quantum circuit is a data structure representing a sequence of quantum gates
  to be applied to a quantum system. This function initializes an empty circuit
  with the specified number of qubits.
  
  Parameters:
  - num-qubits: Positive integer specifying the number of qubits in the circuit
  - name: (optional) String name for the circuit for identification
  - description: (optional) String description explaining what the circuit does
  
  Returns:
  A quantum circuit map containing:
  - :gates - Empty vector to hold quantum gates
  - :num-qubits - Number of qubits the circuit operates on
  - :name - Optional name for the circuit
  - :description - Optional description of the circuit's purpose
  
  Examples:
  (create-circuit 2)
  ;=> {:gates [], :num-qubits 2}
  
  (create-circuit 3 \"My Circuit\")
  ;=> {:gates [], :num-qubits 3, :name \"My Circuit\"}
  
  (create-circuit 2 \"Bell State\" \"Prepares entangled Bell state\")
  ;=> {:gates [], :num-qubits 2, :name \"Bell State\", :description \"Prepares entangled Bell state\"}"
  ([num-qubits]
   {:gates []
    :num-qubits num-qubits})
  ([num-qubits name]
   {:gates []
    :num-qubits num-qubits
    :name name})
  ([num-qubits name description]
   {:gates []
    :num-qubits num-qubits
    :name name
    :description description}))

(defn add-gate
  "Add a quantum gate to a quantum circuit.
  
  This is the core function for building quantum circuits by appending gates
  to the gate sequence. Gates are represented as maps with a gate type and
  optional parameters.
  
  Parameters:
  - circuit: A quantum circuit map (created with create-circuit)
  - gate-type: Keyword identifying the type of gate (:x, :y, :z, :h, :cnot, etc.)
  - target: (optional) Integer index of the target qubit (0-indexed)
  - target1: (optional) Integer index of the target1 qubit (0-indexed)
  - target2: (optional) Integer index of the target2 qubit (0-indexed)
  - control: (optional) Integer index of the control qubit for controlled gates
  - control1: (optional) Integer index of the control1 qubit for controlled gates
  - control2: (optional) Integer index of the control2 qubit for controlled gates
  - angle: (optional) Rotation angle for parameterized gates (in radians)
  
  Returns:
  Updated quantum circuit with the new gate appended to the :gates vector
  
  Examples:
  (add-gate (create-circuit 2) :x :target 0)
  ;=> {:gates [{:gate-type :x, :gate-params {:target 0}}], :num-qubits 2}
  
  (add-gate (create-circuit 2) :cnot :control 0 :target 1)
  ;=> {:gates [{:gate-type :cnot, :gate-params {:control 0, :target 1}}], :num-qubits 2}
  
  (add-gate (create-circuit 1) :rx :target 0 :angle 1.57)
  ;=> {:gates [{:gate-type :rx, :gate-params {:target 0, :angle 1.57}}], :num-qubits 1}"
  [circuit gate-type & {:keys [target target1 target2 control control1 control2 angle] :as params}]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (s/valid? ::gate-type gate-type)]}
  (let [gate {:gate-type gate-type}
        gate-with-params (if (seq params)
                           (assoc gate :gate-params params)
                           gate)]
    (update circuit :gates conj gate-with-params)))

;; Common gate addition functions
(defn x-gate
  "Add a Pauli-X (NOT) gate to the quantum circuit.
  
  The Pauli-X gate flips the state of a qubit: |0⟩ → |1⟩ and |1⟩ → |0⟩.
  It's equivalent to a classical NOT gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with X gate appended
  
  Example:
  (x-gate (create-circuit 2) 0)
  ;=> Circuit with X gate on qubit 0"
  ([circuit target]
   (add-gate circuit :x :target target)))

(defn y-gate
  "Add a Pauli-Y gate to the quantum circuit.
  
  The Pauli-Y gate applies a bit flip and phase flip: |0⟩ → i|1⟩ and |1⟩ → -i|0⟩.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with Y gate appended
  
  Example:
  (y-gate (create-circuit 2) 1)
  ;=> Circuit with Y gate on qubit 1"
  ([circuit target]
   (add-gate circuit :y :target target)))

(defn z-gate
  "Add a Pauli-Z gate to the quantum circuit.
  
  The Pauli-Z gate applies a phase flip: |0⟩ → |0⟩ and |1⟩ → -|1⟩.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with Z gate appended
  
  Example:
  (z-gate (create-circuit 2) 0)
  ;=> Circuit with Z gate on qubit 0"
  ([circuit target]
   (add-gate circuit :z :target target)))

(defn h-gate
  "Add a Hadamard gate to the quantum circuit.
  
  The Hadamard gate creates superposition: |0⟩ → (|0⟩ + |1⟩)/√2 and |1⟩ → (|0⟩ - |1⟩)/√2.
  It's fundamental for creating quantum superposition states.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with Hadamard gate appended
  
  Example:
  (h-gate (create-circuit 2) 0)
  ;=> Circuit with Hadamard gate on qubit 0"
  ([circuit target]
   (add-gate circuit :h :target target)))

(defn s-gate
  "Add an S (phase) gate to the quantum circuit.
  
  The S gate applies a phase of π/2: |0⟩ → |0⟩ and |1⟩ → i|1⟩.
  It's equivalent to a Z^(1/2) gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with S gate appended
  
  Example:
  (s-gate (create-circuit 2) 0)
  ;=> Circuit with S gate on qubit 0"
  ([circuit target]
   (add-gate circuit :s :target target)))

(defn s-dag-gate
  "Add an S-dagger (S†) gate to the quantum circuit.
  
  The S† gate applies a phase of -π/2: |0⟩ → |0⟩ and |1⟩ → -i|1⟩.
  It's equivalent to a Z^(-1/2) gate and is the inverse of the S gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with S† gate appended
  
  Example:
  (s-dag-gate (create-circuit 2) 0)
  ;=> Circuit with S† gate on qubit 0"
  ([circuit target]
   (add-gate circuit :s-dag :target target)))

(defn t-gate
  "Add a T gate to the quantum circuit.
  
  The T gate applies a phase of π/4: |0⟩ → |0⟩ and |1⟩ → e^(iπ/4)|1⟩.
  It's equivalent to a Z^(1/4) gate and is important for universal quantum computation.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with T gate appended
  
  Example:
  (t-gate (create-circuit 2) 1)
  ;=> Circuit with T gate on qubit 1"
  ([circuit target]
   (add-gate circuit :t :target target)))

(defn t-dag-gate
  "Add a T-dagger (T†) gate to the quantum circuit.
  
  The T† gate applies a phase of -π/4: |0⟩ → |0⟩ and |1⟩ → e^(-iπ/4)|1⟩.
  It's equivalent to a Z^(-1/4) gate and is the inverse of the T gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with T† gate appended
  
  Example:
  (t-dag-gate (create-circuit 2) 0)
  ;=> Circuit with T† gate on qubit 0"
  ([circuit target]
   (add-gate circuit :t-dag :target target)))

(defn rx-gate
  "Add a rotation around the X-axis gate to the quantum circuit.
  
  The RX gate rotates a qubit around the X-axis of the Bloch sphere by the specified angle.
  RX(θ) = cos(θ/2)I - i*sin(θ/2)X
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with RX gate appended
  
  Example:
  (rx-gate (create-circuit 1) 0 1.57)  ; π/2 rotation
  ;=> Circuit with RX(π/2) gate on qubit 0"
  ([circuit target angle]
   (add-gate circuit :rx :target target :angle angle)))

(defn ry-gate
  "Add a rotation around the Y-axis gate to the quantum circuit.
  
  The RY gate rotates a qubit around the Y-axis of the Bloch sphere by the specified angle.
  RY(θ) = cos(θ/2)I - i*sin(θ/2)Y
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with RY gate appended
  
  Example:
  (ry-gate (create-circuit 1) 0 3.14)  ; π rotation
  ;=> Circuit with RY(π) gate on qubit 0"
  ([circuit target angle]
   (add-gate circuit :ry :target target :angle angle)))

(defn rz-gate
  "Add a rotation around the Z-axis gate to the quantum circuit.
  
  The RZ gate rotates a qubit around the Z-axis of the Bloch sphere by the specified angle.
  RZ(θ) = cos(θ/2)I - i*sin(θ/2)Z
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with RZ gate appended
  
  Example:
  (rz-gate (create-circuit 1) 0 0.785)  ; π/4 rotation
  ;=> Circuit with RZ(π/4) gate on qubit 0"
  ([circuit target angle]
   (add-gate circuit :rz :target target :angle angle)))

(defn phase-gate
  "Add a phase gate with arbitrary phase to the quantum circuit.
  
  The phase gate applies a phase rotation only to the |1⟩ component:
  P(φ)|0⟩ = |0⟩
  P(φ)|1⟩ = e^(iφ)|1⟩
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - target: Integer index of the target qubit (0-indexed)
  - angle: Phase angle in radians
  
  Returns:
  Updated quantum circuit with phase gate appended
  
  Example:
  (phase-gate (create-circuit 1) 0 (/ Math/PI 2))  ; Same as S gate
  ;=> Circuit with P(π/2) gate on qubit 0"
  ([circuit target angle]
   (add-gate circuit :phase :target target :angle angle)))

(defn cnot-gate
  "Add a CNOT (Controlled-NOT) gate to the quantum circuit.
  
  The CNOT gate flips the target qubit if and only if the control qubit is |1⟩.
  It's a fundamental two-qubit gate used for creating entanglement.
  
  Truth table:
  |00⟩ → |00⟩
  |01⟩ → |01⟩  
  |10⟩ → |11⟩
  |11⟩ → |10⟩
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with CNOT gate appended
  
  Example:
  (cnot-gate (create-circuit 2) 0 1)
  ;=> Circuit with CNOT gate, control on qubit 0, target on qubit 1"
  ([circuit control target]
   (add-gate circuit :cnot :control control :target target)))

(def cx-gate
  "Alias for CNOT gate.
  
  The CX gate is another name for the CNOT gate, which flips the target qubit
  if the control qubit is |1⟩. This function is provided for consistency with
  other quantum programming languages.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with CNOT gate appended
  
  Example:
  (cx-gate (create-circuit 2) 0 1)
  ;=> Circuit with CNOT gate, control on qubit 0, target on qubit 1"
  cnot-gate)

(defn cz-gate
  "Add a Controlled-Z gate to the quantum circuit.
  
  The CZ gate applies a phase flip (Z gate) to the target qubit only when the 
  control qubit is in state |1⟩. It's symmetric - either qubit can be considered
  the control or target.
  
  Truth table:
  |00⟩ → |00⟩
  |01⟩ → |01⟩  
  |10⟩ → |10⟩
  |11⟩ → -|11⟩  (negative phase applied only to |11⟩ state)
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with CZ gate appended
  
  Example:
  (cz-gate (create-circuit 2) 0 1)
  ;=> Circuit with CZ gate, control on qubit 0, target on qubit 1"
  ([circuit control target]
   (add-gate circuit :cz :control control :target target)))

(defn cy-gate
  "Add a Controlled-Y gate to the quantum circuit.
  
  The CY gate applies a Y gate (bit and phase flip) to the target qubit only when 
  the control qubit is in state |1⟩.
  
  Truth table:
  |00⟩ → |00⟩
  |01⟩ → |01⟩
  |10⟩ → i|11⟩   (target flipped with i phase)
  |11⟩ → -i|10⟩  (target flipped with -i phase)
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with CY gate appended
  
  Example:
  (cy-gate (create-circuit 2) 0 1)
  ;=> Circuit with CY gate, control on qubit 0, target on qubit 1"
  ([circuit control target]
   (add-gate circuit :cy :control control :target target)))

(defn controlled-gate
  "Add a general controlled gate to the quantum circuit.
  
  Creates a controlled version of any single-qubit gate, where the gate
  is applied to the target qubit only if the control qubit is |1⟩.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - gate-type: Keyword specifying the type of gate to control (:x, :y, :z, :h, etc.)
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with controlled gate appended
  
  Example:
  (controlled-gate (create-circuit 2) :z 0 1)
  ;=> Circuit with controlled-Z gate, control on qubit 0, target on qubit 1"
  ([circuit gate-type control target]
   (add-gate circuit :controlled :control control :target target :gate-type gate-type)))

(defn crx-gate
  "Add a controlled RX gate to the quantum circuit.
  
  The CRX gate applies an RX rotation to the target qubit only when the 
  control qubit is |1⟩.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with CRX gate appended
  
  Example:
  (crx-gate (create-circuit 2) 0 1 (/ Math/PI 4))
  ;=> Circuit with CRX(π/4) gate, control on qubit 0, target on qubit 1"
  ([circuit control target angle]
   (add-gate circuit :crx :control control :target target :angle angle)))

(defn cry-gate
  "Add a controlled RY gate to the quantum circuit.
  
  The CRY gate applies an RY rotation to the target qubit only when the 
  control qubit is |1⟩.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with CRY gate appended
  
  Example:
  (cry-gate (create-circuit 2) 0 1 (/ Math/PI 3))
  ;=> Circuit with CRY(π/3) gate, control on qubit 0, target on qubit 1"
  ([circuit control target angle]
   (add-gate circuit :cry :control control :target target :angle angle)))

(defn crz-gate
  "Add a controlled RZ gate to the quantum circuit.
  
  The CRZ gate applies an RZ rotation to the target qubit only when the 
  control qubit is |1⟩. Essential for quantum algorithms like QFT.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  - angle: Rotation angle in radians
  
  Returns:
  Updated quantum circuit with CRZ gate appended
  
  Example:
  (crz-gate (create-circuit 2) 0 1 (/ Math/PI 2))
  ;=> Circuit with CRZ(π/2) gate, control on qubit 0, target on qubit 1"
  ([circuit control target angle]
   (add-gate circuit :crz :control control :target target :angle angle)))

(defn swap-gate
  "Add a SWAP gate to the quantum circuit.
  
  The SWAP gate exchanges the states of two qubits. It effectively swaps
  the quantum states stored in the two qubits.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - qubit1: Integer index of the first qubit (0-indexed)
  - qubit2: Integer index of the second qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with SWAP gate appended
  
  Example:
  (swap-gate (create-circuit 3) 0 2)
  ;=> Circuit with SWAP gate between qubits 0 and 2"
  ([circuit qubit1 qubit2]
   (add-gate circuit :swap :qubit1 qubit1 :qubit2 qubit2)))

(defn iswap-gate
  "Add an iSWAP gate to the quantum circuit.
  
  The iSWAP gate exchanges the states of two qubits with an additional phase factor i.
  It transforms |01⟩ → i|10⟩ and |10⟩ → i|01⟩, while leaving |00⟩ and |11⟩ unchanged.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - qubit1: Integer index of the first qubit (0-indexed)
  - qubit2: Integer index of the second qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with iSWAP gate appended
  
  Example:
  (iswap-gate (create-circuit 2) 0 1)
  ;=> Circuit with iSWAP gate between qubits 0 and 1"
  ([circuit qubit1 qubit2]
   (add-gate circuit :iswap :qubit1 qubit1 :qubit2 qubit2)))

(defn toffoli-gate
  "Add a Toffoli (CCNOT) gate to the quantum circuit.
  
  The Toffoli gate is a three-qubit gate that applies an X (NOT) to the target qubit 
  if and only if both control qubits are in state |1⟩. It is also known as the CCX 
  (controlled-controlled-X) gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control1: Integer index of the first control qubit (0-indexed)
  - control2: Integer index of the second control qubit (0-indexed)
  - target: Integer index of the target qubit (0-indexed)
  
  Returns:
  Updated quantum circuit with Toffoli gate appended
  
  Example:
  (toffoli-gate (create-circuit 3) 0 1 2)
  ;=> Circuit with Toffoli gate with controls on qubits 0,1 and target on qubit 2"
  ([circuit control1 control2 target]
   (add-gate circuit :toffoli :control1 control1 :control2 control2 :target target)))

(defn fredkin-gate
  "Add a Fredkin (CSWAP) gate to the quantum circuit.
  
  The Fredkin gate is a three-qubit gate that swaps the states of the two target
  qubits if and only if the control qubit is in state |1⟩. It is also known
  as the controlled-SWAP gate.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control: Integer index of the control qubit (0-indexed)
  - target1: Integer index of the first target qubit to swap (0-indexed)
  - target2: Integer index of the second target qubit to swap (0-indexed)
  
  Returns:
  Updated quantum circuit with Fredkin gate appended
  
  Example:
  (fredkin-gate (create-circuit 3) 0 1 2)
  ;=> Circuit with Fredkin gate with control on qubit 0, swapping qubits 1 and 2"
  ([circuit control target1 target2]
   (add-gate circuit :fredkin :control control :target1 target1 :target2 target2)))

;; Quantum algorithms - Higher-level constructs
(defn quantum-fourier-transform-circuit
  "Create a Quantum Fourier Transform (QFT) circuit.
  
  Creates a complete QFT circuit that transforms computational basis states
  into their quantum Fourier transformed states. The QFT is the quantum
  analog of the discrete Fourier transform and is essential for many quantum
  algorithms including Shor's factoring algorithm and quantum phase estimation.
  
  The QFT algorithm consists of:
  1. Apply Hadamard gate to each qubit
  2. Apply controlled rotation gates with angles π/2^k
  3. Reverse qubit order with SWAP gates
  
  Parameters:
  - n: Number of qubits
  
  Returns:
  Quantum circuit implementing the complete QFT
  
  Example:
  (def qft-circuit (quantum-fourier-transform-circuit 3))
  ;=> Complete 3-qubit QFT circuit"
  [n]
  {:pre [(pos-int? n)]}
  (let [circuit (create-circuit n "QFT" "Quantum Fourier Transform")]
    (-> circuit
        ;; Apply QFT to each qubit
        ((fn [c]
           (reduce (fn [circuit qubit]
                     ;; Apply Hadamard gate to current qubit
                     (let [h-circuit (h-gate circuit qubit)]
                       ;; Apply controlled rotation gates
                       (reduce (fn [inner-circuit k]
                                 (let [control-qubit (+ qubit k 1)
                                       angle (/ Math/PI (Math/pow 2 (inc k)))]
                                   (if (< control-qubit n)
                                     (crz-gate inner-circuit control-qubit qubit angle)
                                     inner-circuit)))
                               h-circuit
                               (range (- n qubit 1)))))
                   c
                   (range n))))
        ;; Reverse qubit order with SWAP gates
        ((fn [c]
           (reduce (fn [circuit i]
                     (let [j (- n 1 i)]
                       (if (< i j)
                         (swap-gate circuit i j)
                         circuit)))
                   c
                   (range (quot n 2))))))))

;; Circuit execution
(defn apply-gate-to-state
  "Apply a single quantum gate to a quantum state.
  
  This is an internal function that translates circuit gate representations
  into actual quantum gate operations on quantum states. It handles the
  mapping from gate types to the corresponding gate functions.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - gate: Gate map containing :gate-type and optional :gate-params
  
  Returns:
  New quantum state after applying the gate
  
  Example:
  (apply-gate-to-state |0⟩ {:gate-type :x, :gate-params {:target 0}})
  ;=> |1⟩ state"
  [state gate]
  (let [gate-type (:gate-type gate)
        params (:gate-params gate)
        target (:target params)
        control (:control params)
        angle (:angle params)
        qubit1 (:qubit1 params)
        qubit2 (:qubit2 params)]
    (case gate-type
      :x (if target
           (qg/x-gate state target)
           (qg/x-gate state))
      :y (if target
           (qg/y-gate state target)
           (qg/y-gate state))
      :z (if target
           (qg/z-gate state target)
           (qg/z-gate state))
      :h (if target
           (qg/h-gate state target)
           (qg/h-gate state))
      :s (qg/apply-single-qubit-gate qg/s-gate state (or target 0))
      :s-dag (qg/apply-single-qubit-gate qg/s-dag-gate state (or target 0))
      :t (qg/apply-single-qubit-gate qg/t-gate state (or target 0))
      :t-dag (qg/apply-single-qubit-gate qg/t-dag-gate state (or target 0))
      :rx (qg/apply-single-qubit-gate (qg/rx-gate angle) state (or target 0))
      :ry (qg/apply-single-qubit-gate (qg/ry-gate angle) state (or target 0))
      :rz (qg/apply-single-qubit-gate (qg/rz-gate angle) state (or target 0))
      :phase (qg/apply-single-qubit-gate (qg/phase-gate angle) state (or target 0))
      :cnot (if (and control target)
              (qg/cnot state control target)
              (throw (ex-info "CNOT requires both control and target qubits"
                              {:gate gate})))
      :cz (if (and control target)
            (qg/controlled-z state control target)
            (throw (ex-info "CZ requires both control and target qubits"
                            {:gate gate})))
      :crz (if (and control target angle)
             (qg/controlled-rz state control target angle)
             (throw (ex-info "CRZ requires control, target qubits and angle"
                             {:gate gate})))
      :crx (if (and control target angle)
             (qg/controlled-rx state control target angle)
             (throw (ex-info "CRX requires control, target qubits and angle"
                             {:gate gate})))
      :cry (if (and control target angle)
             (qg/controlled-ry state control target angle)
             (throw (ex-info "CRY requires control, target qubits and angle"
                             {:gate gate})))
      :swap (let [qubit1 (:qubit1 params)
                  qubit2 (:qubit2 params)]
              (if (and qubit1 qubit2)
                (qg/swap-gate state qubit1 qubit2)
                (throw (ex-info "SWAP requires both qubit1 and qubit2 parameters"
                                {:gate gate}))))
      :iswap (let [qubit1 (:qubit1 params)
                   qubit2 (:qubit2 params)]
               (if (and qubit1 qubit2)
                 (qg/iswap-gate state qubit1 qubit2)
                 (throw (ex-info "iSWAP requires both qubit1 and qubit2 parameters"
                                 {:gate gate}))))
      :toffoli (let [control1 (:control1 params)
                     control2 (:control2 params)
                     target (:target params)]
                 (if (and control1 control2 target)
                   (qg/toffoli-gate state control1 control2 target)
                   (throw (ex-info "Toffoli requires control1, control2, and target parameters"
                                   {:gate gate}))))
      :fredkin (let [control (:control params)
                     target1 (:target1 params)
                     target2 (:target2 params)]
                 (if (and control target1 target2)
                   (qg/fredkin-gate state control target1 target2)
                   (throw (ex-info "Fredkin requires control, target1, and target2 parameters"
                                   {:gate gate}))))
      :controlled (throw (ex-info "General controlled gates not yet implemented"
                                  {:gate gate}))
      (throw (ex-info "Unknown gate type" {:gate-type gate-type})))))

(defn execute-circuit
  "Execute a quantum circuit on an initial quantum state.
  
  Applies all gates in the circuit sequentially to transform the initial
  quantum state into the final state. This is the main function for
  running quantum computations.
  
  Parameters:
  - circuit: Quantum circuit containing the sequence of gates to apply
  - initial-state: Initial quantum state to start the computation from
  
  Returns:
  Final quantum state after applying all gates in the circuit
  
  Throws:
  Exception if circuit and state have mismatched number of qubits
  
  Example:
  (execute-circuit (bell-state-circuit) (qs/zero-state 2))
  ;=> Bell state (|00⟩ + |11⟩)/√2"
  [circuit initial-state]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (s/valid? ::qs/quantum-state initial-state)
         (= (:num-qubits circuit) (:num-qubits initial-state))]}
  (reduce apply-gate-to-state initial-state (:gates circuit)))

;; Circuit composition and transformation
;; Helper function for circuit composition
(defn update-gate-params
  "Update gate parameters based on a qubit mapping function.
  
  This function handles all types of quantum gates and updates any qubit
  indices in their parameters based on the provided mapping function.
  
  Parameters:
  - gate: The quantum gate to update
  - mapping-fn: Function that takes a qubit index and returns a new index
  
  Returns:
  Updated gate with remapped qubit indices"
  [gate mapping-fn]
  (if-let [params (:gate-params gate)]
    (let [updated-params
          (reduce-kv (fn [m k v]
                       (cond
                         ;; Check if this is a qubit index parameter
                         (#{:target :control :qubit1 :qubit2 :control1 :control2 :target1 :target2} k)
                         (assoc m k (mapping-fn v))

                         ;; Keep other parameters as they are
                         :else
                         (assoc m k v)))
                     {}
                     params)]
      (assoc gate :gate-params updated-params))
    gate))

(defn extend-circuit
  "Extend a quantum circuit to support a larger number of qubits.
  
  This function creates a new circuit with the specified number of qubits
  while preserving all the gates of the original circuit. The original 
  circuit operations will apply to the same qubits as before.
  
  With the optional qubit-mapping parameter, you can specify a function
  to map original qubit indices to new indices in the extended circuit.
  
  Parameters:
  - circuit: Original quantum circuit to extend
  - new-num-qubits: New total number of qubits (must be >= original)
  - qubit-mapping: (Optional) Function that maps original qubit indices to new indices
  
  Returns:
  A new circuit with increased qubit count and remapped qubit operations if specified
  
  Examples:
  (extend-circuit (h-gate (create-circuit 1) 0) 3)
  ;=> 3-qubit circuit with Hadamard gate on qubit 0
  
  ;; Shift all qubits by 2 positions  
  (extend-circuit (h-gate (create-circuit 1) 0) 3 #(+ % 2))
  ;=> 3-qubit circuit with Hadamard gate on qubit 2"
  [circuit new-num-qubits & {:keys [qubit-mapping] :or {qubit-mapping identity}}]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (>= new-num-qubits (:num-qubits circuit))]}

  ;; Only update gate parameters if the qubit mapping is not identity
  (let [gates (if (= qubit-mapping identity)
                (:gates circuit)
                (mapv #(update-gate-params % qubit-mapping) (:gates circuit)))]
    (-> circuit
        (assoc :num-qubits new-num-qubits)
        (assoc :gates gates)
        (update :name #(str % (when % " ") "(extended to " new-num-qubits " qubits)")))))

(defn compose-circuits
  "Compose two quantum circuits sequentially.
  
  Creates a new circuit that applies the first circuit followed by the second.
  If the circuits have different numbers of qubits, the one with fewer qubits
  will be automatically extended to match the larger one.
  
  With optional parameters, you can control how the circuits are composed:
  - qubit-mapping: Function to map circuit2's qubit indices to the combined circuit
  - offset: Simple integer offset for circuit2's qubits (shorthand for adding offset)
  - control-qubits-only: Boolean flag to indicate circuit2 should only operate on the
    first n qubits of circuit1 where n is the number of qubits in circuit2
  
  Parameters:
  - circuit1: First quantum circuit to execute
  - circuit2: Second quantum circuit to execute after the first
  - options: (Optional) Map with composition options:
    - :qubit-mapping - Function mapping circuit2's qubit indices
    - :offset - Integer to add to circuit2's qubit indices
    - :control-qubits-only - Boolean indicating circuit2 operates on control qubits only
  
  Returns:
  New quantum circuit containing all gates from circuit1 followed by all gates from circuit2
  
  Examples:
  (compose-circuits (h-gate (create-circuit 1) 0) (x-gate (create-circuit 1) 0))
  ;=> Circuit that applies H then X on qubit 0
  
  (compose-circuits (h-gate (create-circuit 1) 0) (cnot-gate (create-circuit 2) 0 1))
  ;=> 2-qubit circuit that applies H on qubit 0, then CNOT from qubit 0 to 1
  
  ;; With offset, apply second circuit starting at qubit 3
  (compose-circuits (create-circuit 5) (h-gate (create-circuit 2) 0) {:offset 3})
  ;=> 5-qubit circuit with H gate on qubit 3"
  [circuit1 circuit2 & [options]]
  {:pre [(s/valid? ::quantum-circuit circuit1)
         (s/valid? ::quantum-circuit circuit2)]}
  (let [{:keys [qubit-mapping offset control-qubits-only]} (or options {})
        num-qubits-1 (:num-qubits circuit1)
        num-qubits-2 (:num-qubits circuit2)
        max-qubits (max num-qubits-1 num-qubits-2)

        ;; Determine the appropriate qubit mapping function
        mapping-fn (cond
                     ;; Explicit mapping function provided
                     (fn? qubit-mapping)
                     qubit-mapping

                     ;; Simple offset provided
                     (integer? offset)
                     #(+ % offset)

                     ;; Circuit2 operates on control qubits only (for algorithms like Shor's)
                     control-qubits-only
                     identity

                     ;; Default - identity mapping
                     :else
                     identity)

        ;; Apply the mapping function to circuit2's gates
        mapped-gates-2 (if (= mapping-fn identity)
                         (:gates circuit2)
                         (mapv #(update-gate-params % mapping-fn)
                               (:gates circuit2)))

        ;; Extend circuit1 if needed
        target-qubits (if (< num-qubits-1 max-qubits)
                        max-qubits
                        num-qubits-1)

        ;; Calculate proper name for the composed circuit
        circuit-name (str (get circuit1 :name "Circuit1") " + "
                          (get circuit2 :name "Circuit2")
                          (when offset (str " (offset " offset ")"))
                          (when control-qubits-only " (control qubits only)"))]

    (-> circuit1
        (assoc :num-qubits target-qubits)
        (update :gates #(into % mapped-gates-2))
        (assoc :name circuit-name))))

(defn inverse-gate
  "Get the inverse (adjoint) of a quantum gate.
  
  Returns the quantum gate that undoes the effect of the input gate.
  For unitary gates, this is the complex conjugate transpose.
  
  Parameters:
  - gate: Gate map containing :gate-type and optional :gate-params
  
  Returns:
  Gate map representing the inverse gate
  
  Example:
  (inverse-gate {:gate-type :s, :gate-params {:target 0}})
  ;=> {:gate-type :s-dagger, :gate-params {:target 0}}"
  [gate]
  (let [gate-type (:gate-type gate)
        params (:gate-params gate)]
    (case gate-type
      ;; Self-inverse gates
      (:x :y :z :h :cnot) gate
      ;; Gates with simple inverses
      :s (assoc gate :gate-type :s-dagger)
      :t (assoc gate :gate-type :t-dagger)
      ;; Rotation gates - negate angle
      (:rx :ry :rz) (update-in gate [:gate-params :angle] -)
      ;; Default: assume self-inverse
      gate)))

(defn inverse-circuit
  "Create the inverse (adjoint) of a quantum circuit.
  
  The inverse circuit undoes the effect of the original circuit.
  It applies the inverse of each gate in reverse order.
  
  Parameters:
  - circuit: Quantum circuit to invert
  
  Returns:
  New quantum circuit that is the inverse of the input circuit
  
  Example:
  (inverse-circuit (bell-state-circuit))
  ;=> Circuit that converts Bell state back to |00⟩"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (let [inverse-gates (mapv inverse-gate (reverse (:gates circuit)))]
    (-> circuit
        (assoc :gates inverse-gates)
        (update :name #(str (or % "Circuit") " (inverse)")))))

;; Circuit analysis and utility functions
(defn- gate-qubits
  "Extract all qubit indices that a gate operates on."
  [gate]
  (let [params (:gate-params gate)]
    (remove nil? [(:target params)
                  (:control params)
                  (:qubit1 params)
                  (:qubit2 params)])))

(defn inverse-quantum-fourier-transform-circuit
  "Create an Inverse Quantum Fourier Transform (IQFT) circuit.
  
  The IQFT undoes the QFT and is critical for quantum phase estimation
  in Shor's algorithm and other quantum algorithms.
  
  Parameters:
  - n: Number of qubits
  
  Returns:
  Quantum circuit implementing the complete IQFT
  
  Example:
  (def iqft-circuit (inverse-quantum-fourier-transform-circuit 3))"
  [n]
  (inverse-circuit (quantum-fourier-transform-circuit n)))

(defn circuit-depth
  "Calculate the depth (number of sequential gate layers) of a quantum circuit.
  
  Circuit depth is the minimum number of time steps needed to execute the circuit,
  assuming gates that operate on different qubits can be executed in parallel.
  This is an important metric for circuit optimization and noise analysis.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Integer representing the circuit depth
  
  Example:
  (circuit-depth bell-circuit)
  ;=> 2 (H gate in layer 1, CNOT in layer 2)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (let [gates (:gates circuit)
        num-qubits (:num-qubits circuit)]
    (if (empty? gates)
      0
      (let [qubit-last-layer (vec (repeat num-qubits 0))
            final-layers (reduce (fn [last-layers gate]
                                   (let [qubits-used (gate-qubits gate)
                                         max-prev-layer (if (empty? qubits-used)
                                                          0
                                                          (apply max (map #(nth last-layers %) qubits-used)))
                                         new-layer (inc max-prev-layer)]
                                     (reduce #(assoc %1 %2 new-layer) last-layers qubits-used)))
                                 qubit-last-layer
                                 gates)]
        (apply max final-layers)))))

(defn circuit-gate-count
  "Count the total number of gates in a quantum circuit.
  
  Provides a simple metric for circuit complexity and resource requirements.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Integer representing the total number of gates
  
  Example:
  (circuit-gate-count bell-circuit)
  ;=> 2 (H gate + CNOT gate)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (count (:gates circuit)))

(defn circuit-gate-types
  "Get a summary of gate types used in the circuit.
  
  Returns a map with gate types as keys and counts as values,
  useful for analyzing circuit composition.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Map of gate type keywords to counts
  
  Example:
  (circuit-gate-types bell-circuit)
  ;=> {:h 1, :cnot 1}"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (frequencies (map :gate-type (:gates circuit))))

(defn- gate-display
  "Create a string representation of a gate for display purposes."
  [gate-type params]
  (case gate-type
    :x (str "X(" (:target params) ")")
    :y (str "Y(" (:target params) ")")
    :z (str "Z(" (:target params) ")")
    :h (str "H(" (:target params) ")")
    :s (str "S(" (:target params) ")")
    :s-dag (str "S†(" (:target params) ")")
    :t (str "T(" (:target params) ")")
    :t-dag (str "T†(" (:target params) ")")
    :phase (str "P(" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :rx (str "RX(" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :ry (str "RY(" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :rz (str "RZ(" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :cnot (str "CNOT(" (:control params) "→" (:target params) ")")
    :cx (str "CX(" (:control params) "→" (:target params) ")")
    :cy (str "CY(" (:control params) "→" (:target params) ")")
    :cz (str "CZ(" (:control params) "→" (:target params) ")")
    :crx (str "CRX(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :cry (str "CRY(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :crz (str "CRZ(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
    :swap (str "SWAP(" (:qubit1 params) "↔" (:qubit2 params) ")")
    :iswap (str "iSWAP(" (:qubit1 params) "↔" (:qubit2 params) ")")
    :toffoli (str "TOFFOLI(" (:control1 params) "," (:control2 params) "→" (:target params) ")")
    :fredkin (str "FREDKIN(" (:control params) "→" (:target1 params) "↔" (:target2 params) ")")
    :controlled (str "C" (name (:gate-type params)) "(" (:control params) "→" (:target params) ")")
    (str (name gate-type) "(" (pr-str params) ")")))

(defn print-circuit
  "Print a human-readable representation of a quantum circuit.
  
  Displays the circuit gates in a formatted way showing gate types,
  target qubits, control qubits, and parameters for easy visualization
  and debugging.
  
  Parameters:
  - circuit: Quantum circuit to print
  
  Returns:
  nil (prints to stdout)
  
  Example:
  (print-circuit bell-circuit)
  ;; Bell State (2 qubits):
  ;; H(0)
  ;; CNOT(0→1)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (let [name (get circuit :name "Unnamed Circuit")
        description (get circuit :description "")
        num-qubits (:num-qubits circuit)
        gates (:gates circuit)]
    (println (str name " (" num-qubits " qubits):"))
    (when (seq description)
      (println (str "  " description)))
    (if (empty? gates)
      (println "  (empty circuit)")
      (doseq [gate gates]
        (let [gate-type (:gate-type gate)
              params (:gate-params gate)]
          (println (str "  " (gate-display gate-type params))))))))

;; Predefined quantum circuits
(defn bell-state-circuit
  "Create a Bell state preparation circuit.
  
  Creates a quantum circuit that prepares the Bell state (|00⟩ + |11⟩)/√2
  from the initial state |00⟩. This is achieved by applying a Hadamard gate
  to the first qubit followed by a CNOT gate.
  
  The Bell state is a maximally entangled two-qubit state fundamental to
  quantum information protocols like quantum teleportation and superdense coding.
  
  Parameters: None
  
  Returns:
  Quantum circuit that prepares Bell state when applied to |00⟩
  
  Example:
  (def bell-circuit (bell-state-circuit))
  (execute-circuit bell-circuit (qs/zero-state 2))
  ;=> Bell state (|00⟩ + |11⟩)/√2"
  []
  (-> (create-circuit 2 "Bell State" "Prepares (|00⟩ + |11⟩)/√2")
      (h-gate 0)
      (cnot-gate 0 1)))

(defn ghz-state-circuit
  "Create a GHZ (Greenberger-Horne-Zeilinger) state preparation circuit.
  
  Creates a quantum circuit that prepares an n-qubit GHZ state of the form
  (|00...0⟩ + |11...1⟩)/√2 from the initial state |00...0⟩.
  
  The GHZ state is a maximally entangled n-qubit state that exhibits
  non-classical correlations and is used in quantum cryptography and
  tests of quantum mechanics.
  
  Parameters:
  - n: Number of qubits (must be ≥ 2)
  
  Returns:
  Quantum circuit that prepares n-qubit GHZ state when applied to |00...0⟩
  
  Example:
  (def ghz3-circuit (ghz-state-circuit 3))
  (execute-circuit ghz3-circuit (qs/zero-state 3))
  ;=> 3-qubit GHZ state (|000⟩ + |111⟩)/√2"
  [n]
  {:pre [(>= n 2)]}
  (let [circuit (create-circuit n "GHZ State" (str "Prepares " n "-qubit GHZ state"))]
    (reduce (fn [c i]
              (if (= i 0)
                (h-gate c 0)
                (cnot-gate c 0 i)))
            circuit
            (range n))))

(defn all-gates-circuit
  "Create a quantum circuit that demonstrates all supported gates.
   
  Parameters: None
   
  Returns:
  Quantum circuit that contains all supported gates"
  []
  (-> (create-circuit 3 "All Gates Circuit")
      (x-gate 0)
      (y-gate 1)
      (z-gate 2)
      (h-gate 0)
      (s-gate 1)
      (s-dag-gate 2)
      (t-gate 0)
      (t-dag-gate 1)
      (rx-gate 0 (/ Math/PI 4))
      (ry-gate 1 (/ Math/PI 4))
      (rz-gate 2 (/ Math/PI 4))
      (cnot-gate 0 1)
      (cz-gate 1 2)
      (crx-gate 0 2 (/ Math/PI 8))
      (cry-gate 1 2 (/ Math/PI 8))
      (crz-gate 0 1 (/ Math/PI 8))
      (swap-gate 0 2)
      (iswap-gate 1 2)
      (toffoli-gate 0 1 2)
      (fredkin-gate 0 1 2)))

(comment
  ;; Create a simple circuit with a CZ gate
  (def cz-circuit
    (-> (create-circuit 2 "CZ Test")
        (h-gate 0)
        (h-gate 1)
        (cz-gate 0 1)))

  ;; Print the circuit
  (print-circuit cz-circuit)

  ;; Execute the circuit
  (def initial-state (qs/zero-state 2))
  (def final-state (execute-circuit cz-circuit initial-state))

  ;; Check the final state - should be (|00⟩ + |01⟩ + |10⟩ - |11⟩)/2
  (:state-vector final-state)


  ;; Test circuit creation in REPL

  ;; Create a simple Bell state circuit
  (def bell-circuit (bell-state-circuit))
  (print-circuit bell-circuit)

  ;; Execute the circuit
  (def |00⟩ (qs/zero-state 2))
  (def bell-state (execute-circuit bell-circuit |00⟩))
  (:state-vector bell-state)

  ;; Create a custom circuit
  (def custom-circuit
    (-> (create-circuit 2 "Custom Circuit")
        (x-gate 0)
        (h-gate 1)
        (cnot-gate 0 1)))

  (print-circuit custom-circuit)

  ;; Create GHZ state
  (def ghz-circuit (ghz-state-circuit 3))
  (print-circuit ghz-circuit)

  (print-circuit (all-gates-circuit))

  ;; Circuit composition
  (def composed (compose-circuits bell-circuit custom-circuit))
  (print-circuit composed)

  ;; Inverse circuit
  (def bell-inverse (inverse-circuit bell-circuit))
  (print-circuit bell-inverse)

  ;; Circuit analysis
  (circuit-depth bell-circuit)
  (circuit-gate-count bell-circuit)

  ;; Test the circuit-depth function
  (let [;; Create a simple Bell state circuit (2 layers)
        bell-circuit (bell-state-circuit)

        ;; Create a more complex circuit with 3 layers
        complex-circuit (-> (create-circuit 3 "Complex Circuit")
                            (h-gate 0)          ;; Layer 1
                            (h-gate 1)          ;; Also Layer 1 (parallel with previous)
                            (cnot-gate 0 2)     ;; Layer 2
                            (x-gate 1)          ;; Also Layer 2 (parallel with previous)
                            (cnot-gate 1 2))    ;; Layer 3 (must wait for previous gates)

        ;; Create a circuit with no gates (depth 0)
        empty-circuit (create-circuit 2 "Empty Circuit")]

    (println "Bell state circuit depth:" (circuit-depth bell-circuit))
    (println "Complex circuit depth:" (circuit-depth complex-circuit))
    (println "Empty circuit depth:" (circuit-depth empty-circuit)))
  ;  
  )

;; Helper function for circuit composition
