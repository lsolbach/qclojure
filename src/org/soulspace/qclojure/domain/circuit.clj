(ns org.soulspace.qclojure.domain.circuit
  "Quantum circuit representation and execution"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;; Specs for quantum circuits
(s/def ::operation-kind keyword?)
(s/def ::operation-type keyword?)
(s/def ::qubit-target nat-int?)
(s/def ::qubit-control nat-int?)
(s/def ::angle number?)
(s/def ::measurement-qubits (s/coll-of nat-int? :kind vector?))
(s/def ::operation-params (s/keys :opt-un [::qubit-target ::qubit-control ::angle ::measurement-qubits]))

;; Quantum operation (gates + measurements + other non-unitary operations)
(s/def ::quantum-operation
  (s/keys :req-un [;::operation-kind
                   ::operation-type]
          :opt-un [::operation-params]))

(s/def ::quantum-circuit
  (s/keys :req-un [::operations ::num-qubits]
          :opt-un [::name ::description]))

(s/def ::operations (s/coll-of ::quantum-operation :kind vector?))
(s/def ::num-qubits pos-int?)
(s/def ::name string?)
(s/def ::description string?)

;; Execution result specs
(s/def ::measurement-outcome nat-int?)
(s/def ::measurement-results (s/coll-of ::measurement-outcome :kind vector?))
(s/def ::circuit-execution-result 
  (s/keys :req-un [::final-state]
          :opt-un [::measurement-results]))

;; Circuit creation functions
(defn create-circuit
  "Create a new quantum circuit.
  
  A quantum circuit is a data structure representing a sequence of quantum operations
  to be applied to a quantum system. This function initializes an empty circuit
  with the specified number of qubits.
  
  Parameters:
  - num-qubits: Positive integer specifying the number of qubits in the circuit
  - name: (optional) String name for the circuit for identification
  - description: (optional) String description explaining what the circuit does
  
  Returns:
  A quantum circuit map containing:
  - :operations - Empty vector to hold quantum operations (gates + measurements)
  - :num-qubits - Number of qubits the circuit operates on
  - :name - Optional name for the circuit
  - :description - Optional description of the circuit's purpose
  
  Examples:
  (create-circuit 2)
  ;=> {:operations [], :num-qubits 2}
  
  (create-circuit 3 \"My Circuit\")
  ;=> {:operations [], :num-qubits 3, :name \"My Circuit\"}
  
  (create-circuit 2 \"Bell State\" \"Prepares entangled Bell state\")
  ;=> {:operations [], :num-qubits 2, :name \"Bell State\", :description \"Prepares entangled Bell state\"}"
  ([num-qubits]
   {:operations []
    :num-qubits num-qubits})
  ([num-qubits name]
   {:operations []
    :num-qubits num-qubits
    :name name})
  ([num-qubits name description]
   {:operations []
    :num-qubits num-qubits
    :name name
    :description description}))

(defn add-operation
  "Add a quantum operation (gate or measurement) to a quantum circuit.
  
  This is the core function for building quantum circuits by appending operations
  to the operation sequence. Operations can be either unitary gates or non-unitary
  measurements.
  
  Parameters:
  - circuit: A quantum circuit map (created with create-circuit)
  - operation-type: Keyword identifying the type of operation (:x, :y, :z, :h, :cnot, :measure, etc.)
  - params: Map of operation parameters
  
  Returns:
  Updated quantum circuit with the new operation appended to the :operations vector"
  [circuit operation-type params]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (keyword? operation-type)]}
  (let [operation {:operation-type operation-type
                   :operation-params params}]
    (update circuit :operations conj operation)))

(defn add-gate
  "Add a quantum gate to a quantum circuit.
  
  This function builds quantum circuits by appending unitary gates. Gates are
  represented as operations with specific gate types and parameters.
  
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
  Updated quantum circuit with the new gate appended to the :operations vector
  
  Examples:
  (add-gate (create-circuit 2) :x :target 0)
  ;=> {:operations [{:operation-type :x, :operation-params {:target 0}}], :num-qubits 2}
  
  (add-gate (create-circuit 2) :cnot :control 0 :target 1)
  ;=> {:operations [{:operation-type :cnot, :operation-params {:control 0, :target 1}}], :num-qubits 2}
  
  (add-gate (create-circuit 1) :rx :target 0 :angle 1.57)
  ;=> {:operations [{:operation-type :rx, :operation-params {:target 0, :angle 1.57}}], :num-qubits 1}"
  [circuit gate-type & {:keys [target target1 target2 control control1 control2 angle] :as params}]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (keyword? gate-type)]}
  (let [resolved-gate-type (gr/resolve-gate-alias gate-type)]
    ;; Validate that the resolved gate type is known
    (when-not (gr/get-gate-info-with-alias resolved-gate-type)
      (throw (ex-info "Unknown gate type"
                      {:operation-type gate-type
                       :resolved-operation-type resolved-gate-type})))
    ;; Add as an operation 
    (add-operation circuit resolved-gate-type params)))

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
   (add-gate circuit :controlled :control control :target target :operation-type gate-type)))

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

(defn measure-operation
  "Add a measurement operation to the quantum circuit.
  
  The measurement operation performs a quantum measurement in the computational basis
  on the specified qubits. This collapses the quantum state and produces classical
  measurement outcomes. Unlike gates, measurements are non-unitary operations.
  
  Parameters:
  - circuit: Quantum circuit to add the measurement to
  - qubits: Vector of qubit indices to measure (0-indexed)
  
  Returns:
  Updated quantum circuit with measurement operation appended
  
  Example:
  (measure-operation (create-circuit 2) [0 1])
  ;=> Circuit with measurement of qubits 0 and 1"
  ([circuit qubits]
   {:pre [(s/valid? ::quantum-circuit circuit)
          (vector? qubits)
          (every? nat-int? qubits)]}
   (add-operation circuit :measure {:measurement-qubits qubits})))

(defn measure-all-operation
  "Add a measurement operation for all qubits in the circuit.
  
  Convenience function that measures all qubits in the quantum circuit.
  This is equivalent to calling (measure-operation circuit (range num-qubits)).
  
  Parameters:
  - circuit: Quantum circuit to add the measurement to
  
  Returns:
  Updated quantum circuit with measurement of all qubits
  
  Example:
  (measure-all-operation (create-circuit 3))
  ;=> Circuit with measurement of qubits [0 1 2]"
  ([circuit]
   (let [num-qubits (:num-qubits circuit)]
     (measure-operation circuit (vec (range num-qubits))))))

;; Circuit execution
(defn apply-gate-to-state
  "Apply a single quantum gate to a quantum state.
  
  This is an internal function that translates circuit gate representations
  into actual quantum gate operations on quantum states. It handles the
  mapping from gate types to the corresponding gate functions.
  
  Note: This function only handles unitary gates, not measurements.
  
  Parameters:
  - state: Quantum state to apply the gate to
  - gate: Gate map containing :operation-type and optional :operation-params
  
  Returns:
  New quantum state after applying the gate
  
  Example:
  (apply-gate-to-state |0⟩ {:operation-type :x, :operation-params {:target 0}})
  ;=> |1⟩ state"
  [state gate]
  (let [gate-type (gr/resolve-gate-alias (:operation-type gate))
        params (:operation-params gate)
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
      (throw (ex-info "Unknown gate type" {:operation-type gate-type})))))

(defn apply-measurement-to-state
  "Apply a measurement operation to a quantum state.
  
  This function handles non-unitary measurement operations that collapse
  the quantum state.
  
  Parameters:
  - state: Quantum state to measure
  - measurement: Measurement operation map
  
  Returns:
  New quantum state after measurement collapse"
  [state measurement]
  (let [params (:operation-params measurement)
        measurement-qubits (:measurement-qubits params)]
    (if measurement-qubits
      (:collapsed-state (qs/measure-specific-qubits state measurement-qubits))
      (throw (ex-info "Measure requires measurement-qubits parameter"
                      {:operation measurement})))))

(defn apply-operation-to-state
  "Apply a quantum operation (gate or measurement) to a quantum state.
  
  This function dispatches between unitary gates and non-unitary operations
  like measurements.
  
  Parameters:
  - state: Quantum state to apply the operation to
  - operation: Operation map containing :operation-type and :operation-params
  
  Returns:
  New quantum state after applying the operation"
  [state operation]
  (let [operation-type (:operation-type operation)]
    (case operation-type
      :measure (apply-measurement-to-state state operation)
      ;; All other operations are treated as unitary gates
      (apply-gate-to-state state operation))))

(defn execute-circuit
  "Execute a quantum circuit on an initial quantum state.
  
  Applies all operations in the circuit sequentially to transform the initial
  quantum state into the final state. This is the main function for
  running quantum computations.
  
  Parameters:
  - circuit: Quantum circuit containing the sequence of operations to apply
  - initial-state: Initial quantum state to start the computation from
  
  Returns:
  Final quantum state after applying all operations in the circuit
  
  Throws:
  Exception if circuit and state have mismatched number of qubits
  
  Example:
  (execute-circuit (bell-state-circuit) (qs/zero-state 2))
  ;=> Bell state (|00⟩ + |11⟩)/√2"
  [circuit initial-state]
  {:pre [(s/valid? ::quantum-circuit circuit)
         (s/valid? ::qs/quantum-state initial-state)
         (= (:num-qubits circuit) (:num-qubits initial-state))]}
  (reduce apply-operation-to-state initial-state (:operations circuit)))

;; Convenience functions for measurement
(defn measure-subsystem
  "Measure specific qubits and return just the measurement outcome.
  
  This is a convenience function that combines partial trace with measurement
  for algorithms that only care about specific qubit outcomes."
  [state qubit-indices]
  (if (= (count qubit-indices) (:num-qubits state))
    ;; Measuring all qubits - use direct measurement
    (qs/measure-state state)
    ;; Measuring subset - use partial trace approach
    (let [other-qubits (remove (set qubit-indices) (range (:num-qubits state)))
          traced-state (reduce qs/partial-trace state (reverse other-qubits))]
      (qs/measure-state traced-state))))

(defn inverse-gate
  "Get the inverse (adjoint) of a quantum gate.
  
  Returns the quantum gate that undoes the effect of the input gate.
  For unitary gates, this is the complex conjugate transpose.
  
  Parameters:
  - gate: Gate map containing :gate-type and optional :gate-params
  
  Returns:
  Gate map representing the inverse gate
  
  Example:
  (inverse-gate {:operation-type :s, :operation-params {:target 0}})
  ;=> {:operation-type :s-dagger, :operation-params {:target 0}}"
  [gate]
  (let [gate-type (:operation-type gate)]
    (case gate-type
      ;; Self-inverse gates
      (:x :y :z :h :cnot) gate
      ;; Gates with simple inverses
      :s (assoc gate :operation-type :s-dagger)
      :t (assoc gate :operation-type :t-dagger)
      ;; Rotation gates - negate angle
      (:rx :ry :rz) (update-in gate [:operation-params :angle] -)
      ;; Default: assume self-inverse
      gate)))

(defn inverse-operation
  "Get the inverse (adjoint) of a quantum operation.
  
  For unitary gates, returns the inverse gate operation.
  For measurements, returns nil as measurements cannot be inverted.
  
  Parameters:
  - operation: Operation map containing :operation-type and optional :operation-params
  
  Returns:
  Operation map representing the inverse operation, or nil for measurements"
  [operation]
  (let [operation-kind (:operation-kind operation)
        operation-type (:operation-type operation)]
    (case operation-kind
      ;; Measurements cannot be inverted
      :measurement nil
      ;; For all other operations, treat as gates and invert
      (let [as-gate {:operation-type operation-type 
                     :operation-params (:operation-params operation)}
            inverse-gate (inverse-gate as-gate)]
        {:operation-type (:operation-type inverse-gate)
         :operation-params (:operation-params inverse-gate)}))))

(defn inverse-circuit
  "Create the inverse (adjoint) of a quantum circuit.
  
  The inverse circuit undoes the effect of the original circuit.
  It applies the inverse of each unitary operation in reverse order.
  Measurement operations are filtered out as they cannot be inverted.
  
  Parameters:
  - circuit: Quantum circuit to invert
  
  Returns:
  New quantum circuit that is the inverse of the input circuit
  
  Example:
  (inverse-circuit (bell-state-circuit))
  ;=> Circuit that converts Bell state back to |00⟩"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (let [;; Get inverse operations, filtering out measurements (nil values)
        inverse-operations (->> (:operations circuit)
                                (map inverse-operation)
                                (filter some?)
                                reverse
                                vec)]
    (-> circuit
        (assoc :operations inverse-operations)
        (update :name #(str (or % "Circuit") " (inverse)")))))

;; Circuit analysis and utility functions
(defn- operation-qubits
  "Extract all qubit indices that an operation operates on."
  [operation]
  (let [params (:operation-params operation)
        operation-type (:operation-type operation)]
    (case operation-type
      :measure
      ;; For measurements, return the qubits being measured
      (or (:measurement-qubits params) [])
      
      ;; For gates, extract target, control, and other qubit parameters
      (remove nil? [(:target params)
                    (:control params)
                    (:qubit1 params)
                    (:qubit2 params)
                    (:control1 params)
                    (:control2 params)
                    (:target1 params)
                    (:target2 params)]))))

(defn circuit-depth
  "Calculate the depth (number of sequential operation layers) of a quantum circuit.
  
  Circuit depth is the minimum number of time steps needed to execute the circuit,
  assuming operations that operate on different qubits can be executed in parallel.
  This is an important metric for circuit optimization and noise analysis.
  
  Note: Measurements are treated as operations that can be parallelized with gates
  on different qubits but require their own time step.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Integer representing the circuit depth
  
  Example:
  (circuit-depth bell-circuit)
  ;=> 2 (H gate in layer 1, CNOT in layer 2)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (let [operations (:operations circuit)
        num-qubits (:num-qubits circuit)]
    (if (empty? operations)
      0
      (let [qubit-last-layer (vec (repeat num-qubits 0))
            final-layers (reduce (fn [last-layers operation]
                                   (let [qubits-used (operation-qubits operation)
                                         max-prev-layer (if (empty? qubits-used)
                                                          0
                                                          (apply max (map #(nth last-layers %) qubits-used)))
                                         new-layer (inc max-prev-layer)]
                                     (reduce #(assoc %1 %2 new-layer) last-layers qubits-used)))
                                 qubit-last-layer
                                 operations)]
        (apply max final-layers)))))

(defn circuit-operation-count
  "Count the total number of operations in a quantum circuit.
  
  Provides a simple metric for circuit complexity and resource requirements.
  Includes both gates and measurements.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Integer representing the total number of operations
  
  Example:
  (circuit-operation-count bell-circuit)
  ;=> 2 (H gate + CNOT gate)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (count (:operations circuit)))

(defn circuit-gate-count
  "Count the number of gates (excluding measurements) in a quantum circuit.
  
  Provides a metric for unitary operation complexity.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Integer representing the number of gates (excluding measurements)
  
  Example:
  (circuit-gate-count bell-circuit)
  ;=> 2 (H gate + CNOT gate)"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (count (filter #(not= (:operation-type %) :measure) (:operations circuit))))

(defn circuit-operation-types
  "Get a summary of operation types used in the circuit.
  
  Returns a map with operation types as keys and counts as values,
  useful for analyzing circuit composition. Includes both gates and measurements.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Map of operation type keywords to counts
  
  Example:
  (circuit-operation-types bell-circuit-with-measurement)
  ;=> {:h 1, :cnot 1, :measure 1}"
  [circuit]
  {:pre [(s/valid? ::quantum-circuit circuit)]}
  (frequencies (map :operation-type (:operations circuit))))

(defn circuit-gate-types
  "Get a summary of gate types used in the circuit (excluding measurements).
  
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
  (frequencies (map :operation-type 
                    (filter #(not= (:operation-type %) :measure) (:operations circuit)))))

(defn- operation-display
  "Create a string representation of an operation (gate or measurement) for display purposes."
  [operation]
  (let [operation-type (:operation-type operation)
        params (:operation-params operation)]
    (case operation-type
      ;; Measurement operations
      :measure (let [qubits (:measurement-qubits params)]
                 (if (= 1 (count qubits))
                   (str "MEASURE(" (first qubits) ")")
                   (str "MEASURE(" (str/join "," qubits) ")")))
      
      ;; Single-qubit gates
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
      
      ;; Two-qubit gates
      :cnot (str "CNOT(" (:control params) "→" (:target params) ")")
      :cx (str "CX(" (:control params) "→" (:target params) ")")
      :cy (str "CY(" (:control params) "→" (:target params) ")")
      :cz (str "CZ(" (:control params) "→" (:target params) ")")
      :crx (str "CRX(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
      :cry (str "CRY(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
      :crz (str "CRZ(" (:control params) "→" (:target params) ", " (format "%.3f" (:angle params)) ")")
      :swap (str "SWAP(" (:qubit1 params) "↔" (:qubit2 params) ")")
      :iswap (str "iSWAP(" (:qubit1 params) "↔" (:qubit2 params) ")")
      
      ;; Multi-qubit gates
      :toffoli (str "TOFFOLI(" (:control1 params) "," (:control2 params) "→" (:target params) ")")
      :fredkin (str "FREDKIN(" (:control params) "→" (:target1 params) "↔" (:target2 params) ")")
      :controlled (str "C" (name (:gate-type params)) "(" (:control params) "→" (:target params) ")")
      
      ;; Default case for unknown operations
      (str (name operation-type) "(" (pr-str params) ")"))))

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
        operations (:operations circuit)]
    (println (str name " (" num-qubits " qubits):"))
    (when (seq description)
      (println (str "  " description)))
    (if (empty? operations)
      (println "  (empty circuit)")
      (doseq [operation operations]
        (println (str "  " (operation-display operation)))))))

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

  ;; Create a measured circuit
  (def measured-circuit
    (-> (create-circuit 2 "Measured Circuit")
        (x-gate 0)
        (h-gate 1)
        (cnot-gate 0 1)
        (measure-operation 1)))
  
  (print-circuit measured-circuit)

  ;; Create GHZ state
  (def ghz-circuit (ghz-state-circuit 3))
  (print-circuit ghz-circuit)

  (print-circuit (all-gates-circuit))

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
