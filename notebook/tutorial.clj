
;; # Clojure Quantum Computing Tutorial
;; This is a simple tutorial to demonstrate the use of the
;; [QClojure](https://github.com/soulspace/qclojure) library.
;; It covers the creation of quantum states, gates, and circuits.
;;
;; ## Introduction
;; Quantum computing is a fascinating field that combines computer science, physics, and mathematics.
;; It allows us to perform computations that are not possible with classical computers.
;; Quantum computers use quantum bits, or [qubits](https://en.wikipedia.org/wiki/Qubit), which can be in a superposition of states.
;; This means that a qubit can be in a state of 0, 1, or both at the same time.
;; Quantum computing is based on the principles of quantum mechanics, which describe the behavior of particles at the quantum level.
;;
;; Quantum computing has the potential to revolutionize many fields, including cryptography, optimization, and machine learning.
;; It can solve certain problems much faster than classical computers, by performing many calculations at once.
;; Quantum Algorithms, such as Shor's algorithm for factoring large numbers and Grover's algorithm for searching unsorted databases, demonstrate the power of quantum computing.
;;
;; Quantum algorithms are defined in terms of quantum gates, which are operations that can be applied to qubits.
;; Quantum gates manipulate the state of qubits and can be combined to create quantum circuits.
;; Quantum circuits are sequences of quantum gates applied to qubits, similar to classical logic circuits.
;;
;; The QClojure library provides a Clojure interface to quantum computing concepts.
;; It allows us to create and manipulate quantum states, gates, and circuits in a functional programming style.
;; QClojure can also be used to simulate quantum circuits and, by implementing backends, run them on quantum hardware.
;; This tutorial will guide you through the basics of quantum computing using QClojure.
;;
;; For a general introduction to quantum computing, take a look at
;; [Quantum Computing](https://en.wikipedia.org/wiki/Quantum_computing).
;;
;; ## Imports
;; We use kindly to visualize the output of our code.
;; Then we import the relevant namespaces for the domain concepts of the Qclojure library.
;; The `state` namespace provides functions to create and manipulate quantum states.
;; The `gate` namespace provides functions to create quantum gates.
;; The `circuit` namespace provides functions to create and manipulate quantum circuits.
;;
;; We also import the visualization namespace and the svg renderer.

(ns tutorial
  (:require
   [scicloj.kindly.v4.api :as kindly]
   [scicloj.kindly.v4.kind :as kind]
   [org.soulspace.qclojure.domain.state :as qs]
   [org.soulspace.qclojure.domain.gate :as qg]
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.adapter.visualization.svg :as svg]
   [org.soulspace.qclojure.adapter.visualization :as viz]))

;; ## Quantum States
;; A quantum state is a mathematical object that describes the state of a quantum system.
;; In QClojure, quantum states are represented as vectors of complex numbers.
;; The vector of complex numbers represents the amplitudes of the basis states, which represent the possible states of the system.
;; The notation |⟩ is called a "[braket](https://en.wikipedia.org/wiki/Dirac_notation)" and is used to represent a vector in a complex vector space.
;; The Qubit is the basic unit of quantum information, and it can be in a [superposition](https://en.wikipedia.org/wiki/Superposition) of the states |0⟩ and |1⟩.
;; A classic bit can be in one of two states, 0 or 1, but a qubit can be in a superposition of both states.
;; This means that a qubit can represent 0, 1, or both at the same time, with different probabilities.
;;
;; ### Basic Quantum States
;; The *qs* namespace defines several basic quantum states.
;; Let's look at the quantum state |0⟩, which is the ground state of a qubit.

qs/|0⟩

;; The measured value of a quantum state is probabilistic.
;; We have a probability of measuring the state |0⟩ as 0, and a probability of measuring it as 1.
;; We can visualize the probability distribution of the quantum state |0⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|0⟩))

;; It shows that the probability of measuring the state |0⟩ results in 0 is 1, which is certain.
;;
;; The [Bloch sphere](https://en.wikipedia.org/wiki/Bloch_sphere) is a geometrical representation of quantum states.
;; We can visualize the quantum state |0⟩ as a vector on the Bloch sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|0⟩))

;; The Bloch sphere representation shows that the state |0⟩ is at the north pole of the sphere.
;;
;; Let's look at another quantum state, the excited state |1⟩.

qs/|1⟩

;; We can visualize the probability distribution of the quantum state |1⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|1⟩))

;; It shows that the probability of measuring the state |1⟩ results in 1 is 1, which is also certain.
;; The Bloch sphere representation shows that the state |1⟩ is at the south pole of the sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|1⟩))

;; ### Superposition States
;; Quantum states can also be in a superposition of the ground and excited states.
;; Superposition states are linear combinations of the basic quantum states.
;;
;; Let's look at the quantum state |+⟩, which is a superposition of the ground and excited states.
;; The state |+⟩ is defined as (|0⟩ + |1⟩) / √2.
;; The quantum state |+⟩ is a superposition of the ground and excited states.

qs/|+⟩

;; We can visualize the probability distribution of the quantum state |+⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|+⟩))

;; The Bloch sphere representation shows that the state |+⟩ is on the equator of the sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|+⟩))

;; The quantum state |-⟩ is another superposition of the ground and excited states.
;; The state |-⟩ is defined as (|0⟩ - |1⟩) / √2.

qs/|-⟩

;; We can visualize the probability distribution of the quantum state |-⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|-⟩))

;; The Bloch sphere representation shows that the state |-⟩ is also on the equator of the sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|-⟩))

;; ### Multi-Qubit States and Quantum Registers
;; Tensor products can be used to create multi-qubit states from single-qubit states.
;; For example, the state |00⟩ is the tensor product of two |0⟩ states.

qs/|00⟩

;; We can visualize the probability distribution of the quantum state |00⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|00⟩))

;; ## Quantum Gates
;; Quantum gates are operations that can be applied to quantum states.
;; They are represented as matrices that act on the quantum states.
;; The *qg* namespace defines several quantum gates.
;;
;; ### Pauli Gates
;; The [Pauli gates](https://en.wikipedia.org/wiki/Pauli_matrices) are a set of quantum gates that can be applied to single qubits.
;; The Pauli-X gate is a quantum gate that flips the state of a qubit.

qg/pauli-x

;; The Pauli-Y gate is a quantum gate that flips the state of a qubit and adds a phase.

qg/pauli-y

;; The Pauli-Z gate is a quantum gate that adds a phase to the state of a qubit.

qg/pauli-z

;; ### Hadamard Gate
;; The [Hadamard gate](https://en.wikipedia.org/wiki/Hadamard_gate) is a quantum gate that creates superposition states.
;; It transforms the state |0⟩ into the state |+⟩ and |1⟩ into the state |-⟩.
;; The Hadamard gate is defined as the matrix:

qg/hadamard

;; We can apply the Hadamard gate to the state |0⟩ to create the superposition state |+⟩.

(def hadamard-state
  (qg/h-gate qs/|0⟩))

;; We can visualize the probability distribution of the Hadamard state.

(kind/html (viz/visualize-quantum-state :svg hadamard-state))

;; The probability distribution shows that the Hadamard state is in a superposition of the ground and excited states.

(kind/html (viz/visualize-bloch-sphere :svg hadamard-state))

;; The Bloch sphere representation shows that the Hadamard state is on the equator of the sphere.

;; ## Quantum Circuits
;; Quantum circuits are sequences of quantum gates applied to quantum states.
;; The *qc* namespace provides functions to create and manipulate quantum circuits.
;;
;; ### Creating a Quantum Circuit
;; We can create a simple quantum circuit that applies the Hadamard gate to the state |0⟩.

(def simple-circuit
  (-> (qc/create-circuit 1 "Hadamard on qubit 0")
      (qc/h-gate 0)))

;; We can visualize the quantum circuit.

(kind/html (viz/visualize-circuit :svg simple-circuit))

;; The circuit shows that the Hadamard gate is applied to the qubit 0.
;;
;; We can execute the circuit on the state |0⟩ to create the Hadamard state.

(def hadamard-circuit-state
  (qc/execute-circuit simple-circuit qs/|0⟩))

;; We can visualize the probability distribution of the Hadamard circuit state.

(kind/html (viz/visualize-quantum-state :svg hadamard-circuit-state))

;; The probability distribution shows that the Hadamard circuit state is in a superposition of the ground and excited states.

(kind/html (viz/visualize-bloch-sphere :svg hadamard-circuit-state))

;; The *qc* namespace also has some predefined circuits.
;;
;; For example, the 'qc/bell-state-circuit' creates a circuit that prepares a Bell state,
;; which is a two-qubit entangled state.

(def bell-circuit
  (qc/bell-state-circuit))

;; We can visualize the Bell circuit.

(kind/html (viz/visualize-circuit :svg bell-circuit))

;; The Bell circuit shows that the Hadamard gate is applied to the first qubit, followed by a CNOT gate between the first and second qubits.
;; The Bell state is a two-qubit state that is [entangled](https://en.wikipedia.org/wiki/Entanglement).

(def bell-state
  (qc/execute-circuit bell-circuit (qs/zero-state 2)))
;; We can visualize the probability distribution of the Bell state.

(kind/html (viz/visualize-quantum-state :svg bell-state))

;; The *qc* namespace also has some predefined circuits for multi-qubit states.
;; These circuits can be used to create entangled states with more than two qubits.
;;
;; For example, the `qc/ghz-circuit` creates a circuit that prepares a Greenberger-Horne-Zeilinger (GHZ) state.

(def ghz-circuit
  (qc/ghz-state-circuit 3))

;; We can visualize the GHZ circuit.

(kind/html (viz/visualize-circuit :svg ghz-circuit))

;; The GHZ circuit shows that the Hadamard gate is applied to the first qubit, followed by CNOT gates between the first and second qubits, and between the second and third qubits.
;; The GHZ state is a multi-qubit state that is entangled.
;;
;; We can apply the [GHZ](https://en.wikipedia.org/wiki/Greenberger%E2%80%93Horne%E2%80%93Zeilinger_state)
;; circuit, named after Greenberger, Horne and Zeilinger, to the state |000⟩ to create the GHZ state.

(def ghz-state
  (qc/execute-circuit ghz-circuit (qs/zero-state 3)))

;; We can visualize the probability distribution of the GHZ state.

(kind/html (viz/visualize-quantum-state :svg ghz-state))

;; The probability distribution shows that the GHZ state is in a superposition of the states |000⟩, |111⟩, and |110⟩.
;;
;; ### Measurement
;; Measurement is the process of extracting classical information from a quantum state.
;; The measurement process is probabilistic, and the probability of measuring a certain state depends on the amplitudes of the basis states in the quantum state.
;; When we measure a quantum state, we collapse it to one of the basis states with a certain probability.
;; The result of the measurement is a classical bit, which can be either 0 or 1.

;;
;;
;; ## Backends
;; QClojure can be extended with backends to run quantum circuits on quantum hardware.
;; The *backend* namespace provides a simple interface for running quantum circuits on quantum hardware.
;; QClojure comes with a simulator backend that can be used to simulate quantum circuits on a classical computer.
;;
;;
;;; ## Algorithms
;; QClojure comes with a set of predefined quantum algorithms that can be used to solve specific problems.
;; These algorithms are implemented as quantum circuits and can be executed on quantum hardware or simulated using the simulator backend.
;; Some of the algorithms include:
;; - [Deutsch algorithm](https://en.wikipedia.org/wiki/Deutsch_algorithm)
;; - [Simon's algorithm](https://en.wikipedia.org/wiki/Simon's_algorithm)
;; - [Bernstein-Vazirani algorithm](https://en.wikipedia.org/wiki/Bernstein%E2%80%93Vazirani_algorithm)
;; - [Grover's algorithm](https://en.wikipedia.org/wiki/Grover%27s_algorithm)
;; - [Shor's algorithm](https://en.wikipedia.org/wiki/Shor%27s_algorithm)
;; - [Quantum Fourier Transform](https://en.wikipedia.org/wiki/Quantum_Fourier_transform)
;; - [Quantum Phase Estimation](https://en.wikipedia.org/wiki/Quantum_phase_estimation_algorithm)
