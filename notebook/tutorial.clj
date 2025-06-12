
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
   [org.soulspace.qclojure.adapter.visualization.html :as html]
   [org.soulspace.qclojure.adapter.visualization :as viz]
   [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;; ## Quantum States
;; A quantum state is a mathematical object that describes the state of a quantum system.
;; In QClojure, quantum states are represented as vectors of complex numbers.
;; The vector of complex numbers represents the amplitudes of the basis states, which represent the possible states of the system.
;; The notation |⟩ is called a "[braket](https://en.wikipedia.org/wiki/Dirac_notation)" and is used to represent a vector in a complex vector space.
;; The Qubit is the basic unit of quantum information, and it can be in a [superposition](https://en.wikipedia.org/wiki/Superposition) of the states |0⟩ and |1⟩.
;; A classic bit can be in one of two states, 0 or 1, but a qubit can be in a superposition of both states.
;; This means that a qubit can represent 0, 1, or both at the same time, with different probabilities.
;;
;; ## Measurement
;; Measurement is the process of extracting classical information from a quantum state.
;; The measurement process is probabilistic, and the probability of measuring
;; a certain state depends on the amplitudes of the basis states in the quantum state.
;; When we measure a quantum state, we collapse it to one of the basis states
;; with a certain probability. After measurement, the quantum state is no longer
;; in a superposition, but in one of the basis states.
;;
;; The result of the measurement is a classical bit, which can be either 0 or 1.
;; The measurement process is a fundamental aspect of quantum mechanics and is
;; described by the [Born rule](https://en.wikipedia.org/wiki/Born_rule).
;; The Born rule states that the probability of measuring a certain state is equal
;; to the square of the amplitude of that state in the quantum state vector.
;;
;; ### Basic Quantum States
;; The *qs* namespace defines some basic quantum states.
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
;; We can execute the circuit with the `qc/execute-circuit` function
;; on the state |0⟩ to create the Hadamard state.

(def hadamard-circuit-state
  (qc/execute-circuit simple-circuit qs/|0⟩))

;; We can visualize the probability distribution of the Hadamard circuit state.

(kind/html (viz/visualize-quantum-state :svg hadamard-circuit-state))

;; The probability distribution shows that the Hadamard circuit state is
;; in a superposition of the ground and excited states.

(kind/html (viz/visualize-bloch-sphere :svg hadamard-circuit-state))

;; The *qc* namespace also has some predefined circuits.
;;
;; For example, the 'qc/bell-state-circuit' creates a circuit that prepares a Bell state,
;; which is a two-qubit entangled state.

(def bell-circuit
  (qc/bell-state-circuit))

;; We can visualize the Bell circuit.

(kind/html (viz/visualize-circuit :svg bell-circuit))

;; The Bell circuit shows that the Hadamard gate is applied to the first qubit,
;; followed by a CNOT gate between the first and second qubits.
;; The Bell state is a two-qubit state that is
;;[entangled](https://en.wikipedia.org/wiki/Entanglement).

(def bell-state
  (qc/execute-circuit bell-circuit (qs/zero-state 2)))
;; We can visualize the probability distribution of the Bell state.

(kind/html (viz/visualize-quantum-state :svg bell-state))

;; The *qc* namespace also has some predefined circuits for multi-qubit states.
;; These circuits can be used to create entangled states with more than two qubits.
;;
;; For example, the `qc/ghz-circuit` creates a circuit that prepares
;; a Greenberger-Horne-Zeilinger ([GHZ](https://en.wikipedia.org/wiki/Greenberger%E2%80%93Horne%E2%80%93Zeilinger_state)) state.

(def ghz-circuit
  (qc/ghz-state-circuit 3))

;; We can visualize the GHZ circuit.

(kind/html (viz/visualize-circuit :svg ghz-circuit))

;; The GHZ circuit shows that the Hadamard gate is applied to the first qubit, followed by CNOT gates between the first and second qubits, and between the second and third qubits.
;; The GHZ state is a multi-qubit state that is entangled.
;;
;; We can apply the GHZ circuit to the state |000⟩ to create the GHZ state.

(def ghz-state
  (qc/execute-circuit ghz-circuit (qs/zero-state 3)))

;; We can visualize the probability distribution of the GHZ state.

(kind/html (viz/visualize-quantum-state :svg ghz-state))

;; The probability distribution shows that the GHZ state is in a superposition of the states |000⟩, |111⟩, and |110⟩.
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
;;
;; ### Deutsch Algorithm
;; The Deutsch algorithm is a quantum algorithm that determines whether a function is constant or balanced.
;; It uses a quantum circuit to evaluate the function with only one query, compared to two queries needed for classical algorithms.
;; The quantum circuit uses an oracle to implement the function and applies a Hadamard gate to the input qubit.
;;
;; To examine the Deutsch algorithm, we need to require the `deutsch` namespace from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.deutsch :as deutsch])

;; Lets define a constant function and a balanced function first.

(def constant-fn (fn [x] true))  ; Constant function: f(x) = 1
(def balanced-fn (fn [x] x))      ; Balanced function: f(x) = x

;; Now we can create the circuit for the Deutsch algorithm for the constant function.

(def constant-deutsch-circuit
  (deutsch/deutsch-circuit constant-fn))

;; We can visualize the circuit for the constant oracle.

(kind/html (viz/visualize-circuit :svg constant-deutsch-circuit))

;; The circuit shows that the Hadamard gate is applied to the input qubit, followed
;; by the oracle function Uf. The oracle function Uf is implemented as a series
;; of quantum gates that applies the constant function.
;; Now we can execute the Deutsch algorithm with the constant function.
;; We use the simulator backend to execute the circuit.

(def deutsch-constant-result
  (deutsch/deutsch-algorithm (sim/create-simulator) constant-fn))

;; The result of the Deutsch algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.
deutsch-constant-result

;; The result shows that the Deutsch algorithm correctly identifies the constant function.
;; The measurement outcome is 0, which indicates that the function is constant.
(:result deutsch-constant-result)

;; Lets visualize the final quantum state after executing the Deutsch algorithm
;; with the constant function. It is contained in the execution result of the algorithm.

(kind/html (viz/visualize-quantum-state :svg (get-in deutsch-constant-result [:execution-result :final-state])))

;; For the balanced function, we can create the circuit for the Deutsch algorithm.

(def balanced-deutsch-circuit
  (deutsch/deutsch-circuit balanced-fn))

;; We can visualize the circuit for the balanced oracle.

(kind/html (viz/visualize-circuit :svg balanced-deutsch-circuit))

;; Execute the Deutsch algorithm with the balanced function.
(def deutsch-balanced-result
  (deutsch/deutsch-algorithm (sim/create-simulator) balanced-fn))

;; The result of the Deutsch algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.
deutsch-balanced-result

;; The result shows that the Deutsch algorithm correctly identifies the balanced function.
(:result deutsch-balanced-result)

;; Lets visualize the final quantum state after executing the Deutsch algorithm
;; with the balanced function.
(kind/html (viz/visualize-quantum-state :svg (get-in deutsch-balanced-result [:execution-result :final-state])))

;; ### Bernstein-Vazirani Algorithm
;; The Bernstein-Vazirani algorithm is a powerful quantum algorithm that can be
;; used to solve problems that are difficult for classical computers.
;; It is a quantum algorithm that determines a hidden binary string using a
;; quantum circuit to evaluate the function with only one query, compared to
;; n queries needed for classical algorithms.
;;
;; The quantum circuit uses an oracle to implement the function and applies a Hadamard gate to the input qubit.
;; To examine the Bernstein-Vazirani algorithm, we need to require the `bernstein-vazirani` namespace from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv])

;; Let's define a hidden binary string first.

(def hidden-string [1 1 0 1])  ; Hidden binary string: 1101

;; Now we can create the circuit for the Bernstein-Vazirani algorithm.

(def bv-circuit
  (bv/bernstein-vazirani-circuit hidden-string))

;; We can visualize the circuit for the Bernstein-Vazirani algorithm.

(kind/html (viz/visualize-circuit :svg bv-circuit))

;; The circuit shows that the Hadamard gate is applied to the input qubits, followed
;; by the oracle function Uf. The oracle function Uf is implemented as a series
;; of quantum gates that applies the hidden binary string.
;; Now we can execute the Bernstein-Vazirani algorithm with the hidden binary string.

(def bv-result
  (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string))

;; The result of the Bernstein-Vazirani algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.

bv-result
;; The result shows that the Bernstein-Vazirani algorithm correctly identifies the hidden binary string.

(:result bv-result)

;; The measurement outcome is the hidden binary string, which is 1101.
;; Lets visualize the final quantum state after executing the Bernstein-Vazirani algorithm.

(kind/html (viz/visualize-quantum-state :svg (get-in bv-result [:execution-result :final-state])))

;; The final quantum state shows that the Bernstein-Vazirani algorithm correctly identifies the hidden binary string.
;; The final quantum state is a superposition of the states that represent the hidden binary string.
