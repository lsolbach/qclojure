
;; # QClojure Quantum Computing Tutorial
;; This is a simple tutorial to demonstrate the use of the
;; [QClojure](https://github.com/soulspace/qclojure) library.
;; It covers the creation of quantum states, gates, and circuits and the
;; excution of quantum and hybrid algorithms on a QClojure backend.
;;
;; ## Introduction to Quantum Computing
;; Quantum computing is a fascinating field that combines computer science,
;; physics, and mathematics. It allows us to perform computations that are
;; not possible with classical computers. Quantum computers use quantum bits,
;; or [qubits](https://en.wikipedia.org/wiki/Qubit), which can be in a
;; superposition of states. This means that a qubit can be in a state of 0, 1,
;; or both at the same time. Quantum computing is based on the principles of
;; quantum mechanics, which describe the behavior of particles at the quantum
;; level.
;;
;; Quantum computing has the potential to revolutionize many fields, including
;; cryptography, optimization, and machine learning. It can solve certain
;; problems much faster than classical computers, by performing many
;; calculations at once.
;; Quantum Algorithms, such as Shor's algorithm for factoring large numbers
;; and Grover's algorithm for searching unsorted databases, demonstrate the
;; power of quantum computing.
;;
;; Quantum algorithms are defined in terms of quantum gates, which are
;; operations that can be applied to qubits. Quantum gates manipulate the state
;; of qubits and can be combined to create quantum circuits.
;; Quantum circuits are sequences of quantum gates applied to qubits, similar
;; to classical logic circuits.
;;
;; For a general introduction to quantum computing, take a look at
;; * [Quantum Computing](https://en.wikipedia.org/wiki/Quantum_computing).
;; * [But what is quantum computing? (Grover's Algorithm) - 3blue1brown](https://www.youtube.com/watch?v=RQWpF2Gb-gU) 
;;
;; ## QClojure
;; The QClojure library provides a Clojure interface to quantum computing
;; concepts. It allows us to create and manipulate quantum states, gates, and
;; circuits in a functional programming style.
;; QClojure can also be used to simulate quantum circuits and, by implementing
;; backends, run them on quantum hardware.
;; This tutorial will guide you through the basics of quantum computing using
;; QClojure.
;;
;; ### Prerequisites
;; As QClojure is running on Clojure and Clojure itself on the JVM, you need to
;; have the following prerequisites installed on your system:
;; * [JDK 11 or higher](https://openjdk.org/install/)
;; * [Clojure](https://clojure.org/)
;; * [Leiningen](https://leiningen.org/) or [Clojure CLI](https://clojure.org/guides/getting_started) to manage dependencies and run Clojure code.
;;
;; If you are new to Clojure, I recommend reading the
;; [Clojure Getting Started Guide](https://clojure.org/guides/getting_started).
;;
;; ### Usage
;; To use QClojure, you have to include it as a dependency in your Clojure
;; project.
;;
;; If you are using Leiningen, add the following dependency to your
;; `project.clj` file:
;;
;; ```clojure
;; [org.soulspace/qclojure "0.7.0"]
;; ```
;;
;; If you are using Clojure CLI, add the following to your `deps.edn` file:
;;
;; ```clojure
;; {:deps {org.soulspace/qclojure {:mvn/version "0.7.0"}}}
;; ```
;;
;; ### Imports
;; We use kindly to visualize the output of our code.
;; Then we import the relevant namespaces for the domain concepts of the
;; QClojure library.
;; The `state` namespace provides functions to create and manipulate quantum
;; states.
;; The `gate` namespace provides functions to create quantum gates.
;; The `circuit` namespace provides functions to create and manipulate quantum
;; circuits.
;;
;; We also import the visualization namespace and the svg renderer.

(ns tutorial
  (:require
   [fastmath.core :as fm]
   [scicloj.kindly.v4.api :as kindly]
   [scicloj.kindly.v4.kind :as kind]
   [org.soulspace.qclojure.domain.state :as qs]
   [org.soulspace.qclojure.domain.gate :as qg]
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.adapter.visualization.svg :as svg]
   [org.soulspace.qclojure.adapter.visualization.html :as html]
   [org.soulspace.qclojure.adapter.visualization :as viz]
   [org.soulspace.qclojure.application.algorithm.shor :as shor]
   [org.soulspace.qclojure.application.backend :as qb]))

;; ## Quantum States
;; A quantum state is a mathematical object that describes the state of a
;; quantum system.
;; In QClojure, quantum states are represented as vectors of complex numbers.
;; The vector of complex numbers represents the amplitudes of the basis states,
;; which represent the possible states of the system.
;; The notation |⟩ is called a "[braket](https://en.wikipedia.org/wiki/Dirac_notation)"
;; and is used to represent a vector in a complex vector space.
;; The Qubit is the basic unit of quantum information, and it can be in a
;; [superposition](https://en.wikipedia.org/wiki/Superposition) of the states
;; |0⟩ and |1⟩.
;; A classic bit can be in one of two states, 0 or 1, but a qubit can be in
;; a superposition of both states.
;; This means that a qubit can represent 0, 1, or both at the same time, with
;; different probabilities.
;;
;; ### Measurement
;; Measurement is the process of extracting classical information from a quantum
;; state. The measurement process is probabilistic, and the probability of
;; measuring a certain state depends on the amplitudes of the basis states in
;; the quantum state.
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
;; We have a probability of measuring the state |0⟩ as 0, and a probability of
;; measuring it as 1.
;; We can visualize the probability distribution of the quantum state |0⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|0⟩))

;; It shows that the probability of measuring the state |0⟩ results in 0 is 1,
;; which is certain.
;;
;; The [Bloch sphere](https://en.wikipedia.org/wiki/Bloch_sphere) is a
;; geometrical representation of quantum states.
;; We can visualize the quantum state |0⟩ as a vector on the Bloch sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|0⟩))

;; The Bloch sphere representation shows that the state |0⟩ is at the north pole
;; of the sphere.
;;
;; Let's look at another quantum state, the excited state |1⟩.

qs/|1⟩

;; We can visualize the probability distribution of the quantum state |1⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|1⟩))

;; It shows that the probability of measuring the state |1⟩ results in 1 is 1,
;; which is also certain.
;; The Bloch sphere representation shows that the state |1⟩ is at the south pole
;; of the sphere.

(kind/html (viz/visualize-bloch-sphere :svg qs/|1⟩))

;; ### Superposition States
;; Quantum states can also be in a superposition of the ground and excited
;; states.
;; Superposition states are linear combinations of the basic quantum states.
;;
;; Let's look at the quantum state |+⟩, which is a superposition of the ground
;; and excited states.
;; The state |+⟩ is defined as (|0⟩ + |1⟩) / √2.

qs/|+⟩

;; We can visualize the probability distribution of the quantum state |+⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|+⟩))

;; The Bloch sphere representation shows that the state |+⟩ is on the
;; equator of the sphere, which means, that the probabilities for
;; measuring 0 or 1 are the same.

(kind/html (viz/visualize-bloch-sphere :svg qs/|+⟩))

;; The quantum state |-⟩ is another superposition of the ground and
;; excited states. The state |-⟩ is defined as (|0⟩ - |1⟩) / √2.

qs/|-⟩

;; We can visualize the probability distribution of the quantum state |-⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|-⟩))

;; The Bloch sphere representation shows that the state |-⟩ is also on the
;; equator of the sphere, but pointing in the opposite direction.

(kind/html (viz/visualize-bloch-sphere :svg qs/|-⟩))

;; ### Multi-Qubit States and Quantum Registers
;; Tensor products can be used to create multi-qubit states from single-qubit
;; states. For example, the state |00⟩ is the tensor product of two |0⟩ states.

qs/|00⟩

;; We can visualize the probability distribution of the quantum state |00⟩.

(kind/html (viz/visualize-quantum-state :svg qs/|00⟩))

;; ## Quantum Gates
;; Quantum gates are operations that can be applied to quantum states.
;; They are represented as matrices that act on the quantum states.
;; The *qg* namespace defines several quantum gates.
;;
;; ### Pauli Gates
;; The [Pauli gates](https://en.wikipedia.org/wiki/Pauli_matrices) are a set of
;; quantum gates that can be applied to single qubits.
;; 
;; The Pauli-X gate is a quantum gate that flips the state of a qubit around
;; the X axis which swaps the amplitudes of |0⟩ and |1⟩.

qg/pauli-x

;; The Pauli-Y gate is a quantum gate that flips the state of a qubit around
;; the Y axis which swaps the amplitudes of |0⟩ and |1⟩ and also adds a phase.

qg/pauli-y

;; The Pauli-Z gate is a quantum gate that flips the state of a qubit around
;; the Y axis which adds a phase to the state of a qubit.

qg/pauli-z

;; The Pauli gates are self inverse, applying the same gate twice results
;; in the original value.
;;
;; ### Hadamard Gate
;; The [Hadamard gate](https://en.wikipedia.org/wiki/Hadamard_gate) is a
;; quantum gate that creates superposition states.
;; It transforms the state |0⟩ into the state |+⟩ and |1⟩ into the state |-⟩.
;; The Hadamard gate is defined as the matrix:

qg/hadamard

;; We can apply the Hadamard gate to the state |0⟩ to create the superposition 
;; state |+⟩.

(def hadamard-state
  (qg/h-gate qs/|0⟩))

;; We can visualize the probability distribution of the Hadamard state.

(kind/html (viz/visualize-quantum-state :svg hadamard-state))

;; The probability distribution shows that the Hadamard state is in a
;; superposition of the ground and excited states.

(kind/html (viz/visualize-bloch-sphere :svg hadamard-state))

;; The Bloch sphere representation shows that the Hadamard state is on the
;; equator of the sphere.
;;
;; The Hadamard gate is also self inverse, resulting in the input state again
;; if applied twice.
;;
;; ### Phase Gates
;; Phase gates are quantum gates that add a phase to the state of a qubit.
;;
;; The [S gate](https://en.wikipedia.org/wiki/S_gate) is a phase gate that adds
;; a phase of π/2 to the state of a qubit.

qg/s-gate

;; The [S† gate](https://en.wikipedia.org/wiki/S_gate#S%E2%81%BF_gate) is the
;; inverse of the S gate and adds a phase of -π/2 to the state of a qubit.

qg/s-dag-gate

;; The [T gate](https://en.wikipedia.org/wiki/T_gate) is a phase gate that adds
;; a phase of π/4 to the state of a qubit.

qg/t-gate

;; The [T† gate](https://en.wikipedia.org/wiki/T_gate#T%E2%81%BF_gate) is the
;; inverse of the T gate and adds a phase of -π/4 to the state of a qubit.

qg/t-dag-gate

;; ### Rotation Gates
;; Rotation gates are quantum gates that rotate the state of a qubit around
;; the Bloch sphere.
;;
;; The [RX gate](https://en.wikipedia.org/wiki/Rotation_gate#RX_gate) is a
;; rotation gate that rotates the state of a qubit around the X axis of the
;; Bloch sphere.

(qg/rx-gate fm/-QUARTER_PI)

;; The [RY gate](https://en.wikipedia.org/wiki/Rotation_gate#RY_gate) is a
;; rotation gate that rotates the state of a qubit around the Y axis of the
;; Bloch sphere.

(qg/ry-gate fm/-QUARTER_PI)

;; The [RZ gate](https://en.wikipedia.org/wiki/Rotation_gate#RZ_gate) is a
;; rotation gate that rotates the state of a qubit around the Z axis of the
;; Bloch sphere.

(qg/rz-gate fm/-QUARTER_PI)

;; ### Controlled Gates
;; Controlled gates are quantum gates that act on multiple qubits.
;; They are defined as a combination of a control qubit and a target qubit.
;; The control qubit determines whether the target qubit is affected by the gate.
;;
;; The controlled-X gate ([CNOT gate](https://en.wikipedia.org/wiki/CNOT_gate))
;; is a controlled gate that flips the state of the target qubit  if the control
;; qubit is in the state |1⟩.

(qg/cnot-gate)

;; The controlled-Y gate is a controlled gate that flips the state of the target
;; qubit and adds a phase if the control qubit is in the state |1⟩.
;;
;; The controlled-Z gate is a controlled gate that adds a phase to the target
;; qubit if the control qubit is in the state |1⟩.
;;
;; ## Quantum Circuits
;; Quantum circuits are sequences of quantum gates applied to quantum states.
;; The *qc* namespace provides functions to create and manipulate quantum
;; circuits.
;;
;; ### Creating a Quantum Circuit
;; We can create a simple quantum circuit that applies the Hadamard gate to the
;; state |0⟩.

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
;; in a superposition of the ground and excited states. It is the same as the
;; Hadamard state we created earlier, but now created by a quantum circuit, not
;; just the application of a single gate on a quantum state.

(kind/html (viz/visualize-bloch-sphere :svg hadamard-circuit-state))

;; The *qc* namespace also has some predefined circuits.
;;
;; For example, the 'qc/bell-state-circuit' creates a circuit that prepares 
;; Bell state, which is a two-qubit entangled state.

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

;; The *qc* namespace also has a predefined circuit for multi-qubit states.
;; This circuit can be used to create entangled states with more than two
;; qubits.
;;
;; For example, the `qc/ghz-circuit` creates a circuit that prepares
;; a Greenberger-Horne-Zeilinger ([GHZ](https://en.wikipedia.org/wiki/Greenberger%E2%80%93Horne%E2%80%93Zeilinger_state))
;; state.

(def ghz-circuit
  (qc/ghz-state-circuit 3))

;; We can visualize the GHZ circuit.

(kind/html (viz/visualize-circuit :svg ghz-circuit))

;; The GHZ circuit shows that the Hadamard gate is applied to the first qubit,
;; followed by CNOT gates between the first and second qubits, and between the
;; first and third qubits. The GHZ state is a multi-qubit state that is entangled.
;;
;; We can apply the GHZ circuit to the state |000⟩ to create the GHZ state.

(def ghz-state
  (qc/execute-circuit ghz-circuit (qs/zero-state 3)))

;; We can visualize the probability distribution of the GHZ state.

(kind/html (viz/visualize-quantum-state :svg ghz-state))

;; The probability distribution shows that the GHZ state is in a superposition
;; of the states |000⟩ and |111⟩.
;;
;; ## Backends
;; QClojure can be extended with backends to run quantum circuits on quantum hardware.
;; The *application.backend* namespace contains the protocols to be implemented by a
;; specific backend. A backend can be used to execute a quantum circuit.

(require '[org.soulspace.qclojure.application.backend :as qb])

;; QClojure comes with two simulator backends in the *adapter.backend* that can be used to
;; simulate quantum circuits on a classical computer.
;; * The ideal simulator backend simulates an ideal quantum computer without
;;   phyiscal constraints like noise.
;; * The noisy simulator backend simulates a real quantum computer with various kinds of noise.
;;
;; ### Ideal Simulator Backend
;; Let's try the ideal simulator first by requiring the `simulator` namespace.

(require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])

;; We create the simulator backend with the `create-simulator` function.

(def simulator (sim/create-simulator))

;; Now we can use the simulator to execute the ghz circuit on the simulator.

(qb/execute-circuit simulator (qc/ghz-state-circuit 3))

;; When executing a circuit on a backend, it will be executed multiple times,
;; because of the probabilistic nature of quantum computing. One execution of the
;; circuit is called a *shot*. The default number of shots is 512, but it can be
;; configured via an options map.

(qb/execute-circuit simulator (qc/ghz-state-circuit 3) {:shots 10})

;; ### Noisy Simulator Backend
;; Real quantum hardware is subject to various kinds of noise, which can
;; affect the results of quantum computations. The noisy simulator backend
;; simulates a quantum computer with noise, allowing us to study the effects of
;; noise on quantum circuits.
;;
;; The noisy simulator backend simulates these kinds of noise:
;; * depolarizing noise is a type of noise that randomly flips the state of a
;;   qubit with a certain probability.
;; * amplitude damping noise is a type of noise that causes the state of a
;;   qubit to decay over time with a certain probability.
;; * bit flip noise is a type of noise that flips the state of a qubit from
;;   |0⟩ to |1⟩ or from |1⟩ to |0⟩ with a certain probability.
;; * phase flip noise is a type of noise that flips the phase of the state of
;;   a qubit from |0⟩ to |1⟩ or from |1⟩ to |0⟩ with a certain probability.
;; * readout noise is a type of noise that affects the measurement of the
;;   state of a qubit, causing the measured value to be incorrect with a
;;   certain probability.
;;
;; The noisy simulator backend applies the noise to the quantum states
;; and gates in the circuit, simulating the effects of noise on the quantum
;; computation.
;;
;; Let's try the noisy simulator first by requiring the `noisy-similator` namespace.

(require '[org.soulspace.qclojure.adapter.backend.noisy-simulator :as noisy])

;; We can instanciate the noisy simulator with the `create-noisy-simulator` function
;; and provide a noise profile. The noise profile we use is derived from the
;; IBM Lagos Quantum Computer.

(noisy/noise-model-for :ibm-lagos)

;; The noise profile shows configurations for the different types of noise,
;; a physical quantum computer can have. All different types of noise contribute
;; to the errors in the measurement.
;;
;; Let's instanciate the noisy simulator with the IBM Lagos profile.

(def lagos-simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos)))

;; Now we can use the simulator to execute the ghz circuit on the simulator.
;; Because we use a noisy simulator, we may measure wrong answers.

(def lagos-50-result (qb/execute-circuit lagos-simulator (qc/ghz-state-circuit 3) {:shots 50}))

lagos-50-result

(kind/html (viz/visualize-quantum-state :svg (:final-state lagos-50-result)))

;; We see, that not all measurements measure the states |000⟩ and |111⟩,
;; even though those states should have the highest counts. The other states
;; should have a distinctivly lower count. But if you use to few shots, you
;; could be unlucky and measure the wrong answers. The probability to measure
;; the wrong answers gets lower by increasing the number of shots.

(def lagos-10k-result (qb/execute-circuit lagos-simulator (qc/ghz-state-circuit 3) {:shots 10000}))

lagos-10k-result

;; With 10000 shots, the difference of the counts of the correct answers
;; and the counts of the wrong anwsers should be quite significant.

(kind/html (viz/visualize-measurement-histogram :svg (:measurement-results lagos-10k-result)))

;; We can also use the noisy simulator with a different noise profile,
;; e.g. for an IonQ Forte quantum computer.

(noisy/noise-model-for :ionq-forte)

;; Let's instanciate the noisy simulator with the IonQ Forte profile.

(def forte-simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ionq-forte)))

;; We now execute the GHZ circuit on this simulator with 10000 shots and
;; compare the results with the IBM Lagos simulation.

(def forte-10k-result (qb/execute-circuit forte-simulator (qc/ghz-state-circuit 3) {:shots 10000}))

forte-10k-result

;; Compared to the IBM Lagos simulation, the IonQ Forte simulation should have
;; distinctly lower noise and thus a higher count for the correct answers.

(kind/html (viz/visualize-measurement-histogram :svg (:measurement-results forte-10k-result)))

;; ## Error Mitigation
;; Error mitigation is a collection of techniques used to reduce the effects
;; of noise in quantum computations. It is not a full error correction, but it
;; can improve the results of quantum computations by reducing the errors
;; caused by noise.
;; Error mitigation techniques can be applied to quantum circuits to improve
;; the results of quantum computations.
;; QClojure provides a set of error mitigation techniques that can be used to
;; reduce the effects of noise in quantum computations.
;;
;; ### Readout Error Mitigation
;; Readout error mitigation is a technique used to reduce the effects of readout
;; noise in quantum computations. It is based on the idea that the readout noise
;; can be modeled as a matrix that describes the probability of measuring a
;; certain state given the true state of the qubit.
;;
;; ### Zero Noise Extrapolation
;; Zero noise extrapolation is a technique used to reduce the effects of noise
;; in quantum computations by extrapolating the results of the computation to
;; zero noise. It is based on the idea that the results of the computation can
;; be extrapolated to zero noise by measuring the results of the computation
;; with different noise levels and fitting a curve to the results.
;; 
;; ### Symmetry Verification
;; Symmetry verification is a technique used to reduce the effects of noise in
;; quantum computations by verifying the symmetry of the quantum circuit.
;; It is based on the idea that the results of the computation can be verified
;; by checking the symmetry of the quantum circuit. If the circuit is symmetric,
;; the results of the computation should be the same for all qubits.
;;
;; ### Virtual Distillation
;; Virtual distillation is a technique that improves computation fidelity
;; by running multiple copies of quantum circuits and applying sophisticated
;; post-processing to extract high-fidelity results through probabilistic error
;; cancellation.
;;
;; ## Algorithms
;; QClojure comes with a set of predefined quantum algorithms that can be used
;; to solve specific problems.
;; These algorithms are implemented as quantum circuits and can be executed on
;; quantum hardware or simulated using the simulator backends.
;;
;; ### Deutsch Algorithm
;; The [Deutsch algorithm](https://en.wikipedia.org/wiki/Deutsch_algorithm) is
;; a simple quantum algorithm that determines whether a function is constant or
;; balanced. It uses a quantum circuit to evaluate the function with only one
;; query, compared to two queries needed for classical algorithms.
;; The quantum circuit uses an oracle to implement the function and applies a
;; Hadamard gate to the input qubit.
;;
;; #### Problem Statement
;; Given a function f: {0, 1} → {0, 1}, the goal is to determine if f is constant
;; (returns the same value for both inputs) or balanced (returns 0 for one input
;; and 1 for the other).
;;
;; #### Classical Approach
;; In a classical setting, we would need to evaluate the function f twice:
;; - If f(0) = f(1), then f is constant.
;; - If f(0) != f(1), then f is balanced.
;;
;; #### Quantum Approach
;; The Deutsch algorithm allows us to determine the nature of the function with
;; only one evaluation by leveraging quantum superposition and interference.
;;
;; #### Quantum Circuit
;; The Deutsch algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize a qubit in the state |0⟩ and an auxiliary qubit in the state |1⟩.
;; 2. Apply a Hadamard gate to both qubits to create superposition.
;; 3. Apply the function f as a quantum gate, which will entangle the qubits.
;; 4. Apply another Hadamard gate to the first qubit.
;; 5. Measure the first qubit.
;;
;; To examine the Deutsch algorithm, we need to require the `deutsch` namespace
;; from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.deutsch :as deutsch])

;; Lets define a constant function and a balanced function first.

(def constant-fn (fn [_x] true))  ; Constant function: f(x) = 1
(def balanced-fn (fn [x] x))     ; Balanced function: f(x) = x

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
;; The [Bernstein-Vazirani algorithm](https://en.wikipedia.org/wiki/Bernstein%E2%80%93Vazirani_algorithm)
;; is a powerful quantum algorithm that can be
;; used to solve problems that are difficult for classical computers.
;; It is a quantum algorithm that determines a hidden binary string using a
;; quantum circuit to evaluate the function with only one query, compared to
;; n queries needed for classical algorithms.
;;
;; The quantum circuit uses an oracle to implement the function and applies
;; a Hadamard gate to the input qubit.
;;
;; #### Problem Statement
;; Given a function f: {0, 1}ⁿ → {0, 1} defined as f(x) = s ⨯ x
;; (where s is a hidden string and ⨯ denotes the dot product),
;; the goal is to find the hidden string s using as few evaluations of f as possible.
;;
;; #### Classical Approach
;; In a classical setting, we would need to evaluate the function f multiple times
;; to determine the hidden string s. The number of evaluations required can grow
;; linearly with the size of the input.
;;
;; #### Quantum Approach
;; The Bernstein-Vazirani algorithm allows us to find the hidden string s with
;; only one evaluation by leveraging quantum superposition and interference.
;;
;; #### Quantum Circuit
;; The Bernstein-Vazirani algorithm can be implemented using a quantum circuit
;; with the following steps:
;; 1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |1⟩.
;; 2. Apply a Hadamard gate to all n qubits to create superposition.
;; 3. Apply the function f as a quantum gate, which will entangle the qubits.
;; 4. Apply another Hadamard gate to all n qubits.
;; 5. Measure the qubits to obtain the hidden string s.
;;
;; To examine the Bernstein-Vazirani algorithm, we need to require the
;; `bernstein-vazirani` namespace from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv])

;; Let's define a hidden binary string first.

(def hidden-string [1 1 0])  ; Hidden binary string: 110

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

;; The result shows that the Bernstein-Vazirani algorithm correctly identifies
;; the hidden binary string.

(:result bv-result)

;; The measurement outcome is the hidden binary string, which is 110.
;; Lets visualize the final quantum state after executing the Bernstein-Vazirani algorithm.

(kind/html (viz/visualize-quantum-state :svg (get-in bv-result [:execution-result :final-state])))

;; The final quantum state shows that the Bernstein-Vazirani algorithm correctly
;; identifies the hidden binary string. The final quantum state is a superposition
;; of the states that represent the hidden binary string.
;;
;; ### Simon's Algorithm
;; [Simon's algorithm](https://en.wikipedia.org/wiki/Simon's_algorithm) solves
;; the hidden subgroup problem for the group (Z₂)ⁿ.
;; Given a function f: {0,1}ⁿ → {0,1}ⁿ that is either one-to-one or two-to-one,
;; and if two-to-one then f(x) = f(x ⊕ s) for some hidden string s ≠ 0ⁿ,
;; the algorithm finds s with exponential speedup over classical methods.
;;
;; The quantum circuit uses an oracle to implement the function and applies a
;; Hadamard gate to the input qubits.
;;
;; #### Problem Statement
;; Given a function f: {0,1}ⁿ → {0,1}ⁿ that is promised to be periodic with a hidden period s,
;; the goal is to find s using fewer evaluations of f than would be possible classically.
;;
;; #### Classical Approach
;; In a classical setting, we would need to evaluate the function f multiple times
;; to find the period s. The number of evaluations required can grow exponentially
;; with the size of the input.
;;
;; #### Quantum Approach
;; The Simon algorithm allows us to find the hidden period s with a polynomial number
;; of evaluations by leveraging quantum superposition and interference.
;;
;; #### Quantum Circuit
;; The Simon algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize n qubits in the state |0⟩ and n auxiliary qubits in the state |1⟩.
;; 2. Apply a Hadamard gate to all n qubits to create superposition.
;; 3. Apply the function f as a quantum gate, which will entangle the qubits.
;; 4. Measure the auxiliary qubits to obtain a set of equations that can be solved
;;    classically to find the hidden period s.
;;
;; To examine Simon's algorithm, we need to require the `simon` namespace
;; from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.simon :as simon])

;; Let's define a hidden binary string first.

(def hidden-string-simon [1 0 1])  ; Hidden binary string: 101

;; Now we can create the circuit for Simon's algorithm.

(def simon-circuit
  (simon/simon-circuit hidden-string-simon))

;; We can visualize the circuit for Simon's algorithm.

(kind/html (viz/visualize-circuit :svg simon-circuit))

;; The circuit shows that the Hadamard gate is applied to the input qubits, followed
;; by the oracle function Uf. The oracle function Uf is implemented as a series
;; of quantum gates that applies the hidden binary string.
;;
;; Now we can execute Simon's algorithm with the hidden binary string.

(def simon-result
  (simon/simon-algorithm (sim/create-simulator) hidden-string-simon))

;; The result of Simon's algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.

simon-result

;; The result shows that Simon's algorithm correctly identifies the hidden binary string.

(:result simon-result)

;; Lets visualize the final quantum states after executing Simon's algorithm.
;; As Simon's algorithm can return multiple results, depending on the size of the hidden
;; string, we visualize the final states.

(mapv #(kind/html (viz/visualize-quantum-state :svg (:final-state %))) (:execution-results simon-result))

;; ### Grover's Search Algorithm
;;
;; [Grover's algorithm](https://en.wikipedia.org/wiki/Grover%27s_algorithm)
;; is a quantum algorithm that provides a quadratic speedup for searching
;; an unsorted database. It is one of the most well-known quantum algorithms
;; and demonstrates the power of quantum computing for search problems.
;;
;; #### Problem Statement
;; Given a function f: {0, 1}ⁿ → {0, 1}ⁿ that is promised to have exactly one
;; input x such that f(x) = 1 (the "marked" item), the goal is to
;; find this input x using as few evaluations of f as possible.
;;
;; #### Classical Approach
;; In a classical setting, we would need to evaluate the function f up to
;; 2^(n-1) times in the worst case to find the marked item. This is because we
;; would have to check each possible input until we find the one that satisfies
;; f(x) = 1.
;;
;; #### Quantum Approach
;; Grover's search algorithm allows us to find the marked item with only
;; O(√2ⁿ) evaluations of f, which is a significant improvement over the
;; classical approach.
;;
;; #### Quantum Circuit
;; The Grover's search algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize n qubits in the state |0⟩.
;; 2. Apply a Hadamard gate to all n qubits to create superposition, resulting in an equal superposition of all possible inputs.
;; 3. Apply the Grover diffusion operator, which consists of:
;;    - Applying the oracle function f as a quantum gate, which flips the sign of the amplitude of the marked item.
;;    - Applying a Hadamard gate to all qubits.
;;    - Applying a conditional phase shift to the |0⟩ state.
;;    - Applying another Hadamard gate to all qubits.
;; 4. Repeat the Grover diffusion operator O(√2ⁿ) times.
;;
;; To examine Grover's algorithm, we need to require the `grover` namespace
;; from the `application.algorithm` package.

(require '[org.soulspace.qclojure.application.algorithm.grover :as grover])

;; Let's define a function that marks a specific input.

(def grover-oracle (grover/single-target-oracle 5))

;; Now we can define a circuit with a search space of 3 qubits for Grover's
;; search algorithm.

(def grover-circuit
  (grover/grover-circuit 3 grover-oracle))

;; Let's visualize the circuit for Grover's search algorithm.

(kind/html (viz/visualize-circuit :svg grover-circuit))

;; Now we can execute Grover's search algorithm with the defined oracle.

(def grover-result
  (grover/grover-algorithm (sim/create-simulator) 8 grover-oracle))

;; Like the previous algorithms, the result of Grover's search algorithm
;; is a map that contains the result of the algorithm, the measurement outcome,
;; and the circuit used to execute the algorithm.

grover-result

;; The result shows that Grover's search algorithm correctly identifies the marked item.

(:result grover-result)

;; #### Quantum Fourier Transform
;; The [Quantum Fourier Transform (QFT)](https://en.wikipedia.org/wiki/Quantum_Fourier_transform)
;; is a quantum algorithm that performs the discrete Fourier transform on a quantum state.
;; It is a key component of many quantum algorithms, including Shor's algorithm.
;;
;; The QFT transforms a quantum state into its frequency domain representation,
;; allowing us to extract periodicity and other properties of the quantum state.
;;
;; #### Problem Statement
;; Given a quantum state |ψ⟩, the goal is to apply the QFT to the state
;; and obtain a new quantum state that represents the frequency domain of |ψ⟩.
;;
;; #### Classical Approach
;; In a classical setting, the discrete Fourier transform can be computed
;; using classical algorithms, but it requires O(N log N) time complexity,
;; where N is the number of elements in the input.
;;
;; #### Quantum Approach
;; The QFT allows us to compute the discrete Fourier transform in O(log² N)
;; time complexity, which is a significant improvement over the classical approach.
;;
;; #### Quantum Circuit
;; The QFT can be implemented using a quantum circuit with the following steps:
;; 1. Initialize n qubits in the state |ψ⟩.
;; 2. Apply a series of controlled phase gates to the qubits, which introduces
;;    phase shifts based on the relative positions of the qubits.
;; 3. Apply a series of Hadamard gates to the qubits, which creates
;;    superposition states that represent the frequency domain.
;; 4. Reverse the order of the qubits to obtain the final quantum state
;;    that represents the frequency domain of |ψ⟩.
;;
;; Let's require the `quantum-fourier-transform` namespace to explore the QFT.

(require '[org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft])

;; We can create a quantum circuit for the QFT with a specified number of qubits.

(def qft-circuit
  (qft/quantum-fourier-transform-circuit 3))

;; We can visualize the circuit for the Quantum Fourier Transform.

(kind/html (viz/visualize-circuit :svg qft-circuit))

;; The circuit shows that the QFT applies a series of controlled phase gates
;; and Hadamard gates to the qubits, transforming the quantum state into its
;; frequency domain representation.

(def qft-state
  (qb/execute-circuit (sim/create-simulator) qft-circuit))

qft-state

;; The circuit for the QFT can also be used to implement the inverse QFT,
;; which is the reverse operation of the QFT.

(def inverse-qft-circuit
  (qft/inverse-quantum-fourier-transform-circuit 3))

;; We can visualize the circuit for the inverse Quantum Fourier Transform.

(kind/html (viz/visualize-circuit :svg inverse-qft-circuit))

;; The inverse QFT circuit applies the inverse operations of the controlled phase gates
;; and Hadamard gates to the qubits, transforming the quantum state back to its
;; original representation.
;; The inverse QFT can be used to recover the original quantum state from its
;; frequency domain representation.
;;
;; ### Quantum Phase Estimation
;; The [Quantum Phase Estimation (QPE)](https://en.wikipedia.org/wiki/Quantum_phase_estimation_algorithm)
;; is a quantum algorithm that estimates the eigenvalues of a unitary operator.
;; It is used in many quantum algorithms, including Shor's algorithm and the Quantum Fourier Transform.
;;
;; #### Problem Statement
;; Given a unitary operator U and an eigenstate |ψ⟩ of U, the goal is to estimate
;; the phase θ such that U|ψ⟩ = e^(2πiθ)|ψ⟩, where θ is the eigenvalue of U.
;;
;; #### Classical Approach
;; In a classical setting, estimating the phase of a unitary operator requires
;; multiple evaluations of the operator and can be computationally expensive.
;;
;; #### Quantum Approach
;; The Quantum Phase Estimation algorithm allows us to estimate the phase θ
;; with high precision using a quantum circuit that requires only a polynomial
;; number of evaluations of the unitary operator U.
;;
;; #### Quantum Circuit
;; The Quantum Phase Estimation algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |ψ⟩.
;; 2. Apply a Hadamard gate to the auxiliary qubit to create superposition.
;; 3. Apply controlled-U gates to the auxiliary qubit, where U is the unitary operator.
;; 4. Apply the inverse Quantum Fourier Transform (QFT) to the auxiliary qubit.
;; 5. Measure the auxiliary qubit to obtain the estimated phase θ.

(require '[org.soulspace.qclojure.application.algorithm.quantum-phase-estimation :as qpe])

;;
;; ### Quantum Period Finding
;; The [Quantum Period Finding](https://en.wikipedia.org/wiki/Quantum_period_finding)
;; is a quantum algorithm that finds the period of a function.
;; It is used in many quantum algorithms, including Shor's algorithm and the Quantum Fourier Transform
;;
;; #### Problem Statement
;; Given a function f: {0, 1}ⁿ → {0, 1}ⁿ that is periodic with period r,
;; the goal is to find the period r using as few evaluations of f as possible.
;;
;; #### Classical Approach
;; In a classical setting, finding the period of a function requires
;; multiple evaluations of the function and can be computationally expensive.
;;
;; #### Quantum Approach
;; The Quantum Period Finding algorithm allows us to find the period r
;; with high precision using a quantum circuit that requires only a polynomial
;; number of evaluations of the function f.
;;
;; #### Quantum Circuit
;; The Quantum Period Finding algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |1⟩.
;; 2. Apply a Hadamard gate to all n qubits to create superposition.
;; 3. Apply the function f as a quantum gate, which will entangle the qubits.
;; 4. Apply the Quantum Fourier Transform (QFT) to the qubits.
;; 5. Measure the qubits to obtain a value that can be used to find the period r.
;;
;; To explore the Quantum Period Finding algorithm, we need to require the
;; `quantum-period-finding` namespace.

(require '[org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf])

;; ### Shor's Algorithm
;; [Shor's algorithm](https://en.wikipedia.org/wiki/Shor%27s_algorithm) is a quantum algorithm
;; that can factor large integers in polynomial time.
;; It is one of the most famous quantum algorithms and has significant implications for
;; cryptography, as it can break many classical encryption schemes.
;; Shor's algorithm uses the Quantum Fourier Transform and Quantum Phase Estimation to find the period
;; of a function related to the integer to be factored.
;;
;; #### Problem Statement
;; Given a composite integer N, the goal is to find its prime factors using
;; as few evaluations of a function as possible.
;;
;; #### Classical Approach
;; In a classical setting, factoring large integers is a computationally
;; hard problem. The best-known classical algorithms for factoring have
;; exponential time complexity, making them impractical for large N.
;;
;; #### Quantum Approach
;; Shor's algorithm allows us to factor large integers in polynomial time
;; by leveraging quantum superposition, interference, and the quantum
;; Fourier transform.
;;
;; #### Quantum Circuit
;; The Shor's algorithm can be implemented using a quantum circuit with
;; the following steps:
;; 1. Choose a random integer a such that 1 < a < N.
;; 2. Use the quantum period-finding algorithm to find the order r of a modulo N.
;; This involves:
;;    - Initialize n qubits in the state |0⟩.
;;    - Apply a Hadamard gate to all n qubits to create superposition.
;;    - Apply the modular exponentiation function a^x mod N as a quantum gate,
;;      which will entangle the qubits.
;;    - Apply the quantum Fourier transform to the qubits.
;;    - Measure the qubits to obtain a value that can be used to find the
;;      order r.
;; 3. If r is even and a^(r/2) != -1 mod N, then compute the factors:
;;    - Compute gcd(a^(r/2) - 1, N and gcd(a^(r/2) + 1, N).
;; 4. If the factors are non-trivial, return them as the prime factors of N.
;; 5. If the order r is odd or if the above conditions are not met, repeat
;;    the process with a different random integer a.
;;
;; Let's examine Shor's algorithm by requiring the `shor` namespace.

(require '[org.soulspace.qclojure.application.algorithm.shor :as shor])

;; We can use Shor's algorithm to factor a composite integer.

; (def shor-result (shor/shor-algorithm (sim/create-simulator) 15))

;; The result of Shor's algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.

; shor-result

;; The result shows that Shor's algorithm correctly factors the composite integer 15
;; into its prime factors 3 and 5.
;; The measurement outcome is the prime factors of 15, which are 3 and 5.

; (:result shor-result)

;; ### HHL Algorithm
;; The [HHL algorithm](https://en.wikipedia.org/wiki/HHL_algorithm) is a
;; quantum algorithm for solving linear systems of equations.
;; It is named after its inventors Harrow, Hassidim, and Lloyd.
;; The HHL algorithm can solve a system of linear equations in polynomial time,
;; which is a significant improvement over classical algorithms that require
;; exponential time for large systems.
;; The current implementation works for a hermitian n x n matrix A and a vector b.
;;
;; #### Problem Statement
;; Given a system of linear equations Ax = b, where A is a hermitian matrix,
;; x is the vector of unknowns, and b is the vector of constants, the goal is to
;; find the vector x that satisfies the equations using as few evaluations
;; of the matrix A as possible.
;;
;; #### Classical Approach
;; In a classical setting, solving a system of linear equations requires
;; O(N³) time complexity, where N is the number of equations in the system.
;; Classical algorithms such as Gaussian elimination or LU decomposition
;; can be used to solve the system, but they are computationally expensive
;; for large systems.
;;
;; #### Quantum Approach
;; The HHL algorithm allows us to solve a system of linear equations in
;; O(log N) time complexity by leveraging quantum superposition, interference,
;; and the quantum Fourier transform.
;;
;; #### Quantum Circuit
;; The HHL algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Prepare the input state |b⟩, which represents the vector of constants b.
;; 2. Use the quantum phase estimation algorithm to estimate the eigenvalues
;;    of the matrix A. This involves:
;;    - Initialize n qubits in the state |0⟩.
;;    - Apply a Hadamard gate to all n qubits to create superposition.
;;    - Apply controlled-U gates to the qubits, where U is the unitary operator
;;      that represents the matrix A.
;;    - Apply the inverse quantum Fourier transform to the qubits.
;;    - Measure the qubits to obtain the estimated eigenvalues of A.
;; 3. Use the estimated eigenvalues to compute the inverse of the matrix A.
;; 4. Apply the inverse of the matrix A to the input state |b⟩ to obtain the
;;    output state |x⟩, which represents the solution to the system of
;;    linear equations Ax = b.
;; 5. Measure the output state |x⟩ to obtain the vector of unknowns x that
;;    satisfies the equations.
;;
;; Let's examine the HHL algorithm by requiring the `hhl` namespace.

(require '[org.soulspace.qclojure.application.algorithm.hhl :as hhl])

;; We can use the HHL algorithm to solve a system of linear equations.
;; For example, let's solve the system of equations represented by the
;; hermitian matrix A and the vector b.

(def hhl-matrix [[1 2] [2 3]]) ; Hermitian matrix A
(def hhl-vector [5 6])          ; Vector b

;; Now we can create the circuit for the HHL algorithm with the given matrix
;; and vector.

(def hhl-circuit
  (hhl/hhl-circuit hhl-matrix hhl-vector 4 1))

;; We can visualize the circuit for the HHL algorithm.

(kind/html (viz/visualize-circuit :svg hhl-circuit))

;; The circuit shows that the HHL algorithm applies a series of controlled-U gates
;; to the qubits, which represent the matrix A, and applies the inverse quantum
;; Fourier transform to the qubits. The circuit also applies the inverse of the
;; matrix A to the input state |b⟩ to obtain the output state |x⟩, which represents
;; the solution to the system of linear equations Ax = b.

(def hhl-result
  (hhl/hhl-algorithm (sim/create-simulator) hhl-matrix hhl-vector))

;; The result of the HHL algorithm is a map that contains the result of the
;; algorithm, the measurement outcome, and the circuit used to execute the algorithm.

hhl-result

;; The result shows that the HHL algorithm correctly solves the system of
;; linear equations represented by the matrix A and the vector b.

;; The measurement outcome is the vector of unknowns x that satisfies the equations.

(:result hhl-result)

;; Lets visualize the final quantum state after executing the HHL algorithm.

(kind/html (viz/visualize-quantum-state :svg (get-in hhl-result [:execution-result :final-state])))

;; The final quantum state shows that the HHL algorithm correctly solves the
;; system of linear equations Ax = b. The final quantum state is a superposition
;; of the states that represent the solution to the system of equations.

;; ### Variational Quantum Eigensolver Algorithm
;; 