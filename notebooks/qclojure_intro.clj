^{:kindly/hide-code true}
(ns qclojure-intro
  (:require
   [scicloj.kindly.v4.kind :as kind]))

;; ## About me
;;
;; * Ludger Solbach
;; * Software Architect at 'msg for automotive'
;; * 30+ years experience in different industries
;; * Clojure enthusiast since 2011
;; * [My GitHub page](https://github.com/lsolbach)
;; 
;; ## What to expect
;;
;; * Introduction to QClojure
;; * Overview of features and capabilities
;; * Getting started with installation and documentation
;; * Examples of quantum states, gates, circuits and backends
;; * Examples of implemented quantum/hybrid algorithms
;; * Future plans and missing pieces in the Clojure ecosystem
;; 
;; ## What not to expect
;;
;; * In-depth explanations of quantum computing concepts
;; * Detailed tutorials on using QClojure
;; * Comprehensive coverage of all features and capabilities
;;
;; ## What is Quantum Computing
;;
;; * Paradigm of computation based on quantum mechanics
;; * Uses quantum bits (qubits), superposition and entangled states
;; * Performs some computations exponentially faster
;; * Promises breakthroughs in cryptography, optimization, simulation, and machine learning
;; * Requires new algorithms and hardware architectures
;; * Still a research field 
;;
;; ## What is QClojure
;;
;; * A Clojure library for quantum computing
;; * Provides tools for constructing and simulating quantum circuits
;; * Supports local simulators and cloud backends
;; * Offers visualization and analysis tools for quantum states and circuits
;; * Open source and actively developed
;;
;; ## QClojure Features
;;
;; * Pure functional quantum circuit construction
;; * Comprehensive gate library
;; * Library of quantum/hybrid algorithm implementations
;; * Extensible quantum backend system for simulators and hardware
;; * Ideal and hardware simulation
;; * Error mitigation and error correction
;; * Visualization and Analysis Tools
;;
;; # Getting Started
;;
;; * Installation
;;   * Add [QClojure](https://clojars.org/org.soulspace/qclojure) dependency to your `deps.edn` or `project.clj`
;; * Documentation
;;   * [API Documentation and Tutorial](https://https://cljdoc.org/d/org.soulspace/qclojure)
;; 
;; ## Required Namespaces
;;
;; * Domain Namespaces
(require '[org.soulspace.qclojure.domain.state :as state])
(require '[org.soulspace.qclojure.domain.gate :as gate])
(require '[org.soulspace.qclojure.domain.circuit :as circuit])
;;
;; * Visualisation Namespaces
(require '[org.soulspace.qclojure.application.visualization :as viz])
(require '[org.soulspace.qclojure.adapter.visualization.ascii :as ascii])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; # Quantum States
;;
;; * Represent the state of qubits in a quantum system
;; * Used as input and output of quantum circuits
;; * Can be in superposition and entangled states
;; * Can interfere and evolve through quantum gates
;; * Represented as state vectors in a complex Hilbert space
;; * QClojure provides a library of common quantum states
;;
;; ## Zero State
;;
;; * Fundamental basis state in quantum computing
;; * Represents the binary value 0 in a qubit
;; * Represented as a state vector
state/|0⟩

;; ## Probability Distribution of the Zero State
^kind/hiccup (viz/visualize-quantum-state :svg state/|0⟩)

;; ## Bloch Sphere visualization of the Zero State
^kind/hiccup (viz/visualize-bloch-sphere :svg state/|0⟩)

;; ## One State
;; * Fundamental basis state in quantum computing
;; * Represents the binary value 1 in a qubit
;; * Represented as a state vector
state/|1⟩

;; ## Probability Distribution of the One State
^kind/hiccup (viz/visualize-quantum-state :svg state/|1⟩)

;; ## Bloch Sphere visualization of the One State
^kind/hiccup (viz/visualize-bloch-sphere :svg state/|1⟩)

;; ## Plus State
;; * Superposition state
;; * Equal probability of measuring 0 or 1
;; * Represented as a state vector
state/|+⟩

;; ## Probability Distribution of the Plus State
^kind/hiccup (viz/visualize-quantum-state :svg state/|+⟩)

;; ## Bloch Sphere visualization of the Plus State
^kind/hiccup (viz/visualize-bloch-sphere :svg state/|+⟩)

;; ## Quantum Register
;; * Represents multiple qubits in a single state vector
;; * tensor product of individual qubit states
;; * Example: two-qubit register in the |00⟩ state
state/|00⟩

;; ## Probability Distribution of the Quantum Register
^kind/hiccup (viz/visualize-quantum-state :svg state/|00⟩)

;; # Quantum Gates
;; * Basic building blocks of quantum circuits
;; * Represented as unitary matrices
;; * Operate on qubit states to perform quantum computations
;; * QClojure provides a comprehensive library of 20+ quantum gates
;;
;; ## Hadamard Gate
;; * Creates superposition states
gate/hadamard
;; ## Pauli-X Gate
;; * flips the state of a qubit
gate/pauli-x
;; ## Pauli-Z Gate
;; * flips the phase of a qubit
gate/pauli-z
;; ## Pauli-Y Gate
;; * combines bit-flip and phase-flip operations
gate/pauli-y
;; ## CNOT Gate
;; * flips the target qubit if the control qubit is in state |1⟩
;; * entangles two qubits
#_gate/cnot-gate

;;
;; # Quantum Circuits
;; ## Creating a Quantum Circuit
(def test-circuit
  (-> (circuit/create-circuit 2 "Bell Circuit" "Creates a Bell state")
      (circuit/h-gate 0)
      (circuit/cnot-gate 0 1)))
;; ## Visualizing the Circuit in SVG
^kind/hiccup
(viz/visualize-circuit :svg test-circuit)

;; # Backends
;; * Hardware Abstraction Layer
;; * Multiple Backend Implementations
(require '[org.soulspace.qclojure.application.backend :as backend])

;; ## Result Extraction
;; * Specify what results to extract from backend execution
;; * E.g. measurement results for qubits 0, 1, and 2 with 64 shots
(def result-specs {:measurements {:qubits [0 1 2]
                                  :shots 64}})
;; ## Ideal Simulator Backend
;; * Noiseless simulation of quantum circuits
;; * Suitable for testing and debugging quantum algorithms
(require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as ideal])
(def ideal-simulator (ideal/create-simulator))

;; ## Executing Circuit on Ideal Simulator Backend
(def ideal-result (backend/execute-circuit
                   ideal-simulator
                   (circuit/ghz-state-circuit 3)
                   {:result-specs result-specs}))

#_ideal-result

;; ## Visualizing the Measurement Frequency Histogram
^kind/hiccup
(viz/visualize-measurement-histogram
 :hiccup
 (get-in ideal-result [:results :measurement-results :frequencies]))

;; ## Hardware Simulator Backend
;; * Realistic simulation of quantum devices
;;   * Native gate sets
;;   * Qubit topologies
;;   * Noise models
(require '[org.soulspace.qclojure.adapter.backend.hardware-simulator :as hwsim])
(def hw-simulator (hwsim/create-hardware-simulator))

;; ## Selecting a Device
;; * IBM Lagos device
(def device (backend/select-device hw-simulator (:ibm-lagos hwsim/device-map)))

;; ## Device Information
;; * Native Gates
(:native-gates device)
;; * Noise Model
(:noise-model device)

;; ## Executing Circuit on Hardware Simulator Backend
(def noisy-result (backend/execute-circuit
                   hw-simulator
                   (circuit/ghz-state-circuit 3)
                   {:result-specs result-specs}))
#_noisy-result

;; ## Visualizing the Measurement Frequency Histogram
^kind/hiccup
(viz/visualize-measurement-histogram
 :hiccup
 (get-in noisy-result [:results :measurement-results :frequencies]))

;; # QClojure Quantum/Hybrid Algorithms
;;
;; * Textbook implementations of quantum algorithms
;;   * Deutsch, Bernstein-Vazirani, Simon, Grover
;; * Quantum Fourier Transform (QFT), Phase Estimation (QPE), Shor's Algorithm
;; * Variational Quantum Algorithms (VQE, QAOA)
;;
;; ## Grover's Algorithm Example
;; * Unstructured search algorithm
;; * Quadratic speedup over classical search algorithms
;; * Finds a marked item in an unsorted database
;; * Uses amplitude amplification to increase the probability of measuring the marked item
;; * Requires an oracle to identify the marked item
;; * Example: searching for the item '101' in a 3-qubit database

(require '[org.soulspace.qclojure.application.algorithm.grover :as grover])

;; ## Grover Oracle
(def grover-oracle (grover/single-target-oracle 5))

;; ## Grover Circuit
(def grover-circuit
  (grover/grover-circuit 3 grover-oracle))

;; ## Visualizing the Grover Circuit
^kind/hiccup
(viz/visualize-circuit :svg grover-circuit)

;; ## Executing Grover Circuit
(def grover-result
  (grover/grover-algorithm ideal-simulator 8 grover-oracle {:shots 1}))

;; ## QAOA Example
;; * Variational hybrid algorithm for combinatorial optimization
;;   * Combines quantum and classical computation
;;   * Uses parameterized quantum circuits and classical optimization
;; * Solves combinatorial optimization problems
;;   * Max-Cut, Max-SAT and Travelling Salesman Problem

(require '[org.soulspace.qclojure.application.algorithm.qaoa :as qaoa])

;; ## Max-Cut Problem
;; * Simple triangle graph
(def triangle-graph [[0 1 1.0] [1 2 1.0] [0 2 1.0]])

;; ## Running QAOA on the Triangle Graph
(def triangle-qaoa-result
  (qaoa/quantum-approximate-optimization-algorithm
   (ideal/create-simulator)
   {:problem-type :max-cut
    :problem-hamiltonian (qaoa/max-cut-hamiltonian triangle-graph 3)
    :problem-instance triangle-graph  ; Triangle with unit weights
    :num-qubits 3
    :num-layers 2
    :optimization-method :adam
    :max-iterations 100
    :tolerance 1e-6
    :shots 1000}))

;; ## Planned Enhancements
;; 
;; * Asynchronous Infrastructure for Backend Execution
;; 
;; * Backends for Quantum Hardware
;;   * [qclojure-braket](https://github.com/lsolbach/qclojure-braket) for Amazon Braket
;;   * [qclojure-ibmq](https://github.com/lsolbach/qclojure-ibmq) for IBM Quantum
;;
;; * Domain specific libraries
;;   * [qclojure-ml](https://github.com/lsolbach/qclojure-ml) for Quantum Machine Learning
;;   * Quantum Chemistry
;;
;; ## Missing pieces in the Clojure Ecosystem
;;
;; * Fast complex linear algebra (BLAS/LAPACK on CPU/GPU)
;;   * needed for efficient quantum simulation
;;   * also needed for physics and ML applications
;;
;; * Mitigation Options
;;   * Neandertal extension for complex numbers
;;   * ArrayFire bindings via Java22+ FFI
;;
;; ## Contribution
;; * We need an ecosystem around QClojure
;; * Contributions and feedback are welcome
;;   * Discussions on Slack #quantum-computing channel
;;   * Tutorials, examples, documentation
;;   * Bug reports, feature requests
;;   * Code contributions via pull requests
;; 
;; ## Summary
;;
;; * Functional approach is well suited for quantum computing
;; * QClojure is a capable and extensible library for quantum computing
;; * Active development and future plans
;; * Opportunities for contributions and collaboration
