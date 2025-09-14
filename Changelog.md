# Changelog

## Version (NEXT)
* moved device management functions from hardware-simulator to backend
* enhanced gate optimizations
  * added inverse gate cancellation
  * added rotation folding
* enhanced the tutorial
* fixed optimize-qubits? check 

## Version 0.16.0
* added `device` namespace in domain
  * all QPU specific information is captured in the device map
* decoupled backend and device
  * restructured and refactured the backend protocols
  * added `MultiDeviceBackend` protocol
  * added `BatchJobBackend` protocol
* updated ideal and hardware simulator for new backend protocols
* moved `topology` namespace to domain
* enabled topology optimizations to the hardware simulator
* simplified circuit optimization pipeline code
  * introduced optimization context
* added statistics for circuits and optimizations
* improved the operation registry
  * added the measure operation
* made current QASM 2/3 handling more robust
* added EBNF grammars for OpenQASM 2/3 adapted from the ANTLR grammars
* added OpenQASM 2/3 parsers with instaparse for
improved compatibility including expression evaluation
* improved docstrings
* updated the tutorial to reflect the backend changes

## Version 0.15.0
* extracted `topology` namespace from `hardware-optimization`
  * differenciated between coupling and topology
  * added `coupling-for-topology` and `all-to-all-coupling`
* qubit and gate optimizations throw an exception when the optimization leads to an empty circuit
* hardware simulator creates a failed job on optimization failures 
* activated hardware optimizations in hardware simulator
* removed create-noisy-simulator, use create-hardware-simulator
* integrated trajectory-state pipeline with result extraction to hardware simulator
* extracted common framework for variational algorithms
* improved VQE/QAOA optimization and integrated result extraction
* added standard gate library import to generated QASM3
* shortened some spec names by removing the quantum prefix
* renamed `math.core` namespace to `math.complex-linear-algebra`
* restructured and enhanced the tutorial
* registered project with `zenodo.org` for DOI generation
  * makes qclojure releases citeable in papers

## Version 0.14.0
* simulator enhancements
  * renamed `simulator` namespace to `ideal-simulator`
    * transports the role of the simulator 
  * renamed `noisy-simulator` namespace to `hardware-simulator`
    * transforming noisy simulator in a full quantum hardware simulator
    * use device profiles for max qubits, native gates, topology and noise model
    * optimize circuits for native gate set and topology
  * integrating result extraction in simulators (work in progress)
    done for ideal simulator
* removed native hardware gate sets from operation-registry
  * device profiles provide the information about native gates
* improved noise application in noisy simulator
* added density matrix functions in state, obervables and hamiltonian namespaces
* improved measurement probabilities calculation for observables
* improved vector encoding and accuracy assesment in HHL algorithm
* improved Simon's algorithm
* integrating result extraction in algorithms (work in progress)
  * done for Deutsch, BV, Simon, Grover and QPE
* moved QPE related functions from quantum-period-finging to quantum-phase-estimation
* improved complex linear algebra backends
  * fixed bug in eigen-hermitian in fastmath backend
  * sorted eigen decompositions consistently across functions and backends
  * fixed matrix-log to compute the complex logarithm in the case of negative eigenvalues too
  * added diagonal? to MatrixAlgebra protocol and implementations
* added and improved docstrings
* updated the tutorial

## Version 0.13.0
* added unified, comprehensive result preparation/extraction, supports
  * `:measurements` (basic sampling)
  * `:observables` (observable expectations)
  * `:variance` (observable variances)
  * `:hamiltonian` (hamiltonian energy)
  * `:probabilities` (probability distributions)
  * `:amplitudes` (amplitude extraction)
  * `:state-vector` (simulation only)
  * `:density-matrix` (simulation only)
  * `:fidelity` (fidelity analysis)
  * `:sample` (sampling for QPU hardware)
* changed the return type of execute-circuit to return a map instead of just the state vector to incorporate the result types
  * use `(:final-state result)` to extract the state-vector
* improved QASM2/3 support
  * added missing QClojure gates
  * added result type pragmas for QASM3
  * improved problem reporting
* added hardware topology visualization
  * supports linear, grid, star and ring topologies
* moved `visualization` namespace from `adapter` to `application`

## Version 0.12.0
* added Quantum Approximate Optimization Algorithm (QAOA) for Max-CUT, Max-SAT and TSP problems
* added parameter values to tooltips in SVG circuit visualization
* added lein profiles with JVM options for bigger simulations and for running in a container
* enhanced and improved the tutorial

## Version 0.11.1
* removed unfinished neanderthal backend
* updated the tutorial

## Version 0.11.0
* added protocols for complex linear algebra backends
* implemented complex linear algebra on clojure.math
* implemented complex linear algebra on fastmath
* added clojure.math based linear algebra backend
* added fastmath based linear algebra backend
* added public API for linear algebra in math.core
* added benchmarks for linear algebra backends
* improved layout of the svg circuit visualization
* removed duplicated code
* increased test coverage
* updated tutorial

## Version 0.10.0
* added domain namespaces for hamiltonian and ansatz functions
* added chemistry inspired ansatz
* added optimization namespace in algorithms
* added various optimization methods to be used with vqe-optimization
* moved linear algebra functions to seperate namespace
* fixed HHL measurement
* updated docstrings
* updated tutorial

## Version 0.9.0
* added variational quantum eigensolver algorithm
* added heavy-hex-topology (up to 127 qubits)
* refactored error mitigation for backend support
* added QPU data to backend namespace
* added basis-strings and basis-labels functions
* updated tutorial

## Version 0.8.0
* improved x-axis label placement in bar charts
* improved color palette generation
* aligned return value of algorithms to include
  * success
  * result
  * execution result
  * circuit
* fixed bugs in grover algorithm
* enhanced tutorial

## Version 0.7.0
* added error mitigation strategies
  * readout error mitigation
  * zero noise extrapolation
  * symmetry verification
  * virtual distillation
* moved noise functions to application.noise namespace
* added correlated readout errors
* refactored noisy simulator to use noise namespace

## Version 0.6.0
* added observables namespace
* added gate cancellation optimization
* added rydberg and global-drive gate support for neutral atom QPUs
* defined computational basis states for 3 qubits
* updated circuit optimization pipeline
* consolidated operation-qubits-with-spans functions
* fixed measurement filtering in circuit inversion
* fixed layer assignment for circuit visualization
* improved gate tests
* fixed fredkin gate
* enhanced tutorial

## Version 0.5.0
* improved svg blochsphere visualization
* improved y-axis labels in svg state visualization
* extracted gate layering functions
* added gate layering to the ascii circuit visualisation
* improved gate spacing in  ascii circuit visualisation
* added channel namespace for quantum channel operations
* refactored noisy simulator to use quantum channel functions
* added plus-i-state and minus-i-state functions
* added |+i⟩ and |-i⟩ states
* use matrix instead of phase gate to define s and s-dag gates
* moved hardware optimizations to application layer
* moved oracle functions from grover-test to grover ns
* moved complex? predicate to math
* extracted methods from tests
* enhanced tutorial

## Version 0.4.0
* added device map in backend, moved noise models to device map
* unified functions for ideal and noisy simulators
* improved quantum period finding
* added quantum arithmetic functions
* improved svg circuit visualization
* enhanced tutorial

## Version 0.3.0
* implemented HHL algorithm for hermitian n x n matrices
* enhanced gate decomposition
  * parameterized gates
  * hardware specific native gate sets
* splitted circuit-transformation namespace into
  * gate-decomposition for the decomposition of gates for different hardware 
  * circuit-composition to extend and compose circuits
  * qubit-optimization to reduce the number of qubits needed
* fixed noise application in noisy simulator
* enhanced tests
* updated tutorial

## Version 0.2.0
* added a noisy simulator backend to simulate errors of real quantum hardware
* improved circuit optimization pipeline
* generalized quantum phase estimation and integrated in quantum-period-finding
* phase estimation handles the number of measurements, reduced the number of circuit creations
* fixed edge cases for quantum phase estimation
* fixed coprime selection in shor
* refactored tests for quantum period finding
* used a single state atom in simulators
* added perfect power factor to math
* added prime? and complete-factorization functions
* added more tests for the math namespace
* added prime check in shor

## Version 0.1.0
* implemented quantom states, quantum gates, gate registry and quantum cirquits
* implemented quantum backend protocol with a simulator implementation
* implemented a cirquit transformer and optimizer
* implemented quantum algorithms
* implemented visualizations in ascii and svg and html formats