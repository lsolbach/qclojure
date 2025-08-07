# Changelog

## Version (NEXT)
* fixed some bugs
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