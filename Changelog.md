# Changelog

## Version (NEXT)
* improved svg blochsphere visualization
* added plus-i-state and minus-i-state functions
* added |+i⟩ and |-i⟩ states
* use matrix instead of phase gate to define s and s-dag gates
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