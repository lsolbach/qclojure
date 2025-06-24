# Changelog

## Version (NEXT)

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