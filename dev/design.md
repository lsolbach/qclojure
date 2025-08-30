# QClojure Design

## Design Goals
QClojure should provide a
* extensible, full featured, production ready Quantum Computing library
  * general quantum computing concepts
  * quantum and hybrid algorithms
  * pluggable quantum computing backends ready for quantum hardware (QPU) integration
  * ideal/noisy simulator backends
  * adaptable complex linear algebra backends
  * vizualizations of quantum computing concepts

### Extensions
QClojure focuses on the core capablities of quantum computing.
Specific applications of quantum computing, e.g. quantum machine learning or quantum chemistry are added as extension libraries
based on QClojure. The backend adapters for real QPUs are also added as extension libraries.

This separation keeps QClojure lean and decoupled.

## Architecture
The implementation architecture for QClojure is *Clean Architecture*.

### Domain Core
The domain core is implemented hardware agnostic, stateless and
in a purely functional way. It provides the necessary data structures and
calculations as composable building blocks for quantum applications.
It contains the implementations of core quantum computing concepts like
* quantum states
* quantum gates (with gate registry, decompositions and optimisations) 
* quantum circuits (with compositions and transformations) 
* observables
* hamiltonians
* quantum channels
* noise modelling

It also currently contains the math required for the quantum computing
calculations.

### Application Layer
The application layer contains the applications of the core quantum
computing concepts, e.g.
* quantum/hybrid algorithms
* protocols for quantum computing backends
* protocols for visualisation
* hardware specific optimizations and transformations
* hardware specific noise and error mitigation

### Adapter Layer
The adapter layer handles all interfacing with the outside world.
It contains
* ideal/noisy simulator backends
* external hardware/simulator backends (as extension libraries)
* I/O handling (export/import of QClojure quantum states and circuits)
* visualization (ASCII, SVG) of quantum states and circuits, results and hardware topologies

