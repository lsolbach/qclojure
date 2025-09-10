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

QClojure has a modular architecture with a small core and extension libraries. QClojure focuses on the core capablities of quantum computing and on providing mechanisms for extension, like data specs and protocols. These can be implemented in extension libraries. This separation keeps QClojure lean, comprehensible and decoupled.

## Core Functionality
The QClojure library provides the core functionality needed to build circuits and algorithms and to optimize and simulate them.

The core functionality includes
* Domain constructs for
  * quantum states, gates and circuits
  * observables, hamiltonians, results
  * quantum channels and noise
  * circuit transformation and optimization logic
  * analytics and statistics

* Application workflows and abstractions
  * backend protocols
  * circuit optimization workflows
  * error mitigation workflows
  * format transformations

* Adapter implementations
  * simulation backends
  * visualization formats
  * I/O adapters

## Extensibility

Specific code lives (or will live) in separate libraries.

* Provider backends, e.g.
  * [qclojure-braket](https://github.com/lsolbach/qclojure-braket) - Amazon Braket backend
  * [qclojure-ibmq](https://github.com/lsolbach/qclojure-ibmq) - IBM Quantum backend
* Domain specific algorithms
  * [qclojure-ml](https://github.com/lsolbach/qclojure-ml) - for quantum machine learning
  * qclojure-chemistry - for quantum chemistry (to be done)


## Architecture
The implementation architecture for QClojure is *Clean Architecture*.
It decouples the domain logic from application orchestration and interfacing with other systems. It defines clear, directed dependencies from the adapter layer to
the application layer and from the application layer to the domain core.

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


### Central Concepts
These concepts are central for QClojure

* Circuit
  * qubits
  * operations
    * gates, measurements

* Result Specs/Result
  * states
  * measurment outcomes
  * probabilities
  * observables
  
* Algorithm
  * circuit
  * result specs

* Device
  * native gates
  * topology/coupling
  * noise model/profile
  * calibration

* Backend
  * devices
  * jobs

