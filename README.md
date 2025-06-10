# QClojure
The QClojure library provides a Clojure interface to quantum computing concepts. It allows us to create and manipulate quantum states, gates, and circuits in a functional programming style. QClojure can also be used to simulate quantum circuits and, by implementing backends, run them on quantum hardware.

*The API is in alpha state, I expect the api to change!*

[![Clojars Project](https://img.shields.io/clojars/v/org.soulspace/qclojure.svg)](https://clojars.org/org.soulspace/qclojure)
[![cljdoc badge](https://cljdoc.org/badge/org.soulspace/qclojure)](https://cljdoc.org/d/org.soulspace/qclojure)
![GitHub](https://img.shields.io/github/license/lsolbach/QClojure)

## Main Features of QClojure
1. **Pure Functional Quantum Circuit Construction**

   Build quantum circuits using    immutable data structures and functional composition with convenience gate functions (H, X, Y, Z, CNOT, etc.)

2. **Comprehensive Gate Library**

   Support for 20+ quantum gates including single-qubit gates (Pauli, Hadamard, phase), controlled gates (CNOT, CZ, CY), rotation gates (RX, RY, RZ), and multi-qubit gates (Toffoli, Fredkin, SWAP)

3. **Quantum Algorithm Implementations**

   Built-in implementations of classic quantum algorithms including Deutsch, Bernstein-Vazirani, Simon and Grover's search algorithms

5. **Extensible Backend System**

   Protocol-based architecture allowing integration with different quantum simulators and hardware backends

6. **Circuit Transformation & Optimization**

   Automatic gate decomposition and circuit transformation to match backend-supported gate sets and topologies

7. **Local Quantum Simulator Backend**

   High-performance local simulator with job submission, status tracking, and statistical measurement results with configurable shot counts

4. **OpenQASM 2.0 Integration**

   Bidirectional conversion between quantum circuits and OpenQASM format for interoperability with other quantum computing platforms

4. **Visualization**

   Vizualization of quantum states and quantum circuits as ASCII, SVG or HTML.

8. **Comprehensive Testing & Validation**

   Extensive test suite with statistical validation of quantum circuit behavior and measurement outcomes


## Usage
QClojure is a library to be used in programs or interactive within the REPL.

This small example show the creation of a quantum circuit in a functional way
and the execution on the simulator.

```clojure
(require '[org.soulspace.qclojure.domain.circuit :as qc])
(require '[org.soulspace.qclojure.application.backend :as qb])
(require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])

;; Create a simple circuit
(def circuit (-> (qc/create-circuit 2)
                 (qc/h-gate 0)
                 (qc/cnot-gate 0 1)))

;; Run the circuit on the simulator
(def simulator (sim/create-simulator))
(def result (qb/execute-circuit simulator circuit {:shots 1000}))

;; Examine the results
(:final-state result)
(:measurement-results result)
```

## Examples
Some examples are provided in the [examples](/examples) folder.

## Build
QClojure is currently build with [Leiningen](https://leiningen.org/).

Compile the code with:
```
lein compile
```

Run the test suite with:

```
lein test
```

## Copyright
© 2025 Ludger Solbach

## License
Eclipse Public License 1.0 (EPL1.0)

