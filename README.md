# QClojure
The QClojure library provides a Clojure interface to quantum computing concepts. It allows us to create and manipulate quantum states, gates, and circuits in a functional programming style. QClojure can also be used to simulate quantum circuits and, by implementing backends, run them on quantum hardware.

*The API should be reasonable stable now, especially for states, gates and circuits. It may change for the backends and visualization.*

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

4. **Extensible Backend System**

   Protocol-based architecture allowing integration with different quantum simulators and hardware backends

5. **Circuit Transformation & Optimization**

   Automatic gate decomposition and circuit transformation to match backend-supported gate sets and topologies

6. **Local Quantum Simulator Backend**

   Local simulators (ideal/noisy) with job submission, status tracking, and statistical measurement results with configurable shot counts

7. **OpenQASM Integration**

   Bidirectional conversion between quantum circuits and OpenQASM 2/3 formats for interoperability with other quantum computing platforms

8. **Visualization**

   Vizualization of quantum states and quantum circuits as ASCII, SVG or HTML.

9. **Comprehensive Testing & Validation**

   Extensive test suite with statistical validation of quantum circuit behavior and measurement outcomes

## Usage
QClojure is a library to be used in programs or interactive within the REPL.

To use QClojure, add a dependency to your project definition.

This small example shows the creation of a quantum circuit in a functional way,
the execution of the circuit on the simulator. It also prints the final state
and the measurement outcomes and generates SVG images of the circuit and the
final state.

```clojure
(require '[org.soulspace.qclojure.domain.circuit :as qc])
(require '[org.soulspace.qclojure.application.backend :as qb])
(require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])
(require '[org.soulspace.qclojure.adapter.visualization :as vis])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; Create a Bell state circuit
(def circuit (-> (qc/create-circuit 2 "Bell Circuit" "Creates a Bell state")
                 (qc/h-gate 0)
                 (qc/cnot-gate 0 1)))

;; Run the circuit on the simulator
(def simulator (sim/create-simulator))
(def result (qb/execute-circuit simulator circuit {:shots 1000}))

;; Examine the results
(:final-state result)
(:measurement-results result)

;; Create SVG visualizations for the circuit and the final state
(spit "bell-circuit.svg" (vis/visualize-circuit :svg circuit))
(spit "bell-state.svg" (vis/visualize-quantum-state :svg (:final-state result)))
```

### Visualizations of the Bell Circuit and State
![Visualization of the Bell circuit](/doc/images/bell-circuit.svg)
![Visualization of the probabilities of the Bell state](/doc/images/bell-state.svg)

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

## Known Issues
* Grover algorithm doesn't use the provided backend
* Error handling on backend failure missing, e.g. timeouts on Shor algorithm

## Copyright
© 2025 Ludger Solbach

## License
Eclipse Public License 1.0 (EPL1.0)

