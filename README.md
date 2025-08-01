# QClojure
The QClojure library provides a Clojure interface to quantum computing concepts. It allows us to create and manipulate quantum states, gates, and circuits in a functional programming style. QClojure can also be used to simulate quantum circuits and, by implementing backends, run them on quantum hardware.

*The API should be quite stable now, especially for states, gates and circuits.*

[![Clojars Project](https://img.shields.io/clojars/v/org.soulspace/qclojure.svg)](https://clojars.org/org.soulspace/qclojure)
[![cljdoc badge](https://cljdoc.org/badge/org.soulspace/qclojure)](https://cljdoc.org/d/org.soulspace/qclojure)
![GitHub](https://img.shields.io/github/license/lsolbach/QClojure)

## Main Features of QClojure
1. **Pure Functional Quantum Circuit Construction**

   Build quantum circuits using immutable data structures and functional composition with convenience gate functions (H, X, Y, Z, CNOT, etc.)

2. **Comprehensive Gate Library**

   Support for 20+ quantum gates including single-qubit gates (Pauli, Hadamard, phase), controlled gates (CNOT, CZ, CY), rotation gates (RX, RY, RZ), multi-qubit gates (Toffoli, Fredkin, SWAP), rydberg and global gates

3. **Quantum Algorithm Implementations**

   Built-in implementations of classic quantum algorithms including Deutsch, Bernstein-Vazirani, Simon, Grover, QFT, QPE, QPF, HHL, VQE and Shor algorithms

4. **Extensible Backend System**

   Protocol-based architecture allowing integration with different quantum simulators and hardware backends

5. **Circuit Transformation & Optimization**

   Automatic gate cancellation, qubit optimization, gate decomposition and circuit transformation to match backend-supported gate sets and topologies

6. **Error Mitigation**
   
   Different strategies like readout error mitigation, ZNE, symmetry verification and virtual distillation, support for autoselection based on circuit and noise profile properties

6. **Local Quantum Simulator Backend**

   Local ideal/noisy simulators with job submission, status tracking, statistical measurement results with configurable shot counts and realistic noise profiles

7. **OpenQASM Integration**

   Bidirectional conversion between quantum circuits and OpenQASM 2/3 formats for interoperability with other quantum computing platforms

8. **Visualization**

   Vizualization of quantum states and quantum circuits as ASCII, SVG or HTML.

9. **Comprehensive Testing & Validation**

   Extensive test suite with statistical validation of quantum circuit behavior and measurement outcomes

## Usage
QClojure is a library to be used in programs or interactive within the REPL.

To use QClojure, add a dependency to your project definition.

### Simple Plus State Qubit Example
The plus state is a single qubit state in superposition with equal
probabilities of measuring |0⟩ or |1⟩.
This example prints the internal representation of the plus state
and generates two different SVG visualizations of this state.

```clojure
(require '[org.soulspace.qclojure.domain.state :as qs])
(require '[org.soulspace.qclojure.adapter.visualization :as vis])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; The plus state, a state in superposition with equal
;; probabilities of measuring |0⟩ or |1⟩
qs/|+⟩

;; Create two SVG visualizations of the plus state 
(spit "prob-plus-state.svg" (vis/visualize-quantum-state :svg qs/|+⟩))
(spit "bloch-plus-state.svg" (vis/visualize-bloch-sphere :svg qs/|+⟩))
```
#### Visualizations of the Plus State
![Bloch Sphere Visualization of the Plus state](/doc/images/bloch-plus-state.svg)
![Visualization of the probabilities of the Plus state](/doc/images/prob-plus-state.svg)



### Bell Circuit Example
This example shows the creation of a quantum circuit in a functional way and
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
(def result (qb/execute-circuit (sim/create-simulator) circuit {:shots 1000}))

;; Examine the results
(:final-state result)
(:measurement-results result)

;; Create SVG visualizations for the circuit and the final state
(spit "bell-circuit.svg" (vis/visualize-circuit :svg circuit))
(spit "bell-state.svg" (vis/visualize-quantum-state :svg (:final-state result)))
```

#### Visualizations of the Bell Circuit and State
![Visualization of the Bell circuit](/doc/images/bell-circuit.svg)
![Visualization of the probabilities of the Bell state](/doc/images/bell-state.svg)

### Bernstein-Vazirani Algorithm Example
This example shows the execution of a quantum algorithm on the simulator backend.It also prints the final state and the measurement outcomes and generates SVG
images of the circuit and the final state.

```clojure
(require '[org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv])
(require '[org.soulspace.qclojure.application.backend :as qb])
(require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])
(require '[org.soulspace.qclojure.adapter.visualization :as vis])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; Bernstein-Vazirani algorithm example
(def bv-result (bv/bernstein-vazirani-algorithm
                (sim/create-simulator) [1 0 1 0] {:shots 100}))

;; Examine the results
(:final-state bv-result)
(:measurement-results bv-result)

;; Create SVG visualizations for the circuit and the final state
(spit "bv-circuit.svg" (vis/visualize-circuit :svg (bv/bernstein-vazirani-circuit [1 0 1 0])))
(spit "bv-state.svg" (vis/visualize-quantum-state :svg (:final-state bv-result)))
```

#### Visualizations of the Bernstein-Varizani Circuit and Final State
![Visualization of the Bell circuit](/doc/images/bv-circuit.svg)
![Visualization of the probabilities of the Bell state](/doc/images/bv-state.svg)

## Tutorial
To learn about quantum computing with QClojure please take a look at the [tutorial](/notebook/tutorial.clj).

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
* Error handling on backend failure missing, e.g. timeouts on Shor algorithm

## Copyright
© 2025 Ludger Solbach

## License
Eclipse Public License 1.0 (EPL1.0)

