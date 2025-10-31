
# QClojure Quantum Computing Tutorial
This tutorial will guide you through the basics of quantum computing using
[QClojure](https://github.com/lsolbach/qclojure).

The tutorial demonstrates the use of the QClojure. It will introduce you
to the fascinating world of quantum computing and show you how to use
QClojure to create and run quantum programs. It covers

* the creation and visualization of quantum states and quantum registers
* the application of quantum gates
* the creation, visualization and simulation of quantum circuits
* the export and import of quantum data
* the exchange of quantum circuits with other frameworks
* the use of ideal and realistic quantum simulator backends
* the optimization of quantum circuits for specific hardware topologies
* the use of error mitigation techniques
* the description and execution of quantum and hybrid algorithms


## The Tutorial as a Notebook
The tutorial is written as a literate programming notebook, in a style
called 'Namespace as a Notebook', which means that the code and the
documentation are interleaved. You can read the notebook in an editor and
run the code snippets in a Clojure REPL.


### Reproducible Notebooks
You can also generate documentation with [Clay](https://github.com/scicloj/clay).
With Clay a notebook can be rendered to HTML or via [Quarto](https://quarto.org/)
to various formats like PDF, revealjs presentations or Github flavoured markdown.
Quarto also supports articles, books and websites, so you can easily create an
article, book or website from your notebooks.

Generating notebooks with Clay always produces **reproducible** results, as
the code is in the namespace is executed during the rendering process
in a deterministic way. Even with quarto, the code is executed by Clay,
not by Quarto or jupyter, so the results are always the same, no matter how often
you render the notebook.


## Introduction to Quantum Computing
Quantum computing is a fascinating field that combines computer science,
physics and math. It allows us to perform computations that are
not possible with classical computers. Quantum computers use quantum bits,
or [qubits](https://en.wikipedia.org/wiki/Qubit), which can be in a
superposition of states. This means that a qubit can be in a state of 0, 1,
or both at the same time. Quantum computing is based on the principles of
quantum mechanics, which describe the behavior of particles at the quantum
level.

Quantum computing has the potential to revolutionize many fields, including
cryptography, optimization, and machine learning. It can solve certain
problems much faster than classical computers, by performing many
calculations at once.
Quantum Algorithms, such as Shor's algorithm for factoring large numbers
and Grover's algorithm for searching unsorted databases, demonstrate the
power of quantum computing.

Quantum algorithms are defined in terms of quantum gates, which are
operations that can be applied to qubits. Quantum gates manipulate the state
of qubits and can be combined to create quantum circuits.
Quantum circuits are sequences of quantum gates applied to qubits, similar
to classical logic circuits.

For a general introduction to quantum computing, take a look at

* [Quantum Computing](https://en.wikipedia.org/wiki/Quantum_computing)
* [But what is quantum computing? (Grover's Algorithm) - 3blue1brown](https://www.youtube.com/watch?v=RQWpF2Gb-gU) 


## QClojure
The QClojure library provides a Clojure interface to quantum computing concepts.
It allows us to create and manipulate quantum states, gates, and circuits in a functional programming style.
QClojure can also be used to simulate quantum circuits and, by implementing
backends, run them on quantum hardware.

QClojure is focused on the core concepts of quantum computing and provides a
simple and intuitive API to work with quantum states, gates, and circuits.
It also has a comprehensive library of quantum and hybrid algorithms,
including Grover's search algorithm, the Quantum Approximate Optimization
Algorithm (QAOA), and the Variational Quantum Eigensolver (VQE).

QClojure also provides visualization functions to visualize quantum states, circuits
and results, making it easier to understand and debug quantum algorithms.

QClojure is designed to be extensible, allowing the implementation of backends
to run quantum circuits on different quantum hardware. It can also be extended
to specialized domains like quantum chemistry or quantum machine learning.
Those extensions will be available as separate libraries, to keep the core
library focused and lightweight.


### Source Code
The source code is available on [GitHub](https://github.com/lsolbach/qclojure).

[![GitHub Repo stars](https://img.shields.io/github/stars/lsolbach/qclojure?style=social)](https://github.com/lsolbach/qclojure)


### Release Artifacts
The release artifacts (JAR files) are available on [Clojars](https://clojars.org/org.soulspace/qclojure).
Click on the badge below to go to the Clojars page of the latest release.

[![Clojars Project](https://img.shields.io/clojars/v/org.soulspace/qclojure.svg)](https://clojars.org/org.soulspace/qclojure)

QClojure provides citeable releases, so if you use it in your research, you
can cite it. Click on the badge below for the DOI of the latest release.

[![DOI](https://zenodo.org/badge/993970268.svg)](https://doi.org/10.5281/zenodo.17059552)


### Documentation
QClojure provides extensive API documentation, which is available on [CljDoc](https://cljdoc.org/d/org.soulspace/qclojure).

[![cljdoc badge](https://cljdoc.org/badge/org.soulspace/qclojure)](https://cljdoc.org/d/org.soulspace/qclojure)


### License
QClojure is open source and licensed under the Eclipse Public License 1.0. 

![GitHub](https://img.shields.io/github/license/lsolbach/QClojure)


### Prerequisites
As QClojure is running on Clojure and Clojure itself on the JVM, you need to
have the following prerequisites installed on your system:

* [JDK 11 or higher](https://openjdk.org/install/)
* [Clojure](https://clojure.org/)
* [Leiningen](https://leiningen.org/) or [Clojure CLI](https://clojure.org/guides/getting_started)
  to manage dependencies and run Clojure code.

If you are new to Clojure, I recommend reading the
[Clojure Getting Started Guide](https://clojure.org/guides/getting_started).


### Usage
To use QClojure, you have to include it as a dependency in your Clojure
project. Please use the [latest version](https://clojars.org/org.soulspace/qclojure).

If you are using Leiningen, add the following dependency to your
`project.clj` file:

```clojure
[org.soulspace/qclojure "0.23.0"]
```

If you are using Clojure CLI, add the following to your `deps.edn` file:

```clojure
{:deps {org.soulspace/qclojure {:mvn/version "0.23.0"}}}
```


### Imports
We use kindly to visualize the output of our code.
Then we import the relevant namespaces for the domain concepts of the
QClojure library.
The `state` namespace provides functions to create and manipulate quantum
states.
The `gate` namespace provides functions to create quantum gates.
The `circuit` namespace provides functions to create and manipulate quantum
circuits.

We also import the visualization namespace and the svg renderer.
```clj
(ns tutorial
  (:require
   [fastmath.core :as fm]
   [scicloj.kindly.v4.kind :as kind]
   [org.soulspace.qclojure.domain.state :as state]
   [org.soulspace.qclojure.domain.gate :as gate]
   [org.soulspace.qclojure.domain.circuit :as circuit]
   [org.soulspace.qclojure.application.visualization :as viz]
   [org.soulspace.qclojure.adapter.visualization.ascii :as ascii]
   [org.soulspace.qclojure.adapter.visualization.svg :as svg]
   [org.soulspace.qclojure.application.hardware-optimization :as hwopt]))
```
Some namespaces, like visualization namespaces contain multimethod
implementations. To make sure that the implementations are loaded, we
require the namespaces. They will not be used directly in the code, only
indirectly by calling the multimethod, so a warning might be shown by
your IDE. 


## Quantum States
A quantum state is a mathematical object that describes the state of a
quantum system.
In QClojure, quantum states are represented as vectors of complex numbers.
The vector of complex numbers represents the amplitudes of the basis states,
which represent the possible states of the system.
The notation |⟩ is called a "[braket](https://en.wikipedia.org/wiki/Dirac_notation)"
and is used to represent a vector in a complex vector space.
The Qubit is the basic unit of quantum information, and it can be in a
[superposition](https://en.wikipedia.org/wiki/Superposition) of the states
|0⟩ and |1⟩.
A classic bit can be in one of two states, 0 or 1, but a qubit can be in
a superposition of both states.
This means that a qubit can represent 0, 1, or both at the same time, with
different probabilities.


### Measurement
Measurement is the process of extracting classical information from a quantum
state. The measurement process is probabilistic, and the probability of
measuring a certain state depends on the amplitudes of the basis states in
the quantum state.
When we measure a quantum state, we collapse it to one of the basis states
with a certain probability. After measurement, the quantum state is no longer
in a superposition, but in one of the basis states.

The result of the measurement is a classical bit, which can be either 0 or 1.
The measurement process is a fundamental aspect of quantum mechanics and is
described by the [Born rule](https://en.wikipedia.org/wiki/Born_rule).
The Born rule states that the probability of measuring a certain state is equal
to the square of the amplitude of that state in the quantum state vector.


### Basic Quantum States
The *state* namespace defines some basic quantum states.
Let's look at the quantum state |0⟩, which is the ground state of a qubit.
```clj
state/|0⟩

;; =>
{:state-vector [[1.0 0.0] [0.0 0.0]], :num-qubits 1}

```
The measured value of a quantum state is probabilistic.
We have a probability of measuring the state |0⟩ as 0, and a probability of
measuring it as 1.

We can visualize the probability distribution of the quantum state |0⟩.
QClojure provides several visualization functions in the
`org.soulspace.qclojure.application.visualization` namespace.
They all take the desired output format as the first argument.
We can use the `:ascii` format to generate an ASCII art representation
and `:hiccup` format to generate an SVG image.
The ASCII format is useful for quick visualizations in the REPL,
while the SVG format is more suitable for embedding in documents like
tutorials, papers or presentations. We tag the code blocks with
`^kind/code` and `^kind/hiccup` to indicate the type of content.
With these tags, the Clay notebook renderer can render the output
appropriately.

Here is the ascii representation of the quantum state |0⟩, which is useful, when you
are working in the REPL.
It shows that the probability of measuring the state |0⟩ results in 0 is 1,
which is certain.
```clj
^kind/code
(viz/visualize-quantum-state :ascii state/|0⟩)

Quantum State Probability Distribution:
|0⟩: ████████████████████████████████████████ 100.0%
State Summary:
Total qubits: 1
Total probability shown: 100.0%
Hidden states (below threshold): 1

```
And this is the SVG representation of the same quantum state.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup state/|0⟩)
```
![](tutorial_files/image0.svg)

The [Bloch sphere](https://en.wikipedia.org/wiki/Bloch_sphere) is a
geometrical representation of quantum states.
We can visualize the quantum state |0⟩ as a vector on the Bloch sphere.

First the ASCI representation of the Bloch sphere.
```clj
^kind/code
(viz/visualize-bloch-sphere :ascii state/|0⟩)

Bloch Sphere Visualization:

         ······●●●······         
      ·······       ·······      
    ·····               ·····    
  ····                     ····  
 ····                       ···· 
···                           ···
···                           ···
··                             ··
··              +              ··
··                             ··
···                           ···
···                           ···
 ····                       ···· 
  ····                     ····  
    ·····               ·····    
      ·······       ·······      
         ···············         

State: 1.0+0.0i |0⟩ + 0.0+0.0i |1⟩
Coordinates: θ=0.0°, φ=0.0°
Bloch vector: (0.0, 0.0, 1.0)

Legend: ● = current state, · = sphere outline, + = axes
Distances: |0⟩:0.0, |1⟩:2.0, |+⟩:1.41, |-⟩:1.41, |+i⟩:1.41, |-i⟩:1.41
```
And now the Bloch sphere in SVG format of the same quantum state.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup state/|0⟩)
```
![](tutorial_files/image1.svg)

The Bloch sphere representation shows that the state |0⟩ is at the north pole
of the sphere.

Let's look at another quantum state, the excited state |1⟩.
```clj
state/|1⟩

;; =>
{:state-vector [[0.0 0.0] [1.0 0.0]], :num-qubits 1}

```
We can visualize the probability distribution of the quantum state |1⟩.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup state/|1⟩)
```
![](tutorial_files/image2.svg)

It shows that the probability of measuring the state |1⟩ results in 1 is 1,
which is also certain.
The Bloch sphere representation shows that the state |1⟩ is at the south pole
of the sphere.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup state/|1⟩)
```
![](tutorial_files/image3.svg)


### Superposition States
Quantum states can also be in a superposition of the ground and excited
states.
Superposition states are linear combinations of the basic quantum states.

Let's look at the quantum state |+⟩, which is a superposition of the ground
and excited states.
The state |+⟩ is defined as (|0⟩ + |1⟩) / √2.
```clj
state/|+⟩

;; =>
{:state-vector [[0.7071067811865475 0.0] [0.7071067811865475 0.0]],
 :num-qubits 1}

```
We can visualize the probability distribution of the quantum state |+⟩.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup state/|+⟩)
```
![](tutorial_files/image4.svg)

The Bloch sphere representation shows that the state |+⟩ is on the
equator of the sphere, which means, that the probabilities for
measuring 0 or 1 are the same.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup state/|+⟩)
```
![](tutorial_files/image5.svg)

The quantum state |-⟩ is another superposition of the ground and
excited states. The state |-⟩ is defined as (|0⟩ - |1⟩) / √2.
```clj
state/|-⟩

;; =>
{:state-vector [[0.7071067811865475 0.0] [-0.7071067811865475 0.0]],
 :num-qubits 1}

```
We can visualize the probability distribution of the quantum state |-⟩.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup state/|-⟩)
```
![](tutorial_files/image4.svg)

The Bloch sphere representation shows that the state |-⟩ is also on the
equator of the sphere, but pointing in the opposite direction.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup state/|-⟩)
```
![](tutorial_files/image6.svg)


### Multi-Qubit States and Quantum Registers
Tensor products can be used to create multi-qubit states from single-qubit
states. For example, the state |00⟩ is the tensor product of two |0⟩ states.
```clj
state/|00⟩

;; =>
{:state-vector [[1.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0]], :num-qubits 2}

```
We can visualize the probability distribution of the quantum state |00⟩.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup state/|00⟩)
```
![](tutorial_files/image7.svg)


## Quantum Gates
Quantum gates are operations that can be applied to quantum states.
They are represented as matrices that act on the quantum states.
The *gate* namespace defines several quantum gates.


### Pauli Gates
The [Pauli gates](https://en.wikipedia.org/wiki/Pauli_matrices) are a set of
quantum gates that can be applied to single qubits.

The Pauli-X gate is a quantum gate that flips the state of a qubit around
the X axis which swaps the amplitudes of |0⟩ and |1⟩.
```clj
gate/pauli-x

;; =>
[[[0.0 0.0] [1.0 0.0]] [[1.0 0.0] [0.0 0.0]]]

```
The Pauli-Y gate is a quantum gate that flips the state of a qubit around
the Y axis which swaps the amplitudes of |0⟩ and |1⟩ and also adds a phase.
```clj
gate/pauli-y

;; =>
[[[0.0 0.0] [0.0 -1.0]] [[0.0 1.0] [0.0 0.0]]]

```
The Pauli-Z gate is a quantum gate that flips the state of a qubit around
the Y axis which adds a phase to the state of a qubit.
```clj
gate/pauli-z

;; =>
[[[1.0 0.0] [0.0 0.0]] [[0.0 0.0] [-1.0 0.0]]]

```
The Pauli gates are self inverse, applying the same gate twice results
in the original value.


### Hadamard Gate
The [Hadamard gate](https://en.wikipedia.org/wiki/Hadamard_gate) is a
quantum gate that creates superposition states.
It transforms the state |0⟩ into the state |+⟩ and |1⟩ into the state |-⟩.
The Hadamard gate is defined as the matrix:
```clj
gate/hadamard

;; =>
[[[0.7071067811865475 0.0] [0.7071067811865475 0.0]]
 [[0.7071067811865475 0.0] [-0.7071067811865475 0.0]]]

```
We can apply the Hadamard gate to the state |0⟩ to create the superposition 
state |+⟩.
```clj
(def hadamard-state
  (gate/h-gate state/|0⟩))
```
We can visualize the probability distribution of the Hadamard state.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup hadamard-state)
```
![](tutorial_files/image4.svg)

The probability distribution shows that the Hadamard state is in a
superposition of the ground and excited states.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup hadamard-state)
```
![](tutorial_files/image5.svg)

The Bloch sphere representation shows that the Hadamard state is on the
equator of the sphere.

The Hadamard gate is also self inverse, resulting in the input state again
if applied twice.


### Phase Gates
Phase gates are quantum gates that add a phase to the state of a qubit.

The [S gate](https://en.wikipedia.org/wiki/S_gate) is a phase gate that adds
a phase of π/2 to the state of a qubit.
```clj
gate/s-gate

;; =>
[[[1.0 0.0] [0.0 0.0]] [[0.0 0.0] [0.0 1.0]]]

```
The [S† gate](https://en.wikipedia.org/wiki/S_gate#S%E2%81%BF_gate) is the
inverse of the S gate and adds a phase of -π/2 to the state of a qubit.
```clj
gate/s-dag-gate

;; =>
[[[1.0 0.0] [0.0 0.0]] [[0.0 0.0] [0.0 -1.0]]]

```
The [T gate](https://en.wikipedia.org/wiki/T_gate) is a phase gate that adds
a phase of π/4 to the state of a qubit.
```clj
gate/t-gate

;; =>
[[[1.0 0.0] [0.0 0.0]]
 [[0.0 0.0] [0.7071067811865476 0.7071067811865475]]]

```
The [T† gate](https://en.wikipedia.org/wiki/T_gate#T%E2%81%BF_gate) is the
inverse of the T gate and adds a phase of -π/4 to the state of a qubit.
```clj
gate/t-dag-gate

;; =>
[[[1.0 0.0] [0.0 0.0]]
 [[0.0 0.0] [0.7071067811865476 -0.7071067811865475]]]

```

### Rotation Gates
Rotation gates are quantum gates that rotate the state of a qubit around
the Bloch sphere.

The [RX gate](https://en.wikipedia.org/wiki/Rotation_gate#RX_gate) is a
rotation gate that rotates the state of a qubit around the X axis of the
Bloch sphere.
```clj
(gate/rx-gate fm/-QUARTER_PI)

;; =>
[[[0.9238795325112867 0.0] [0.0 0.3826834323650898]]
 [[0.0 0.3826834323650898] [0.9238795325112867 0.0]]]

```
The [RY gate](https://en.wikipedia.org/wiki/Rotation_gate#RY_gate) is a
rotation gate that rotates the state of a qubit around the Y axis of the
Bloch sphere.
```clj
(gate/ry-gate fm/-QUARTER_PI)

;; =>
[[[0.9238795325112867 0.0] [0.3826834323650898 0.0]]
 [[-0.3826834323650898 0.0] [0.9238795325112867 0.0]]]

```
The [RZ gate](https://en.wikipedia.org/wiki/Rotation_gate#RZ_gate) is a
rotation gate that rotates the state of a qubit around the Z axis of the
Bloch sphere.
```clj
(gate/rz-gate fm/-QUARTER_PI)

;; =>
[[[0.9238795325112867 0.3826834323650898] [0.0 0.0]]
 [[0.0 0.0] [0.9238795325112867 -0.3826834323650898]]]

```

### Controlled Gates
Controlled gates are quantum gates that act on multiple qubits.
They are defined as a combination of a control qubit and a target qubit.
The control qubit determines whether the target qubit is affected by the gate.

The controlled-X gate ([CNOT gate](https://en.wikipedia.org/wiki/CNOT_gate))
is a controlled gate that flips the state of the target qubit  if the control
qubit is in the state |1⟩.
```clj
(gate/cnot-gate)

;; =>
[[[1.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0]]
 [[0.0 0.0] [1.0 0.0] [0.0 0.0] [0.0 0.0]]
 [[0.0 0.0] [0.0 0.0] [0.0 0.0] [1.0 0.0]]
 [[0.0 0.0] [0.0 0.0] [1.0 0.0] [0.0 0.0]]]

```
The controlled-Y gate is a controlled gate that flips the state of the target
qubit and adds a phase if the control qubit is in the state |1⟩.

The controlled-Z gate is a controlled gate that adds a phase to the target
qubit if the control qubit is in the state |1⟩.


## Quantum Circuits
Quantum circuits are sequences of quantum gates applied to quantum states.
The *circuit* namespace provides functions to create and manipulate quantum
circuits.


### Creating a Quantum Circuit
We can create a simple quantum circuit that applies the Hadamard gate to the
state |0⟩.
```clj
(def simple-circuit
  (-> (circuit/create-circuit 1 "Hadamard on qubit 0")
      (circuit/h-gate 0)))
```
We can visualize the quantum circuit as ASCII or SVG, like we did with
quantum states.

Here is the ascii representation of the quantum circuit.
```clj
^kind/code
(viz/visualize-circuit :ascii simple-circuit)

Circuit: Hadamard on qubit 0

q0 |0⟩──────[H]───╫──═
Gates: 1, Depth: 1, Column width: 5
```
And this is the SVG representation of the same quantum circuit.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup simple-circuit)
```
![](tutorial_files/image8.svg)

The circuit shows that the Hadamard gate is applied to the qubit 0.

We can execute the circuit with the `qc/execute-circuit` function
on the state |0⟩ to create the Hadamard state.
```clj
(def hadamard-circuit-result
  (circuit/execute-circuit simple-circuit state/|0⟩))
```
We can visualize the probability distribution of the Hadamard circuit state.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (:final-state hadamard-circuit-result))
```
![](tutorial_files/image4.svg)

The probability distribution shows that the Hadamard circuit state is
in a superposition of the ground and excited states. It is the same as the
Hadamard state we created earlier, but now created by a quantum circuit, not
just the application of a single gate on a quantum state.
```clj
^kind/hiccup
(viz/visualize-bloch-sphere :hiccup (:final-state hadamard-circuit-result))
```
![](tutorial_files/image5.svg)

The *circuit* namespace also has some predefined circuits.

For example, the 'qc/bell-state-circuit' creates a circuit that prepares 
Bell state, which is a two-qubit entangled state.
```clj
(def bell-circuit
  (circuit/bell-state-circuit))
```
We can visualize the Bell circuit.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup bell-circuit)
```
![](tutorial_files/image9.svg)

The Bell circuit shows that the Hadamard gate is applied to the first qubit,
followed by a CNOT gate between the first and second qubits.
The Bell state is a two-qubit state that is
[entangled](https://en.wikipedia.org/wiki/Entanglement).
```clj
(def bell-result
  (circuit/execute-circuit bell-circuit (state/zero-state 2)))
```
We can visualize the probability distribution of the Bell state.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (:final-state bell-result))
```
![](tutorial_files/image10.svg)

The *circuit* namespace also has a predefined circuit for multi-qubit states.
This circuit can be used to create entangled states with more than two
qubits.

For example, the `qc/ghz-circuit` creates a circuit that prepares
a Greenberger-Horne-Zeilinger ([GHZ](https://en.wikipedia.org/wiki/Greenberger%E2%80%93Horne%E2%80%93Zeilinger_state))
state.
```clj
(def ghz-circuit
  (circuit/ghz-state-circuit 3))
```
We can visualize the GHZ circuit.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup ghz-circuit)
```
![](tutorial_files/image11.svg)

The GHZ circuit shows that the Hadamard gate is applied to the first qubit,
followed by CNOT gates between the first and second qubits, and between the
first and third qubits. The GHZ state is a multi-qubit state that is entangled.

We can apply the GHZ circuit to the state |000⟩ to create the GHZ state.
```clj
(def ghz-result
  (circuit/execute-circuit ghz-circuit (state/zero-state 3)))
```
We can visualize the probability distribution of the GHZ state.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (:final-state ghz-result))
```
![](tutorial_files/image12.svg)

The probability distribution shows that the GHZ state is in a superposition
of the states |000⟩ and |111⟩.


## Data Format and I/O
Sometimes we want to save quantum circuits or quantum states to disk
or read them from disk.
This is especially useful if we want to share quantum circuits or states
with others or if we want to use quantum circuits or states created
in other quantum computing frameworks.

QClojure supports various input and output formats for quantum circuits
and quantum states. This allows users to easily import and export quantum
circuits and states between different quantum computing frameworks and tools.
The supported formats include:
* Extensible Data Notation (EDN)
* JSON (JavaScript Object Notation)
* QASM (Quantum Assembly Language)

Let's import the I/O namespace providing the API for reading and writing
quantum circuits and quantum states.
```clj
(require '[org.soulspace.qclojure.adapter.io :as io])
```
Now we define some test data to demonstrate the I/O capabilities.
We create a quantum circuit with medium complexity for import and export.
```clj
(def test-circuit
  (-> (circuit/create-circuit 3 "I/O Test Circuit" "A circuit with medium complexity")
      (circuit/h-gate 0)
      (circuit/cnot-gate 0 1)
      (circuit/t-gate 1)
      (circuit/cnot-gate 1 2)
      (circuit/measure-operation [0 1 2])))
```

## EDN Support
EDN (Extensible Data Notation) is a data format that is similar to JSON
but is more expressive and flexible. It is a subset of Clojure syntax
and is used for representing data structures in a human-readable format.
QClojure supports EDN format for importing and exporting quantum circuits
and quantum states.
```clj
(require '[org.soulspace.qclojure.adapter.io.edn :as edn])
```
Let's first write a simple quantum state to disk in EDN format.
```clj
(io/export-quantum-state :edn state/|+⟩ "export/plus-state.edn")

;; =>
nil

```
We can read the quantum state in EDN form back from disk.
```clj
(io/import-quantum-state :edn "export/plus-state.edn")

;; =>
{:state-vector [[0.7071067811865475 0.0] [0.7071067811865475 0.0]],
 :num-qubits 1,
 :metadata {}}

```

## JSON Support
JSON (JavaScript Object Notation) is a lightweight data interchange format
that is easy for humans to read and write and easy for machines to parse
and generate. It is widely used for data exchange between web applications
and servers.
QClojure supports JSON format for importing and exporting quantum circuits
and quantum states. To use JSON I/O capabilities, we need to require
the `io.json` namespace.
```clj
(require '[org.soulspace.qclojure.adapter.io.json :as json])
```
The functions for JSON I/O are the same, just with the different format
keyword `:json`.
Let's write the same quantum state to disk in JSON format.
```clj
(io/export-quantum-state :json state/|+⟩ "export/plus-state.json")

;; =>
"export/plus-state.json"

```
We can read the quantum state in JSON form back from disk.
```clj
(io/import-quantum-state :json "export/plus-state.json")

;; =>
{:state-vector [[0.7071067811865475 0.0] [0.7071067811865475 0.0]],
 :num-qubits 1,
 :metadata {}}

```

### OpenQASM Support
QASM (Quantum Assembly Language) is a low-level programming language
used to describe quantum circuits. It is a standard format for representing
quantum circuits and is supported by many quantum computing frameworks.
QASM allows us to define quantum gates, measurements, and other operations
in a text-based format that can be easily shared and executed on different
quantum computing platforms.
QASM 3 for example is used as the exchange format for quantum circuits
by Amazon Braket. QASM 2 is supported by IBM Quantum and other
quantum computing frameworks.

QClojure supports QASM 2 and QASM 3 formats, allowing users to import
and export quantum circuits in QASM format. It does not support quantum states
in QASM format, as QASM is primarily used for representing quantum circuits.

Let's require the QASM namespace to explore the QASM I/O capabilities.
```clj
(require '[org.soulspace.qclojure.adapter.io.qasm :as qasm])
```
We can export the quantum circuit to QASM 2 format.
```clj
(io/export-quantum-circuit :qasm2 test-circuit "export/test-circuit-qasm2.qasm")

;; =>
"export/test-circuit-qasm2.qasm"

```
To see how the QASM 2 output looks like, we can read the file with slurp.
```clj
^kind/code
(slurp "export/test-circuit-qasm2.qasm")

OPENQASM 2.0;
include "qelib1.inc";
qreg q[3];
creg c[3];

h q[0];
cx q[0],q[1];
t q[1];
cx q[1],q[2];
// Measurement will be handled by final measure statement
measure q -> c;
```
We can read the quantum circuit in QASM 2 format back from disk.
```clj
(io/import-quantum-circuit :qasm2 "export/test-circuit-qasm2.qasm")

;; =>
{:operations
 [{:operation-type :h, :operation-params {:target 0}}
  {:operation-type :cnot, :operation-params {:control 0, :target 1}}
  {:operation-type :t, :operation-params {:target 1}}
  {:operation-type :cnot, :operation-params {:control 1, :target 2}}],
 :num-qubits 3,
 :name "Converted Circuit"}

```
We can also export the quantum circuit to QASM 3 format.
```clj
(io/export-quantum-circuit :qasm3 test-circuit "export/test-circuit-qasm3.qasm")

;; =>
"export/test-circuit-qasm3.qasm"

```
To see how the QASM 3 output looks like, we can read the file with slurp.
```clj
^kind/code
(slurp "export/test-circuit-qasm3.qasm")

OPENQASM 3.0;
include "stdgates.inc";

qubit[3] q;
bit[3] c;

h q[0];
cx q[0], q[1];
t q[1];
cx q[1], q[2];
c[0] = measure q[0];
c[1] = measure q[1];
c[2] = measure q[2];
```
We can read the quantum circuit in QASM 3 format back from disk.
```clj
(io/import-quantum-circuit :qasm3 "export/test-circuit-qasm3.qasm")

;; =>
{:operations
 [{:operation-type :h, :operation-params {:target 0}}
  {:operation-type :cnot, :operation-params {:control 0, :target 1}}
  {:operation-type :t, :operation-params {:target 1}}
  {:operation-type :cnot, :operation-params {:control 1, :target 2}}
  {:operation-type :measure,
   :operation-params {:measurement-qubits [0]}}
  {:operation-type :measure,
   :operation-params {:measurement-qubits [1]}}
  {:operation-type :measure,
   :operation-params {:measurement-qubits [2]}}],
 :num-qubits 3,
 :name "Converted Circuit",
 :result-specs {}}

```


## Math Backends for Complex Linear Algebra
Simulating quantum circuits on a classical computer requires efficient
linear algebra operations on complex numbers. QClojure provides a
`domain.math.complex-linear-algebra` namespace that abstracts the underlying complex linear algebra
implementation. This namespace provides the public API for complex linear
algebra operations used in QClojure. It allows to switch between different
implementations of complex linear algebra without changing the QClojure code.

Protocols define the operations that need to be implemented by a
specific complex linear algebra backend implementation:

* MatrixAlgebra - defining basic matrix operations like addition, multiplication,
  products (kronecker/tensor, outer, inner, hadamard), conjugate transpose, etc.
* MatrixDecompositions - defining matrix decompositions like
  singular value decomposition, eigenvalue decomposition, etc.
* MatrixFunctions - defining matrix functions like exponentiation, logarithm, square root, etc.
* MatrixAnalysis - defining matrix analysis functions like spectral norm, condition number, etc.

Currently, QClojure supports two backend implementations:

* Fastmath Backend (`:fastmath`), based on FastMath, a high-performance numerical computing
  library for Clojure. It provides efficient implementations of complex linear
  algebra operations based on Apache Commons Math.
* Pure Clojure Math Backend (`:pure`), based on Clojure Math, a pure Clojure implementation
  of complex linear algebra operations for educational purpose only.

The Fastmath backend is the default backend used by QClojure, as it provides
better performance for large quantum states and circuits. The Clojure Math
backend should be used only for educational purposes or for small quantum states and
circuits.
```clj
(require '[org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla])
```
You can switch between the backends by using the `set-backend` function.
After switching the backend, all complex linear algebra operations
will use the selected backend. Backends may use a different representation
for complex numbers and matrices, but the public API handles the conversion
between the different representations.

The backend can be switched at any time, but it is recommended to set the
backend at the beginning of the program, before any quantum states or
circuits are created.

To switch to the pure Clojure Math backend, use the following code:
```clj
(cla/set-backend! :pure)

;; =>
:pure

```
Now, all complex linear algebra operations will use the pure Clojure Math
backend.

You can switch back to the Fastmath backend by using the following code:
```clj
(cla/set-backend! :fastmath)

;; =>
:fastmath

```
A BLAS/LAPACK (CPU) and OpenCL/CUDA (GPU) enabled backend would be desirable
for simulating larger quantum states and circuits, but is not yet available.


## Quantum Devices and Quantum Computing Backends
A device represents a quantum computer,also known as a Quantum Processing
Unit (QPU), with a certain number of qubits,
a native gate set, a topology of coupled qubits and various kinds of noise.

Devices are handled by backends, which are responsible for executing
quantum circuits on a specific quantum computer or a simulator.
Some backends may only support a single device, while others may support
multiple devices. A device can represent a real quantum computer or a
simulator.
Backends that support multiple devices are called multi-device backends.
For multi-device backends a specific device can be selected on a backend
for the execution of quantum circuits. An example will be shown in the
[Hardware Simulator Backend](#hardware-simulator-backend) section below.

A quantum backend is an adapter for one or more devices to QClojure.
Each backend implements the QuantumBackend protocol. A backend may also
implement more specific protocols, like the CloudQuantumBackend, the
MultiDeviceBackend or the BatchJobBackend protocol.
Backends also provide additional functionality, like optimizing
and transforming quantum circuits to run on a specific quantum device.

QClojure can be extended with backend implementations to run quantum
circuits on real quantum hardware.
The *application.backend* namespace contains the protocols to be implemented
by a specific backend. A backend can be used to execute a quantum circuit.
```clj
(require '[org.soulspace.qclojure.application.backend :as backend])
```
QClojure comes with two simulator backends in the *adapter.backend* that can be used to
simulate quantum circuits on a classical computer.

* The ideal simulator backend simulates an ideal quantum computer without
  physical constraints like noise.
* The hardware simulator backend simulates a real quantum computer with
  a native gate set, a topology of coupled qubits and various kinds of noise.

Additional backends to access quantum hardware will be available as separate
libraries. There is already an experimental implementation for an Amazon
Braket backend available in the [qclojure-braket](https://github.com/lsolbach/qclojure-braket)
project.

Simulating quantum circuits on a classical computer is computationally expensive,
as the state space of a quantum system grows exponentially with the number of qubits.
Therefore, the simulator backends are limited to a certain number of qubits,
depending on the available memory and processing power of the classical computer.
The simulator backends are limited to about 20 qubits on a typical desktop computer.
Also note that the time to simulate a quantum circuit grows exponentially with the
number of qubits and the depth of the circuit.
Therefore, simulating quantum circuits with a large number of qubits or a deep
circuit can take a long time.


## Ideal Simulator Backend
Let's try the ideal simulator first by requiring the `ideal-simulator` namespace.
```clj
(require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
```
We create the simulator backend with the `create-simulator` function.
```clj
(def simulator (sim/create-simulator))
```
Now we can use the simulator to execute the ghz circuit on the simulator.
```clj
(backend/execute-circuit simulator (circuit/ghz-state-circuit 3))

;; =>
{:job-status :completed,
 :results
 {:final-state
  {:state-vector
   [[0.7071067811865475 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.7071067811865475 0.0]],
   :num-qubits 3},
  :result-types #{},
  :circuit
  {:operations
   [{:operation-type :h, :operation-params {:target 0}}
    {:operation-type :cnot, :operation-params {:control 0, :target 1}}
    {:operation-type :cnot,
     :operation-params {:control 0, :target 2}}],
   :num-qubits 3,
   :name "GHZ State",
   :description "Prepares 3-qubit GHZ state"},
  :circuit-metadata
  {:circuit-depth 3,
   :circuit-operation-count 3,
   :circuit-gate-count 3}},
 :execution-time-ms 1,
 :job-id "sim_job_3193_1761945921529"}

```
When executing a circuit on a backend, it will be executed multiple times,
because of the probabilistic nature of quantum computing. One execution of the
circuit is called a *shot*. The default number of shots is 512, but it can be
configured via an options map.
```clj
(backend/execute-circuit simulator (circuit/ghz-state-circuit 3) {:shots 10})

;; =>
{:job-status :completed,
 :results
 {:final-state
  {:state-vector
   [[0.7071067811865475 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.7071067811865475 0.0]],
   :num-qubits 3},
  :result-types #{},
  :circuit
  {:operations
   [{:operation-type :h, :operation-params {:target 0}}
    {:operation-type :cnot, :operation-params {:control 0, :target 1}}
    {:operation-type :cnot,
     :operation-params {:control 0, :target 2}}],
   :num-qubits 3,
   :name "GHZ State",
   :description "Prepares 3-qubit GHZ state"},
  :circuit-metadata
  {:circuit-depth 3,
   :circuit-operation-count 3,
   :circuit-gate-count 3}},
 :execution-time-ms 0,
 :job-id "sim_job_3194_1761945921630"}

```

## Hardware Simulator Backend
Currently existing real quantum hardware has a set of limitations compared
to what our ideal simulator supports:

* supports only a limited set of native quantum gates
* may have a limited topology of coupled qubits
* is subject to various kinds of noise

This has some consequences for running quantum circuits on real quantum
hardware, also known as Quantum Processing Units (QPUs).
The limited topology means that not all qubits can be used freely in 
a multi-qubit gate, e.g. a CNOT gate. If a CNOT gate should be applied to
qubits which are not coupled in the topology, represente by a coupling map,
Swap gates have to be introduced to 'move' the information to qubits that
are coupled, so that the CNOT gate can be applied. 

The limited native gate set means that our circuit has to be transformed to
only use those native gates. This is done by decomposing unsupported gates
to supported gates.

The hardware simulator backend optimizes and transforms a quantum circuit
on submission to the backend, so that it can be executed on the
simulated quantum hardware.
It optimizes the circuit by reducing the number of gates and the depth
of the circuit. It transforms the circuit by decomposing unsupported gates
to supported gates and by adding Swap gates to respect the coupling map.
Details about the optimization and transformation process are described
in the [Circuit Optimization and Transformation](#circuit-optimization-and-transformation)
section below.

The various kinds of noise affect the results of quantum computations.
They can be addressed by error correction, which uses a number of
physical qubits to form a logical qubit. On current QPUs with limited
qubit counts this is not always an option. Error mitigation strategies try 
to address the problem mathematically or with more executions.

The hardware simulator backend also simulates the noise of a given
quantum device, based on a device map provided to the backend,
allowing us to study the effects of noise on quantum circuits.
It simulates these kinds of noise:

* depolarizing noise is a type of noise that randomly flips the state of a
  qubit with a certain probability.
* amplitude damping noise is a type of noise that causes the state of a
  qubit to decay over time with a certain probability.
* bit flip noise is a type of noise that flips the state of a qubit from
  |0⟩ to |1⟩ or from |1⟩ to |0⟩ with a certain probability.
* phase flip noise is a type of noise that flips the phase of the state of
  a qubit from |0⟩ to |1⟩ or from |1⟩ to |0⟩ with a certain probability.
* readout noise is a type of noise that affects the measurement of the
  state of a qubit, causing the measured value to be incorrect with a
  certain probability.

The hardware simulator backend applies the noise to the quantum states
and gates in the circuit, simulating the effects of noise on the quantum
computation.

Let's try the hardware simulator first by requiring the `hardware-similator` namespace.
```clj
(require '[org.soulspace.qclojure.adapter.backend.hardware-simulator :as hwsim])
```
We can instantiate the hardware simulator with the `create-hardware-simulator` function
and provide a provide a device map. The device map we use here is derived from the
IBM Lagos Quantum Computer.
```clj
(def hardware-simulator (hwsim/create-hardware-simulator))

(backend/select-device hardware-simulator (:ibm-lagos hwsim/device-map))

;; =>
{:native-gates #{:y :rx :z :h :x :rz :ry :cnot},
 :performance
 {:single-qubit-gate-fidelity 0.9995,
  :two-qubit-gate-fidelity 0.994,
  :readout-fidelity 0.972,
  :gate-times {:single-qubit 35.6, :two-qubit 476},
  :coherence-times {:t1 125.0, :t2 89.0}},
 :name "IBM Lagos",
 :connectivity :limited,
 :type :qpu,
 :noise-model
 {:gate-noise
  {:h
   {:noise-type :depolarizing,
    :noise-strength 5.0E-4,
    :t1-time 125.0,
    :t2-time 89.0,
    :gate-time 35.6},
   :x
   {:noise-type :depolarizing,
    :noise-strength 3.0E-4,
    :t1-time 125.0,
    :t2-time 89.0,
    :gate-time 35.6},
   :cnot
   {:noise-type :depolarizing,
    :noise-strength 0.006,
    :t1-time 125.0,
    :t2-time 89.0,
    :gate-time 476.0}},
  :readout-error {:prob-0-to-1 0.013, :prob-1-to-0 0.028}},
 :measurement-basis :computational,
 :technology :superconducting,
 :id :ibm-lagos,
 :virtual-gates #{:cz :s :swap :t},
 :coupling [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6]],
 :num-qubits 7,
 :provider "IBM",
 :platform "Legacy",
 :topology :linear}

```
The device map shows configurations for the different types of noise,
a physical quantum computer can have. All different types of noise contribute
to the errors in the measurement.
The device map also shows the native gate set and the coupling map of the
qubits, which defines the topology of coupled qubits.

Now we can use the simulator to execute the ghz circuit on the simulator.
Because we use a hardware simulator simulating noise, we may measure wrong
answers.
```clj
(def lagos-50-result (backend/execute-circuit hardware-simulator (circuit/ghz-state-circuit 3) {:shots 50}))

lagos-50-result

;; =>
{:job-status :completed,
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}],
  :num-qubits 3,
  :name "GHZ State",
  :description "Prepares 3-qubit GHZ state"},
 :circuit-metadata
 {:circuit-depth 3, :circuit-operation-count 3, :circuit-gate-count 3},
 :shots-executed 50,
 :execution-time-ms 50,
 :results
 {:measurement-results {"111" 23, "000" 25, "011" 1, "110" 1},
  :final-state
  {:num-qubits 3,
   :state-vector
   [[0.7071067811865476 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.7071067811865476 0.0]]},
  :trajectories
  [{:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}],
  :trajectory-count 50,
  :density-matrix
  [[[0.5000000000000002 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.5000000000000002 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.5000000000000002 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.5000000000000002 0.0]]],
  :density-matrix-trace 1.0000000000000004,
  :trajectory-weights
  [0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999
   0.01999999999999999]},
 :job-id "job_7_1761945921733"}


^kind/hiccup
(viz/visualize-quantum-state :hiccup (get-in lagos-50-result [:results :final-state ]))
```
![](tutorial_files/image13.svg)

We see, that not all measurements measure the states |000⟩ and |111⟩,
even though those states should have the highest counts. The other states
should have a distinctively lower count. But if you use to few shots, you
could be unlucky and measure the wrong answers. The probability to measure
the wrong answers gets lower by increasing the number of shots.
```clj
(def lagos-10k-result (backend/execute-circuit hardware-simulator (circuit/ghz-state-circuit 3) {:shots 10000}))

lagos-10k-result

;; =>
{:job-status :completed,
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}],
  :num-qubits 3,
  :name "GHZ State",
  :description "Prepares 3-qubit GHZ state"},
 :circuit-metadata
 {:circuit-depth 3, :circuit-operation-count 3, :circuit-gate-count 3},
 :shots-executed 10000,
 :execution-time-ms 3920,
 :results
 {:measurement-results
  {"111" 4548,
   "000" 4740,
   "110" 150,
   "010" 99,
   "100" 70,
   "011" 167,
   "001" 80,
   "101" 146},
  :final-state
  {:num-qubits 3,
   :state-vector
   [[0.7071067811865476 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.7071067811865476 0.0]]},
  :trajectories
  [{:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.0 0.0]
     [0.0 0.7071067811865476]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 -0.7071067811865476]
     [0.0 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]]}],
  :trajectory-count 100,
  :density-matrix
  [[[0.49500000000000033 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.49500000000000033 0.0]]
   [[0.0 0.0]
    [0.0049999999999999975 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [-0.0049999999999999975 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [-0.0049999999999999975 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0049999999999999975 0.0]
    [0.0 0.0]]
   [[0.49500000000000033 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.49500000000000033 0.0]]],
  :density-matrix-trace 1.0000000000000007,
  :trajectory-weights
  [0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993]},
 :job-id "job_8_1761945921836"}

```
With 10000 shots, the difference of the counts of the correct answers
and the counts of the wrong answers should be quite significant.
```clj
^kind/hiccup
(viz/visualize-measurement-histogram :hiccup (get-in lagos-10k-result [:results :measurement-results]))
```
![](tutorial_files/image14.svg)

We can also use the hardware simulator with a different device map, e.g.
for an IonQ Forte quantum computer.
Let's select IonQ Forte device for the simulation.
```clj
(backend/select-device hardware-simulator (:ionq-forte hwsim/device-map))

;; =>
{:native-gates #{:y :rx :z :h :x :ms :rz :ry :cnot},
 :performance
 {:single-qubit-gate-fidelity 0.9995,
  :two-qubit-gate-fidelity 0.997,
  :readout-fidelity 0.998,
  :gate-times {:single-qubit 30000, :two-qubit 120000},
  :coherence-times {:t1 20000.0, :t2 10000.0}},
 :name "IonQ Forte",
 :connectivity :full,
 :type :qpu,
 :noise-model
 {:gate-noise
  {:h
   {:noise-type :coherent,
    :coherent-error {:rotation-angle 3.0E-4, :rotation-axis :y},
    :t1-time 20000.0,
    :t2-time 10000.0,
    :gate-time 30000.0},
   :x
   {:noise-type :coherent,
    :coherent-error {:rotation-angle 2.0E-4, :rotation-axis :x},
    :t1-time 20000.0,
    :t2-time 10000.0,
    :gate-time 30000.0},
   :y
   {:noise-type :coherent,
    :coherent-error {:rotation-angle 2.0E-4, :rotation-axis :y},
    :t1-time 20000.0,
    :t2-time 10000.0,
    :gate-time 30000.0},
   :z
   {:noise-type :phase-damping,
    :noise-strength 3.0E-5,
    :t1-time 20000.0,
    :t2-time 10000.0,
    :gate-time 30.0},
   :cnot
   {:noise-type :depolarizing,
    :noise-strength 0.003,
    :t1-time 20000.0,
    :t2-time 10000.0,
    :gate-time 120000.0}},
  :readout-error {:prob-0-to-1 0.001, :prob-1-to-0 0.002}},
 :measurement-basis :computational,
 :technology :trapped-ion,
 :id :ionq-forte,
 :virtual-gates #{:cz :s :swap :t},
 :coupling :all-to-all,
 :algorithmic-qubits 36,
 :num-qubits 36,
 :provider "IonQ",
 :arn "arn:aws:braket:us-east-1::device/qpu/ionq/Forte-1",
 :platform "Amazon Braket",
 :topology :all-to-all}

```
We now execute the GHZ circuit on this simulator with 10000 shots and
compare the results with the IBM Lagos simulation.
```clj
(def forte-10k-result (backend/execute-circuit hardware-simulator (circuit/ghz-state-circuit 3) {:shots 10000}))

forte-10k-result

;; =>
{:job-status :completed,
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}],
  :num-qubits 3,
  :name "GHZ State",
  :description "Prepares 3-qubit GHZ state"},
 :circuit-metadata
 {:circuit-depth 3, :circuit-operation-count 3, :circuit-gate-count 3},
 :shots-executed 10000,
 :execution-time-ms 3965,
 :results
 {:measurement-results
  {"000" 4937,
   "111" 4970,
   "110" 24,
   "011" 11,
   "101" 16,
   "010" 17,
   "100" 7,
   "001" 18},
  :final-state
  {:num-qubits 3,
   :state-vector
   [[0.7072128392483766 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.7070007072148161 0.0]]},
  :trajectories
  [{:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}
   {:num-qubits 3,
    :state-vector
    [[0.7072128392483766 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7070007072148161 0.0]]}],
  :trajectory-count 100,
  :density-matrix
  [[[0.5001499999977507 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.49999997749999964 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]]
   [[0.49999997749999964 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.0 0.0]
    [0.49985000000224983 0.0]]],
  :density-matrix-trace 1.0000000000000004,
  :trajectory-weights
  [0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993
   0.009999999999999993]},
 :job-id "job_9_1761945925846"}

```
Compared to the IBM Lagos simulation, the IonQ Forte simulation should have
distinctly lower noise and thus a higher count for the correct answers.
```clj
^kind/hiccup
(viz/visualize-measurement-histogram :hiccup (get-in forte-10k-result [:results :measurement-results]))
```
![](tutorial_files/image15.svg)

You can also create your own device profiles by defining a device map
with the required parameters. You can then use this device map to create
a hardware simulator backend.
A device map may not have all parameters defined. In this case some features
may not be used, e.g. error mitigation techniques.
If you provide a device map with no native-gates, coupling and noise model
defined, the hardware simulator will behave like the ideal simulator.
You can also modify an existing device map to create a new device map for
testing specific aspects or scenarios.


## Circuit Optimization and Transformation
Quantum circuits can be optimized and transformed to improve their performance
and to run on specific quantum hardware. This transformation is also called
*transpilation*. QClojure provides a set of optimization and transformation
techniques that can be applied to quantum circuits.


### Gate Optimization
Gate optimization is a technique used to reduce the number of gates in a
quantum circuit. It is based on the idea that some gates can be combined
or eliminated without changing the overall functionality of the circuit.
Gate optimization can be applied to quantum circuits to improve their
performance and to reduce the effects of noise.
For example, consecutive Pauli and Hadamard gates can be eliminated, as they
are self-inverse. QClojure implements some gate optimization techniques:

* Gate Cancellation - eliminates consecutive gates that cancel each other out.
* Rotation Folding - combines consecutive rotation gates into a single rotation gate.


### Qubit Optimization
Qubit optimization is a technique used to reduce the number of qubits in
a quantum circuit. It is based on the idea that some qubits can be eliminated
without changing the overall functionality of the circuit. The simplest case
is to eliminate qubits that are not used in the circuit.


### Error Correction
Error correction is a technique used to protect quantum information from
errors due to noise and decoherence. It is based on the idea of encoding
quantum information in a way that allows errors to be detected and corrected.
Error correction can be applied to quantum circuits to improve their
performance and to reduce the effects of noise. The encoded circuit will
use more physical qubits to form logical qubits, so it is not always an
option on current QPUs with limited qubit counts.
QClojure has a stabilizer framework with these error correction codes implemented:

* Bit Flip Code - a simple error correction code that can correct a single
  bit flip error.
* Phase Flip Code - a simple error correction code that can correct a single
  phase flip error.
* Shor Code - a more complex error correction code that can correct both
  bit flip and phase flip errors.
* Steane Code - a more complex error correction code that can correct both
  bit flip and phase flip errors.
* 5-Qubit Code - the smallest code that can correct an arbitrary single-qubit error.


### Topology Optimization
Topology optimization is a technique used to transform a quantum circuit
to run on specific quantum hardware. Quantum hardware has a limited topology
of coupled qubits, which means that not all qubits can be used freely in
a multi-qubit gate, e.g. a CNOT gate. If a CNOT gate should be applied to
qubits which are not coupled in the topology, represented by a coupling map,
Swap gates have to be introduced to 'move' the information to qubits that
are coupled, so that the CNOT gate can be applied. Topology optimization
can be applied to quantum circuits to transform them to run on specific
quantum hardware.


### Gate Decomposition
Gate decomposition is a technique used to transform a quantum circuit
to use only a limited set of native quantum gates. Quantum hardware supports
only a limited set of native quantum gates, which means that some gates
in a quantum circuit may not be supported by the hardware. Gate decomposition
can be applied to quantum circuits to transform them to use only the
native gates supported by the hardware. For example, a Toffoli gate can be
decomposed into a series of CNOT and single-qubit gates.


### The Optimization Pipeline
QClojure provides an optimization pipeline that can be used to apply
these optimization and transformation techniques to a quantum circuit.
The optimization pipeline will apply the techniques in a specific order
to optimize and transform the quantum circuit for a specific hardware.

The optimization pipeline is used in the backends, e.g. the hardware
simulator backend, on circuit submission to optimize and transform the
quantum circuit before executing it on the backend. So normally there is no
need to call the optimization pipeline directly.

To see how the optimization pipeline works, let's call it directly. First
we need to require the `hardware-optimization` namespace.
```clj
(require '[org.soulspace.qclojure.application.hardware-optimization :as hwopt])
```
Now we can create a quantum circuit that we want to optimize and transform.
```clj
(def opt-test-circuit1
  (-> (circuit/create-circuit 5 "Optimization Test Circuit")
      (circuit/h-gate 0)
      (circuit/s-gate 0)
      (circuit/h-gate 1)
      (circuit/h-gate 4)
      (circuit/t-gate 4)
      (circuit/rx-gate 1 fm/HALF_PI)
      (circuit/rx-gate 1 fm/PI)
      (circuit/rx-gate 1 fm/HALF_PI)
      (circuit/h-gate 1)
      (circuit/s-dag-gate 0)
      (circuit/cnot-gate 0 1)
      (circuit/t-gate 1)
      (circuit/cnot-gate 1 2)
      (circuit/ry-gate 2 fm/TAU)
      (circuit/t-dag-gate 4)
      (circuit/h-gate 4)))
```
This circuit looks reasonably complex.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup opt-test-circuit1)
```
![](tutorial_files/image16.svg)

Now we can optimize and transform the circuit for the IonQ Forte device.
The optimize function takes the original circuit, a map describing the
device to optimize for and a map of options for the pipeline. In this call
we skip the topology optimization step.
```clj
(def opt-result1
  (hwopt/optimize opt-test-circuit1 (:ionq-forte hwsim/device-map) {:optimize-topology? false}))

(def opt-circuit1 (:circuit opt-result1))
```
The optimized circuit is a lot simpler than the original circuit, but it
produces the same results. As the number of qubits, the number of gates
and the depth of the circuit are relevant for the performance of the circuit
on real quantum hardware and on simulators, this optimization is useful,
sometimes even necessary, to be able to run the circuit.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup opt-circuit1)
```
![](tutorial_files/image17.svg)

The optimized circuit could be reduced so much, because after gate and
qubit optimization, the circuit is compatible with the native gates and
the topology of the IonQ Forte, which supports the following native gates.
```clj
(:native-gates (:ionq-forte hwsim/device-map))

;; =>
#{:y :rx :z :h :x :ms :rz :ry :cnot}

```
To assess the quality of the optimization, we can compare the number of
qubits, gates and the depth of the original and the optimized circuit.
```clj
(hwopt/optimization-statistics opt-test-circuit1 opt-circuit1)

;; =>
{:gates-delta -12,
 :original-circuit-depth 9,
 :optimized-swaps 0,
 :depth-delta -5,
 :optimized-operation-types #{:h :rz :cnot},
 :original-operation-types #{:rx :s-dag :s :t-dag :h :t :ry :cnot},
 :original-swaps 0,
 :gates-delta-percent -75.0,
 :operation-type-difference #{:rx :s-dag :s :t-dag :t :ry},
 :optimized-circuit-depth 4,
 :optimized-qubits 3,
 :optimized-gates 4,
 :qubits-delta-percent -40.0,
 :original-qubits 5,
 :swaps-delta-percent 0,
 :qubits-delta -2,
 :depth-delta-percent -55.55555555555556,
 :original-gates 16,
 :swaps-delta 0}

```
Now we add a toffoli gate, which is a multi-qubit gate involving two control
qubits and one target qubit. This gate is not in the native gate set of
the IonQ Forte device, so it has to be decomposed into supported gates.
```clj
(def opt-test-circuit2
  (-> (circuit/create-circuit 5 "Optimization Test Circuit")
      (circuit/h-gate 0)
      (circuit/s-gate 0)
      (circuit/h-gate 1)
      (circuit/h-gate 4)
      (circuit/t-gate 4)
      (circuit/rx-gate 1 fm/HALF_PI)
      (circuit/rx-gate 1 fm/PI)
      (circuit/rx-gate 1 fm/HALF_PI)
      (circuit/h-gate 1)
      (circuit/s-dag-gate 0)
      (circuit/cnot-gate 0 1)
      (circuit/t-gate 1)
      (circuit/cnot-gate 1 2)
      (circuit/toffoli-gate 0 1 3)
      (circuit/ry-gate 2 fm/TAU)
      (circuit/t-dag-gate 4)
      (circuit/h-gate 4)))

^kind/hiccup
(viz/visualize-circuit :hiccup opt-test-circuit2)
```
![](tutorial_files/image18.svg)

Now we can optimize and transform the circuit for the IonQ Forte device.
```clj
(def opt-result2
  (hwopt/optimize opt-test-circuit2 (:ionq-forte hwsim/device-map) {:optimize-topology? false}))

(def opt-circuit2 (:circuit opt-result2))
```
This time, the optimized circuit has a greater depth and more gates than
before, because the toffoli gate had to be decomposed to the native gates
supported by the IonQ Forte device. The process of decomposing the toffoli gate
into native gates increases the number of gates and the depth of the circuit.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup opt-circuit2)
```
![](tutorial_files/image19.svg)

Let's also print the statistics for the optimization to see the numbers.
```clj
(hwopt/optimization-statistics opt-test-circuit2 opt-circuit2)

;; =>
{:gates-delta 2,
 :original-circuit-depth 10,
 :optimized-swaps 0,
 :depth-delta 5,
 :optimized-operation-types #{:h :rz :cnot},
 :original-operation-types
 #{:rx :s-dag :s :t-dag :h :t :ry :toffoli :cnot},
 :original-swaps 0,
 :gates-delta-percent 11.76470588235294,
 :operation-type-difference #{:rx :s-dag :s :t-dag :t :ry :toffoli},
 :optimized-circuit-depth 15,
 :optimized-qubits 4,
 :optimized-gates 19,
 :qubits-delta-percent -20.0,
 :original-qubits 5,
 :swaps-delta-percent 0,
 :qubits-delta -1,
 :depth-delta-percent 50.0,
 :original-gates 17,
 :swaps-delta 0}

```
Now let's explore the effect of applying error correction codes on a simple
Bell circuit. We use the simple bit-flip code here, which can correct
a single bit-flip error. The bit-flip code uses three physical qubits
to form one logical qubit. Also, it uses ancilla qubits for error
detection and correction. So the circuit will use more qubits after
applying the error correction code.
```clj
(def bf-ecc-circuit
  (:circuit (hwopt/optimize (circuit/bell-state-circuit)
                            {}
                            {:optimize-gates? false
                             :optimize-qubits? false
                             :apply-error-correction? true
                             :error-correction-code :bit-flip
                             :optimize-topology? false
                             :transform-operations? false})))
```
The bit-flip code uses three physical qubits to form one logical qubit,
so the circuit now uses 10 qubits, 6 for the two logical qubits and 4
ancilla qubits for error detection and correction.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup bf-ecc-circuit)
```
![](tutorial_files/image20.svg)

The statistics show the increase in the number of qubits, gates and depth
of the circuit.
```clj
(hwopt/optimization-statistics (circuit/bell-state-circuit) bf-ecc-circuit)

;; =>
{:gates-delta 24,
 :original-circuit-depth 2,
 :optimized-swaps 0,
 :depth-delta 19,
 :optimized-operation-types #{:measure :h :cnot},
 :original-operation-types #{:h :cnot},
 :original-swaps 0,
 :gates-delta-percent 1200.0,
 :operation-type-difference #{},
 :optimized-circuit-depth 21,
 :optimized-qubits 10,
 :optimized-gates 26,
 :qubits-delta-percent 400.0,
 :original-qubits 2,
 :swaps-delta-percent 0,
 :qubits-delta 8,
 :depth-delta-percent 950.0,
 :original-gates 2,
 :swaps-delta 0}

```
You can try the other error correction codes as well.
Note that the Shor code and the Steane code can correct both bit-flip
and phase-flip errors, so they use more qubits than the bit-flip code.
The 5-qubit code is the smallest code that can correct an arbitrary
single-qubit error, so it uses only 5 data qubits and 4 ancilla qubits,
but it is more complex with more gates and depth than the other codes.
All error correction codes have a distance of 3, which means they can
detect up to two errors and correct a single error.

Here is a summary of the different error correction codes and their
qubit requirements:

| Error Correction Code | Data Qubits | Ancilla Qubits | Total Qubits |
|-----------------------|-------------|----------------|--------------|
| :bit-flip             | 3           | 2              | 5            | 
| :phase-flip           | 3           | 2              | 5            |
| :five-qubit           | 5           | 4              | 9            |
| :steane               | 7           | 6              | 13           |
| :shor                 | 9           | 8              | 17           |

As you can see, applying error correction codes increases the number of
qubits, gates and depth of the circuit significantly. This is a trade-off
between protecting the quantum information from errors and the resources
required to implement the error correction code. On current QPUs with
limited qubit counts this is not always an option.


## Advanced Quantum Topics

### Observables
Observables are used to measure specific properties of a quantum state.
They are represented as Hermitian operators, which have real eigenvalues
and orthogonal eigenvectors. Observables can be used to measure various
properties of a quantum state, such as the energy, spin, or position of
a particle.
In QClojure, observables can be represented using Pauli strings and
Hamiltonians, which are described in the next section.


### Pauli Strings and Hamiltonians
Pauli strings and Hamiltonians are used to represent quantum operators
in a compact and efficient way. They are used in various quantum algorithms,
e.g. the Variational Quantum Eigensolver (VQE) and the Quantum Approximate
Optimization (QAOA) algorithm.

A Pauli string is a product of Pauli operators (I, X, Y, Z) applied to
different qubits. For example, the Pauli string "XIZY" represents the
operator X on qubit 0, I on qubit 1, Z on qubit 2, and Y on qubit 3.
A Hamiltonian is a sum of Pauli strings with associated coefficients.
For example, the Hamiltonian H = 0.5 * XIZY + 1.0 * ZIXX represents
the operator 0.5 * X on qubit 0, I on qubit 1, Z on qubit 2, Y on qubit 3
plus 1.0 * Z on qubit 0, I on qubit 1, X on qubit 2, X on qubit 3.
```clj
(require '[org.soulspace.qclojure.domain.observables :as obs])

(obs/pauli-string->observable "XIZY")

;; =>
[[[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 -1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 -1.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 -1.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 1.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 -1.0]
  [-0.0 0.0]]
 [[0.0 0.0]
  [0.0 -1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 -1.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 -1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 1.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]]
 [[0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 -1.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]
  [0.0 0.0]
  [0.0 0.0]
  [-0.0 0.0]
  [-0.0 0.0]]]

```

## Result Extraction
Result extraction is the process of extracting useful information from
the results of a quantum computation. It is used to interpret the results
of a quantum computation and to extract specific properties of the quantum
state.
Result extraction can be applied to the results of a quantum computation
to extract specific properties of the quantum state, such as the expectation
value of an observable or the probability distribution of a measurement.
QClojure provides a set of result extraction techniques that can be used
to extract useful information from the results of a quantum computation.

These techniques include:

* Measurement results (sample outcomes and probabilities)
* Expectation values for observables
* Variance values for observables
* Hamiltonian energy measurements
* Probability distributions for specific basis states
* Amplitude extraction for basis states
* Complete state vector (simulation only)
* Density matrix representation (simulation only)
* Fidelity measurements against reference states
* Sample results for observables (hardware measurement simulation)

The different result types can be specified in the options map
when executing a quantum circuit on a backend, e.g.
```clj
;; =>
{:result-specs
 {:measurements {:shots 100},
  :expectation
  {:observables [[[[1.0 0.0] [0.0 0.0]] [[0.0 0.0] [-1.0 0.0]]]],
   :targets [0]},
  :variance
  {:observables [[[[1.0 0.0] [0.0 0.0]] [[0.0 0.0] [-1.0 0.0]]]],
   :targets [0]},
  :probabilities {:targets [[0 0] [1 1]]},
  :amplitudes {:basis-states [0 3]},
  :state-vector true,
  :density-matrix true}}

```

## Error Mitigation
Error mitigation is a collection of techniques used to reduce the effects
of noise in quantum computations. It is not a full error correction, but it
can improve the results of quantum computations by reducing the errors
caused by noise. Error mitigation techniques can be applied to quantum
circuits to improve the results of quantum computations.

QClojure provides a set of error mitigation techniques that can be used to
reduce the effects of noise in quantum computations.


### Readout Error Mitigation
Readout error mitigation is a technique used to reduce the effects of readout
noise in quantum computations. It is based on the idea that the readout noise
can be modeled as a matrix that describes the probability of measuring a
certain state given the true state of the qubit.


### Zero Noise Extrapolation
Zero noise extrapolation is a technique used to reduce the effects of noise
in quantum computations by extrapolating the results of the computation to
zero noise. It is based on the idea that the results of the computation can
be extrapolated to zero noise by measuring the results of the computation
with different noise levels and fitting a curve to the results.


### Symmetry Verification
Symmetry verification is a technique used to reduce the effects of noise in
quantum computations by verifying the symmetry of the quantum circuit.
It is based on the idea that the results of the computation can be verified
by checking the symmetry of the quantum circuit. If the circuit is symmetric,
the results of the computation should be the same for all qubits.


### Virtual Distillation
Virtual distillation is a technique that improves computation fidelity
by running multiple copies of quantum circuits and applying sophisticated
post-processing to extract high-fidelity results through probabilistic error
cancellation.


## Algorithms
QClojure comes with a set of predefined quantum algorithms that can be used
to solve specific problems.
These algorithms are implemented as quantum circuits and can be executed on
quantum hardware or simulated using the simulator backends.


### Deutsch Algorithm
The [Deutsch algorithm](https://en.wikipedia.org/wiki/Deutsch_algorithm) is
a simple quantum algorithm that determines whether a function is constant or
balanced. It uses a quantum circuit to evaluate the function with only one
query, compared to two queries needed for classical algorithms.
The quantum circuit uses an oracle to implement the function and applies a
Hadamard gate to the input qubit.


#### Problem Statement
Given a function f: {0, 1} → {0, 1}, the goal is to determine if f is constant
(returns the same value for both inputs) or balanced (returns 0 for one input
and 1 for the other).


#### Classical Approach
In a classical setting, we would need to evaluate the function f twice:

- If f(0) = f(1), then f is constant.
- If f(0) != f(1), then f is balanced.


#### Quantum Approach
The Deutsch algorithm allows us to determine the nature of the function with
only one evaluation by leveraging quantum superposition and interference.


#### Quantum Circuit
The Deutsch algorithm can be implemented using a quantum circuit with the following steps:

1. Initialize a qubit in the state |0⟩ and an auxiliary qubit in the state |1⟩.
2. Apply a Hadamard gate to both qubits to create superposition.
3. Apply the function f as a quantum gate, which will entangle the qubits.
4. Apply another Hadamard gate to the first qubit.
5. Measure the first qubit.

To examine the Deutsch algorithm, we need to require the `deutsch` namespace
from the `application.algorithm` package.
```clj
(require '[org.soulspace.qclojure.application.algorithm.deutsch :as deutsch])
```
Let's define a constant function and a balanced function first.

Constant function: f(x) = 1
```clj
(def constant-fn (fn [_x] true))
```
Balanced function: f(x) = x
```clj
(def balanced-fn (fn [x] x))
```
Now we can create the circuit for the Deutsch algorithm for the constant function.
```clj
(def constant-deutsch-circuit
  (deutsch/deutsch-circuit constant-fn))
```
We can visualize the circuit for the constant oracle.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup constant-deutsch-circuit)
```
![](tutorial_files/image21.svg)

The circuit shows that the Hadamard gate is applied to the input qubit, followed
by the oracle function Uf. The oracle function Uf is implemented as a series
of quantum gates that applies the constant function.
Now we can execute the Deutsch algorithm with the constant function.
We use the simulator backend to execute the circuit.
```clj
(def deutsch-constant-result
  (deutsch/deutsch-algorithm (sim/create-simulator) constant-fn {:shots 1}))
```
The result of the Deutsch algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
```clj
deutsch-constant-result

;; =>
{:algorithm "Deutsch",
 :result :constant,
 :probability-zero 0.9999999999999996,
 :circuit
 {:operations
  [{:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 0}}],
  :num-qubits 2,
  :name "Deutsch Algorithm",
  :description "Determines if function is constant or balanced"},
 :execution-result
 {:job-status :completed,
  :results
  {:final-state
   {:state-vector
    [[-0.7071067811865474 0.0]
     [0.7071067811865474 0.0]
     [0.0 0.0]
     [0.0 0.0]],
    :num-qubits 2},
   :result-types #{:measurements :probabilities},
   :circuit
   {:operations
    [{:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}],
    :num-qubits 2,
    :name "Deutsch Algorithm",
    :description "Determines if function is constant or balanced"},
   :circuit-metadata
   {:circuit-depth 3,
    :circuit-operation-count 5,
    :circuit-gate-count 5},
   :measurement-results
   {:measurement-outcomes [0],
    :measurement-probabilities
    [0.4999999999999998 0.4999999999999998 0.0 0.0],
    :empirical-probabilities {0 1},
    :shot-count 1,
    :measurement-qubits (0 1),
    :frequencies {0 1},
    :source :ideal-simulation},
   :probability-results
   {:probability-outcomes
    {0 0.4999999999999998, 1 0.4999999999999998, 2 0.0, 3 0.0},
    :target-qubits [0],
    :all-probabilities
    [0.4999999999999998 0.4999999999999998 0.0 0.0]}},
  :execution-time-ms 0,
  :job-id "sim_job_3195_1761945929896"}}

```
The result shows that the Deutsch algorithm correctly identifies the constant function.
The measurement outcome is 0, which indicates that the function is constant.
```clj
(:result deutsch-constant-result)

;; =>
:constant

```
Let's visualize the final quantum state after executing the Deutsch algorithm
with the constant function. It is contained in the execution result of the algorithm.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (get-in deutsch-constant-result [:execution-result :results :final-state]))
```
![](tutorial_files/image22.svg)

For the balanced function, we can create the circuit for the Deutsch algorithm.
```clj
(def balanced-deutsch-circuit
  (deutsch/deutsch-circuit balanced-fn))
```
We can visualize the circuit for the balanced oracle.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup balanced-deutsch-circuit)
```
![](tutorial_files/image23.svg)

Execute the Deutsch algorithm with the balanced function.
```clj
(def deutsch-balanced-result
  (deutsch/deutsch-algorithm (sim/create-simulator) balanced-fn {:shots 1}))
```
The result of the Deutsch algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
```clj
deutsch-balanced-result

;; =>
{:algorithm "Deutsch",
 :result :balanced,
 :probability-zero 0.0,
 :circuit
 {:operations
  [{:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :h, :operation-params {:target 0}}],
  :num-qubits 2,
  :name "Deutsch Algorithm",
  :description "Determines if function is constant or balanced"},
 :execution-result
 {:job-status :completed,
  :results
  {:final-state
   {:state-vector
    [[0.0 0.0]
     [0.0 0.0]
     [0.7071067811865474 0.0]
     [-0.7071067811865474 0.0]],
    :num-qubits 2},
   :result-types #{:measurements :probabilities},
   :circuit
   {:operations
    [{:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :cnot, :operation-params {:control 0, :target 1}}
     {:operation-type :h, :operation-params {:target 0}}],
    :num-qubits 2,
    :name "Deutsch Algorithm",
    :description "Determines if function is constant or balanced"},
   :circuit-metadata
   {:circuit-depth 4,
    :circuit-operation-count 5,
    :circuit-gate-count 5},
   :measurement-results
   {:measurement-outcomes [2],
    :measurement-probabilities
    [0.0 0.0 0.4999999999999998 0.4999999999999998],
    :empirical-probabilities {2 1},
    :shot-count 1,
    :measurement-qubits (0 1),
    :frequencies {2 1},
    :source :ideal-simulation},
   :probability-results
   {:probability-outcomes
    {0 0.0, 1 0.0, 2 0.4999999999999998, 3 0.4999999999999998},
    :target-qubits [0],
    :all-probabilities
    [0.0 0.0 0.4999999999999998 0.4999999999999998]}},
  :execution-time-ms 2,
  :job-id "sim_job_3196_1761945930008"}}

```
The result shows that the Deutsch algorithm correctly identifies the balanced function.
```clj
(:result deutsch-balanced-result)

;; =>
:balanced

```
Let's visualize the final quantum state after executing the Deutsch algorithm
with the balanced function.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (get-in deutsch-balanced-result [:execution-result :results :final-state]))
```
![](tutorial_files/image24.svg)


### Bernstein-Vazirani Algorithm
The [Bernstein-Vazirani algorithm](https://en.wikipedia.org/wiki/Bernstein%E2%80%93Vazirani_algorithm)
is a powerful quantum algorithm that can be
used to solve problems that are difficult for classical computers.
It is a quantum algorithm that determines a hidden binary string using a
quantum circuit to evaluate the function with only one query, compared to
n queries needed for classical algorithms.

The quantum circuit uses an oracle to implement the function and applies
a Hadamard gate to the input qubit.


#### Problem Statement
Given a function f: {0, 1}ⁿ → {0, 1} defined as f(x) = s ⨯ x
(where s is a hidden string and ⨯ denotes the dot product),
the goal is to find the hidden string s using as few evaluations of f as possible.


#### Classical Approach
In a classical setting, we would need to evaluate the function f multiple times
to determine the hidden string s. The number of evaluations required can grow
linearly with the size of the input.


#### Quantum Approach
The Bernstein-Vazirani algorithm allows us to find the hidden string s with
only one evaluation by leveraging quantum superposition and interference.


#### Quantum Circuit
The Bernstein-Vazirani algorithm can be implemented using a quantum circuit
with the following steps:

1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |1⟩.
2. Apply a Hadamard gate to all n qubits to create superposition.
3. Apply the function f as a quantum gate, which will entangle the qubits.
4. Apply another Hadamard gate to all n qubits.
5. Measure the qubits to obtain the hidden string s.

To examine the Bernstein-Vazirani algorithm, we need to require the
`bernstein-vazirani` namespace from the `application.algorithm` package.
```clj
(require '[org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv])
```
Let's define a hidden binary string first.
```clj
(def hidden-string [1 1 0])
```
Hidden binary string: 110

Now we can create the circuit for the Bernstein-Vazirani algorithm.
```clj
(def bv-circuit
  (bv/bernstein-vazirani-circuit hidden-string))
```
We can visualize the circuit for the Bernstein-Vazirani algorithm.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup bv-circuit)
```
![](tutorial_files/image25.svg)

The circuit shows that the Hadamard gate is applied to the input qubits, followed
by the oracle function Uf. The oracle function Uf is implemented as a series
of quantum gates that applies the hidden binary string.
Now we can execute the Bernstein-Vazirani algorithm with the hidden binary string.
```clj
(def bv-result
  (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string {:shots 1}))
```
The result of the Bernstein-Vazirani algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
```clj
bv-result

;; =>
{:algorithm "Bernstein-Vazirani",
 :result [1 1 0],
 :success true,
 :hidden-string [1 1 0],
 :circuit
 {:operations
  [{:operation-type :x, :operation-params {:target 3}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :h, :operation-params {:target 3}}
   {:operation-type :cnot, :operation-params {:control 0, :target 3}}
   {:operation-type :cnot, :operation-params {:control 1, :target 3}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [0]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [1]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [2]}}],
  :num-qubits 4,
  :name "Bernstein-Vazirani Algorithm",
  :description "Finds hidden bit string s with one query"},
 :execution-result
 {:job-status :completed,
  :results
  {:final-state
   {:state-vector
    [[0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.0 0.0]
     [0.7071067811865476 0.0]
     [-0.7071067811865476 0.0]
     [0.0 0.0]
     [0.0 0.0]],
    :num-qubits 4},
   :result-types #{:measurements :probabilities},
   :circuit
   {:operations
    [{:operation-type :x, :operation-params {:target 3}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :h, :operation-params {:target 3}}
     {:operation-type :cnot, :operation-params {:control 0, :target 3}}
     {:operation-type :cnot, :operation-params {:control 1, :target 3}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :measure,
      :operation-params {:measurement-qubits [0]}}
     {:operation-type :measure,
      :operation-params {:measurement-qubits [1]}}
     {:operation-type :measure,
      :operation-params {:measurement-qubits [2]}}],
    :num-qubits 4,
    :name "Bernstein-Vazirani Algorithm",
    :description "Finds hidden bit string s with one query"},
   :circuit-metadata
   {:circuit-depth 6,
    :circuit-operation-count 13,
    :circuit-gate-count 10},
   :measurement-results
   {:measurement-outcomes [13],
    :measurement-probabilities
    [0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.5000000000000001
     0.5000000000000001
     0.0
     0.0],
    :empirical-probabilities {13 1},
    :shot-count 1,
    :measurement-qubits (0 1 2 3),
    :frequencies {13 1},
    :source :ideal-simulation},
   :probability-results
   {:probability-outcomes
    {0 0.0,
     7 0.0,
     1 0.0,
     4 0.0,
     15 0.0,
     13 0.5000000000000001,
     6 0.0,
     3 0.0,
     12 0.5000000000000001,
     2 0.0,
     11 0.0,
     9 0.0,
     5 0.0,
     14 0.0,
     10 0.0,
     8 0.0},
    :target-qubits (0 1 2),
    :all-probabilities
    [0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.0
     0.5000000000000001
     0.5000000000000001
     0.0
     0.0]}},
  :execution-time-ms 27,
  :job-id "sim_job_3197_1761945930126"}}

```
The result shows that the Bernstein-Vazirani algorithm correctly identifies
the hidden binary string.
```clj
(:result bv-result)

;; =>
[1 1 0]

```
The measurement outcome is the hidden binary string, which is 110.
Let's visualize the final quantum state after executing the Bernstein-Vazirani algorithm.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (get-in bv-result [:execution-result :results :final-state]))
```
![](tutorial_files/image26.svg)

The final quantum state shows that the Bernstein-Vazirani algorithm correctly
identifies the hidden binary string. The final quantum state is a superposition
of the states that represent the hidden binary string.


### Simon's Algorithm
[Simon's algorithm](https://en.wikipedia.org/wiki/Simon's_algorithm) solves
the hidden subgroup problem for the group (Z₂)ⁿ.
Given a function f: {0,1}ⁿ → {0,1}ⁿ that is either one-to-one or two-to-one,
and if two-to-one then f(x) = f(x ⊕ s) for some hidden string s ≠ 0ⁿ,
the algorithm finds s with exponential speedup over classical methods.

The quantum circuit uses an oracle to implement the function and applies a
Hadamard gate to the input qubits.


#### Problem Statement
Given a function f: {0,1}ⁿ → {0,1}ⁿ that is promised to be periodic with a hidden period s,
the goal is to find s using fewer evaluations of f than would be possible classically.


#### Classical Approach
In a classical setting, we would need to evaluate the function f multiple times
to find the period s. The number of evaluations required can grow exponentially
with the size of the input.


#### Quantum Approach
The Simon algorithm allows us to find the hidden period s with a polynomial number
of evaluations by leveraging quantum superposition and interference.


#### Quantum Circuit
The Simon algorithm can be implemented using a quantum circuit with the following steps:

1. Initialize n qubits in the state |0⟩ and n auxiliary qubits in the state |1⟩.
2. Apply a Hadamard gate to all n qubits to create superposition.
3. Apply the function f as a quantum gate, which will entangle the qubits.
4. Measure the auxiliary qubits to obtain a set of equations that can be solved
   classically to find the hidden period s.

To examine Simon's algorithm, we need to require the `simon` namespace
from the `application.algorithm` package.
```clj
(require '[org.soulspace.qclojure.application.algorithm.simon :as simon])
```
Let's define a hidden binary string first.
```clj
(def hidden-string-simon [1 0 1])
```
Hidden binary string: 101

Now we can create the circuit for Simon's algorithm.
```clj
(def simon-circuit
  (simon/simon-circuit hidden-string-simon))
```
We can visualize the circuit for Simon's algorithm.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup simon-circuit)
```
![](tutorial_files/image27.svg)

The circuit shows that the Hadamard gate is applied to the input qubits, followed
by the oracle function Uf. The oracle function Uf is implemented as a series
of quantum gates that applies the hidden binary string.

Now we can execute Simon's algorithm with the hidden binary string.
```clj
(def simon-result
  (simon/simon-algorithm (sim/create-simulator) hidden-string-simon {:shots 1}))
```
The result of Simon's algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
```clj
simon-result

;; =>
{:algorithm "Simon",
 :result [1 0 1],
 :hidden-period [1 0 1],
 :found-period [1 0 1],
 :measurements [[0 1 0] [1 1 1]],
 :success true,
 :linear-system
 ({:equation [0 1 0],
   :dot-product-with-hidden 0,
   :dot-product-with-found 0}
  {:equation [1 1 1],
   :dot-product-with-hidden 0,
   :dot-product-with-found 0}),
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :cnot, :operation-params {:control 1, :target 3}}
   {:operation-type :cnot, :operation-params {:control 0, :target 4}}
   {:operation-type :cnot, :operation-params {:control 2, :target 4}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [3]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [4]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [5]}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [0]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [1]}}
   {:operation-type :measure,
    :operation-params {:measurement-qubits [2]}}],
  :num-qubits 6,
  :name "Simon's Algorithm",
  :description "Find hidden period of length 3"}}

```
The result shows that Simon's algorithm correctly identifies the hidden binary string.
```clj
(:result simon-result)

;; =>
[1 0 1]

```
Let's visualize the final quantum states after executing Simon's algorithm.
As Simon's algorithm can return multiple results, depending on the size of the hidden
string, we visualize the final states.
```clj
(mapv #(kind/hiccup (viz/visualize-quantum-state :hiccup (:final-state %))) (:execution-results simon-result))

;; =>
[]

```

### Grover's Search Algorithm

[Grover's algorithm](https://en.wikipedia.org/wiki/Grover%27s_algorithm)
is a quantum algorithm that provides a quadratic speedup for searching
an unsorted database. It is one of the most well-known quantum algorithms
and demonstrates the power of quantum computing for search problems.


#### Problem Statement
Given a function f: {0, 1}ⁿ → {0, 1}ⁿ that is promised to have exactly one
input x such that f(x) = 1 (the "marked" item), the goal is to
find this input x using as few evaluations of f as possible.


#### Classical Approach
In a classical setting, we would need to evaluate the function f up to
2^(n-1) times in the worst case to find the marked item. This is because we
would have to check each possible input until we find the one that satisfies
f(x) = 1.


#### Quantum Approach
Grover's search algorithm allows us to find the marked item with only
O(√2ⁿ) evaluations of f, which is a significant improvement over the
classical approach.


#### Quantum Circuit
The Grover's search algorithm can be implemented using a quantum circuit with the following steps:

1. Initialize n qubits in the state |0⟩.
2. Apply a Hadamard gate to all n qubits to create superposition, resulting in an equal superposition of all possible inputs.
3. Apply the Grover diffusion operator, which consists of:
   - Applying the oracle function f as a quantum gate, which flips the sign of the amplitude of the marked item.
   - Applying a Hadamard gate to all qubits.
   - Applying a conditional phase shift to the |0⟩ state.
   - Applying another Hadamard gate to all qubits.
4. Repeat the Grover diffusion operator O(√2ⁿ) times.

To examine Grover's algorithm, we need to require the `grover` namespace
from the `application.algorithm` package.
```clj
(require '[org.soulspace.qclojure.application.algorithm.grover :as grover])
```
Let's define a function that marks a specific input.
```clj
(def grover-oracle (grover/single-target-oracle 5))
```
Now we can define a circuit with a search space of 3 qubits for Grover's
search algorithm.
```clj
(def grover-circuit
  (grover/grover-circuit 3 grover-oracle))
```
Let's visualize the circuit for Grover's search algorithm.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup grover-circuit)
```
![](tutorial_files/image28.svg)

Now we can execute Grover's search algorithm with the defined oracle.
```clj
(def grover-result
  (grover/grover-algorithm (sim/create-simulator) 8 grover-oracle {:shots 1}))
```
Like the previous algorithms, the result of Grover's search algorithm
is a map that contains the result of the algorithm, the measurement outcome,
and the circuit used to execute the algorithm.
```clj
grover-result

;; =>
{:algorithm "Grover",
 :execution-result
 {:job-status :completed,
  :results
  {:final-state
   {:state-vector
    [[-0.08838834764831825 0.0]
     [-0.0883883476483182 0.0]
     [-0.08838834764831825 0.0]
     [-0.0883883476483182 0.0]
     [-0.08838834764831821 0.0]
     [0.9722718241315007 0.0]
     [-0.08838834764831821 0.0]
     [-0.08838834764831821 0.0]],
    :num-qubits 3},
   :result-types #{:measurements :probabilities},
   :circuit
   {:operations
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :toffoli,
      :operation-params {:control1 0, :control2 1, :target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :x, :operation-params {:target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :toffoli,
      :operation-params {:control1 0, :control2 1, :target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :x, :operation-params {:target 2}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :toffoli,
      :operation-params {:control1 0, :control2 1, :target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :x, :operation-params {:target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :toffoli,
      :operation-params {:control1 0, :control2 1, :target 2}}
     {:operation-type :h, :operation-params {:target 2}}
     {:operation-type :x, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :x, :operation-params {:target 2}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 2}}],
    :num-qubits 3,
    :name "Grover Search",
    :description "Search 8 items using 2 iterations"},
   :circuit-metadata
   {:circuit-depth 21,
    :circuit-operation-count 43,
    :circuit-gate-count 43},
   :measurement-results
   {:measurement-outcomes [5],
    :measurement-probabilities
    [0.007812499999999967
     0.007812499999999957
     0.007812499999999967
     0.007812499999999957
     0.007812499999999959
     0.9453124999999959
     0.007812499999999959
     0.007812499999999959],
    :empirical-probabilities {5 1},
    :shot-count 1,
    :measurement-qubits (0 1 2),
    :frequencies {5 1},
    :source :ideal-simulation},
   :probability-results
   {:probability-outcomes
    {0 0.007812499999999967,
     1 0.007812499999999957,
     2 0.007812499999999967,
     3 0.007812499999999957,
     4 0.007812499999999959,
     5 0.9453124999999959,
     6 0.007812499999999959,
     7 0.007812499999999959},
    :target-qubits (0 1 2),
    :all-probabilities
    [0.007812499999999967
     0.007812499999999957
     0.007812499999999967
     0.007812499999999957
     0.007812499999999959
     0.9453124999999959
     0.007812499999999959
     0.007812499999999959]}},
  :execution-time-ms 6,
  :job-id "sim_job_3199_1761945930362"},
 :probability 1,
 :target-indices (5),
 :success true,
 :result 5,
 :measurement-statistics
 {:frequencies {5 1},
  :target-counts 1,
  :total-shots 1,
  :success-probability 1},
 :iterations 2,
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :toffoli,
    :operation-params {:control1 0, :control2 1, :target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 0}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :x, :operation-params {:target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :toffoli,
    :operation-params {:control1 0, :control2 1, :target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 0}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :x, :operation-params {:target 2}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :toffoli,
    :operation-params {:control1 0, :control2 1, :target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 0}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :x, :operation-params {:target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :toffoli,
    :operation-params {:control1 0, :control2 1, :target 2}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :x, :operation-params {:target 0}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :x, :operation-params {:target 2}}
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}],
  :num-qubits 3,
  :name "Grover Search",
  :description "Search 8 items using 2 iterations"},
 :search-space-size 8}

```
The result shows that Grover's search algorithm correctly identifies the marked item.
```clj
(:result grover-result)

;; =>
5

```

#### Quantum Fourier Transform
The [Quantum Fourier Transform (QFT)](https://en.wikipedia.org/wiki/Quantum_Fourier_transform)
is a quantum algorithm that performs the discrete Fourier transform on a quantum state.
It is a key component of many quantum algorithms, including Shor's algorithm.

The QFT transforms a quantum state into its frequency domain representation,
allowing us to extract periodicity and other properties of the quantum state.


#### Problem Statement
Given a quantum state |ψ⟩, the goal is to apply the QFT to the state
and obtain a new quantum state that represents the frequency domain of |ψ⟩.


#### Classical Approach
In a classical setting, the discrete Fourier transform can be computed
using classical algorithms, but it requires O(N log N) time complexity,
where N is the number of elements in the input.


#### Quantum Approach
The QFT allows us to compute the discrete Fourier transform in O(log² N)
time complexity, which is a significant improvement over the classical approach.


#### Quantum Circuit
The QFT can be implemented using a quantum circuit with the following steps:

1. Initialize n qubits in the state |ψ⟩.
2. Apply a series of controlled phase gates to the qubits, which introduces
   phase shifts based on the relative positions of the qubits.
3. Apply a series of Hadamard gates to the qubits, which creates
   superposition states that represent the frequency domain.
4. Reverse the order of the qubits to obtain the final quantum state
   that represents the frequency domain of |ψ⟩.

Let's require the `quantum-fourier-transform` namespace to explore the QFT.
```clj
(require '[org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft])
```
We can create a quantum circuit for the QFT with a specified number of qubits.
```clj
(def qft-circuit
  (qft/quantum-fourier-transform-circuit 3))
```
We can visualize the circuit for the Quantum Fourier Transform.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup qft-circuit)
```
![](tutorial_files/image29.svg)

The circuit shows that the QFT applies a series of controlled phase gates
and Hadamard gates to the qubits, transforming the quantum state into its
frequency domain representation.
```clj
(def qft-result
  (backend/execute-circuit (sim/create-simulator) qft-circuit {:shots 1}))

qft-result

;; =>
{:job-status :completed,
 :results
 {:final-state
  {:num-qubits 3,
   :state-vector
   [[0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]
    [0.3535533905932737 0.0]]},
  :result-types #{},
  :circuit
  {:operations
   [{:operation-type :h, :operation-params {:target 0}}
    {:operation-type :crz,
     :operation-params
     {:control 1, :target 0, :angle 1.5707963267948966}}
    {:operation-type :crz,
     :operation-params
     {:control 2, :target 0, :angle 0.7853981633974483}}
    {:operation-type :h, :operation-params {:target 1}}
    {:operation-type :crz,
     :operation-params
     {:control 2, :target 1, :angle 1.5707963267948966}}
    {:operation-type :h, :operation-params {:target 2}}
    {:operation-type :swap, :operation-params {:qubit1 0, :qubit2 2}}],
   :num-qubits 3,
   :name "QFT",
   :description "Quantum Fourier Transform"},
  :circuit-metadata
  {:circuit-depth 7,
   :circuit-operation-count 7,
   :circuit-gate-count 7}},
 :execution-time-ms 3,
 :job-id "sim_job_3200_1761945930473"}

```
The circuit for the QFT can also be used to implement the inverse QFT,
which is the reverse operation of the QFT.
```clj
(def inverse-qft-circuit
  (qft/inverse-quantum-fourier-transform-circuit 3))
```
We can visualize the circuit for the inverse Quantum Fourier Transform.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup inverse-qft-circuit)
```
![](tutorial_files/image30.svg)

The inverse QFT circuit applies the inverse operations of the controlled phase gates
and Hadamard gates to the qubits, transforming the quantum state back to its
original representation.
The inverse QFT can be used to recover the original quantum state from its
frequency domain representation.


### Quantum Phase Estimation
The [Quantum Phase Estimation (QPE)](https://en.wikipedia.org/wiki/Quantum_phase_estimation_algorithm)
is a quantum algorithm that estimates the eigenvalues of a unitary operator.
It is used in many quantum algorithms, including Shor's algorithm and the Quantum Fourier Transform.


#### Problem Statement
Given a unitary operator U and an eigenstate |ψ⟩ of U, the goal is to estimate
the phase θ such that U|ψ⟩ = e^(2πiθ)|ψ⟩, where θ is the eigenvalue of U.


#### Classical Approach
In a classical setting, estimating the phase of a unitary operator requires
multiple evaluations of the operator and can be computationally expensive.


#### Quantum Approach
The Quantum Phase Estimation algorithm allows us to estimate the phase θ
with high precision using a quantum circuit that requires only a polynomial
number of evaluations of the unitary operator U.


#### Quantum Circuit
The Quantum Phase Estimation algorithm can be implemented using a quantum circuit with the following steps:

1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |ψ⟩.
2. Apply a Hadamard gate to the auxiliary qubit to create superposition.
3. Apply controlled-U gates to the auxiliary qubit, where U is the unitary operator.
4. Apply the inverse Quantum Fourier Transform (QFT) to the auxiliary qubit.
5. Measure the auxiliary qubit to obtain the estimated phase θ.
```clj
(require '[org.soulspace.qclojure.application.algorithm.quantum-phase-estimation :as qpe])
```


### Quantum Period Finding
The [Quantum Period Finding](https://en.wikipedia.org/wiki/Quantum_period_finding)
is a quantum algorithm that finds the period of a function.
It is used in many quantum algorithms, including Shor's algorithm and the Quantum Fourier Transform


#### Problem Statement
Given a function f: {0, 1}ⁿ → {0, 1}ⁿ that is periodic with period r,
the goal is to find the period r using as few evaluations of f as possible.


#### Classical Approach
In a classical setting, finding the period of a function requires
multiple evaluations of the function and can be computationally expensive.


#### Quantum Approach
The Quantum Period Finding algorithm allows us to find the period r
with high precision using a quantum circuit that requires only a polynomial
number of evaluations of the function f.


#### Quantum Circuit
The Quantum Period Finding algorithm can be implemented using a quantum circuit with the following steps:

1. Initialize n qubits in the state |0⟩ and an auxiliary qubit in the state |1⟩.
2. Apply a Hadamard gate to all n qubits to create superposition.
3. Apply the function f as a quantum gate, which will entangle the qubits.
4. Apply the Quantum Fourier Transform (QFT) to the qubits.
5. Measure the qubits to obtain a value that can be used to find the period r.

To explore the Quantum Period Finding algorithm, we need to require the
`quantum-period-finding` namespace.
```clj
(require '[org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf])
```

### Shor's Algorithm
[Shor's algorithm](https://en.wikipedia.org/wiki/Shor%27s_algorithm) is a quantum algorithm
that can factor large integers in polynomial time.
It is one of the most famous quantum algorithms and has significant implications for
cryptography, as it can break many classical encryption schemes.
Shor's algorithm uses the Quantum Fourier Transform and Quantum Phase Estimation to find the period
of a function related to the integer to be factored.

Shor's algorithm uses lots of qubits and is thus not really feasible to run on
current quantum hardware and simulators. For small integers, it can be run on
simulators, but the results may not be very interesting. For a n-bit integer,
Shor's algorithm requires at least about 2n + 3 qubits.


#### Problem Statement
Given a composite integer N, the goal is to find its prime factors using
as few evaluations of a function as possible.


#### Classical Approach
In a classical setting, factoring large integers is a computationally
hard problem. The best-known classical algorithms for factoring have
exponential time complexity, making them impractical for large N.


#### Quantum Approach
Shor's algorithm allows us to factor large integers in polynomial time
by leveraging quantum superposition, interference, and the quantum
Fourier transform.


#### Quantum Circuit
The Shor's algorithm can be implemented using a quantum circuit with
the following steps:

1. Choose a random integer a such that 1 < a < N.
2. Use the quantum period-finding algorithm to find the order r of a modulo N.
   This involves:
   - Initialize n qubits in the state |0⟩.
   - Apply a Hadamard gate to all n qubits to create superposition.
   - Apply the modular exponentiation function a^x mod N as a quantum gate,
     which will entangle the qubits.
   - Apply the quantum Fourier transform to the qubits.
   - Measure the qubits to obtain a value that can be used to find the
     order r.
3. If r is even and a^(r/2) != -1 mod N, then compute the factors:
   - Compute gcd(a^(r/2) - 1, N and gcd(a^(r/2) + 1, N).
4. If the factors are non-trivial, return them as the prime factors of N.
5. If the order r is odd or if the above conditions are not met, repeat
   the process with a different random integer a.

Let's examine Shor's algorithm by requiring the `shor` namespace.
```clj
(require '[org.soulspace.qclojure.application.algorithm.shor :as shor])
```
We can use Shor's algorithm to factor a composite integer, e.g. 15.
The algorithm should return the prime factors 3 and 5, may run for quite
a while depending on the random numbers chosen.

(def shor-result (shor/shor-algorithm (sim/create-simulator) 15 {:shots 10}))

The result of Shor's algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.

shor-result

The result shows that Shor's algorithm correctly factors the composite integer 15
into its prime factors 3 and 5.
The measurement outcome is the prime factors of 15, which are 3 and 5.

(:result shor-result)


### HHL Algorithm
The [HHL algorithm](https://en.wikipedia.org/wiki/HHL_algorithm) is a
quantum algorithm for solving linear systems of equations.
It is named after its inventors Harrow, Hassidim, and Lloyd.
The HHL algorithm can solve a system of linear equations in polynomial time,
which is a significant improvement over classical algorithms that require
exponential time for large systems.
The result is an approximation of the solution vector x that satisfies
the equations Ax = b, where A is a hermitian matrix and b is a vector of constants.

The current implementation works for a hermitian n x n matrix A and a vector b.


#### Problem Statement
Given a system of linear equations Ax = b, where A is a hermitian matrix,
x is the vector of unknowns, and b is the vector of constants, the goal is to
find the vector x that satisfies the equations using as few evaluations
of the matrix A as possible.


#### Classical Approach
In a classical setting, solving a system of linear equations requires
O(N³) time complexity, where N is the number of equations in the system.
Classical algorithms such as Gaussian elimination or LU decomposition
can be used to solve the system, but they are computationally expensive
for large systems.


#### Quantum Approach
The HHL algorithm allows us to solve a system of linear equations in
O(log N) time complexity by leveraging quantum superposition, interference,
and the quantum Fourier transform.


#### Quantum Circuit
The HHL algorithm can be implemented using a quantum circuit with the following steps:

1. Prepare the input state |b⟩, which represents the vector of constants b.
2. Use the quantum phase estimation algorithm to estimate the eigenvalues
   of the matrix A. This involves:
   - Initialize n qubits in the state |0⟩.
   - Apply a Hadamard gate to all n qubits to create superposition.
   - Apply controlled-U gates to the qubits, where U is the unitary operator
     that represents the matrix A.
   - Apply the inverse quantum Fourier transform to the qubits.
   - Measure the qubits to obtain the estimated eigenvalues of A.
3. Use the estimated eigenvalues to compute the inverse of the matrix A.
4. Apply the inverse of the matrix A to the input state |b⟩ to obtain the
   output state |x⟩, which represents the solution to the system of
   linear equations Ax = b.
5. Measure the output state |x⟩ to obtain the vector of unknowns x that
   satisfies the equations.

Let's examine the HHL algorithm by requiring the `hhl` namespace.
```clj
(require '[org.soulspace.qclojure.application.algorithm.hhl :as hhl])
```
We can use the HHL algorithm to solve a system of linear equations.
For example, let's solve the system of equations represented by the
positive definite matrix A and the vector b.

IMPORTANT: HHL works best with positive definite hermitian matrices
(all eigenvalues > 0). 

Positive definite matrix A
```clj
(def hhl-matrix [[3 1] [1 2]])
```
Vector b
```clj
(def hhl-vector [7 5])
```
Now we can create the circuit for the HHL algorithm with the given matrix
and vector.
```clj
(def hhl-circuit
  (hhl/hhl-circuit hhl-matrix hhl-vector 4 1))
```
We can visualize the circuit for the HHL algorithm.
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup hhl-circuit)
```
![](tutorial_files/image31.svg)

The circuit shows that the HHL algorithm applies a series of controlled-U gates
to the qubits, which represent the matrix A, and applies the inverse quantum
Fourier transform to the qubits. The circuit also applies the inverse of the
matrix A to the input state |b⟩ to obtain the output state |x⟩, which represents
the solution to the system of linear equations Ax = b.
```clj
(def hhl-result
  (hhl/hhl-algorithm (sim/create-simulator)
                     hhl-matrix
                     hhl-vector
                     {:shots 10000}))
```
The result of the HHL algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
We don't show the full result here, as it contains 20000 measurement outcomes
for the 20000 shots requested. We only show the probability results
and the final result.
```clj
(get-in hhl-result [:execution-result :results :probability-results])

;; =>
{:probability-outcomes
 {0 0.15670125878642163,
  62 2.935927677620301E-4,
  7 0.012332924504854964,
  59 0.001036013325433737,
  20 0.0035966795732933782,
  58 0.001007387238289471,
  60 3.2860055681845963E-4,
  27 0.0020305861178501246,
  1 0.17207148706378567,
  24 0.0023017184830934857,
  55 0.0015417180055429785,
  39 0.006292308420844369,
  46 0.0011599815215200028,
  4 0.014126966354813652,
  54 0.0015746061568597976,
  15 0.002254532527345905,
  48 0.013113194036970206,
  50 0.007028845712803338,
  21 0.0034528645878414746,
  31 5.786026804023501E-4,
  32 0.07994962182980689,
  40 0.005362295397107446,
  56 0.0011743461648436116,
  33 0.08779157503254365,
  13 0.00252667350667409,
  22 0.003086228067445202,
  36 0.00720763589531308,
  41 0.004977762617291755,
  43 0.00401822937090493,
  61 3.3219852394926784E-4,
  29 6.511091069405663E-4,
  44 0.00131078220399193,
  6 0.011944811403382492,
  28 6.440570913641818E-4,
  51 0.0064450607988867925,
  25 0.002426896096956261,
  34 0.032531303055072736,
  17 0.023149055866472274,
  3 0.07065060025023143,
  12 0.0025691331198241826,
  2 0.06376135398794261,
  23 0.0030217672908642375,
  47 0.0011502716976254615,
  35 0.036046224617465,
  19 0.012632319165818117,
  57 0.001238212294365435,
  11 0.007875729566973664,
  9 0.009756414729891838,
  5 0.014992864704205184,
  14 0.002273563782179206,
  45 0.0012891191360582105,
  53 0.001761665606041567,
  26 0.0019744789870473627,
  16 0.025701860312461595,
  38 0.006094291532338006,
  30 5.754418248135803E-4,
  10 0.008213546045545387,
  18 0.013776537597094544,
  52 0.001835040598619066,
  42 0.004190584717114991,
  37 0.007649420767451619,
  63 2.952054491848731E-4,
  8 0.010510098978330603,
  49 0.011810742789016474},
 :target-qubits (0 1 2 3 4 5),
 :all-probabilities
 [0.15670125878642163
  0.17207148706378567
  0.06376135398794261
  0.07065060025023143
  0.014126966354813652
  0.014992864704205184
  0.011944811403382492
  0.012332924504854964
  0.010510098978330603
  0.009756414729891838
  0.008213546045545387
  0.007875729566973664
  0.0025691331198241826
  0.00252667350667409
  0.002273563782179206
  0.002254532527345905
  0.025701860312461595
  0.023149055866472274
  0.013776537597094544
  0.012632319165818117
  0.0035966795732933782
  0.0034528645878414746
  0.003086228067445202
  0.0030217672908642375
  0.0023017184830934857
  0.002426896096956261
  0.0019744789870473627
  0.0020305861178501246
  6.440570913641818E-4
  6.511091069405663E-4
  5.754418248135803E-4
  5.786026804023501E-4
  0.07994962182980689
  0.08779157503254365
  0.032531303055072736
  0.036046224617465
  0.00720763589531308
  0.007649420767451619
  0.006094291532338006
  0.006292308420844369
  0.005362295397107446
  0.004977762617291755
  0.004190584717114991
  0.00401822937090493
  0.00131078220399193
  0.0012891191360582105
  0.0011599815215200028
  0.0011502716976254615
  0.013113194036970206
  0.011810742789016474
  0.007028845712803338
  0.0064450607988867925
  0.001835040598619066
  0.001761665606041567
  0.0015746061568597976
  0.0015417180055429785
  0.0011743461648436116
  0.001238212294365435
  0.001007387238289471
  0.001036013325433737
  3.2860055681845963E-4
  3.3219852394926784E-4
  2.935927677620301E-4
  2.952054491848731E-4]}


(:result hhl-result)

;; =>
[1.9202228645139552 1.4104657343751934]

```
The result shows that the HHL algorithm correctly aproximates the solution of
the system of linear equations represented by the matrix A and the vector b.

The measurement outcome is an approximation to the vector of unknowns x that
satisfies the equations.

Let's visualize the final quantum state after executing the HHL algorithm.
```clj
^kind/hiccup
(viz/visualize-quantum-state :hiccup (get-in hhl-result [:execution-result :results :final-state]))
```
![](tutorial_files/image32.svg)

The final quantum state shows the approximation of the solution of the
system of linear equations Ax = b. The final quantum state is a superposition
of the states that represent the solution to the system of equations.
```clj
(hhl/solve (sim/create-simulator)
           hhl-matrix
           hhl-vector
           {:shots 10000})

;; =>
[1.923272883109446 1.4055035308028698]

```

### Variational Algorithms
Variational algorithms are a class of hybrid quantum-classical algorithms
that use a parameterized quantum circuit to solve optimization problems.
They are particularly useful for near-term quantum computers, which have
limited qubit counts and are prone to noise and errors.
Variational algorithms use a quantum circuit to prepare a trial state,
and a classical optimization algorithm to minimize a cost function
that depends on the parameters of the quantum circuit.
The goal is to find the optimal parameters that minimize the cost function,
which can be used to solve a variety of problems, including quantum chemistry,
machine learning, and optimization.

QClojure provides a framework for implementing variational algorithms,
including the Variational Quantum Eigensolver (VQE) and the Quantum Approximate
Optimization Algorithm (QAOA). The framework can be used to define
parameterized quantum circuits, cost functions, and optimization strategies.

The variational algorithm framework provides different gradient based and
gradient free optimization methods, including

* gradient based methods:
  * :gradient-descent - a simple gradient descent optimizer
  * :adam - the Adam optimizer (Adaptive Moment Estimation)
  * :quantum-natural-gradient - the quantum natural gradient optimizer using Quantum Fisher Information
* gradient free methods:
  * :nelder-mead - the Nelder-Mead optimizer (also known as the downhill simplex method)
  * :powell - the Powell optimizer
  * :cmaes - the CMA-ES optimizer (Covariance Matrix Adaptation Evolution Strategy)
  * :bobyqa - the BOBYQA optimizer (Bound Optimization BY Quadratic Approximation)

QClojure provides the parameter-shift rule to compute gradients of
parameterized quantum circuits. The parameter-shift rule is a method for
computing the gradient of a quantum circuit with respect to its parameters
by evaluating the circuit at shifted parameter values. This allows us to
compute gradients without the need for backpropagation, which is not
directly applicable to quantum circuits.


### Variational Quantum Eigensolver (VQE) Algorithm
The [Variational Quantum Eigensolver (VQE)](https://en.wikipedia.org/wiki/Variational_quantum_eigensolver)
is a hybrid quantum-classical algorithm used to find the ground state energy
of a quantum system. It is particularly useful for simulating molecular systems
and materials, where the Hamiltonian of the system can be represented as a
sum of Pauli operators.

The VQE algorithm is also used in quantum machine learning and optimization problems,
where it can be used to find the optimal parameters for a quantum circuit
that represents a trial state.

The VQE algorithm uses a parameterized quantum circuit to prepare a trial state,
and a classical optimization algorithm to minimize the expectation value of
the Hamiltonian with respect to the trial state.


#### Problem Statement
Given a Hamiltonian H of a quantum system, the goal is to find the ground state
energy E₀ of the system, which is the lowest eigenvalue of H, using as few evaluations
of the Hamiltonian as possible.


#### Classical Approach
In a classical setting, finding the ground state energy of a quantum system
requires solving the eigenvalue problem for the Hamiltonian H, which can be
computationally expensive for large systems. Classical algorithms such as
diagonalization or iterative methods can be used to find the ground state energy,
but they are limited by the size of the system and the complexity of the Hamiltonian.


#### Quantum Approach
The VQE algorithm allows us to find the ground state energy of a quantum system
using a hybrid quantum-classical approach. It leverages the power of quantum
computing to prepare trial states and measure the expectation value of the
Hamiltonian, while using classical optimization algorithms to minimize the
expectation value and find the optimal parameters for the trial state.


#### Quantum Circuit
The VQE algorithm can be implemented using a quantum circuit with the following steps:

1. Prepare a parameterized quantum circuit that represents the trial state.
   The circuit can be represented as a series of quantum gates that depend on
   a set of parameters θ.
2. Initialize the parameters θ to some initial values.
3. Execute the quantum circuit to prepare the trial state |ψ(θ)⟩.
4. Measure the expectation value of the Hamiltonian H with respect to
   the trial state |ψ(θ)⟩. This involves applying the Hamiltonian as a quantum gate
   and measuring the qubits to obtain the expectation value ⟨H⟩.
5. Use a classical optimization algorithm to update the parameters θ based on the
   measured expectation value ⟨H⟩. The optimization algorithm can be
   gradient-based or gradient-free, depending on the problem.
6. Repeat steps 3-5 until convergence, i.e., until the expectation value ⟨H⟩
   does not change significantly or a maximum number of iterations is reached.
7. The final expectation value ⟨H⟩ represents the ground state energy E₀ of the quantum system.

To explore the VQE algorithm, we need to require the `variational-quantum-eigensolver`
namespace from the `application.algorithm` package. To create a simple Hamiltonian,
we also require the `domain.hamiltonian` namespace.
```clj
(require '[org.soulspace.qclojure.domain.hamiltonian :as ham])

(require '[org.soulspace.qclojure.application.algorithm.vqe :as vqe])
```
Let's start with a simpler Hamiltonian for demonstration,
which can be represented as a sum of Pauli operators.
```clj
(def simple-hamiltonian [(ham/pauli-term -1.0 "IIII")
                         (ham/pauli-term 0.1 "ZIII")])

simple-hamiltonian

;; =>
[{:coefficient -1.0, :pauli-string "IIII"}
 {:coefficient 0.1, :pauli-string "ZIII"}]

```
Let's run the VQE algorithm with the simple Hamiltonian.
We'll use the hardware-efficient ansatz and the gradient descent optimizer
with parameter shift gradients.
```clj
(def simple-vqe-result
  (vqe/variational-quantum-eigensolver
   (sim/create-simulator)
   {:hamiltonian simple-hamiltonian
    :ansatz-type :hardware-efficient
    :num-qubits 4
    :num-layers 1
    :max-iterations 200
    :tolerance 1e-4
    :optimization-method :gradient-descent  ; Use gradient descent optimization
    :learning-rate 0.01
    :shots 1}))

simple-vqe-result

;; =>
{:algorithm "Variational Quantum Eigensolver",
 :config
 {:ansatz-type :hardware-efficient,
  :optimization-method :gradient-descent,
  :tolerance 1.0E-4,
  :max-iterations 200,
  :shots 1,
  :num-qubits 4,
  :hamiltonian
  [{:coefficient -1.0, :pauli-string "IIII"}
   {:coefficient 0.1, :pauli-string "ZIII"}],
  :learning-rate 0.01,
  :num-layers 1},
 :ansatz-type :hardware-efficient,
 :optimization
 {:optimal-energy -0.9005910290330039,
  :total-runtime-ms 3897,
  :algorithm :vqe,
  :initial-parameters
  [-0.0909614392156199
   -0.057943650543641256
   0.03935474267266553
   -0.04551926226364074
   0.018578302182667567
   -0.08103230170141565
   -0.09087239970737511
   0.06834262478165559
   -0.0011230264041670185
   -0.0038273440160551397
   -0.09907097781695232
   0.010477178542720608],
  :enhanced-features
  {:optimization-method :gradient-descent,
   :convergence-monitored true,
   :algorithm-config-driven true},
  :convergence-analysis
  {:energy-std 3.345401179591329E-6,
   :final-gradient-norm 0.010842955887583093,
   :relative-improvement 1.1639689323497674E-5,
   :energy-trajectory-length 10,
   :energy-improvement 1.0482477772688625E-5,
   :final-reason :converged,
   :monotonic-decrease? true,
   :converged true,
   :total-iterations 9,
   :convergence-quality :good,
   :initial-energy -0.9005805465552312,
   :final-energy -0.9005910290330039,
   :function-evaluations 0,
   :convergence-rate 1.0482477772688626E-6},
  :history
  [{:iteration 0,
    :energy -0.9005805465552312,
    :gradients
    [0.009068360867171514
     0.005767181886389205
     -7.850462293418876E-17
     0.0
     1.5700924586837752E-16
     2.355138688025663E-16
     3.1401849173675503E-16
     0.0
     -2.355138688025663E-16
     3.1401849173675503E-16
     1.5700924586837752E-16
     0.0],
    :parameters
    [-0.0909614392156199
     -0.057943650543641256
     0.03935474267266553
     -0.04551926226364074
     0.018578302182667567
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.0011230264041670185
     -0.0038273440160551397
     -0.09907097781695232
     0.010477178542720608]}
   {:iteration 1,
    :energy -0.900581702082158,
    :gradients
    [0.009077346161580785
     0.005772867802883918
     0.0
     -1.5700924586837752E-16
     1.5700924586837752E-16
     0.0
     3.1401849173675503E-16
     -7.850462293418876E-17
     0.0
     -7.850462293418876E-17
     2.355138688025663E-16
     1.5700924586837752E-16],
    :parameters
    [-0.09105212282429162
     -0.058001322362505145
     0.03935474267266553
     -0.04551926226364074
     0.018578302182667567
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.001123026404167016
     -0.0038273440160551428
     -0.09907097781695232
     0.010477178542720608]}
   {:iteration 2,
    :energy -0.9005828598968075,
    :gradients
    [0.009086340193661512
     0.005778559163078304
     -4.710277376051326E-16
     -1.5700924586837752E-16
     -3.1401849173675503E-16
     1.5700924586837752E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     -3.1401849173675503E-16
     -1.5700924586837752E-16
     0.0],
    :parameters
    [-0.09114289628590742
     -0.058059051040533986
     0.03935474267266553
     -0.04551926226364074
     0.018578302182667567
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.001123026404167016
     -0.003827344016055142
     -0.09907097781695232
     0.010477178542720606]}
   {:iteration 3,
    :energy -0.9005840200036597,
    :gradients
    [0.00909534297141567
     0.00578425597169834
     3.1401849173675503E-16
     -1.5700924586837752E-16
     0.0
     4.710277376051326E-16
     -1.5700924586837752E-16
     -2.355138688025663E-16
     -1.5700924586837752E-16
     1.5700924586837752E-16
     -3.1401849173675503E-16
     -1.5700924586837752E-16],
    :parameters
    [-0.09123375968784404
     -0.05811683663216477
     0.03935474267266554
     -0.04551926226364074
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.0011230264041670146
     -0.003827344016055139
     -0.09907097781695232
     0.010477178542720606]}
   {:iteration 4,
    :energy -0.9005851824072023,
    :gradients
    [0.009104354502854422
     0.005789958233471654
     0.0
     1.5700924586837752E-16
     0.0
     7.850462293418876E-17
     3.1401849173675503E-16
     -1.5700924586837752E-16
     -7.850462293418876E-17
     3.1401849173675503E-16
     2.355138688025663E-16
     0.0],
    :parameters
    [-0.0913247131175582
     -0.058174679191881754
     0.03935474267266554
     -0.04551926226364074
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.001123026404167013
     -0.0038273440160551406
     -0.09907097781695232
     0.010477178542720608]}
   {:iteration 5,
    :energy -0.9005863471119339,
    :gradients
    [0.009113374795990576
     0.005795665953128697
     5.495323605393213E-16
     0.0
     0.0
     5.495323605393213E-16
     8.635508522760764E-16
     1.5700924586837752E-16
     1.5700924586837752E-16
     -1.5700924586837752E-16
     1.5700924586837752E-16
     1.5700924586837752E-16],
    :parameters
    [-0.09141575666258675
     -0.05823257877421647
     0.03935474267266554
     -0.04551926226364074
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737511
     0.06834262478165559
     -0.0011230264041670122
     -0.0038273440160551436
     -0.09907097781695232
     0.010477178542720608]}
   {:iteration 6,
    :energy -0.9005875141223606,
    :gradients
    [0.009122403858845109
     0.0058013791354032205
     -3.1401849173675503E-16
     -3.925231146709438E-16
     -1.5700924586837752E-16
     0.0
     5.495323605393213E-16
     -2.355138688025663E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     1.5700924586837752E-16],
    :parameters
    [-0.09150689041054666
     -0.05829053543374776
     0.03935474267266553
     -0.04551926226364074
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737512
     0.06834262478165559
     -0.0011230264041670137
     -0.003827344016055142
     -0.09907097781695232
     0.010477178542720606]}
   {:iteration 7,
    :energy -0.9005886834429973,
    :gradients
    [0.009131441699444959
     0.005807097785029917
     -7.850462293418876E-17
     1.5700924586837752E-16
     -1.5700924586837752E-16
     2.355138688025663E-16
     7.065416064076989E-16
     2.355138688025663E-16
     -2.355138688025663E-16
     0.0
     -7.850462293418876E-17
     2.355138688025663E-16],
    :parameters
    [-0.09159811444913511
     -0.05834854922510179
     0.03935474267266553
     -0.04551926226364073
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737512
     0.06834262478165559
     -0.0011230264041670122
     -0.00382734401605514
     -0.09907097781695232
     0.010477178542720605]}
   {:iteration 8,
    :energy -0.9005898550783682,
    :gradients
    [0.009140488325822722
     0.005812821906748268
     -3.1401849173675503E-16
     3.925231146709438E-16
     -3.1401849173675503E-16
     3.1401849173675503E-16
     -2.355138688025663E-16
     3.1401849173675503E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     -1.5700924586837752E-16
     0.0],
    :parameters
    [-0.09168942886612956
     -0.05840662020295209
     0.03935474267266553
     -0.04551926226364073
     0.01857830218266757
     -0.08103230170141565
     -0.09087239970737514
     0.06834262478165559
     -0.0011230264041670098
     -0.00382734401605514
     -0.09907097781695232
     0.010477178542720603]}
   {:iteration 9,
    :energy -0.9005910290330039,
    :gradients
    [0.009149543746015776
     0.00581855150529893
     -1.5700924586837752E-16
     -3.1401849173675503E-16
     2.355138688025663E-16
     3.925231146709438E-16
     -7.850462293418876E-17
     4.710277376051326E-16
     0.0
     -7.850462293418876E-17
     0.0
     1.5700924586837752E-16],
    :parameters
    [-0.0917808337493878
     -0.058464748422019576
     0.03935474267266553
     -0.04551926226364074
     0.018578302182667573
     -0.08103230170141565
     -0.09087239970737514
     0.06834262478165559
     -0.0011230264041670083
     -0.0038273440160551384
     -0.09907097781695232
     0.010477178542720603]}],
  :reason :converged,
  :objective-kind :hamiltonian,
  :success true,
  :iterations 9,
  :initial-energy -0.9005805465552312,
  :optimal-parameters
  [-0.0917808337493878
   -0.058464748422019576
   0.03935474267266553
   -0.04551926226364074
   0.018578302182667573
   -0.08103230170141565
   -0.09087239970737514
   0.06834262478165559
   -0.0011230264041670083
   -0.0038273440160551384
   -0.09907097781695232
   0.010477178542720603]},
 :success true,
 :analysis
 {:initial-energy -0.9005805465552312,
  :energy-improvement 1.0482477772688625E-5,
  :convergence-achieved true},
 :result -0.9005910290330039,
 :timing
 {:execution-time-ms 3897,
  :start-time 1761945931112,
  :end-time 1761945935009},
 :circuit
 {:operations
  [{:operation-type :rx,
    :operation-params {:target 0, :angle -0.0917808337493878}}
   {:operation-type :ry,
    :operation-params {:target 0, :angle -0.058464748422019576}}
   {:operation-type :rz,
    :operation-params {:target 0, :angle 0.03935474267266553}}
   {:operation-type :rx,
    :operation-params {:target 1, :angle -0.04551926226364074}}
   {:operation-type :ry,
    :operation-params {:target 1, :angle 0.018578302182667573}}
   {:operation-type :rz,
    :operation-params {:target 1, :angle -0.08103230170141565}}
   {:operation-type :rx,
    :operation-params {:target 2, :angle -0.09087239970737514}}
   {:operation-type :ry,
    :operation-params {:target 2, :angle 0.06834262478165559}}
   {:operation-type :rz,
    :operation-params {:target 2, :angle -0.0011230264041670083}}
   {:operation-type :rx,
    :operation-params {:target 3, :angle -0.0038273440160551384}}
   {:operation-type :ry,
    :operation-params {:target 3, :angle -0.09907097781695232}}
   {:operation-type :rz,
    :operation-params {:target 3, :angle 0.010477178542720603}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 1, :target 2}}
   {:operation-type :cnot, :operation-params {:control 2, :target 3}}],
  :num-qubits 4,
  :name "Hardware Efficient Ansatz"},
 :parameter-count 12,
 :hamiltonian
 {:pauli-terms 0, :grouped-pauli-terms nil, :classical-bound nil},
 :results
 {:optimal-energy -0.9005910290330039,
  :optimal-parameters
  [-0.0917808337493878
   -0.058464748422019576
   0.03935474267266553
   -0.04551926226364074
   0.018578302182667573
   -0.08103230170141565
   -0.09087239970737514
   0.06834262478165559
   -0.0011230264041670083
   -0.0038273440160551384
   -0.09907097781695232
   0.010477178542720603],
  :success true,
  :iterations 9,
  :function-evaluations nil}}

```
We can use the VQE algorithm to find the ground state energy of a quantum system.
For example, let's find the ground state energy of molecular hydrogen,
for which the Hamiltonian can be represented as a sum of Pauli operators.
```clj
(def h2-hamiltonian (vqe/molecular-hydrogen-hamiltonian))

h2-hamiltonian

;; =>
[{:coefficient -1.1395602, :pauli-string "IIII"}
 {:coefficient 0.39793742, :pauli-string "IIIZ"}
 {:coefficient -0.39793742, :pauli-string "IIZI"}
 {:coefficient 0.39793742, :pauli-string "IZII"}
 {:coefficient -0.39793742, :pauli-string "ZIII"}
 {:coefficient -0.0112801, :pauli-string "IIZZ"}
 {:coefficient -0.0112801, :pauli-string "IZIZ"}
 {:coefficient -0.0112801, :pauli-string "IZZI"}
 {:coefficient -0.0112801, :pauli-string "ZIIZ"}
 {:coefficient -0.0112801, :pauli-string "ZIZI"}
 {:coefficient -0.0112801, :pauli-string "ZZZZ"}
 {:coefficient -0.1809312, :pauli-string "XXII"}
 {:coefficient -0.1809312, :pauli-string "YYII"}
 {:coefficient -0.1809312, :pauli-string "IIXX"}
 {:coefficient -0.1809312, :pauli-string "IIYY"}]

```
Let's run the VQE algorithm with the defined Hamiltonian.
The result of the VQE algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.

There are different ansatz types available, such as

* `:hardware-efficient` - a general-purpose ansatz that uses a series of parameterized
* `:uccsd` - the unitary coupled cluster ansatz, which is suitable for chemistry problems.
* `:chemistry-inspired` - an ansatz inspired by the structure of molecular Hamiltonians.
* `:symmetry-preserving` - an ansatz that preserves the symmetries of the Hamiltonian.
* `:custom` - allows you to define your own ansatz circuit.

The choice of ansatz depends on the problem and the available quantum hardware.
For a problem like molecular hydrogen, the `:uccsd` ansatz is a good choice,
as it is specifically designed for chemistry problems and can capture the
relevant correlations in the electronic structure of molecules.

We'll compare different optimization methods to show their performance.
These gradient-based optimizers are supported: `:gradient-descent`, `:adam` and `:quantum-natural-gradient`.

For gradient-free optimizers, we can use the following methods: `:nelder-mead`,
`:powell`, `:cmaes` and `:bobyqa`. 

Here we use the adam optimizer, which is a gradient-based optimization method
that uses the parameter shift rule to compute gradients. 
```clj
(def vqe-result
  (vqe/variational-quantum-eigensolver
   (sim/create-simulator)
   {:hamiltonian           (vqe/molecular-hydrogen-hamiltonian)
    :ansatz-type           :uccsd
    :num-qubits            4
    :num-excitations       2
    :num-layers            2
    :max-iterations        200
    :tolerance             1e-5
    :learning-rate         0.1
    :optimization-method   :adam
    :shots 1}))

vqe-result

;; =>
{:algorithm "Variational Quantum Eigensolver",
 :config
 {:num-excitations 2,
  :ansatz-type :uccsd,
  :optimization-method :adam,
  :tolerance 1.0E-5,
  :max-iterations 200,
  :shots 1,
  :num-qubits 4,
  :hamiltonian
  [{:coefficient -1.1395602, :pauli-string "IIII"}
   {:coefficient 0.39793742, :pauli-string "IIIZ"}
   {:coefficient -0.39793742, :pauli-string "IIZI"}
   {:coefficient 0.39793742, :pauli-string "IZII"}
   {:coefficient -0.39793742, :pauli-string "ZIII"}
   {:coefficient -0.0112801, :pauli-string "IIZZ"}
   {:coefficient -0.0112801, :pauli-string "IZIZ"}
   {:coefficient -0.0112801, :pauli-string "IZZI"}
   {:coefficient -0.0112801, :pauli-string "ZIIZ"}
   {:coefficient -0.0112801, :pauli-string "ZIZI"}
   {:coefficient -0.0112801, :pauli-string "ZZZZ"}
   {:coefficient -0.1809312, :pauli-string "XXII"}
   {:coefficient -0.1809312, :pauli-string "YYII"}
   {:coefficient -0.1809312, :pauli-string "IIXX"}
   {:coefficient -0.1809312, :pauli-string "IIYY"}],
  :learning-rate 0.1,
  :num-layers 2},
 :ansatz-type :uccsd,
 :optimization
 {:optimal-energy -1.131258769410312,
  :total-runtime-ms 61356,
  :algorithm :vqe,
  :initial-parameters [-0.05041856591400667 -0.08538249526459525],
  :enhanced-features
  {:optimization-method :adam,
   :convergence-monitored true,
   :algorithm-config-driven true},
  :convergence-analysis
  {:energy-std 0.0038181182158071943,
   :final-gradient-norm 0.04147449813859708,
   :relative-improvement 0.0123755274269448,
   :energy-trajectory-length 200,
   :energy-improvement 0.013828785414629063,
   :final-reason :max-iterations,
   :monotonic-decrease? true,
   :converged false,
   :total-iterations 200,
   :convergence-quality :excellent,
   :initial-energy -1.117429983995683,
   :final-energy -1.131258769410312,
   :function-evaluations 0,
   :convergence-rate 6.914392707314531E-5},
  :history
  [{:iteration 0,
    :energy -1.117429983995683,
    :gradients [3.2410117220188833E-5 0.008874520350787323],
    :parameters [-0.05041856591400667 -0.08538249526459525]}
   {:iteration 1,
    :energy -1.1174395440893299,
    :gradients [7.212064301243763E-5 0.008942571476202532],
    :parameters [-0.05042180692572869 -0.08626994729967398]}
   {:iteration 2,
    :energy -1.117449251671004,
    :gradients [1.1183387666637239E-4 0.00901130287794266],
    :parameters [-0.05042901899002993 -0.08716420444729424]}
   {:iteration 3,
    :energy -1.1174591097361182,
    :gradients [1.51552740586922E-4 0.00908071910724131],
    :parameters [-0.05044020237769657 -0.0880653347350885]}
   {:iteration 4,
    :energy -1.1174691213242627,
    :gradients [1.912801528030106E-4 0.009150824744004016],
    :parameters [-0.05045535765175527 -0.08897340664581263]}
   {:iteration 5,
    :energy -1.1174792895200383,
    :gradients [2.31019027040096E-4 0.009221624396484489],
    :parameters [-0.050474485667035565 -0.08988848912021304]}
   {:iteration 6,
    :energy -1.1174896174538789,
    :gradients [2.7077227278281615E-4 0.009293122700964631],
    :parameters [-0.05049758756973957 -0.09081065155986148]}
   {:iteration 7,
    :energy -1.117500108302899,
    :gradients [3.1054279533795014E-4 0.009365324321406443],
    :parameters [-0.05052466479701785 -0.09173996382995794]}
   {:iteration 8,
    :energy -1.117510765291736,
    :gradients [3.5033349589533747E-4 0.009438233949091854],
    :parameters [-0.050555719076551646 -0.09267649626209859]}
   {:iteration 9,
    :energy -1.1175215916934174,
    :gradients [3.9014727157717923E-4 0.009511856302248874],
    :parameters [-0.05059075242614118 -0.09362031965700778]}
   {:iteration 10,
    :energy -1.1175325908302198,
    :gradients [4.2998701549095E-4 0.009586196125660173],
    :parameters [-0.0506297671532989 -0.09457150528723267]}
   {:iteration 11,
    :energy -1.117543766074557,
    :gradients [4.698556167747736E-4 0.009661258190245435],
    :parameters [-0.05067276585484799 -0.09553012489979869]}
   {:iteration 12,
    :energy -1.1175551208498602,
    :gradients [5.097559606373036E-4 0.00973704729263932],
    :parameters [-0.05071975141652547 -0.09649625071882323]}
   {:iteration 13,
    :energy -1.1175666586314776,
    :gradients [5.496909283946201E-4 0.009813568254741787],
    :parameters [-0.050770727012589195 -0.09746995544808716]}
   {:iteration 14,
    :energy -1.117578382947582,
    :gradients [5.896633975019462E-4 0.009890825923248952],
    :parameters [-0.050825696105428655 -0.09845131227356134]}
   {:iteration 15,
    :energy -1.117590297380086,
    :gradients [6.296762415820661E-4 0.00996882516916981],
    :parameters [-0.05088466244517885 -0.09944039486588624]}
   {:iteration 16,
    :energy -1.1176024055655682,
    :gradients [6.697323304474634E-4 0.010047570887323188],
    :parameters [-0.05094763006933706 -0.10043727738280323]}
   {:iteration 17,
    :energy -1.1176147111962125,
    :gradients [7.098345301153946E-4 0.010127067995802793],
    :parameters [-0.051014603302381806 -0.10144203447153555]}
   {:iteration 18,
    :energy -1.1176272180207472,
    :gradients [7.499857028212345E-4 0.010207321435437284],
    :parameters [-0.05108558675539335 -0.10245474127111583]}
   {:iteration 19,
    :energy -1.1176399298454087,
    :gradients [7.901887070283673E-4 0.010288336169216069],
    :parameters [-0.051160585325675474 -0.10347547341465956]}
   {:iteration 20,
    :energy -1.1176528505349013,
    :gradients [8.304463974266174E-4 0.01037011718169786],
    :parameters [-0.05123960419637831 -0.10450430703158116]}
   {:iteration 21,
    :energy -1.1176659840133762,
    :gradients [8.707616249273818E-4 0.010452669478397235],
    :parameters [-0.05132264883612097 -0.10554131874975095]}
   {:iteration 22,
    :energy -1.1176793342654137,
    :gradients [9.111372366570351E-4 0.010535998085143414],
    :parameters [-0.05140972499861371 -0.10658658569759068]}
   {:iteration 23,
    :energy -1.1176929053370224,
    :gradients [9.515760759435848E-4 0.010620108047417996],
    :parameters [-0.05150083872227941 -0.10764018550610502]}
   {:iteration 24,
    :energy -1.1177067013366406,
    :gradients [9.920809822860535E-4 0.010705004429666778],
    :parameters [-0.051595996329873765 -0.10870219631084682]}
   {:iteration 25,
    :energy -1.1177207264361537,
    :gradients [0.0010326547913390926 0.010790692314586792],
    :parameters [-0.05169520442810237 -0.1097726967538135]}
   {:iteration 26,
    :energy -1.1177349848719171,
    :gradients [0.0010733003348721596 0.010877176802387242],
    :parameters [-0.05179846990723628 -0.11085176598527217]}
   {:iteration 27,
    :energy -1.1177494809457955,
    :gradients [0.0011140204407310513 0.010964463010022223],
    :parameters [-0.051905799940723495 -0.1119394836655109]}
   {:iteration 28,
    :energy -1.1177642190262005,
    :gradients [0.0011548179327936262 0.011052556070394976],
    :parameters [-0.0520172019847966 -0.11303592996651313]}
   {:iteration 29,
    :energy -1.1177792035491496,
    :gradients [0.0011956956309145386 0.011141461131536119],
    :parameters [-0.05213268377807596 -0.11414118557355263]}
   {:iteration 30,
    :energy -1.1177944390193306,
    :gradients [0.0012366563508676147 0.011231183355754373],
    :parameters [-0.05225225334116741 -0.11525533168670624]}
   {:iteration 31,
    :energy -1.1178099300111723,
    :gradients [0.0012777029042758282 0.011321727918752448],
    :parameters [-0.052375918976254175 -0.11637845002228167]}
   {:iteration 32,
    :energy -1.1178256811699274,
    :gradients [0.0013188380985404873 0.011413100008717014],
    :parameters [-0.05250368926668176 -0.11751062281415692]}
   {:iteration 33,
    :energy -1.1178416972127678,
    :gradients [0.0013600647367599055 0.011505304825372726],
    :parameters [-0.05263557307653581 -0.11865193281502862]}
   {:iteration 34,
    :energy -1.1178579829298867,
    :gradients [0.001401385617638178 0.0115983475790141],
    :parameters [-0.0527715795502118 -0.11980246329756589]}
   {:iteration 35,
    :energy -1.117874543185606,
    :gradients [0.0014428035353927042 0.011692233489486526],
    :parameters [-0.05291171811197562 -0.1209622980554673]}
   {:iteration 36,
    :energy -1.1178913829194963,
    :gradients [0.0014843212796439667 0.01178696778515314],
    :parameters [-0.05305599846551489 -0.12213152140441595]}
   {:iteration 37,
    :energy -1.1179085071475126,
    :gradients [0.001525941635310806 0.011882555701818693],
    :parameters [-0.05320443059347929 -0.12331021818293127]}
   {:iteration 38,
    :energy -1.1179259209631276,
    :gradients [0.0015676673824837144 0.011979002481612426],
    :parameters [-0.053357024757010374 -0.12449847375311314]}
   {:iteration 39,
    :energy -1.117943629538477,
    :gradients [0.0016095012962989146 0.012076313371844099],
    :parameters [-0.05351379149525875 -0.12569637400127437]}
   {:iteration 40,
    :energy -1.117961638125517,
    :gradients [0.0016514461468008193 0.012174493623818882],
    :parameters [-0.05367474162488864 -0.12690400533845878]}
   {:iteration 41,
    :energy -1.1179799520571905,
    :gradients [0.0016935046987924017 0.01227354849161615],
    :parameters [-0.05383988623956872 -0.12812145470084066]}
   {:iteration 42,
    :energy -1.1179985767485914,
    :gradients [0.0017356797116880776 0.012373483230824127],
    :parameters [-0.05400923670944796 -0.12934880955000227]}
   {:iteration 43,
    :energy -1.1180175176981526,
    :gradients [0.0017779739393369127 0.012474303097243322],
    :parameters [-0.054182804680616765 -0.1305861578730847]}
   {:iteration 44,
    :energy -1.11803678048883,
    :gradients [0.0018203901298615324 0.012576013345540319],
    :parameters [-0.05436060207455046 -0.13183358818280902]}
   {:iteration 45,
    :energy -1.1180563707892934,
    :gradients [0.0018629310254637424 0.012678619227867042],
    :parameters [-0.05454264108753661 -0.13309118951736304]}
   {:iteration 46,
    :energy -1.1180762943551368,
    :gradients [0.0019055993622387885 0.012782125992435741],
    :parameters [-0.054728934190082984 -0.13435905144014976]}
   {:iteration 47,
    :energy -1.118096557030075,
    :gradients [0.0019483978699676322 0.012886538882045925],
    :parameters [-0.05491949412630686 -0.13563726403939333]}
   {:iteration 48,
    :energy -1.1181171647471735,
    :gradients [0.001991329271903262 0.01299186313257363],
    :parameters [-0.05511433391330362 -0.13692591792759792]}
   {:iteration 49,
    :energy -1.118138123530061,
    :gradients [0.002034396284550723 0.013098103971408537],
    :parameters [-0.05531346684049395 -0.1382251042408553]}
   {:iteration 50,
    :energy -1.1181594394941576,
    :gradients [0.0020776016174243804 0.013205266615852954],
    :parameters [-0.05551690646894902 -0.13953491463799614]}
   {:iteration 51,
    :energy -1.1181811188479085,
    :gradients [0.0021209479728092656 0.013313356271463173],
    :parameters [-0.05572466663069146 -0.14085544129958144]}
   {:iteration 52,
    :energy -1.1182031678940247,
    :gradients [0.0021644380454993424 0.013422378130348427],
    :parameters [-0.05593676142797239 -0.14218677692672776]}
   {:iteration 53,
    :energy -1.1182255930307252,
    :gradients [0.002208074522529648 0.013532337369420243],
    :parameters [-0.056153205232522325 -0.1435290147397626]}
   {:iteration 54,
    :energy -1.1182484007529834,
    :gradients [0.0022518600828961895 0.013643239148590277],
    :parameters [-0.05637401268477529 -0.1448822484767046]}
   {:iteration 55,
    :energy -1.118271597653777,
    :gradients [0.0022957973972634357 0.013755088608913701],
    :parameters [-0.056599198693064914 -0.14624657239156363]}
   {:iteration 56,
    :energy -1.1182951904253475,
    :gradients [0.0023398891276600327 0.013867890870681987],
    :parameters [-0.05682877843279126 -0.14762208125245502]}
   {:iteration 57,
    :energy -1.118319185860451,
    :gradients [0.0023841379271592896 0.01398165103146471],
    :parameters [-0.057062767345557264 -0.1490088703395232]}
   {:iteration 58,
    :energy -1.1183435908536266,
    :gradients [0.0024285464395504023 0.014096374164090078],
    :parameters [-0.05730118113827319 -0.15040703544266967]}
   {:iteration 59,
    :energy -1.1183684124024544,
    :gradients [0.0024731172989971156 0.014212065314571165],
    :parameters [-0.057544035782228234 -0.15181667285907868]}
   {:iteration 60,
    :energy -1.118393657608814,
    :gradients [0.0025178531296802115 0.014328729499979688],
    :parameters [-0.057791347512127944 -0.1532378793905358]}
   {:iteration 61,
    :energy -1.118419333680158,
    :gradients [0.002562756545424142 0.014446371706253054],
    :parameters [-0.05804313282509597 -0.15467075234053376]}
   {:iteration 62,
    :energy -1.1184454479307695,
    :gradients [0.002607830149319265 0.014564996885949292],
    :parameters [-0.05829940847963838 -0.15611538951115908]}
   {:iteration 63,
    :energy -1.118472007783025,
    :gradients [0.0026530765333211584 0.014684609955936033],
    :parameters [-0.058560191494570306 -0.157571889199754]}
   {:iteration 64,
    :energy -1.1184990207686643,
    :gradients [0.002698498277833599 0.014805215795017784],
    :parameters [-0.05882549914790242 -0.15904035019534762]}
   {:iteration 65,
    :energy -1.1185264945300404,
    :gradients [0.0027440979512926506 0.014926819241503226],
    :parameters [-0.05909534897568578 -0.16052087177484942]}
   {:iteration 66,
    :energy -1.118554436821382,
    :gradients [0.0027898781097125884 0.015049425090703587],
    :parameters [-0.059369758770815045 -0.16201355369899975]}
   {:iteration 67,
    :energy -1.1185828555100519,
    :gradients [0.002835841296236228 0.015173038092371664],
    :parameters [-0.0596487465817863 -0.1635184962080701]}
   {:iteration 68,
    :energy -1.1186117585777826,
    :gradients [0.0028819900406605982 0.015297662948066112],
    :parameters [-0.05993233071140992 -0.16503580001730728]}
   {:iteration 69,
    :energy -1.1186411541219383,
    :gradients [0.0029283268589448755 0.015423304308450405],
    :parameters [-0.060220529715475986 -0.1665655663121139]}
   {:iteration 70,
    :energy -1.118671050356737,
    :gradients [0.00297485425270921 0.015549966770529642],
    :parameters [-0.06051336240137047 -0.16810789674295892]}
   {:iteration 71,
    :energy -1.1187014556144927,
    :gradients [0.0030215747087121983 0.015677654874802393],
    :parameters [-0.060810847826641395 -0.1696628934200119]}
   {:iteration 72,
    :energy -1.1187323783468333,
    :gradients [0.0030684906983103016 0.015806373102353038],
    :parameters [-0.06111300529751262 -0.17123065890749214]}
   {:iteration 73,
    :energy -1.1187638271259102,
    :gradients [0.0031156046769067417 0.015936125871867983],
    :parameters [-0.061419854367343645 -0.17281129621772745]}
   {:iteration 74,
    :energy -1.1187958106456142,
    :gradients [0.0031629190833751216 0.01606691753657364],
    :parameters [-0.06173141483503432 -0.17440490880491424]}
   {:iteration 75,
    :energy -1.1188283377227526,
    :gradients [0.003210436339467657 0.016198752381102074],
    :parameters [-0.06204770674337183 -0.1760116005585716]}
   {:iteration 76,
    :energy -1.1188614172982432,
    :gradients [0.0032581588492143028 0.016331634618279994],
    :parameters [-0.062368750377318594 -0.17763147579668181]}
   {:iteration 77,
    :energy -1.1188950584382729,
    :gradients [0.003306088998285607 0.016465568385837688],
    :parameters [-0.06269456626224003 -0.17926463925850983]}
   {:iteration 78,
    :energy -1.118929270335449,
    :gradients [0.003354229153351802 0.016600557743042893],
    :parameters [-0.06302517516206858 -0.1809111960970936]}
   {:iteration 79,
    :energy -1.118964062309941,
    :gradients [0.003402581661420538 0.016736606667246766],
    :parameters [-0.06336059807740377 -0.1825712518713979]}
   {:iteration 80,
    :energy -1.1189994438105955,
    :gradients [0.0034511488491485555 0.016873719050355864],
    :parameters [-0.06370085624354582 -0.18424491253812256]}
   {:iteration 81,
    :energy -1.1190354244160343,
    :gradients [0.0034999330221392234 0.01701189869521844],
    :parameters [-0.06404597112846068 -0.18593228444315815]}
   {:iteration 82,
    :energy -1.1190720138357368,
    :gradients [0.003548936464218887 0.017151149311921833],
    :parameters [-0.06439596443067461 -0.18763347431268]}
   {:iteration 83,
    :energy -1.1191092219110979,
    :gradients [0.003598161436694211 0.017291474514016098],
    :parameters [-0.06475085807709649 -0.18934858924387218]}
   {:iteration 84,
    :energy -1.1191470586164634,
    :gradients [0.003647610177587077 0.017432877814636337],
    :parameters [-0.06511067422076591 -0.19107773669527378]}
   {:iteration 85,
    :energy -1.119185534060139,
    :gradients [0.0036972849008448222 0.017575362622550626],
    :parameters [-0.06547543523852462 -0.1928210244767374]}
   {:iteration 86,
    :energy -1.1192246584853731,
    :gradients [0.003747187795539655 0.01771893223810948],
    :parameters [-0.0658451637286091 -0.19457856073899246]}
   {:iteration 87,
    :energy -1.119264442271319,
    :gradients [0.0037973210250382273 0.017863589849113383],
    :parameters [-0.06621988250816306 -0.1963504539628034]}
   {:iteration 88,
    :energy -1.1193048959339553,
    :gradients [0.003847686726145153 0.018009338526580914],
    :parameters [-0.06659961461066688 -0.19813681294771474]}
   {:iteration 89,
    :energy -1.1193460301269849,
    :gradients [0.0038982870082408696 0.018156181220434598],
    :parameters [-0.06698438328328139 -0.19993774680037282]}
   {:iteration 90,
    :energy -1.1193878556426944,
    :gradients [0.00394912395238009 0.018304120755083145],
    :parameters [-0.06737421198410548 -0.20175336492241627]}
   {:iteration 91,
    :energy -1.1194303834127834,
    :gradients [0.004000199610372828 0.01845315982492014],
    :parameters [-0.06776912437934349 -0.2035837769979246]}
   {:iteration 92,
    :energy -1.1194736245091512,
    :gradients [0.004051516003846267 0.018603300989715837],
    :parameters [-0.06816914434038077 -0.20542909298041662]}
   {:iteration 93,
    :energy -1.119517590144651,
    :gradients [0.004103075123279784 0.018754546669927596],
    :parameters [-0.06857429594076539 -0.2072894230793882]}
   {:iteration 94,
    :energy -1.119562291673795,
    :gradients [0.0041548789270156325 0.01890689914189603],
    :parameters [-0.06898460345309337 -0.20916487774638096]}
   {:iteration 95,
    :energy -1.1196077405934206,
    :gradients [0.004206929340246859 0.019060360532960455],
    :parameters [-0.06940009134579493 -0.21105556766057057]}
   {:iteration 96,
    :energy -1.1196539485433183,
    :gradients [0.00425922825398026 0.01921493281646551],
    :parameters [-0.06982078427981962 -0.2129616037138666]}
   {:iteration 97,
    :energy -1.119700927306792,
    :gradients [0.004311777523975627 0.019370617806666535],
    :parameters [-0.07024670710521765 -0.21488309699551317]}
   {:iteration 98,
    :energy -1.1197486888111896,
    :gradients [0.004364578969660026 0.019527417153544335],
    :parameters [-0.07067788485761521 -0.21682015877617983]}
   {:iteration 99,
    :energy -1.1197972451283702,
    :gradients [0.004417634373014918 0.019685332337510356],
    :parameters [-0.07111434275458121 -0.21877290049153425]}
   {:iteration 100,
    :energy -1.1198466084751202,
    :gradients [0.004470945477439408 0.019844364664015932],
    :parameters [-0.07155610619188271 -0.2207414337252853]}
   {:iteration 101,
    :energy -1.1198967912135078,
    :gradients [0.004524513986591996 0.020004515258058214],
    :parameters [-0.07200320073962665 -0.22272587019168688]}
   {:iteration 102,
    :energy -1.119947805851181,
    :gradients [0.004578341563195099 0.02016578505857903],
    :parameters [-0.07245565213828585 -0.2247263217174927]}
   {:iteration 103,
    :energy -1.1199996650416033,
    :gradients [0.004632429827825457 0.02032817481276858],
    :parameters [-0.07291348629460535 -0.2267429002233506]}
   {:iteration 104,
    :energy -1.1200523815842192,
    :gradients [0.004686780357673132 0.02049168507026034],
    :parameters [-0.0733767292773879 -0.22877571770462746]}
   {:iteration 105,
    :energy -1.120105968424556,
    :gradients [0.0047413946852733445 0.020656316177226405],
    :parameters [-0.07384540731315523 -0.23082488621165348]}
   {:iteration 106,
    :energy -1.1201604386542559,
    :gradients [0.0047962742972111425 0.020822068270366725],
    :parameters [-0.07431954678168257 -0.23289051782937611]}
   {:iteration 107,
    :energy -1.120215805511022,
    :gradients [0.004851420632802845 0.020988941270793256],
    :parameters [-0.07479917421140368 -0.23497272465641278]}
   {:iteration 108,
    :energy -1.120272082378508,
    :gradients [0.004906835082748269 0.02115693487781461],
    :parameters [-0.07528431627468396 -0.23707161878349212]}
   {:iteration 109,
    :energy -1.1203292827861018,
    :gradients [0.004962518987751093 0.0213260485626158],
    :parameters [-0.07577499978295879 -0.23918731227127357]}
   {:iteration 110,
    :energy -1.120387420408651,
    :gradients [0.005018473637122256 0.02149628156182933],
    :parameters [-0.0762712516817339 -0.24131991712753514]}
   {:iteration 111,
    :energy -1.1204465090660862,
    :gradients [0.005074700267343955 0.02166763287101414],
    :parameters [-0.07677309904544613 -0.2434695452837181]}
   {:iteration 112,
    :energy -1.1205065627229547,
    :gradients [0.005131200060617935 0.02184010123801977],
    :parameters [-0.07728056907218053 -0.2456363085708195]}
   {:iteration 113,
    :energy -1.120567595487869,
    :gradients [0.005187974143377198 0.022013685156258192],
    :parameters [-0.07779368907824233 -0.24782031869462148]}
   {:iteration 114,
    :energy -1.1206296216128535,
    :gradients [0.00524502358477008 0.02218838285786857],
    :parameters [-0.07831248649258005 -0.2500216872102473]}
   {:iteration 115,
    :energy -1.1206926554925907,
    :gradients [0.0053023493951265864 0.02236419230677982],
    :parameters [-0.07883698885105705 -0.25224052549603415]}
   {:iteration 116,
    :energy -1.120756711663564,
    :gradients [0.005359952524383584 0.022541111191686294],
    :parameters [-0.07936722379056971 -0.2544769447267121]}
   {:iteration 117,
    :energy -1.1208218048030978,
    :gradients [0.005417833860493204 0.022719136918903288],
    :parameters [-0.07990321904300807 -0.25673105584588074]}
   {:iteration 118,
    :energy -1.120887949728277,
    :gradients [0.005475994227796536 0.022898266605148643],
    :parameters [-0.08044500242905739 -0.2590029695377711]}
   {:iteration 119,
    :energy -1.1209551613947615,
    :gradients [0.005534434385373307 0.023078497070206525],
    :parameters [-0.08099260185183704 -0.26129279619828594]}
   {:iteration 120,
    :energy -1.1210234548954785,
    :gradients [0.005593155025361407 0.023259824829515472],
    :parameters [-0.08154604529037437 -0.2636006459053066]}
   {:iteration 121,
    :energy -1.121092845459189,
    :gradients [0.005652156771255075 0.02344224608665268],
    :parameters [-0.08210536079291052 -0.26592662838825815]}
   {:iteration 122,
    :energy -1.12116334844893,
    :gradients [0.005711440176165379 0.02362575672572817],
    :parameters [-0.08267057647003602 -0.26827085299692344]}
   {:iteration 123,
    :energy -1.1212349793603338,
    :gradients [0.0057710057210653394 0.023810352303693348],
    :parameters [-0.08324172048765256 -0.27063342866949625]}
   {:iteration 124,
    :energy -1.121307753819799,
    :gradients [0.0058308538130007964 0.023996028042564226],
    :parameters [-0.0838188210597591 -0.27301446389986556]}
   {:iteration 125,
    :energy -1.1213816875825355,
    :gradients [0.005890984783272407 0.024182778821555048],
    :parameters [-0.08440190644105919 -0.275414066704122]}
   {:iteration 126,
    :energy -1.121456796530458,
    :gradients [0.005951398885605543 0.024370599169136496],
    :parameters [-0.08499100491938642 -0.2778323445862775]}
   {:iteration 127,
    :energy -1.1215330966699428,
    :gradients [0.0060120962942741855 0.024559483255009038],
    :parameters [-0.08558614480794698 -0.28026940450319116]}
   {:iteration 128,
    :energy -1.121610604129418,
    :gradients [0.00607307710221572 0.024749424882007213],
    :parameters [-0.0861873544373744 -0.28272535282869204]}
   {:iteration 129,
    :energy -1.121689335156815,
    :gradients [0.006134341319111963 0.024940417477926222],
    :parameters [-0.08679466214759597 -0.2852002953168928]}
   {:iteration 130,
    :energy -1.121769306116854,
    :gradients [0.006195888869451831 0.025132454087283318],
    :parameters [-0.08740809627950717 -0.2876943370646854]}
   {:iteration 131,
    :energy -1.1218505334881614,
    :gradients [0.006257719590561184 0.025325527363010778],
    :parameters [-0.08802768516645236 -0.2902075824734137]}
   {:iteration 132,
    :energy -1.1219330338602254,
    :gradients [0.006319833230615719 0.0255196295580942],
    :parameters [-0.08865345712550848 -0.2927401352097148]}
   {:iteration 133,
    :energy -1.1220168239301749,
    :gradients [0.006382229446631094 0.025714752517145993],
    :parameters [-0.08928544044857005 -0.2952920981655242]}
   {:iteration 134,
    :energy -1.1221019204993834,
    :gradients [0.006444907802427144 0.02591088766793236],
    :parameters [-0.08992366339323316 -0.2978635734172388]}
   {:iteration 135,
    :energy -1.1221883404698967,
    :gradients [0.006507867766573106 0.02610802601284911],
    :parameters [-0.09056815417347587 -0.30045466218403205]}
   {:iteration 136,
    :energy -1.122276100840656,
    :gradients [0.006571108710312422 0.026306158120361214],
    :parameters [-0.09121894095013318 -0.303065464785317]}
   {:iteration 137,
    :energy -1.1223652187035653,
    :gradients [0.006634629905462274 0.026505274116395527],
    :parameters [-0.09187605182116441 -0.3056960805973531]}
   {:iteration 138,
    :energy -1.1224557112393225,
    :gradients [0.006698430522300393 0.02670536367570972],
    :parameters [-0.09253951481171065 -0.30834660800899266]}
   {:iteration 139,
    :energy -1.1225475957130866,
    :gradients [0.006762509627432719 0.02690641601323585],
    :parameters [-0.09320935786394069 -0.31101714437656364]}
   {:iteration 140,
    :energy -1.1226408894699216,
    :gradients [0.006826866181641276 0.027108419875398075],
    :parameters [-0.09388560882668397 -0.31370778597788723]}
   {:iteration 141,
    :energy -1.1227356099300434,
    :gradients [0.006891499037714612 0.027311363531420954],
    :parameters [-0.0945682954448481 -0.316418627965427]}
   {:iteration 142,
    :energy -1.1228317745838494,
    :gradients [0.0069564069382653795 0.027515234764639896],
    :parameters [-0.09525744534861956 -0.3191497643185691]}
   {:iteration 143,
    :energy -1.1229294009867286,
    :gradients [0.0070215885135402065 0.02772002086379565],
    :parameters [-0.0959530860424461 -0.32190128779503313]}
   {:iteration 144,
    :energy -1.1230285067536647,
    :gradients [0.007087042279200217 0.027925708614353888],
    :parameters [-0.09665524489380012 -0.3246732898814127]}
   {:iteration 145,
    :energy -1.1231291095536016,
    :gradients [0.00715276663411191 0.028132284289829015],
    :parameters [-0.09736394912172014 -0.32746586074284806]}
   {:iteration 146,
    :energy -1.1232312271035858,
    :gradients [0.007218759858107896 0.02833973364314177],
    :parameters [-0.09807922578513133 -0.33027908917183096]}
   {:iteration 147,
    :energy -1.1233348771626783,
    :gradients [0.007285020109751237 0.028548041898003026],
    :parameters [-0.09880110177094212 -0.33311306253614514]}
   {:iteration 148,
    :energy -1.1234400775256208,
    :gradients [0.007351545424092261 0.028757193740347044],
    :parameters [-0.09952960378191725 -0.3359678667259455]}
   {:iteration 149,
    :energy -1.1235468460162825,
    :gradients [0.00741833371041312 0.02896717330980242],
    :parameters [-0.10026475832432648 -0.33884358609998017]}
   {:iteration 150,
    :energy -1.1236552004808378,
    :gradients [0.007485382749975496 0.029177964191244365],
    :parameters [-0.10100659169536778 -0.3417403034309604]}
   {:iteration 151,
    :energy -1.1237651587807125,
    :gradients [0.007552690193760922 0.029389549406395175],
    :parameters [-0.10175512997036533 -0.34465809985008483]}
   {:iteration 152,
    :energy -1.1238767387852773,
    :gradients [0.007620253560220524 0.029601911405519703],
    :parameters [-0.10251039898974142 -0.34759705479072434]}
   {:iteration 153,
    :energy -1.123989958364285,
    :gradients [0.007688070233009538 0.029815032059214243],
    :parameters [-0.10327242434576347 -0.3505572459312763]}
   {:iteration 154,
    :energy -1.124104835380049,
    :gradients [0.007756137458746784 0.030028892650279106],
    :parameters [-0.10404123136906443 -0.35353874913719774]}
   {:iteration 155,
    :energy -1.1242213876793665,
    :gradients [0.007824452344762218 0.030243473865733905],
    :parameters [-0.1048168451149391 -0.35654163840222564]}
   {:iteration 156,
    :energy -1.1243396330851776,
    :gradients [0.007893011856862366 0.030458755788930366],
    :parameters [-0.10559929034941533 -0.359565985788799]}
   {:iteration 157,
    :energy -1.1244595893879585,
    :gradients [0.007961812817104258 0.030674717891821978],
    :parameters [-0.10638859153510156 -0.362611861367692]}
   {:iteration 158,
    :energy -1.1245812743368362,
    :gradients [0.008030851901577664 0.03089133902737902],
    :parameters [-0.10718477281681199 -0.36567933315687423]}
   {:iteration 159,
    :energy -1.1247047056304638,
    :gradients [0.008100125638210269 0.031108597422154965],
    :parameters [-0.10798785800696975 -0.36876846705961214]}
   {:iteration 160,
    :energy -1.1248299009075866,
    :gradients [0.008169630404583824 0.03132647066905213],
    :parameters [-0.10879787057079078 -0.3718793268018276]}
   {:iteration 161,
    :energy -1.1249568777373664,
    :gradients [0.008239362425773859 0.031544935720255775],
    :parameters [-0.10961483361124916 -0.3750119738687328]}
   {:iteration 162,
    :energy -1.1250856536094078,
    :gradients [0.008309317772210906 0.031763968880384894],
    :parameters [-0.11043876985382654 -0.3781664674407584]}
   {:iteration 163,
    :energy -1.1252162459235258,
    :gradients [0.008379492357566996 0.031983545799853096],
    :parameters [-0.11126970163104763 -0.3813428643287969]}
   {:iteration 164,
    :energy -1.1253486719792287,
    :gradients [0.008449881936674338 0.032203641468471544],
    :parameters [-0.11210765086680433 -0.3845412189087822]}
   {:iteration 165,
    :energy -1.1254829489649263,
    :gradients [0.00852048210346835 0.032424230209291235],
    :parameters [-0.11295263906047176 -0.38776158305562936]}
   {:iteration 166,
    :energy -1.1256190939468609,
    :gradients [0.008591288288975894 0.0326452856727164],
    :parameters [-0.11380468727081859 -0.3910040060765585]}
   {:iteration 167,
    :energy -1.1257571238577633,
    :gradients [0.0086622957593288 0.032866780830883416],
    :parameters [-0.11466381609971618 -0.39426853464383016]}
   {:iteration 168,
    :energy -1.1258970554852388,
    :gradients [0.008733499613827465 0.03308868797235186],
    :parameters [-0.11553004567564906 -0.3975552127269185]}
   {:iteration 169,
    :energy -1.1260389054598574,
    :gradients [0.008804894783045596 0.03331097869709559],
    :parameters [-0.11640339563703181 -0.4008640815241537]}
   {:iteration 170,
    :energy -1.1261826902430028,
    :gradients [0.008876476026980959 0.03353362391182685],
    :parameters [-0.11728388511533637 -0.40419517939386324]}
   {:iteration 171,
    :energy -1.1263284261144186,
    :gradients [0.00894823793326233 0.033756593825655296],
    :parameters [-0.11817153271803446 -0.4075485417850459]}
   {:iteration 172,
    :energy -1.1264761291595102,
    :gradients [0.009020174915401196 0.033979857946127734],
    :parameters [-0.11906635651136069 -0.4109242011676114]}
   {:iteration 173,
    :energy -1.1266258152563668,
    :gradients [0.00909228121111427 0.03420338507562556],
    :parameters [-0.11996837400290081 -0.41432218696222417]}
   {:iteration 174,
    :energy -1.1267775000625213,
    :gradients [0.009164550880699078 0.034427143308173994],
    :parameters [-0.12087760212401223 -0.41774252546978674]}
   {:iteration 175,
    :energy -1.1269311990014592,
    :gradients [0.009236977805475473 0.034651100026657534],
    :parameters [-0.12179405721208214 -0.4211852398006041]}
   {:iteration 176,
    :energy -1.1270869272488668,
    :gradients [0.009309555686300812 0.0348752219004752],
    :parameters [-0.12271775499262969 -0.4246503498032699]}
   {:iteration 177,
    :energy -1.127244699718625,
    :gradients [0.009382278042159534 0.0350994748836355],
    :parameters [-0.12364871056125977 -0.4281378719933174]}
   {:iteration 178,
    :energy -1.1274045310485667,
    :gradients [0.009455138208820102 0.03532382421333584],
    :parameters [-0.12458693836547573 -0.4316478194816809]}
   {:iteration 179,
    :energy -1.1275664355860004,
    :gradients [0.0095281293375882 0.035548234409015445],
    :parameters [-0.12553245218635772 -0.4351802019030145]}
   {:iteration 180,
    :energy -1.1277304273729767,
    :gradients [0.00960124439413261 0.03577266927192057],
    :parameters [-0.12648526512011654 -0.438735025343916]}
   {:iteration 181,
    :energy -1.1278965201313615,
    :gradients [0.009674476157408764 0.03599709188519027],
    :parameters [-0.1274453895595298 -0.4423122922711081]}
   {:iteration 182,
    :energy -1.128064727247672,
    :gradients [0.009747817218672405 0.03622146461449351],
    :parameters [-0.1284128371752707 -0.4459120014596271]}
   {:iteration 183,
    :energy -1.1282350617577033,
    :gradients [0.009821259980591386 0.03644574910922011],
    :parameters [-0.12938761889713793 -0.4495341479210765]}
   {:iteration 184,
    :energy -1.128407536330968,
    :gradients [0.009894796656463613 0.0366699063042516],
    :parameters [-0.13036974489519706 -0.4531787228319985]}
   {:iteration 185,
    :energy -1.1285821632549342,
    :gradients [0.009968419269539078 0.0368938964223362],
    :parameters [-0.13135922456084342 -0.45684571346242364]}
   {:iteration 186,
    :energy -1.128758954419081,
    :gradients [0.010042119652448335 0.03711767897707811],
    :parameters [-0.13235606648779732 -0.4605351031046573]}
   {:iteration 187,
    :energy -1.1289379212987962,
    :gradients [0.010115889446759436 0.03734121277655887],
    :parameters [-0.13336027845304216 -0.4642468710023651]}
   {:iteration 188,
    :energy -1.1291190749391058,
    :gradients [0.01018972010263815 0.037564455927618305],
    :parameters [-0.13437186739771811 -0.467980992280021]}
   {:iteration 189,
    :energy -1.1293024259382638,
    :gradients [0.010263602878647313 0.0377873658407993],
    :parameters [-0.13539083940798194 -0.4717374378727828]}
   {:iteration 190,
    :energy -1.1294879844312047,
    :gradients [0.010337528841663921 0.03800989923599013],
    :parameters [-0.13641719969584667 -0.4755161744568628]}
   {:iteration 191,
    :energy -1.12967576007288,
    :gradients [0.010411488866938169 0.038232012148759764],
    :parameters [-0.13745095258001308 -0.4793171643804618]}
   {:iteration 192,
    :energy -1.1298657620214905,
    :gradients [0.010485473638280608 0.038453659937430655],
    :parameters [-0.1384921014667069 -0.4831403655953378]}
   {:iteration 193,
    :energy -1.1300579989216304,
    :gradients [0.010559473648398142 0.03867479729087631],
    :parameters [-0.13954064883053496 -0.4869857315890809]}
   {:iteration 194,
    :energy -1.1302524788873478,
    :gradients [0.010633479199373375 0.03889537823708504],
    :parameters [-0.1405965961953748 -0.4908532113181685]}
   {:iteration 195,
    :energy -1.1304492094851697,
    :gradients [0.010707480403296893 0.03911535615248707],
    :parameters [-0.14165994411531213 -0.494742749141877]}
   {:iteration 196,
    :energy -1.1306481977170568,
    :gradients [0.010781467183050262 0.03933468377206862],
    :parameters [-0.14273069215564183 -0.4986542847571257]}
   {:iteration 197,
    :energy -1.1308494500033643,
    :gradients [0.010855429273246366 0.039553313200286905],
    :parameters [-0.14380883887394685 -0.5025877531343326]}
   {:iteration 198,
    :energy -1.1310529721657856,
    :gradients [0.010929356221337413 0.03977119592279709],
    :parameters [-0.14489438180127148 -0.5065430844543612]}
   {:iteration 199,
    :energy -1.131258769410312,
    :gradients [0.011003237388891262 0.03998828281899834],
    :parameters [-0.14598731742340523 -0.510520204046641]}],
  :reason :max-iterations,
  :objective-kind :hamiltonian,
  :success false,
  :iterations 200,
  :initial-energy -1.117429983995683,
  :optimal-parameters [-0.14598731742340523 -0.510520204046641]},
 :success false,
 :analysis
 {:initial-energy -1.117429983995683,
  :energy-improvement 0.013828785414629063,
  :convergence-achieved false},
 :result -1.131258769410312,
 :timing
 {:execution-time-ms 61356,
  :start-time 1761945935012,
  :end-time 1761945996368},
 :circuit
 {:operations
  [{:operation-type :x, :operation-params {:target 0}}
   {:operation-type :x, :operation-params {:target 1}}
   {:operation-type :ry,
    :operation-params {:target 0, :angle -0.07299365871170262}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :ry,
    :operation-params {:target 2, :angle -0.07299365871170262}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :ry,
    :operation-params {:target 0, :angle 0.07299365871170262}}
   {:operation-type :ry,
    :operation-params {:target 1, :angle -0.2552601020233205}}
   {:operation-type :cnot, :operation-params {:control 1, :target 3}}
   {:operation-type :ry,
    :operation-params {:target 3, :angle -0.2552601020233205}}
   {:operation-type :cnot, :operation-params {:control 1, :target 3}}
   {:operation-type :ry,
    :operation-params {:target 1, :angle 0.2552601020233205}}],
  :num-qubits 4,
  :name "UCCSD Inspired Ansatz"},
 :parameter-count 2,
 :hamiltonian
 {:pauli-terms 0, :grouped-pauli-terms nil, :classical-bound nil},
 :results
 {:optimal-energy -1.131258769410312,
  :optimal-parameters [-0.14598731742340523 -0.510520204046641],
  :success false,
  :iterations 200,
  :function-evaluations nil}}

```
The calculated ground state energy is
```clj
(:result vqe-result)

;; =>
-1.131258769410312

```
The final circuit with the optimal parameters is
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup (:circuit vqe-result))
```
![](tutorial_files/image33.svg)

We can do a comprehensive analysis of the VQE result, including:

* Performance summary - shows the final energy, number of iterations,
  and optimization method used
* Convergence analysis - shows how the energy converges over iterations
* Parameter sensitivity analysis - shows how sensitive the energy is to changes
  in the parameters
* Parameter landscape analysis - shows the energy landscape as a function
  of the parameters
* Chemical accuracy assessment - compares the calculated energy to
  the exact energy
* Hardware compability metrics - analyzes the circuit for
  depth, number of gates, and qubit connectivity
* VQE-specific quantum state analysis - analyzes the final quantum state
  for entanglement and other properties
* VQE-specific recommendations - provides recommendations for
  improving the VQE performance

The comprehensive analysis can help to understand the performance of
the VQE algorithm and identify areas for improvement.
```clj
(vqe/comprehensive-vqe-analysis (sim/create-simulator)
                                vqe-result
                                {:landscape-analysis? true})

;; =>
{:analysis-type "Comprehensive VQE Analysis",
 :convergence-analysis
 {:energy-std 0.0038181182158071943,
  :final-gradient-norm 0.04147449813859708,
  :relative-improvement 0.0123755274269448,
  :energy-trajectory-length 200,
  :energy-improvement 0.013828785414629063,
  :final-reason :max-iterations,
  :monotonic-decrease? true,
  :converged false,
  :total-iterations 200,
  :convergence-quality :excellent,
  :initial-energy -1.117429983995683,
  :final-energy -1.131258769410312,
  :function-evaluations 0,
  :convergence-rate 6.914392707314531E-5},
 :performance-summary
 {:algorithm "VQE",
  :energy-improvement 0.013828785414629063,
  :efficiency-score nil,
  :success false,
  :iterations 0,
  :runtime-seconds 0.0,
  :convergence-quality :excellent,
  :final-energy nil,
  :function-evaluations 0},
 :vqe-specific
 {:analysis-type "VQE-Specific Analysis",
  :quantum-state nil,
  :chemical-accuracy
  {:threshold-hartree 0.0016,
   :energy-improvement 0.013828785414629063,
   :achieved true},
  :hardware-compatibility
  {:circuit-depth 10,
   :gate-count 12,
   :qubit-count 4,
   :estimated-fidelity 1.0,
   :hardware-efficiency -0.094271564117526},
  :molecular-analysis
  {:ansatz-type :uccsd,
   :hamiltonian-terms 15,
   :final-circuit
   {:operations
    [{:operation-type :x, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :ry,
      :operation-params {:target 0, :angle -0.07299365871170262}}
     {:operation-type :cnot, :operation-params {:control 0, :target 2}}
     {:operation-type :ry,
      :operation-params {:target 2, :angle -0.07299365871170262}}
     {:operation-type :cnot, :operation-params {:control 0, :target 2}}
     {:operation-type :ry,
      :operation-params {:target 0, :angle 0.07299365871170262}}
     {:operation-type :ry,
      :operation-params {:target 1, :angle -0.2552601020233205}}
     {:operation-type :cnot, :operation-params {:control 1, :target 3}}
     {:operation-type :ry,
      :operation-params {:target 3, :angle -0.2552601020233205}}
     {:operation-type :cnot, :operation-params {:control 1, :target 3}}
     {:operation-type :ry,
      :operation-params {:target 1, :angle 0.2552601020233205}}],
    :num-qubits 4,
    :name "UCCSD Inspired Ansatz"}},
  :vqe-recommendations
  ["VQE optimization completed with moderate success"
   "Consider additional iterations or parameter tuning"]},
 :landscape-analysis
 {:optimal-energy -1.131258769410312,
  :sensitivities [8.040607001547073E-5 4.952370356927638E-4],
  :most-sensitive-parameter 1,
  :least-sensitive-parameter 0,
  :perturbation-size 0.01,
  :parameter-count 2,
  :gradients [0.011003237388891262 0.03998828281899834],
  :gradient-norm 0.04147449813859708},
 :parameter-sensitivity
 {:sensitivities [8.040607001547073E-5 4.952370356927638E-4],
  :normalized-sensitivities [0.0 1.0],
  :max-sensitivity 4.952370356927638E-4,
  :min-sensitivity 8.040607001547073E-5,
  :sensitivity-range 4.1483096567729305E-4,
  :ranked-parameters
  ([1 4.952370356927638E-4] [0 8.040607001547073E-5]),
  :high-sensitivity-params (1 0),
  :low-sensitivity-params (1 0)}}

```

### Quantum Approximation Optimization Algorithm (QAOA)
The [Quantum Approximation Optimization Algorithm (QAOA)](https://en.wikipedia.org/wiki/Quantum_approximate_optimization_algorithm)
is a hybrid quantum-classical algorithm used to solve combinatorial optimization problems.
It is particularly useful for problems that can be represented as a cost function,
such as the Max-Cut problem, where the goal is to partition a graph into
two subsets such that the number of edges between the subsets is maximized.
The QAOA algorithm uses a parameterized quantum circuit to prepare a trial state,
and a classical optimization algorithm to maximize the expectation value of
the cost function with respect to the trial state.

* Max-Cut Problem - a combinatorial optimization problem that can be
  represented as a graph. The goal is to partition the vertices of the graph
  into two subsets such that the number of edges between the subsets is maximized.
  The Max-Cut problem can be represented as a cost function, where the cost
  function is the number of edges between the two subsets.
  The QAOA algorithm can be used to find an approximate solution to the Max-Cut

* Max-SAT Problem - a combinatorial optimization problem that can be
  represented as a boolean formula in conjunctive normal form (CNF). The goal
  is to find an assignment of truth values to the variables in the formula
  that maximizes the number of satisfied clauses. The Max-SAT problem can be
  represented as a cost function, where the cost function is the number of
  satisfied clauses. The QAOA algorithm can be used to find an approximate
  solution to the Max-SAT problem.

* Traveling Salesperson Problem - a combinatorial optimization
  problem that can be represented as a graph. The goal is to find the shortest
  possible route that visits each vertex exactly once and returns to the
  starting vertex. The TSP can be represented as a cost function, where the
  cost function is the total distance of the route. The QAOA algorithm can
  be used to find an approximate solution to the TSP.

The QAOA algorithm is also used in quantum machine learning and other optimization problems,
where it can be used to find the optimal parameters for a quantum circuit
that represents a trial state.


#### Problem Statement
Given a cost function C that represents a combinatorial optimization problem,
the goal is to find the optimal solution x* that maximizes the cost function
using as few evaluations of the cost function as possible.


#### Classical Approach
In a classical setting, finding the optimal solution to a combinatorial
optimization problem requires evaluating the cost function for all possible
solutions, which can be computationally expensive for large problems.
Classical algorithms such as simulated annealing or genetic algorithms can
be used to find approximate solutions, but they are limited by the size of
the problem and the complexity of the cost function.


#### Quantum Approach
The QAOA algorithm allows us to find approximate solutions to combinatorial
optimization problems using a hybrid quantum-classical approach. It leverages
the power of quantum computing to prepare trial states and measure the
expectation value of the cost function, while using classical optimization
algorithms to maximize the expectation value and find the optimal parameters
for the trial state.


#### Quantum Circuit
The QAOA algorithm can be implemented using a quantum circuit with the following steps:

1. Prepare a parameterized quantum circuit that represents the trial state.
   The circuit can be represented as a series of quantum gates that depend on
   a set of parameters θ.
2. Initialize the parameters θ to some initial values.
3. Execute the quantum circuit to prepare the trial state |ψ(θ)⟩.
4. Measure the expectation value of the cost function C with respect to
   the trial state |ψ(θ)⟩. This involves applying the cost function as a quantum gate
   and measuring the qubits to obtain the expectation value ⟨C⟩.
5. Use a classical optimization algorithm to update the parameters θ based on the
   measured expectation value ⟨C⟩. The optimization algorithm can be
   gradient-based or gradient-free, depending on the problem.
6. Repeat steps 3-5 until convergence, i.e., until the expectation value ⟨C⟩
   does not change significantly or a maximum number of iterations is reached.
7. The final expectation value ⟨C⟩ represents the optimal solution x* to the combinatorial
   optimization problem.

To explore the QAOA algorithm, we need to require the `qaoa` namespace.
```clj
(require '[org.soulspace.qclojure.application.algorithm.qaoa :as qaoa])
```
Let's define a simple triangular graph for the Max-Cut problem.
```clj
(def triangle-graph [[0 1 1.0] [1 2 1.0] [0 2 1.0]])
```
We can use the QAOA algorithm to solve the Max-Cut problem for the defined graph.
```clj
(def triangle-qaoa-result
  (qaoa/quantum-approximate-optimization-algorithm
   (sim/create-simulator)
   {:problem-type :max-cut
    :problem-hamiltonian (qaoa/max-cut-hamiltonian triangle-graph 3)
    :problem-instance triangle-graph  ; Triangle with unit weights
    :num-qubits 3
    :num-layers 2
    :optimization-method :adam
    :max-iterations 100
    :tolerance 1e-6
    :shots 1000}))
```
The result of the QAOA algorithm is a map that contains the result of the
algorithm, the measurement outcome, and the circuit used to execute the algorithm.
```clj
triangle-qaoa-result

;; =>
{:optimal-energy 1.348181822310566,
 :problem-hamiltonian
 ({:coefficient 0.5, :pauli-string "III"}
  {:coefficient -0.5, :pauli-string "ZZI"}
  {:coefficient 0.5, :pauli-string "III"}
  {:coefficient -0.5, :pauli-string "IZZ"}
  {:coefficient 0.5, :pauli-string "III"}
  {:coefficient -0.5, :pauli-string "ZIZ"}),
 :total-runtime-ms 31677,
 :problem-solutions
 {:solution-type :max-cut,
  :cut-edges ([0 1 1.0] [0 2 1.0]),
  :all-solutions
  ({:partition {0 1, 1 1, 2 0},
    :cut-edges ([1 2 1.0] [0 2 1.0]),
    :cut-weight 2.0,
    :index 6,
    :count 97,
    :probability 97/1000,
    :objective-value 2.0}
   {:partition {0 0, 1 1, 2 0},
    :cut-edges ([0 1 1.0] [1 2 1.0]),
    :cut-weight 2.0,
    :index 2,
    :count 123,
    :probability 123/1000,
    :objective-value 2.0}
   {:partition {0 0, 1 0, 2 0},
    :cut-edges (),
    :cut-weight 0,
    :index 0,
    :count 166,
    :probability 83/500,
    :objective-value 0}
   {:partition {0 0, 1 1, 2 1},
    :cut-edges ([0 1 1.0] [0 2 1.0]),
    :cut-weight 2.0,
    :index 3,
    :count 113,
    :probability 113/1000,
    :objective-value 2.0}
   {:partition {0 0, 1 0, 2 1},
    :cut-edges ([1 2 1.0] [0 2 1.0]),
    :cut-weight 2.0,
    :index 1,
    :count 119,
    :probability 119/1000,
    :objective-value 2.0}
   {:partition {0 1, 1 1, 2 1},
    :cut-edges (),
    :cut-weight 0,
    :index 7,
    :count 163,
    :probability 163/1000,
    :objective-value 0}
   {:partition {0 1, 1 0, 2 1},
    :cut-edges ([0 1 1.0] [1 2 1.0]),
    :cut-weight 2.0,
    :index 5,
    :count 120,
    :probability 3/25,
    :objective-value 2.0}
   {:partition {0 1, 1 0, 2 0},
    :cut-edges ([0 1 1.0] [0 2 1.0]),
    :cut-weight 2.0,
    :index 4,
    :count 99,
    :probability 99/1000,
    :objective-value 2.0}),
  :measurement-distribution
  {6 97, 2 123, 0 166, 3 113, 1 119, 7 163, 5 120, 4 99},
  :cut-weight 2.0,
  :partition {0 1, 1 0, 2 0},
  :solution-probability 99/1000,
  :approximation-ratio 1.0,
  :classical-optimum 2.0},
 :algorithm "QAOA",
 :initial-parameters [0.37 0.19 0.42 0.21],
 :enhanced-features
 {:optimization-method :adam,
  :convergence-monitored true,
  :algorithm-config-driven true},
 :convergence-analysis
 {:energy-std 0.19468934796202048,
  :final-gradient-norm 0.9408440083896897,
  :relative-improvement 0.32244167521480405,
  :energy-trajectory-length 100,
  :energy-improvement 0.641583151410295,
  :final-reason :max-iterations,
  :monotonic-decrease? true,
  :converged false,
  :total-iterations 100,
  :convergence-quality :excellent,
  :initial-energy 1.989764973720861,
  :final-energy 1.348181822310566,
  :function-evaluations 0,
  :convergence-rate 0.00641583151410295},
 :qaoa-analysis
 {:parameter-summary
  {:gamma-range [0.6616952997953864 0.9314517127019206],
   :beta-range [0.19 0.21],
   :gamma-mean 0.7965735062486535,
   :beta-mean 0.2,
   :total-parameters 4},
  :convergence-analysis
  {:energy-std 0.19468934796202048,
   :final-gradient-norm 0.9408440083896897,
   :relative-improvement 0.32244167521480405,
   :energy-trajectory-length 100,
   :energy-improvement 0.641583151410295,
   :final-reason :max-iterations,
   :monotonic-decrease? true,
   :converged false,
   :total-iterations 100,
   :convergence-quality :excellent,
   :initial-energy 1.989764973720861,
   :final-energy 1.348181822310566,
   :function-evaluations 0,
   :convergence-rate 0.00641583151410295},
  :performance-metrics
  {:function-evaluations 0,
   :total-runtime-ms 31677,
   :approximation-ratio nil,
   :measurement-based-solutions true}},
 :history
 [{:iteration 0,
   :energy 1.989764973720861,
   :gradients
   [-0.17163040862054754
    0.0
    -0.07388833596129012
    -2.355138688025663E-16],
   :parameters [0.37 0.19 0.42 0.21]}
  {:iteration 1,
   :energy 1.9892643149143725,
   :gradients [-0.17611950339196827 0.0 -0.07659661110546469 0.0],
   :parameters [0.3717163040862055 0.19 0.4207388833596129 0.21]}
  {:iteration 2,
   :energy 1.9887354349551916,
   :gradients [-0.1807277212380563 0.0 -0.07937409289746021 0.0],
   :parameters [0.3734774991201252 0.19 0.4215048494706676 0.21]}
  {:iteration 3,
   :energy 1.9881767728451458,
   :gradients
   [-0.18545763077711697
    0.0
    -0.08222190626563784
    -1.5700924586837752E-16],
   :parameters [0.37528477633250573 0.19 0.4222985903996422 0.21]}
  {:iteration 4,
   :energy 1.9875866877710449,
   :gradients
   [-0.19031180829459793
    0.0
    -0.08514114759310719
    -3.1401849173675503E-16],
   :parameters [0.3771393526402769 0.19 0.4231208094622986 0.21]}
  {:iteration 5,
   :energy 1.9869634557231344,
   :gradients
   [-0.19529283336834397
    0.0
    -0.08813287999319049
    -7.850462293418876E-17],
   :parameters [0.3790424707232229 0.19 0.42397222093822967 0.21]}
  {:iteration 6,
   :energy 1.9863052660461635,
   :gradients
   [-0.200403284084684
    0.0
    -0.09119812823094609
    -1.1775693440128314E-16],
   :parameters [0.3809953990569063 0.19 0.4248535497381616 0.21]}
  {:iteration 7,
   :energy 1.9856102179310464,
   :gradients [-0.2056457318203689 0.0 -0.09433787327254797 0.0],
   :parameters [0.38299943189775315 0.19 0.42576553102047104 0.21]}
  {:iteration 8,
   :energy 1.9848763168567407,
   :gradients [-0.21102273556452567 0.0 -0.09755304644409868 0.0],
   :parameters [0.38505588921595685 0.19 0.42670890975319653 0.21]}
  {:iteration 9,
   :energy 1.9841014709933162,
   :gradients [-0.21653683575423 0.0 -0.10084452318140445 0.0],
   :parameters [0.3871661165716021 0.19 0.4276844402176375 0.21]}
  {:iteration 10,
   :energy 1.983283487579107,
   :gradients
   [-0.22219054759667703
    0.0
    -0.1042131163522348
    -3.925231146709438E-17],
   :parameters [0.3893314849291444 0.19 0.42869288544945156 0.21]}
  {:iteration 11,
   :energy 1.9824200692865914,
   :gradients
   [-0.2279863538505423
    0.0
    -0.1076595691328352
    -7.850462293418876E-17],
   :parameters [0.39155339040511117 0.19 0.4297350166129739 0.21]}
  {:iteration 12,
   :energy 1.9815088105937073,
   :gradients
   [-0.23392669703886568 0.0 -0.111184547420802 7.850462293418876E-17],
   :parameters [0.39383325394361657 0.19 0.43081161230430226 0.21]}
  {:iteration 13,
   :energy 1.9805471941795618,
   :gradients
   [-0.2400139710656622
    0.0
    -0.11478863176705129
    -3.925231146709438E-17],
   :parameters [0.39617252091400523 0.19 0.43192345777851027 0.21]}
  {:iteration 14,
   :energy 1.9795325873658385,
   :gradients
   [-0.24625051220867458
    0.0
    -0.11847230881041572
    3.925231146709438E-17],
   :parameters [0.39857266062466185 0.19 0.43307134409618075 0.21]}
  {:iteration 15,
   :energy 1.978462238627734,
   :gradients
   [-0.25263858946097695
    0.0
    -0.12223596219952378
    -3.1401849173675503E-16],
   :parameters [0.4010351657467486 0.19 0.4342560671842849 0.21]}
  {:iteration 16,
   :energy 1.9773332742010379,
   :gradients
   [-0.2591803941949206
    0.0
    -0.1260798629879974
    -3.925231146709438E-17],
   :parameters [0.4035615516413584 0.19 0.43547842680628013 0.21]}
  {:iteration 17,
   :energy 1.9761426948148442,
   :gradients
   [-0.2658780291228208 0.0 -0.1300041594907697 3.925231146709438E-17],
   :parameters [0.40615335558330756 0.19 0.4367392254361601 0.21]}
  {:iteration 18,
   :energy 1.974887372582485,
   :gradients
   [-0.27273349653026563
    0.0
    -0.13400886659143638
    3.925231146709438E-17],
   :parameters [0.4088121358745358 0.19 0.4380392670310678 0.21]}
  {:iteration 19,
   :energy 1.9735640480864924,
   :gradients [-0.27974868575968054 0.0 -0.13809385449314032 0.0],
   :parameters [0.41153947083983844 0.19 0.43937935569698217 0.21]}
  {:iteration 20,
   :energy 1.972169327696863,
   :gradients
   [-0.28692535992414536
    0.0
    -0.14225883690846813
    3.925231146709438E-17],
   :parameters [0.4143369576974352 0.19 0.44076029424191354 0.21]}
  {:iteration 21,
   :energy 1.970699681165393,
   :gradients
   [-0.2942651418342099
    0.0
    -0.14650335868742526
    -1.5700924586837752E-16],
   :parameters [0.4172062112966767 0.19 0.4421828826109982 0.21]}
  {:iteration 22,
   :energy 1.9691514395425993,
   :gradients
   [-0.30176949912400586
    0.0
    -0.15082678288664314
    -1.962615573354719E-16],
   :parameters [0.4201488627150188 0.19 0.44364791619787247 0.21]}
  {:iteration 23,
   :energy 1.9675207934674466,
   :gradients
   [-0.30943972856690466
    0.0
    -0.15522827728767738
    7.850462293418876E-17],
   :parameters [0.42316655770625883 0.19 0.4451561840267389 0.21]}
  {:iteration 24,
   :energy 1.965803791884093,
   :gradients
   [-0.3172769395758913
    0.0
    -0.15970680037765356
    -7.850462293418876E-17],
   :parameters [0.4262609549919279 0.19 0.4467084667996157 0.21]}
  {:iteration 25,
   :energy 1.9639963412436119,
   :gradients [-0.3252820368893416 0.0 -0.16426108681162135 0.0],
   :parameters [0.4294337243876868 0.19 0.4483055348033922 0.21]}
  {:iteration 26,
   :energy 1.9620942052527015,
   :gradients [-0.3334557024493254 0.0 -0.16888963238275412 0.0],
   :parameters [0.4326865447565802 0.19 0.4499481456715084 0.21]}
  {:iteration 27,
   :energy 1.960093005235116,
   :gradients
   [-0.34179837648692357
    0.0
    -0.1735906785342499
    -3.925231146709438E-16],
   :parameters [0.43602110178107345 0.19 0.45163704199533594 0.21]}
  {:iteration 28,
   :energy 1.95798822117535,
   :gradients
   [-0.3503102378372914
    0.0
    -0.17836219645520865
    -7.850462293418876E-17],
   :parameters [0.4394390855459427 0.19 0.45337294878067846 0.21]}
  {:iteration 29,
   :energy 1.9557751935176262,
   :gradients [-0.35899118351657966 0.0 -0.18320187081210426 0.0],
   :parameters [0.4429421879243156 0.19 0.4551565707452305 0.21]}
  {:iteration 30,
   :energy 1.9534491257964781,
   :gradients [-0.367840807603133 0.0 -0.1881070831777016 0.0],
   :parameters [0.4465320997594814 0.19 0.4569885894533516 0.21]}
  {:iteration 31,
   :energy 1.9510050881781604,
   :gradients
   [-0.3768583794769292
    0.0
    -0.19307489523039603
    -2.355138688025663E-16],
   :parameters [0.4502105078355127 0.19 0.4588696602851286 0.21]}
  {:iteration 32,
   :energy 1.9484380219945536,
   :gradients
   [-0.3860428214838316
    0.0
    -0.19810203180893451
    -1.5700924586837752E-16],
   :parameters [0.453979091630282 0.19 0.46080040923743254 0.21]}
  {:iteration 33,
   :energy 1.9457427453531038,
   :gradients
   [-0.3953926861049304
    0.0
    -0.20318486392042387
    -1.5700924586837752E-16],
   :parameters [0.45783951984512034 0.19 0.4627814295555219 0.21]}
  {:iteration 34,
   :energy 1.9429139599074454,
   :gradients
   [-0.4049061327262243
    0.0
    -0.20831939181321726
    7.850462293418876E-17],
   :parameters [0.4617934467061696 0.19 0.4648132781947261 0.21]}
  {:iteration 35,
   :energy 1.9399462588738057,
   :gradients [-0.4145809041198799 0.0 -0.21350122824081746 0.0],
   :parameters [0.46584250803343186 0.19 0.4668964721128583 0.21]}
  {:iteration 36,
   :energy 1.9368341363774555,
   :gradients
   [-0.4244143027653133
    0.0
    -0.21872558205812295
    1.5700924586837752E-16],
   :parameters [0.4699883170746307 0.19 0.4690314843952665 0.21]}
  {:iteration 37,
   :energy 1.9335719982118484,
   :gradients [-0.43440316715650784 0.0 -0.22398724230716302 0.0],
   :parameters [0.4742324601022838 0.19 0.4712187402158477 0.21]}
  {:iteration 38,
   :energy 1.930154174089882,
   :gradients [-0.44454384826075405 0.0 -0.22928056296563842 0.0],
   :parameters [0.47857649177384887 0.19 0.4734586126389193 0.21]}
  {:iteration 39,
   :energy 1.926574931462146,
   :gradients [-0.4548321863137069 0.0 -0.2345994485480912 0.0],
   :parameters [0.4830219302564564 0.19 0.4757514182685757 0.21]}
  {:iteration 40,
   :energy 1.9228284909709359,
   :gradients
   [-0.4652634881557392
    0.0
    -0.23993734076598644
    7.850462293418876E-17],
   :parameters [0.4875702521195935 0.19 0.4780974127540566 0.21]}
  {:iteration 41,
   :energy 1.9189090436006755,
   :gradients [-0.47583250533498045 0.0 -0.24528720646923727 0.0],
   :parameters [0.49222288700115086 0.19 0.4804967861617165 0.21]}
  {:iteration 42,
   :energy 1.914810769575524,
   :gradients [-0.4865334132227773 0.0 -0.2506415271073891 0.0],
   :parameters [0.49698121205450063 0.19 0.48294965822640884 0.21]}
  {:iteration 43,
   :energy 1.9105278590428594,
   :gradients [-0.49735979140737596 0.0 -0.25599228996343654 0.0],
   :parameters [0.5018465461867284 0.19 0.4854560734974827 0.21]}
  {:iteration 44,
   :energy 1.9060545345670672,
   :gradients
   [-0.5083046056508447
    0.0
    -0.26133098142673217
    -7.850462293418876E-17],
   :parameters [0.5068201441008022 0.19 0.48801599639711707 0.21]}
  {:iteration 45,
   :energy 1.9013850754413482,
   :gradients [-0.5193601917122943 0.0 -0.2666485825831158 0.0],
   :parameters [0.5119031901573107 0.19 0.4906293062113844 0.21]}
  {:iteration 46,
   :energy 1.8965138438063278,
   :gradients
   [-0.5305182413568481
    0.0
    -0.2719355674098417
    -1.1775693440128314E-16],
   :parameters [0.5170967920744336 0.19 0.4932957920372156 0.21]}
  {:iteration 47,
   :energy 1.8914353125426966,
   :gradients
   [-0.541769790883833 0.0 -0.2771819038695938 3.925231146709438E-17],
   :parameters [0.5224019744880021 0.19 0.496015147711314 0.21]}
  {:iteration 48,
   :energy 1.8861440948811716,
   :gradients [-0.5531052125189185 0.0 -0.2823770582011706 0.0],
   :parameters [0.5278196723968404 0.19 0.49878696675000994 0.21]}
  {:iteration 49,
   :energy 1.8806349756468714,
   :gradients
   [-0.5645142090226553 0.0 -0.2875100027038853 7.850462293418876E-17],
   :parameters [0.5333507245220296 0.19 0.5016107373320217 0.21]}
  {:iteration 50,
   :energy 1.8749029440266487,
   :gradients [-0.5759858118714268 0.0 -0.2925692273076897 0.0],
   :parameters [0.5389958666122562 0.19 0.5044858373590605 0.21]}
  {:iteration 51,
   :energy 1.8689432277174123,
   :gradients [-0.5875083833656083 0.0 -0.29754275521083595 0.0],
   :parameters [0.5447557247309704 0.19 0.5074115296321374 0.21]}
  {:iteration 52,
   :energy 1.862751328281316,
   :gradients
   [-0.599069623012925 0.0 -0.3024181628511707 1.5700924586837752E-16],
   :parameters [0.5506308085646265 0.19 0.5103869571842458 0.21]}
  {:iteration 53,
   :energy 1.8563230575000853,
   :gradients [-0.6106565785221624 0.0 -0.30718260445520357 0.0],
   :parameters [0.5566215047947558 0.19 0.5134111388127575 0.21]}
  {:iteration 54,
   :energy 1.8496545744863735,
   :gradients
   [-0.6222556617226569
    0.0
    -0.31182284138053107
    -1.1775693440128314E-16],
   :parameters [0.5627280705799774 0.19 0.5164829648573095 0.21]}
  {:iteration 55,
   :energy 1.8427424232752998,
   :gradients [-0.6338526696980925 0.0 -0.31632527643170344 0.0],
   :parameters [0.568950627197204 0.19 0.5196011932711148 0.21]}
  {:iteration 56,
   :energy 1.8355835705850037,
   :gradients
   [-0.6454328113884109
    0.0
    -0.32067599328684354
    -1.1775693440128314E-16],
   :parameters [0.5752891538941849 0.19 0.5227644460354318 0.21]}
  {:iteration 57,
   :energy 1.828175443401752,
   :gradients [-0.6569807398708848 0.0 -0.32486080112222887 0.0],
   :parameters [0.581743482008069 0.19 0.5259712059683003 0.21]}
  {:iteration 58,
   :energy 1.820515966013875,
   :gradients
   [-0.668480590480483 0.0 -0.32886528446465707 3.925231146709438E-17],
   :parameters [0.5883132894067779 0.19 0.5292198139795226 0.21]}
  {:iteration 59,
   :energy 1.812603596090185,
   :gradients
   [-0.679916024870458 0.0 -0.3326748582369835 2.355138688025663E-16],
   :parameters [0.5949980953115827 0.19 0.5325084668241691 0.21]}
  {:iteration 60,
   :energy 1.8044373593739171,
   :gradients
   [-0.6912702810470368
    0.0
    -0.33627482789118124
    1.1775693440128314E-16],
   :parameters [0.6017972555602872 0.19 0.5358352154065389 0.21]}
  {:iteration 61,
   :energy 1.7960168825431004,
   :gradients
   [-0.7025262293373924
    0.0
    -0.33965045444642866
    -3.925231146709438E-17],
   :parameters [0.6087099583707576 0.19 0.5391979636854507 0.21]}
  {:iteration 62,
   :energy 1.7873424237739817,
   :gradients [-0.7136664341688234 0.0 -0.3427870241677851 0.0],
   :parameters [0.6157352206641316 0.19 0.542594468229915 0.21]}
  {:iteration 63,
   :energy 1.7784149005364418,
   :gradients
   [-0.7246732214498275
    0.0
    -0.3456699225355202
    -1.1775693440128314E-16],
   :parameters [0.6228718850058198 0.19 0.5460223384715929 0.21]}
  {:iteration 64,
   :energy 1.7692359141501528,
   :gradients [-0.7355287512521396 0.0 -0.3482847120671626 0.0],
   :parameters [0.630118617220318 0.19 0.5494790376969481 0.21]}
  {:iteration 65,
   :energy 1.759807770638397,
   :gradients
   [-0.7462150953981549 0.0 -0.3506172134658596 3.925231146709438E-17],
   :parameters [0.6374739047328394 0.19 0.5529618848176197 0.21]}
  {:iteration 66,
   :energy 1.7501334974335134,
   :gradients
   [-0.7567143194622965
    0.0
    -0.35265358948127695
    -1.1775693440128314E-16],
   :parameters [0.6449360556868209 0.19 0.5564680569522783 0.21]}
  {:iteration 67,
   :energy 1.7402168555144695,
   :gradients
   [-0.7670085685999005
    0.0
    -0.35438043078538045
    2.7476618026966064E-16],
   :parameters [0.6525031988814438 0.19 0.5599945928470911 0.21]}
  {:iteration 68,
   :energy 1.7300623465931797,
   :gradients
   [-0.7770801565251353
    0.0
    -0.35578484308692826
    3.1401849173675503E-16],
   :parameters [0.6601732845674428 0.19 0.5635383971549449 0.21]}
  {:iteration 69,
   :energy 1.719675215012137,
   :gradients
   [-0.7869116568728125
    0.0
    -0.35685453463788724
    -1.1775693440128314E-16],
   :parameters [0.6679440861326942 0.19 0.5670962455858142 0.21]}
  {:iteration 70,
   :energy 1.7090614440711218,
   :gradients
   [-0.7964859960999882
    0.0
    -0.3575779032243982
    -1.5700924586837752E-16],
   :parameters [0.6758132027014223 0.19 0.570664790932193 0.21]}
  {:iteration 71,
   :energy 1.6982277465650553,
   :gradients
   [-0.8057865470144757 0.0 -0.357944121686602 7.850462293418876E-17],
   :parameters [0.6837780626624221 0.19 0.574240569964437 0.21]}
  {:iteration 72,
   :energy 1.6871815493872682,
   :gradients
   [-0.8147972219609654
    0.0
    -0.3579432209776224
    -3.925231146709438E-17],
   :parameters [0.6918359281325669 0.19 0.577820011181303 0.21]}
  {:iteration 73,
   :energy 1.6759309721316478,
   :gradients
   [-0.8235025646537759
    0.0
    -0.3575661697542299
    -1.1775693440128314E-16],
   :parameters [0.6999839003521765 0.19 0.5813994433910792 0.21]}
  {:iteration 74,
   :energy 1.6644847997119314,
   :gradients
   [-0.8318878396199152 0.0 -0.3568049494914534 7.850462293418876E-17],
   :parameters [0.7082189259987143 0.19 0.5849751050886215 0.21]}
  {:iteration 75,
   :energy 1.6528524491048626,
   :gradients
   [-0.8399391182088979
    0.0
    -0.35565262413199555
    -1.5700924586837752E-16],
   :parameters [0.7165378043949134 0.19 0.588543154583536 0.21]}
  {:iteration 76,
   :energy 1.6410439304145954,
   :gradients
   [-0.8476433601377351
    0.0
    -0.3541034033192454
    -3.925231146709438E-17],
   :parameters [0.7249371955770024 0.19 0.592099680824856 0.21]}
  {:iteration 77,
   :energy 1.6290698025461399,
   :gradients [-0.8549884895711753 0.0 -0.3521526983201762 0.0],
   :parameters [0.7334136291783798 0.19 0.5956407148580485 0.21]}
  {:iteration 78,
   :energy 1.6169411238639573,
   :gradients
   [-0.861963464789061 0.0 -0.3497971698209781 7.850462293418876E-17],
   :parameters [0.7419635140740916 0.19 0.5991622418412502 0.21]}
  {:iteration 79,
   :energy 1.604669398295629,
   :gradients
   [-0.8685583405636826
    0.0
    -0.3470347668728893
    1.5700924586837752E-16],
   :parameters [0.7505831487219822 0.19 0.60266021353946 0.21]}
  {:iteration 80,
   :energy 1.5922665174179178,
   :gradients
   [-0.8747643224596574 0.0 -0.3438647563769871 7.850462293418876E-17],
   :parameters [0.759268732127619 0.19 0.6061305612081889 0.21]}
  {:iteration 81,
   :energy 1.5797446991312047,
   :gradients
   [-0.8805738123753157
    0.0
    -0.3402877426222313
    -1.5700924586837752E-16],
   :parameters [0.7680163753522156 0.19 0.6095692087719587 0.21]}
  {:iteration 82,
   :energy 1.5671164235867492,
   :gradients [-0.8859804447655834 0.0 -0.33630567652861126 0.0],
   :parameters [0.7768221134759687 0.19 0.6129720861981811 0.21]}
  {:iteration 83,
   :energy 1.5543943670774765,
   :gradients
   [-0.8909791131196709
    0.0
    -0.3319218543935333
    -1.5700924586837752E-16],
   :parameters [0.7856819179236246 0.19 0.6163351429634673 0.21]}
  {:iteration 84,
   :energy 1.5415913346362313,
   :gradients
   [-0.8955659864090751 0.0 -0.3271409060915266 2.355138688025663E-16],
   :parameters [0.7945917090548213 0.19 0.6196543615074026 0.21]}
  {:iteration 85,
   :energy 1.528720192104401,
   :gradients [-0.8997385153695631 0.0 -0.3219687728312456 0.0],
   :parameters [0.8035473689189121 0.19 0.6229257705683179 0.21]}
  {:iteration 86,
   :energy 1.5157937984382843,
   :gradients
   [-0.9034954286311597
    0.0
    -0.3164126747261067
    -2.355138688025663E-16],
   :parameters [0.8125447540726077 0.19 0.6261454582966304 0.21]}
  {:iteration 87,
   :energy 1.502824939010273,
   :gradients [-0.9068367188593024 0.0 -0.31048106858208935 0.0],
   :parameters [0.8215797083589194 0.19 0.6293095850438915 0.21]}
  {:iteration 88,
   :energy 1.4898262606375172,
   :gradients
   [-0.9097636192147542
    0.0
    -0.3041835964448664
    1.5700924586837752E-16],
   :parameters [0.8306480755475124 0.19 0.6324143957297124 0.21]}
  {:iteration 89,
   :energy 1.4768102090325874,
   :gradients
   [-0.9122785705762285
    0.0
    -0.29753102557534933
    7.850462293418876E-17],
   :parameters [0.83974571173966 0.19 0.635456231694161 0.21]}
  {:iteration 90,
   :energy 1.4637889693203123,
   :gradients
   [-0.9143851800948984 0.0 -0.2905351806351793 7.850462293418876E-17],
   :parameters [0.8488684974454223 0.19 0.6384315419499145 0.21]}
  {:iteration 91,
   :energy 1.4507744102036813,
   :gradients [-0.9160881717615477 0.0 -0.2832088689593264 0.0],
   :parameters [0.8580123492463713 0.19 0.6413368937562662 0.21]}
  {:iteration 92,
   :energy 1.4377780322912117,
   :gradients [-0.9173933297626817 0.0 -0.2755657998700925 0.0],
   :parameters [0.8671732309639868 0.19 0.6441689824458595 0.21]}
  {:iteration 93,
   :energy 1.4248109210205597,
   :gradients
   [-0.9183074354799784
    0.0
    -0.2676204990441267
    3.1401849173675503E-16],
   :parameters [0.8763471642616136 0.19 0.6469246404445604 0.21]}
  {:iteration 94,
   :energy 1.4118837045303887,
   :gradients [-0.9188381990467367 0.0 -0.2593882189810326 0.0],
   :parameters [0.8855302386164134 0.19 0.6496008454350017 0.21]}
  {:iteration 95,
   :energy 1.3990065167466414,
   :gradients
   [-0.9189941864151484
    0.0
    -0.25088484663882765
    1.5700924586837752E-16],
   :parameters [0.8947186206068808 0.19 0.6521947276248121 0.21]}
  {:iteration 96,
   :energy 1.3861889658627393,
   :gradients
   [-0.9187847429090191
    0.0
    -0.2421268092981753
    3.1401849173675503E-16],
   :parameters [0.9039085624710322 0.19 0.6547035760912004 0.21]}
  {:iteration 97,
   :energy 1.3734401083075733,
   :gradients
   [-0.9182199142388802
    0.0
    -0.23313097969540406
    7.850462293418876E-17],
   :parameters [0.9130964099001224 0.19 0.6571248441841822 0.21]}
  {:iteration 98,
   :energy 1.3607684282124954,
   :gradients [-0.9173103659409333 0.0 -0.22391458142502238 0.0],
   :parameters [0.9222786090425112 0.19 0.6594561539811362 0.21]}
  {:iteration 99,
   :energy 1.348181822310566,
   :gradients [-0.9160673021696734 0.0 -0.21449509555781202 0.0],
   :parameters [0.9314517127019206 0.19 0.6616952997953864 0.21]}],
 :mixer-hamiltonian
 ({:coefficient 1.0, :pauli-string "XII"}
  {:coefficient 1.0, :pauli-string "IXI"}
  {:coefficient 1.0, :pauli-string "IIX"}),
 :problem-type :max-cut,
 :reason :max-iterations,
 :objective-kind :hamiltonian,
 :beta-parameters [0.19 0.21],
 :approximation-ratio nil,
 :success false,
 :iterations 100,
 :circuit
 {:operations
  [{:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}}
   {:operation-type :h, :operation-params {:target 2}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :rz,
    :operation-params {:target 1, :angle -0.9314517127019206}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 1, :target 2}}
   {:operation-type :rz,
    :operation-params {:target 2, :angle -0.9314517127019206}}
   {:operation-type :cnot, :operation-params {:control 1, :target 2}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :rz,
    :operation-params {:target 2, :angle -0.9314517127019206}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :rx, :operation-params {:target 0, :angle 0.38}}
   {:operation-type :rx, :operation-params {:target 1, :angle 0.38}}
   {:operation-type :rx, :operation-params {:target 2, :angle 0.38}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :rz,
    :operation-params {:target 1, :angle -0.6616952997953864}}
   {:operation-type :cnot, :operation-params {:control 0, :target 1}}
   {:operation-type :cnot, :operation-params {:control 1, :target 2}}
   {:operation-type :rz,
    :operation-params {:target 2, :angle -0.6616952997953864}}
   {:operation-type :cnot, :operation-params {:control 1, :target 2}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :rz,
    :operation-params {:target 2, :angle -0.6616952997953864}}
   {:operation-type :cnot, :operation-params {:control 0, :target 2}}
   {:operation-type :rx, :operation-params {:target 0, :angle 0.42}}
   {:operation-type :rx, :operation-params {:target 1, :angle 0.42}}
   {:operation-type :rx, :operation-params {:target 2, :angle 0.42}}],
  :num-qubits 3,
  :name
  "QAOA Ansatz + Hamiltonian Evo + Hamiltonian Evo + Hamiltonian Evo + Hamiltonian Evo"},
 :initial-energy 1.989764973720861,
 :num-qubits 3,
 :gamma-parameters [0.9314517127019206 0.6616952997953864],
 :optimal-parameters [0.9314517127019206 0.19 0.6616952997953864 0.21],
 :num-layers 2}

```
The result shows that the QAOA algorithm approximated the optimal solution
for the Max-Cut problem on the triangular graph, which is 2.0.
```clj
(:result triangle-qaoa-result)

;; =>
nil

```
The final circuit with the optimal parameters is
```clj
^kind/hiccup
(viz/visualize-circuit :hiccup (:circuit triangle-qaoa-result))
```
![](tutorial_files/image34.svg)

The circuit shows that the QAOA algorithm applies a series of parameterized
quantum gates to the qubits, which represent the trial state for the Max-Cut problem.
The circuit also applies the cost function as a quantum gate and measures
the qubits to obtain the expectation value ⟨C⟩, which is maximized using
a classical optimization algorithm.

The circuit is composed of alternating layers of problem unitary and mixer unitary gates,
where the problem unitary encodes the cost function and the mixer unitary
introduces superposition and entanglement among the qubits.
The number of layers determines the depth of the circuit and the expressibility
of the trial state.