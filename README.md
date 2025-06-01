# QClojure
A functional library for building and simulating quantum algorithms.

**Very alpha, I expect the api to change a lot!** 

## Features
- Pure functional approach to quantum computing
- Simulates quantum circuits with various gate operations
- Supports common quantum algorithms (Deutsch, Grover's, Bernstein-Vazirani, Simon's)
- Extensible backend system for different simulators

## Usage
```clojure
(require '[org.soulspace.qclojure.domain.quantum-circuit :as qc])
(require '[org.soulspace.qclojure.adapter.backend.quantum-simulator :as sim])

;; Create a simple circuit
(def circuit (-> (qc/create-circuit 2)
                 (qc/add-gate (qc/h-gate 0))
                 (qc/add-gate (qc/cnot-gate 0 1))))

;; Run the circuit on the simulator
(def simulator (sim/create-simulator))
(def result (qc/execute-circuit circuit simulator))

;; Examine the results
(qc/get-state result)
(qc/measure-all result)
```

## Build
QClojure currently has a Leiningen build.

## Copyright
© 2025 Ludger Solbach

## License
Eclipse Public License 1.0 (EPL1.0)

