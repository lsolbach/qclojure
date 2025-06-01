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
(require '[org.soulspace.qclojure.application.quantum-backend :as qb])
(require '[org.soulspace.qclojure.adapter.backend.quantum-simulator :as sim])

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

## Build
QClojure currently has a Leiningen build.

## Copyright
© 2025 Ludger Solbach

## License
Eclipse Public License 1.0 (EPL1.0)

