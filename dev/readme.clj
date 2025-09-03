(ns readme)

;; Example code for the README

(comment
(require '[org.soulspace.qclojure.domain.state :as qs])
(require '[org.soulspace.qclojure.application.visualization :as viz])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; The plus state, a state in superposition with equal
;; probabilities of measuring |0⟩ or |1⟩
qs/|+⟩

;; Create two SVG visualizations of the plus state 
(spit "prob-plus-state.svg" (viz/visualize-quantum-state :svg qs/|+⟩))
(spit "bloch-plus-state.svg" (viz/visualize-bloch-sphere :svg qs/|+⟩))
)

(comment
(require '[org.soulspace.qclojure.domain.circuit :as qc])
(require '[org.soulspace.qclojure.application.backend :as qb])
(require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
(require '[org.soulspace.qclojure.application.visualization :as viz])
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
(spit "bell-circuit.svg" (viz/visualize-circuit :svg circuit))
(spit "bell-state.svg" (viz/visualize-quantum-state :svg (:final-state result)))
)

(comment
(require '[org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv])
(require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
(require '[org.soulspace.qclojure.application.visualization :as viz])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; Bernstein-Vazirani algorithm example
(def bv-result (bv/bernstein-vazirani-algorithm
                (sim/create-simulator) [1 0 1 0] {:shots 100}))

;; Examine the results
(get-in bv-result [:execution-result :final-state])
(get-in bv-result [:execution-result :measurement-results])

;; Create SVG visualizations for the circuit and the final state
(spit "bv-circuit.svg" (viz/visualize-circuit :svg (bv/bernstein-vazirani-circuit [1 0 1 0])))
(spit "bv-state.svg" (viz/visualize-quantum-state :svg (get-in bv-result [:execution-result :final-state])))
)
