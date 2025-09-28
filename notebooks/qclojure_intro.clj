;; # Overview
^{:kindly/hide-code true}
(ns qclojure-intro
  (:require
   [scicloj.kindly.v4.kind :as kind]))

;; ## Quantum Computing Basics
;;
;; * Qubits and Quantum States
;; * Quantum Gates and Circuits
;; * Measurement and Observables
;; * Quantum Algorithms
;;
;; ## QClojure Features
;;
;; * Pure Functional Quantum Circuit Construction
;; * Comprehensive Gate Library
;; * Quantum Algorithm Implementations
;; * Extensible Quantum Backend System
;; 

;; # Getting Started
;; ## Required Namespaces
;;
;; * Domain Namespaces
(require '[org.soulspace.qclojure.domain.state :as state])
(require '[org.soulspace.qclojure.domain.gate :as gate])
(require '[org.soulspace.qclojure.domain.circuit :as circuit])
;;
;; * Visualisation Namespaces
(require '[org.soulspace.qclojure.application.visualization :as viz])
(require '[org.soulspace.qclojure.adapter.visualization.ascii :as ascii])
(require '[org.soulspace.qclojure.adapter.visualization.svg :as svg])

;; # Quantum States
;;
;; ## Zero State
;;
;; * Fundamental basis state in quantum computing
;; * Represents the binary value 0 in a qubit
;; * Represented as a state vector
state/|0⟩
;; ## Probability Distribution of Zero State
;;
^kind/hiccup (viz/visualize-quantum-state :svg state/|0⟩)

;; ## One State
state/|1⟩

;; ## Plus State
state/|+⟩

;; ## Minus State
state/|-⟩

;; ## i State
state/|+i⟩

;; ## -i State
state/|-i⟩

;; # Quantum Gates
;; ## Pauli-X Gate
gate/pauli-x
;; ## Pauli-Y Gate
gate/pauli-y
;; ## Pauli-Z Gate
gate/pauli-z
;; ## Hadamard Gate
gate/hadamard
;; ## CNOT Gate
gate/cnot

;; # Quantum Circuits
;; ## Creating a Quantum Circuit
(def test-circuit
  (-> (circuit/create-circuit 2 "Bell Circuit" "Creates a Bell state")
      (circuit/h-gate 0)
      (circuit/cnot-gate 0 1)))
;; ## Visualizing the Circuit in ASCII
^kind/code
(viz/visualize-circuit :ascii test-circuit)

;; ## Visualizing the Circuit in SVG
^kind/hiccup
(viz/visualize-circuit :svg test-circuit)

;; # Backends
;; * Hardware Abstraction Layer
;; * Multiple Backend Implementations
(require '[org.soulspace.qclojure.application.backend :as backend])
(require '[org.soulspace.qclojure.domain.result :as result])
(require '[clojure.spec.alpha :as s])
(require '[fastmath.complex :as fc])

;; ## Result Extraction
;;
(def result-specs {:measurements {:qubits [0 1 2]
                                  :shots 64}})
;; ## Ideal Simulator Backend
;; * Noiseless simulation of quantum circuits
;; * Suitable for testing and debugging quantum algorithms
(require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as ideal])
(def ideal-simulator (ideal/create-simulator))
;; ## Executing Circuit on Ideal Simulator Backend
(def ideal-result (backend/execute-circuit
                   ideal-simulator
                   (circuit/ghz-state-circuit 3)
                   {:result-specs result-specs}))


(result/extract-results {:final-state {:state-vector [(fc/complex 0.7071067811865475, 0.0) (fc/complex 0.0, 0.0)
                                                      (fc/complex 0.0, 0.0) (fc/complex 0.0, 0.0)
                                                      (fc/complex 0.0, 0.0) (fc/complex 0.0, 0.0)
                                                      (fc/complex 0.0, 0.0) (fc/complex 0.7071067811865475, 0.0)],
                                       :num-qubits 3},
                         :result-types #{:measurements},
                         :circuit {:operations [{:operation-type :h, :operation-params {:target 0}}
                                                {:operation-type :cnot, :operation-params {:control 0, :target 1}}
                                                {:operation-type :cnot, :operation-params {:control 0, :target 2}}],
                                   :num-qubits 3,
                                   :name "GHZ State",
                                   :description "Prepares 3-qubit GHZ state"},
                         :circuit-metadata {:circuit-depth 3,
                                            :circuit-operation-count 3,
                                            :circuit-gate-count 3,
                                            :num-qubits 3}}
                        {:measurements {:qubits [0 1 2], :shots 64}})

ideal-result

;; ## Visualizing the Measurement Frequency Histogram
#_^kind/hiccup
(viz/visualize-measurement-histogram
 :hiccup
 (get-in ideal-result [:results :frequencies]))

;; ## Hardware Simulator Backend
(require '[org.soulspace.qclojure.adapter.backend.hardware-simulator :as hwsim])
(def hw-simulator (hwsim/create-hardware-simulator))

(backend/select-device hw-simulator (:ibm-lagos hwsim/device-map))
;; ## Executing Circuit on Hardware Simulator Backend
(def noisy-result (backend/execute-circuit
                   hw-simulator
                   (circuit/ghz-state-circuit 3)
                   {:shots 128
                    :result-specs result-specs}))

(result/extract-noisy-results {:job-status :completed,
                               :measurement-results {111 451, 000 501, 011 17, 001 16, 100 5, 110 17, 101 14, 010 3},
                               :final-state {:state-vector [(fc/complex 0.7071067811865475, 0.0) (fc/complex 0.0, 0.0)
                                                            (fc/complex 0.0, 0.0) (fc/complex 0.0, 0.0)
                                                            (fc/complex 0.0, 0.0) (fc/complex 0.0, 0.0)
                                                            (fc/complex 0.0, 0.0) (fc/complex 0.7071067811865475, 0.0)],
                                             :num-qubits 3},
                               :noise-applied true,
                               :shots-executed 1024,
                               :execution-time-ms 395}
                              {:measurements {:qubits [0 1 2], :shots 64}}
                              (circuit/ghz-state-circuit 3))

noisy-result

;; ## Visualizing the Measurement Frequency Histogram
#_^kind/hiccup
(viz/visualize-measurement-histogram
 :hiccup 
 (get-in noisy-result [:measurement-results]))

;; # Algorithms
;; * Textbook implementations of quantum algorithms
;;   * Deutsch, Bernstein-Vazirani, Simon, Grover
;; * Quantum Fourier Transform (QFT), Phase Estimation (QPE), Shor's Algorithm
;; * Variational Quantum Algorithms (VQE, QAOA)
;;
;; # Future Directions
;;
;; * Fast complex linear algebra (CPU/GPU)
;;   * Bring complex BLAS/LAPACK algorithms to Clojure
;; * Domain specific libraries
;;   * Quantum chemistry, Quantum Machine Learning
;; * More backends