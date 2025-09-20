;; 
;;

(ns qclojure-intro
  (:require
   [org.soulspace.qclojure.domain.state :as state]
   [org.soulspace.qclojure.domain.circuit :as circuit]))

;; # Overview
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
;; 
(viz/visualize-quantum-state :ascii state/|0⟩)
;;
(viz/visualize-quantum-state :svg state/|0⟩)

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
(viz/visualize-circuit :ascii test-circuit)

;; ## Visualizing the Circuit in SVG
(viz/visualize-circuit :svg test-circuit)

;; ## Simulating the Circuit
(def result (circuit/execute-circuit test-circuit))

result