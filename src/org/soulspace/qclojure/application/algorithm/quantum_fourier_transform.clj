(ns org.soulspace.qclojure.application.algorithm.quantum-fourier-transform 
  "Quantum Fourier Transform (QFT) and Inverse QFT (IQFT) implementation.

   The QFT is a fundamental quantum algorithm that transforms quantum states
   into their frequency domain representation. It is widely used in various
   quantum algorithms, including Shor's factoring algorithm and quantum phase estimation.

   This namespace provides functions to create QFT and IQFT circuits for a given
   number of qubits."
  (:require [org.soulspace.qclojure.domain.circuit :as qc]))

(defn quantum-fourier-transform-circuit
  "Create a Quantum Fourier Transform (QFT) circuit.
  
  Creates a complete QFT circuit that transforms computational basis states
  into their quantum Fourier transformed states. The QFT is the quantum
  analog of the discrete Fourier transform and is essential for many quantum
  algorithms including Shor's factoring algorithm and quantum phase estimation.
  
  The QFT algorithm consists of:
  1. Apply Hadamard gate to each qubit
  2. Apply controlled rotation gates with angles π/2^k
  3. Reverse qubit order with SWAP gates
  
  Parameters:
  - n: Number of qubits
  
  Returns:
  Quantum circuit implementing the complete QFT
  
  Example:
  (def qft-circuit (quantum-fourier-transform-circuit 3))
  ;=> Complete 3-qubit QFT circuit"
  [n]
  {:pre [(pos-int? n)]}
  (let [circuit (qc/create-circuit n "QFT" "Quantum Fourier Transform")]
    (-> circuit
        ;; Apply QFT to each qubit
        ((fn [c]
           (reduce (fn [circuit qubit]
                     ;; Apply Hadamard gate to current qubit
                     (let [h-circuit (qc/h-gate circuit qubit)]
                       ;; Apply controlled rotation gates
                       (reduce (fn [inner-circuit k]
                                 (let [control-qubit (+ qubit k 1)
                                       angle (/ Math/PI (Math/pow 2 (inc k)))]
                                   (if (< control-qubit n)
                                     (qc/crz-gate inner-circuit control-qubit qubit angle)
                                     inner-circuit)))
                               h-circuit
                               (range (- n qubit 1)))))
                   c
                   (range n))))
        ;; Reverse qubit order with SWAP gates
        ((fn [c]
           (reduce (fn [circuit i]
                     (let [j (- n 1 i)]
                       (if (< i j)
                         (qc/swap-gate circuit i j)
                         circuit)))
                   c
                   (range (quot n 2))))))))

(defn inverse-quantum-fourier-transform-circuit
  "Create an Inverse Quantum Fourier Transform (IQFT) circuit.
  
  The IQFT undoes the QFT and is critical for quantum phase estimation
  in Shor's algorithm and other quantum algorithms.
  
  Parameters:
  - n: Number of qubits
  
  Returns:
  Quantum circuit implementing the complete IQFT
  
  Example:
  (def iqft-circuit (inverse-quantum-fourier-transform-circuit 3))"
  [n]
  (qc/inverse-circuit (quantum-fourier-transform-circuit n)))

