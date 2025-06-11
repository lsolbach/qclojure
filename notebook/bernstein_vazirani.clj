(ns bernstein-vazirani)

;; # The Bernstein-Vazirani Algorithm
;; The Bernstein-Vazirani algorithm is a quantum algorithm that solves the problem of determining a hidden string using fewer evaluations than would be possible classically.
;;
;; ## Problem Statement
;; Given a function \( f: \{0, 1\}^n \to \{0, 1\} \) defined as \( f(x) = s \cdot x \) (where \( s \) is a hidden string and \( \cdot \) denotes the dot product), the goal is to find the hidden string \( s \) using as few evaluations of \( f \) as possible.
;;
;; ## Classical Approach
;; In a classical setting, we would need to evaluate the function \( f \) multiple times to determine the hidden string \( s \). The number of evaluations required can grow linearly with the size of the input.
;;
;; ## Quantum Approach
;; The Bernstein-Vazirani algorithm allows us to find the hidden string \( s \) with only one evaluation by leveraging quantum superposition and interference.
;;
;; ## Quantum Circuit
;; The Bernstein-Vazirani algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize \( n \) qubits in the state \( |0\rangle \) and an auxiliary qubit in the state \( |1\rangle \).
;; 2. Apply a Hadamard gate to all \( n \) qubits to create superposition.
;; 3. Apply the function \( f \) as a quantum gate, which will entangle the qubits.
;; 4. Apply another Hadamard gate to all \( n \) qubits.
;; 5. Measure the qubits to obtain the hidden string \( s \).

