(ns grover)

;; # The Grover's Search Algorithm
;; Grover's search algorithm is a quantum algorithm that provides a quadratic speedup for searching an unsorted database. It is one of the most well-known quantum algorithms and demonstrates the power of quantum computing for search problems.
;;
;; ## Problem Statement
;; Given a function \( f: \{0, 1\}^n \to \{0, 1\} \) that is promised to have exactly one input \( x \) such that \( f(x) = 1 \) (the "marked" item), the goal is to find this input \( x \) using as few evaluations of \( f \) as possible.
;;
;; ## Classical Approach
;; In a classical setting, we would need to evaluate the function \( f \) up to \( 2^{n-1} \) times in the worst case to find the marked item. This is because we would have to check each possible input until we find the one that satisfies \( f(x) = 1 \).
;;
;; ## Quantum Approach
;; Grover's search algorithm allows us to find the marked item with only \( O(\sqrt{2^n}) \) evaluations of \( f \), which is a significant improvement over the classical approach.
;;
;; ## Quantum Circuit
;; The Grover's search algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Initialize \( n \) qubits in the state \( |0\rangle \).
;; 2. Apply a Hadamard gate to all \( n \) qubits to create superposition, resulting in an equal superposition of all possible inputs.
;; 3. Apply the Grover diffusion operator, which consists of:
;;    - Applying the oracle function \( f \) as a quantum gate, which flips the sign of the amplitude of the marked item.
;;    - Applying a Hadamard gate to all qubits.
;;    - Applying a conditional phase shift to the \( |0\rangle \) state.
;;    - Applying another Hadamard gate to all qubits.
;; 4. Repeat the Grover diffusion operator \( O(\sqrt{2^n}) \) times.
