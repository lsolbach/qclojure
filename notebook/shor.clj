(ns shor)

;; # The Shor's Algorithm
;; Shor's algorithm is a quantum algorithm that efficiently factors large integers, which is a problem that is believed to be hard for classical computers. It is one of the most famous quantum algorithms and has significant implications for cryptography.
;;
;; ## Problem Statement
;; Given a composite integer \( N \), the goal is to find its prime factors using as few evaluations of a function as possible.
;;
;; ## Classical Approach
;; In a classical setting, factoring large integers is a computationally hard problem. The best-known classical algorithms for factoring have exponential time complexity, making them impractical for large \( N \).
;;
;; ## Quantum Approach
;; Shor's algorithm allows us to factor large integers in polynomial time by leveraging quantum superposition, interference, and the quantum Fourier transform.
;;
;; ## Quantum Circuit
;; The Shor's algorithm can be implemented using a quantum circuit with the following steps:
;; 1. Choose a random integer \( a \) such that \( 1 < a < N \).
;; 2. Use the quantum period-finding algorithm to find the order \( r \) of \( a \) modulo \( N \). This involves:
;;    - Initialize \( n \) qubits in the state \( |0\rangle \).
;;    - Apply a Hadamard gate to all \( n \) qubits to create superposition.
;;    - Apply the modular exponentiation function \( a^x \mod N \) as a quantum gate, which will entangle the qubits.
;;    - Apply the quantum Fourier transform to the qubits.
;;    - Measure the qubits to obtain a value that can be used to find the order \( r \).
;; 3. If \( r \) is even and \( a^{r/2} \not\equiv -1 \mod N \), then compute the factors:
;;    - Compute \( \gcd(a^{r/2} - 1, N) \) and \( \gcd(a^{r/2} + 1, N) \).
;; 4. If the factors are non-trivial, return them as the prime factors of \( N \).
;; 5. If the order \( r \) is odd or if the above conditions are not met, repeat the process with a different random integer \( a \).
