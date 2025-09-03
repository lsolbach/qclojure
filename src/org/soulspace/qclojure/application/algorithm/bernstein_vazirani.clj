(ns org.soulspace.qclojure.application.algorithm.bernstein-vazirani
  "Bernstein-Vazirani Algorithm
   
   The Bernstein-Vazirani algorithm is a quantum algorithm that efficiently
   determines a hidden bit string s using only one query to a quantum oracle.
   It is a foundational example of quantum speedup over classical algorithms,
   demonstrating how quantum circuits can solve specific problems more
   efficiently than their classical counterparts.
   
   This implementation builds the quantum circuit for the Bernstein-Vazirani
   algorithm and executes it on a specified quantum backend.
   
   The algorithm uses a quantum oracle Uf that computes f(x) = s·x (mod 2),
   where s is the hidden string and x is the input bit string.
   
   The algorithm requires only one query to the oracle to determine the hidden
   string s, while classical algorithms would require n queries for an n-bit
   string."
  (:require
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.domain.state :as qs]
   [org.soulspace.qclojure.application.backend :as qb]))

;;;
;;; Bernstein-Vazirani Algorithm
;;;

(defn add-oracle-fn
  "Build the quantum circuit for Bernstein-Vazirani oracle Uf.
  
  Parameters:
  - hidden-string: Vector of bits representing the hidden string s
  - n-qubits: Number of input qubits
  
  Returns:
  A function that takes a quantum circuit and applies the Bernstein-Vazirani oracle Uf to it."
  [hidden-string n-qubits]
  {:pre [(vector? hidden-string)
         (every? #(or (= % 0) (= % 1)) hidden-string)
         (= (count hidden-string) n-qubits)]}
  
  (fn [circuit]
    ;; Apply CNOT gates based on hidden string
    (reduce (fn [c bit-idx]
              (if (= 1 (nth hidden-string bit-idx))
                (qc/cnot-gate c bit-idx n-qubits)  ; Control: input qubit, Target: ancilla
                c))
            circuit
            (range n-qubits))))

(defn bernstein-vazirani-circuit
  "Build the quantum circuit for Bernstein-Vazirani algorithm.
  
  Parameters:
  - hidden-string: Vector of bits representing the hidden string s
  - n-qubits: Number of input qubits
  
  Returns:
  A quantum circuit implementing the Bernstein-Vazirani algorithm using the provided hidden string."
  [hidden-string]
  {:pre [(vector? hidden-string)
         (every? #(or (= % 0) (= % 1)) hidden-string)]}
  
  (let [n (count hidden-string)]
    (-> (qc/create-circuit (inc n) "Bernstein-Vazirani Algorithm"
                           "Finds hidden bit string s with one query")
        ;; Initialize ancilla qubit to |1⟩
        (qc/x-gate n)
        ;; Apply Hadamard to all qubits
        ((fn [circ]
           (reduce #(qc/h-gate %1 %2) circ (range (inc n)))))
        ;; Apply oracle Uf based on hidden string
        ((add-oracle-fn hidden-string n))
        ;; Apply Hadamard to input qubits only
        ((fn [circ]
           (reduce #(qc/h-gate %1 %2) circ (range n))))
        ;; Measure input qubits
        ((fn [circ]
           (reduce #(qc/measure-operation %1 [%2]) circ (range n)))))))

(defn bernstein-vazirani-algorithm
  "Implement the Bernstein-Vazirani algorithm to find a hidden bit string.
  
  The algorithm determines a hidden n-bit string s efficiently, given access to
  a quantum oracle that computes f(x) = s·x (mod 2) where s·x is the dot product.
  Classical algorithms require n queries, while BV requires only 1 quantum query.
  
  Algorithm steps:
  1. Initialize |0⟩^⊗n|1⟩ (n input qubits + 1 ancilla)
  2. Apply Hadamard to all qubits: |+⟩^⊗n|-⟩ 
  3. Apply oracle function f(x) = s·x (mod 2)
  4. Apply Hadamard to input qubits
  5. Measure input qubits to get s directly
  
  Parameters:
  - backend: Quantum backend implementing the QuantumBackend protocol to execute the circuit
  - hidden-string: Vector of bits [0 1 0 1 ...] representing the hidden string s
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - Measured bit string (should match hidden-string)
  - :hidden-string - The original hidden string
  - :success - Boolean indicating if measurement matched hidden string
  - :circuit - Description of the quantum circuit used
  - :execution-result - Backend execution results
  
  Example:
  (bernstein-vazirani-algorithm [1 0 1 0] backend)  ;=> Should measure [1 0 1 0]"
  ([backend hidden-string]
   (bernstein-vazirani-algorithm backend hidden-string {:shots 1024}))
  ([backend hidden-string options]
  {:pre [(vector? hidden-string)
         (every? #(or (= % 0) (= % 1)) hidden-string)]}
  
  (let [;; Build circuit for the Bernstein-Vazirani algorithm
        circuit (bernstein-vazirani-circuit hidden-string)
        n-input-qubits (count hidden-string)
        n-total-qubits (inc n-input-qubits)

        ;; Result specifications for the Bernstein-Vazirani algorithm
        ;; Include both measurements and probabilities for validation
        result-specs {:result-specs {:measurements {:shots (:shots options)}
                                     :probabilities {:qubits (range n-input-qubits)}}}

        options (merge options result-specs)
        
        ;; Execute circuit on backend
        execution-result (qb/execute-circuit backend circuit options)
        results (:results execution-result)

        ;; Extract measurement results
        measurement-results (:measurement-results results)
        frequencies (:frequencies measurement-results)

        ;; Convert the most frequent measurement outcome to bit string and extract input qubits
        ;; BV algorithm should consistently measure the hidden string on input qubits
        most-frequent-outcome (key (apply max-key val frequencies))
        outcome-bits (qs/measurement-outcomes-to-bits most-frequent-outcome n-total-qubits)
        measured-bits (vec (take n-input-qubits outcome-bits))

        success (= measured-bits hidden-string)]

    {:algorithm "Bernstein-Vazirani"
     :result measured-bits
     :success success
     :hidden-string hidden-string
     :circuit circuit
     :execution-result execution-result})))

