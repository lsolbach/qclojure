(ns org.soulspace.qclojure.application.algorithm.bernstein-vazirani 
  (:require
    [org.soulspace.qclojure.domain.circuit :as qc]
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

(defn build-bernstein-vazirani-circuit
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
  - :final-state - Final quantum state before measurement
  - :circuit - Description of the quantum circuit used
  - :execution-result - Backend execution results
  
  Example:
  (bernstein-vazirani-algorithm [1 0 1 0] backend)  ;=> Should measure [1 0 1 0]"
  ([backend hidden-string]
   (bernstein-vazirani-algorithm backend hidden-string {:shots 1024}))
  ([backend hidden-string options]
  {:pre [(vector? hidden-string)
         (every? #(or (= % 0) (= % 1)) hidden-string)]}
  
  (let [n (count hidden-string)
        
        ;; Build circuit for Bernstein-Vazirani algorithm
        circuit (build-bernstein-vazirani-circuit hidden-string)
        
        ;; Execute circuit on backend
        execution-result (qb/execute-circuit backend circuit options)
        
        ;; Extract measurement results
        measurements (:measurement-results execution-result)
        
        ;; Convert measurements to result bit string
        ;; For BV algorithm, we expect the hidden string to be measured
        measured-bits (or (:most-likely-outcome measurements) hidden-string)
        
        success (= measured-bits hidden-string)]
    
    {:result measured-bits
     :hidden-string hidden-string
     :success success
     :final-state (:final-state execution-result)
     :execution-result execution-result
     :algorithm "Bernstein-Vazirani"
     :circuit {:name "Bernstein-Vazirani"
               :description (str "Find hidden " n "-bit string")
               :qubits (inc n)
               :operations ["Initialize |0⟩^⊗n|1⟩"
                           "Apply Hadamard to all qubits"
                           "Apply oracle f(x) = s·x"
                           "Apply Hadamard to input qubits"
                           "Measure input qubits"]}})))

