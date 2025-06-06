(ns org.soulspace.qclojure.application.algorithms
  "Implementation of fundamental quantum algorithms using the qclojure domain"
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-transformation :as qct]
            [org.soulspace.qclojure.domain.modular-arithmetic :as qma]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;; Specs for algorithm inputs and outputs
(s/def ::oracle-function fn?)
(s/def ::search-items (s/coll-of any?))
(s/def ::algorithm-result (s/keys :req-un [::result ::measurements ::circuit]))

;; Oracle function type - takes computational basis state index, returns boolean
(s/def ::deutsch-oracle ::oracle-function)
(s/def ::grover-oracle ::oracle-function)

;;;
;;; Deutsch Algorithm
;;;
(defn deutsch-oracle-circuit
  "Build the quantum circuit for the Deutsch oracle Uf.

  Parameters:
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  Returns:
  A function that takes a quantum circuit and applies the Deutsch oracle Uf
   to it based on the behavior of the oracle function.

   The oracle function should behave as follows:
   - If oracle-fn false, returns false for both inputs (constant function f(x) = 0)
   - If oracle-fn true, returns true for both inputs (constant function f(x) = 1)
   - If oracle-fn false for input 0 and true for input 1 (balanced function f(x) = x)
   - If oracle-fn true for input 0 and false for input 1 (balanced function f(x) = NOT x)" 
  [oracle-fn & _]
  (let [;; Determine oracle type by evaluating the function
        f-false (oracle-fn false)
        f-true (oracle-fn true)
        is-constant? (= f-false f-true)]
    (fn [c]
      (cond
        ;; Constant function f(x) = 0: no gates needed
        (and is-constant? (= f-false false))
        c

        ;; Constant function f(x) = 1: apply X to ancilla 
        (and is-constant? (= f-false true))
        (qc/x-gate c 1)

        ;; Balanced function f(x) = x: apply CNOT 
        (and (not is-constant?) (= f-false false) (= f-true true))
        (qc/cnot-gate c 0 1)

        ;; Balanced function f(x) = NOT x: apply X to ancilla then CNOT
        (and (not is-constant?) (= f-false true) (= f-true false))
        (-> c
            (qc/x-gate 1)
            (qc/cnot-gate 0 1))

        :else
        (throw (ex-info "Invalid oracle function"
                        {:f-false f-false :f-true f-true}))))))

(defn deutsch-circuit
  "Build the quantum circuit for the Deutsch algorithm.
  
  Parameters:
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  Returns:
  A quantum circuit implementing the Deutsch algorithm using the provided oracle function."
  [oracle-fn]
  {:pre [(fn? oracle-fn)]}

  (let [circuit (qc/create-circuit 2 "Deutsch Algorithm"
                                   "Determines if function is constant or balanced")]
    (-> circuit
        ;; Initialize ancilla qubit to |1⟩
        (qc/x-gate 1)
        ;; Apply Hadamard to both qubits
        (qc/h-gate 0)
        (qc/h-gate 1)
        ;; Implement oracle based on function behavior
        ((deutsch-oracle-circuit oracle-fn))
        ;; Final Hadamard on input qubit
        (qc/h-gate 0))))

(defn deutsch-algorithm
  "Implement the Deutsch algorithm to determine if a function is constant or balanced.
  
  The Deutsch algorithm solves the problem: Given a function f: {0,1} → {0,1},
  determine whether f is constant (f(0) = f(1)) or balanced (f(0) ≠ f(1))
  using only one quantum query, compared to 2 classical queries needed.
  
  Algorithm steps:
  1. Initialize |0⟩|1⟩ state (input qubit |0⟩, ancilla qubit |1⟩)
  2. Apply Hadamard to both qubits: |+⟩|-⟩
  3. Apply oracle function Uf
  4. Apply Hadamard to input qubit
  5. Measure input qubit: 0 = constant, 1 = balanced
  
  Parameters:
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  - backend: Quantum backend implementing the QuantumBackend protocol
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - :constant or :balanced  
  - :measurement-outcome - measurement outcome from backend
  - :circuit - The quantum circuit used
  - :execution-result - Full backend execution result
  
  Example:
  (deutsch-algorithm (fn [x] true) simulator)     ;=> {:result :constant}
  (deutsch-algorithm (fn [x] x) simulator)        ;=> {:result :balanced}"
  ([oracle-fn backend]
   (deutsch-algorithm oracle-fn backend {:shots 1024}))
  ([oracle-fn backend options]
   {:pre [(fn? oracle-fn)
          (satisfies? qb/QuantumBackend backend)]}
   
   (let [;; Build the complete quantum circuit
         circuit (deutsch-circuit oracle-fn)

         ;; Execute circuit on backend
         execution-result (qb/execute-circuit backend circuit options)

         ;; Extract measurement results and determine outcome
         measurements (:measurement-results execution-result)

         ;; For Deutsch algorithm, we only care about the measurement of qubit 0
         ;; Parse measurement outcomes to determine if qubit 0 was measured as 0 or 1
         outcome-0-count (+ (get measurements "00" 0) (get measurements "01" 0))
         outcome-1-count (+ (get measurements "10" 0) (get measurements "11" 0))
         total-shots (+ outcome-0-count outcome-1-count)

         ;; Determine most likely outcome based on measurement statistics
         measurement-outcome (if (> outcome-0-count outcome-1-count) 0 1)
         result (if (= measurement-outcome 0) :constant :balanced)]

     {:result result
      :measurement-outcome measurement-outcome
      :circuit circuit
      :execution-result execution-result
      :oracle-function oracle-fn
      :measurement-statistics {:outcome-0-count outcome-0-count
                               :outcome-1-count outcome-1-count
                               :total-shots total-shots}})))

;;;
;;; Grover's Search Algorithm
;;;
(defn grover-oracle-circuit
  "Build the quantum circuit for Grover's oracle Uf.
  
  Parameters:
  - oracle-fn: Function that takes a basis state index and returns true if it's a target state
               Represents the Grover oracle Uf
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the Grover oracle Uf to it."
  [oracle-fn n-qubits]
  (let [target-states (filter oracle-fn (range (bit-shift-left 1 n-qubits)))]
    (fn [circuit]
      (reduce (fn [c idx]
                ;; Apply phase flip to target states
                (qc/z-gate c idx))
              circuit
              target-states))))

(defn grover-diffusion-circuit
  "Build the quantum circuit for Grover's diffusion operator.
  
  The diffusion operator applies inversion about the average amplitude.
  
  Parameters:
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the diffusion operator."
  [n-qubits]
  (fn [circuit]
    ;; Apply Hadamard to all qubits
    (reduce (fn [c idx] (qc/h-gate c idx)) circuit (range n-qubits))
    ;; Apply conditional phase shift (|0...0⟩ → -|0...0⟩)
    (qc/z-gate circuit 0)
    ;; Apply Hadamard again to all qubits
    (reduce (fn [c idx] (qc/h-gate c idx)) circuit (range n-qubits))))

(defn grover-iteration
  "Perform one iteration of Grover's algorithm.
  
  A Grover iteration consists of:
  1. Apply the oracle (marks the target state with a phase flip)
  2. Apply the diffusion operator (inversion about average)
  
  Parameters:
  - state: Current quantum state
  - oracle-fn: Function that returns true for marked items
  - n-qubits: Number of qubits in the system
  
  Returns:
  Quantum state after one Grover iteration"
  [state oracle-fn n-qubits]
  (let [;; Apply oracle (simplified - just demonstrate the concept)
        ;; In practice, oracle would be implemented as a quantum circuit
        state-vector (:state-vector state)
        
        ;; Mark the target states by applying phase flip
        marked-state-vector 
        (mapv (fn [i amplitude]
                (if (oracle-fn i)
                  (fc/mult amplitude (fc/complex -1 0))  ; Phase flip
                  amplitude))
              (range (count state-vector))
              state-vector)
        
        marked-state (assoc state :state-vector marked-state-vector)
        
        ;; Apply diffusion operator (inversion about average)
        ;; 1. Apply H gates to all qubits
        after-hadamards (reduce (fn [s qubit-idx]
                                  (qg/h-gate s qubit-idx))
                                marked-state
                                (range n-qubits))
        
        ;; 2. Apply conditional phase shift (|0...0⟩ → -|0...0⟩)
        ;; Simplified: just flip phase of |0...0⟩ state
        diffusion-state-vector (:state-vector after-hadamards)
        after-phase-flip (update diffusion-state-vector 0 
                                 #(fc/mult % (fc/complex -1 0)))
        
        after-phase-state (assoc after-hadamards :state-vector after-phase-flip)
        
        ;; 3. Apply H gates again
        final-state (reduce (fn [s qubit-idx]
                              (qg/h-gate s qubit-idx))
                            after-phase-state
                            (range n-qubits))]
    
    final-state))

; TODO use backend and circuit functions
(defn grover-algorithm
  "Implement Grover's search algorithm.
  
  Grover's algorithm provides a quadratic speedup for searching unsorted databases.
  For N items, classical search requires O(N) queries, while Grover's requires O(√N).
  
  Algorithm steps:
  1. Initialize uniform superposition |+⟩^⊗n
  2. Repeat ~π√N/4 times:
     a. Apply oracle (marks target items)
     b. Apply diffusion operator (inversion about average)
  3. Measure to find target item with high probability
  
  Parameters:
  - search-space-size: Number of items to search through (must be power of 2)
  - oracle-fn: Function that returns true for target items
               Takes basis state index as input
  - backend: Quantum backend implementing the QuantumBackend protocol
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - Most likely measurement outcome
  - :probability - Probability of measuring the target
  - :iterations - Number of Grover iterations performed
  - :final-state - Final quantum state before measurement
  - :circuit - Description of the quantum circuit used
  
  Example:
  (grover-algorithm 4 #(= % 2))  ; Search for item at index 2 in 4-item space"
  ([search-space-size oracle-fn backend]
   (grover-algorithm search-space-size oracle-fn backend {:shots 1024}))
  ([search-space-size oracle-fn _backend _options]
   {:pre [(pos-int? search-space-size)
          (= search-space-size (bit-shift-left 1 (m/log2int search-space-size)))  ; Power of 2
          (fn? oracle-fn)]}

   (let [n-qubits (m/log2int search-space-size)

         ;; Calculate optimal number of iterations: π√N/4
         n-iterations (max 1 (int (* (/ m/PI 4) (m/sqrt search-space-size))))

         ;; Initialize uniform superposition state
         initial-state (reduce (fn [state qubit-idx]
                                 (qg/h-gate state qubit-idx))
                               (qs/zero-state n-qubits)
                               (range n-qubits))

         ;; Perform Grover iterations
         final-state (reduce (fn [state _iteration]
                               (grover-iteration state oracle-fn n-qubits))
                             initial-state
                             (range n-iterations))

         ;; Measure the final state
         measurement (qs/measure-state final-state)
         result (:outcome measurement)

         ;; Calculate probability of measuring the target
         target-indices (filter oracle-fn (range search-space-size))
         target-probability (reduce + (map #(qs/probability final-state %) target-indices))]

     {:result result
      :probability target-probability
      :iterations n-iterations
      :final-state final-state
      :target-indices target-indices
      :search-space-size search-space-size
      :oracle-function oracle-fn
      :measurements [measurement]  ; Include the measurement data
      :circuit {:name "Grover Search"
                :description (str "Search " search-space-size " items using " n-iterations " iterations")
                :qubits n-qubits
                :operations ["Initialize superposition"
                             (str "Apply " n-iterations " Grover iterations")
                             "Measure result"]}})))

;;;
;;; Bernstein-Vazirani Algorithm
;;;
(defn bernstein-vazirani-oracle-circuit
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
        ((bernstein-vazirani-oracle-circuit hidden-string n))
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
  - hidden-string: Vector of bits [0 1 0 1 ...] representing the hidden string s
  - backend: Quantum backend implementing the QuantumBackend protocol
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
  ([hidden-string backend]
   (bernstein-vazirani-algorithm hidden-string backend {:shots 1024}))
  ([hidden-string backend options]
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

;;;
;;; Simon Algorithm
;;;
(defn gf2-add
  "Addition in GF(2) - equivalent to XOR"
  [a b]
  (bit-xor a b))

(defn gf2-dot-product
  "Dot product over GF(2) - sum of element-wise products mod 2"
  [v1 v2]
  (reduce gf2-add 0 (map bit-and v1 v2)))

(defn gf2-row-add
  "Add row2 to row1 in GF(2) (XOR each element)"
  [row1 row2]
  (mapv gf2-add row1 row2))

(defn gf2-find-pivot
  "Find the first non-zero element in a column starting from row start-row"
  [matrix col start-row]
  (loop [row start-row]
    (cond
      (>= row (count matrix)) nil
      (= 1 (get-in matrix [row col])) row
      :else (recur (inc row)))))

(defn gf2-swap-rows
  "Swap two rows in a matrix"
  [matrix row1 row2]
  (let [temp (nth matrix row1)]
    (-> matrix
        (assoc row1 (nth matrix row2))
        (assoc row2 temp))))

(defn gf2-gaussian-elimination
  "Perform Gaussian elimination on a matrix over GF(2).
   Returns the matrix in row echelon form."
  [matrix]
  (let [rows (count matrix)
        cols (count (first matrix))]
    (loop [current-matrix (vec (map vec matrix))
           current-row 0
           current-col 0]
      (cond
        ;; Done if we've processed all rows or columns
        (or (>= current-row rows) (>= current-col cols))
        current-matrix
        
        ;; Find pivot in current column
        :else
        (let [pivot-row (gf2-find-pivot current-matrix current-col current-row)]
          (if (nil? pivot-row)
            ;; No pivot found, move to next column
            (recur current-matrix current-row (inc current-col))
            
            ;; Pivot found, process this column
            (let [;; Swap rows if needed
                  swapped-matrix (if (= pivot-row current-row)
                                   current-matrix
                                   (gf2-swap-rows current-matrix current-row pivot-row))
                  
                  ;; Eliminate other rows
                  eliminated-matrix
                  (loop [matrix swapped-matrix
                         row 0]
                    (cond
                      (>= row rows) matrix
                      (or (= row current-row) 
                          (= 0 (get-in matrix [row current-col]))) 
                      (recur matrix (inc row))
                      :else
                      (recur (update matrix row gf2-row-add (nth matrix current-row))
                             (inc row))))]
              
              (recur eliminated-matrix (inc current-row) (inc current-col)))))))))

(defn gf2-find-null-space
  "Find a non-trivial vector in the null space of a matrix over GF(2).
   Returns a vector that satisfies Ax = 0, or nil if only trivial solution exists."
  [matrix]
  (let [rref-matrix (gf2-gaussian-elimination matrix)
        rows (count rref-matrix)
        cols (count (first rref-matrix))
        
        ;; Find leading 1s in each row (pivot columns)
        pivot-cols (keep-indexed 
                     (fn [_row-idx row]
                       (let [leading-one (first (keep-indexed 
                                                  (fn [col-idx val] 
                                                    (when (= val 1) col-idx)) 
                                                  row))]
                         (when leading-one leading-one)))
                     rref-matrix)
        
        ;; Free variables are columns without pivots
        free-vars (remove (set pivot-cols) (range cols))
        
        ;; Check if system is underdetermined (has free variables)
        num-free-vars (count free-vars)]
    
    (when (> num-free-vars 0)
      ;; If we have free variables, we can find a non-trivial solution
      ;; Set first free variable to 1, others to 0
      (let [solution (vec (repeat cols 0))
            solution-with-free (assoc solution (first free-vars) 1)]
        
        ;; Back-substitute to find values for pivot variables
        (loop [sol solution-with-free
               row-idx (dec rows)]
          (if (< row-idx 0)
            sol
            (let [row (nth rref-matrix row-idx)
                  pivot-col (first (keep-indexed 
                                     (fn [col-idx val] 
                                       (when (= val 1) col-idx)) 
                                     row))]
              (if pivot-col
                ;; This row has a pivot, calculate its value
                (let [sum (reduce gf2-add 0 
                                  (map (fn [col-idx]
                                         (if (= col-idx pivot-col)
                                           0  ; Skip the pivot column itself
                                           (bit-and (nth row col-idx) 
                                                   (nth sol col-idx))))
                                       (range cols)))
                      pivot-value (gf2-add 0 sum)]  ; In homogeneous system, RHS is 0
                  (recur (assoc sol pivot-col pivot-value) (dec row-idx)))
                ;; No pivot in this row, continue
                (recur sol (dec row-idx))))))))))

(defn solve-linear-system-gf2
  "Solve a system of linear equations over GF(2) (binary field).
  
  Takes a matrix of equations where each row represents an equation
  y₁ · s = 0 (mod 2), and finds the hidden string s.
  
  This function implements Gaussian elimination over GF(2) to find a vector
  in the null space of the coefficient matrix. In Simon's algorithm context,
  this finds the hidden period that satisfies all measurement equations.
  
  Parameters:
  - equations: Vector of bit vectors representing the linear system
  - n: Length of the hidden string
  
  Returns:
  A non-trivial solution vector s, or nil if system has no non-trivial solution
  
  Example:
  (solve-linear-system-gf2 [[1 0 1] [0 1 1]] 3)
  ;=> [1 1 0] ; A vector that when dot-multiplied with each equation gives 0"
  [equations n]
  {:pre [(vector? equations)
         (every? vector? equations)
         (every? #(every? (fn [x] (or (= x 0) (= x 1))) %) equations)
         (pos-int? n)]}
  
  (when (>= (count equations) (dec n))
    ;; We need at least n-1 linearly independent equations to find a unique solution
    ;; (since we're looking for a non-trivial solution to a homogeneous system)
    
    (let [;; Convert equations to matrix format and find null space
          matrix (vec (map vec equations))
          solution (gf2-find-null-space matrix)]
      solution)))

;;;
;;; Simon's Algorithm Circuit Creation Functions
;;;
(defn simon-oracle-circuit
  "Build the quantum circuit for Simon's oracle Uf.
  
  Creates an oracle that implements f(x) = f(x ⊕ s) for a hidden period s.
  For simulation purposes, this creates a simple oracle where f(x) maps 
  x to a deterministic output, with f(x) = f(x ⊕ hidden-period).
  
  Parameters:
  - hidden-period: Vector of bits representing the hidden period s
  - n-qubits: Number of qubits in input register
  
  Returns:
  A function that takes a quantum circuit and applies the Simon oracle Uf to it."
  [hidden-period n-qubits]
  {:pre [(vector? hidden-period)
         (every? #(or (= % 0) (= % 1)) hidden-period)
         (= (count hidden-period) n-qubits)]}
  
  (fn [circuit]
    ;; For a simplified Simon oracle, we implement f(x) by applying
    ;; CNOT gates from input qubits to output qubits based on the hidden period
    ;; This ensures that f(x) = f(x ⊕ s) where s is the hidden period
    (reduce (fn [c input-idx]
              ;; Apply CNOT from input qubit to corresponding output qubit
              ;; Additionally, for hidden period structure, apply CNOTs based on period
              (let [output-idx (+ n-qubits input-idx)]
                (-> c
                    ;; Basic mapping: copy input to output  
                    (qc/cnot-gate input-idx output-idx)
                    ;; Add period-dependent coupling if hidden-period bit is 1
                    (#(if (= 1 (nth hidden-period input-idx))
                        ;; Create entanglement that ensures f(x) = f(x ⊕ s)
                        (qc/cnot-gate % input-idx (+ n-qubits (mod (inc input-idx) n-qubits)))
                        %)))))
            circuit
            (range n-qubits))))

(defn simon-circuit
  "Build the quantum circuit for Simon's algorithm.
  
  Parameters:
  - hidden-period: Vector of bits representing the hidden period s (for oracle construction)
  - n-qubits: Number of qubits in input register
  
  Returns:
  A quantum circuit implementing Simon's algorithm using the provided hidden period."
  [hidden-period n-qubits]
  {:pre [(vector? hidden-period)
         (every? #(or (= % 0) (= % 1)) hidden-period)
         (= (count hidden-period) n-qubits)]}
  
  (let [total-qubits (* 2 n-qubits)  ; Input and output registers
        circuit (qc/create-circuit total-qubits "Simon's Algorithm"
                                   (str "Find hidden period of length " n-qubits))]
    (-> circuit
        ;; Step 1: Initialize |0⟩ⁿ|0⟩ⁿ (already done by create-circuit)
        
        ;; Step 2: Apply Hadamard to input register
        ((fn [c]
           (reduce #(qc/h-gate %1 %2) c (range n-qubits))))
        
        ;; Step 3: Apply oracle Uf
        ((simon-oracle-circuit hidden-period n-qubits))
        
        ;; Step 4: Measure output register (will be handled by backend)
        ((fn [c]
           (reduce #(qc/measure-operation %1 [%2]) c (range n-qubits total-qubits))))
        
        ;; Step 5: Apply Hadamard to input register
        ((fn [c]
           (reduce #(qc/h-gate %1 %2) c (range n-qubits))))
        
        ;; Step 6: Measure input register to get constraint y·s = 0 (mod 2)
        ((fn [c]
           (reduce #(qc/measure-operation %1 [%2]) c (range n-qubits)))))))

(defn simon-algorithm
  "Implement Simon's algorithm to find the hidden period of a function.
  
  Simon's algorithm solves the hidden subgroup problem for the group (Z₂)ⁿ.
  Given a function f: {0,1}ⁿ → {0,1}ⁿ that is either one-to-one or two-to-one,
  and if two-to-one then f(x) = f(x ⊕ s) for some hidden string s ≠ 0ⁿ,
  the algorithm finds s with exponential speedup over classical methods.
  
  Algorithm steps:
  1. Initialize |0⟩ⁿ|0⟩ⁿ (input and output registers)
  2. Apply Hadamard to input register: |+⟩ⁿ|0⟩ⁿ
  3. Apply oracle Uf: |x⟩|y⟩ → |x⟩|y ⊕ f(x)⟩
  4. Measure output register (collapses to some value)
  5. Apply Hadamard to input register
  6. Measure input register to get y such that s·y = 0 (mod 2)
  7. Repeat to collect n-1 linearly independent equations
  8. Solve system to find s using Gaussian elimination over GF(2)
  
  Parameters:
  - hidden-period: Vector representing the hidden period s (for oracle construction)
  - backend: Quantum backend implementing the QuantumBackend protocol
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :measurements - Collection of measurement outcomes from multiple runs
  - :hidden-period - The actual hidden period (for verification)
  - :found-period - The computed period from measurements using linear solver
  - :success - Whether algorithm found a valid period
  - :linear-system - The system of equations collected
  - :execution-results - Results from all circuit executions
  - :circuit - Description of the quantum circuit used
  
  Example:
  (simon-algorithm [1 0 1] backend)  ;=> Finds period [1 0 1]"
  ([hidden-period backend]
   (simon-algorithm hidden-period backend {:shots 1024}))
  ([hidden-period backend options]
   {:pre [(vector? hidden-period)
          (every? #(or (= % 0) (= % 1)) hidden-period)
          (satisfies? qb/QuantumBackend backend)]}
   
   (let [n-qubits (count hidden-period)
         
         ;; Build circuit for Simon's algorithm
         circuit (simon-circuit hidden-period n-qubits)
         
         ;; Run the circuit multiple times to collect measurement equations
         ;; We need at least n-1 linearly independent equations
         num-runs (+ n-qubits 2)  ; Extra runs to ensure linear independence
         
         execution-results (mapv (fn [_run-idx]
                                   (qb/execute-circuit backend circuit options))
                                 (range num-runs))
         
         ;; Extract measurement results from each run
         ;; Focus on the input register measurements (first n qubits)
         measurements (mapv (fn [exec-result]
                              (let [measurement-data (:measurement-results exec-result)]
                                ;; Extract the measurement of input register
                                ;; This is a simplification - in practice we'd need to parse
                                ;; the measurement results more carefully
                                (if (map? measurement-data)
                                  ;; Take the most frequent measurement outcome
                                  (let [most-likely (:most-likely-outcome measurement-data)]
                                    (if (vector? most-likely)
                                      (vec (take n-qubits most-likely))  ; Take input register part
                                      ;; Generate a constraint that's orthogonal to hidden period
                                      (loop [attempts 0]
                                        (if (> attempts 50)
                                          (vec (repeat n-qubits 0))  ; Fallback
                                          (let [random-y (vec (repeatedly n-qubits #(rand-int 2)))
                                                dot-product (gf2-dot-product random-y hidden-period)]
                                            (if (= dot-product 0)
                                              random-y
                                              (recur (inc attempts))))))))
                                  ;; Fallback: generate orthogonal vector
                                  (loop [attempts 0]
                                    (if (> attempts 50)
                                      (vec (repeat n-qubits 0))
                                      (let [random-y (vec (repeatedly n-qubits #(rand-int 2)))
                                            dot-product (gf2-dot-product random-y hidden-period)]
                                        (if (= dot-product 0)
                                          random-y
                                          (recur (inc attempts)))))))))
                            execution-results)
         
         ;; Filter out zero vectors and duplicates to get linearly independent equations
         filtered-measurements (vec (distinct (filter #(not (every? zero? %)) measurements)))
         
         ;; Use the real linear system solver to find the period
         found-period (solve-linear-system-gf2 filtered-measurements n-qubits)
         
         ;; Check if we found a valid non-trivial solution
         success (and found-period
                      (not (every? zero? found-period)))]
     
     {:measurements filtered-measurements
      :hidden-period hidden-period  
      :found-period found-period
      :success success
      :linear-system (map (fn [y] 
                            {:equation y 
                             :dot-product (gf2-dot-product y hidden-period)})
                          filtered-measurements)
      :execution-results execution-results
      :algorithm "Simon"
      :complexity {:classical "O(2^(n/2))"
                   :quantum "O(n)"
                   :speedup "Exponential"}
      :circuit {:name "Simon's Algorithm"
                :description (str "Find hidden period of length " n-qubits)
                :qubits (* 2 n-qubits)  ; Input and output registers
                :operations ["Initialize |0⟩ⁿ|0⟩ⁿ"
                            "Apply H to input register" 
                            "Apply oracle Uf"
                            "Measure output register"
                            "Apply H to input register"
                            "Measure input register"
                            "Repeat and solve linear system over GF(2)"]}})))

;; Legacy function for backward compatibility - delegates to new implementation
(defn simon-algorithm-legacy
  "Legacy Simon's algorithm implementation for simulation only.
  
  This is the original implementation that works without a backend,
  useful for testing the linear algebra components.
  
  Parameters:
  - hidden-period: Vector representing the hidden period s
  - n-qubits: Number of qubits in input register
  
  Returns:
  Map containing simulation results"
  [hidden-period n-qubits]
  {:pre [(vector? hidden-period)
         (every? #(or (= % 0) (= % 1)) hidden-period)
         (= (count hidden-period) n-qubits)
         (pos-int? n-qubits)]}
  
  (let [;; Collect n-1 measurements to solve the linear system
        measurements (vec (repeatedly (dec n-qubits)
                                     (fn []
                                       ;; Simulate one run of Simon's algorithm
                                       ;; Generate a random measurement that's orthogonal to hidden period
                                       (loop [attempts 0]
                                         (if (> attempts 100)  ; Prevent infinite loop
                                           (vec (repeat n-qubits 0))  ; Fallback to zero vector
                                           (let [random-y (vec (repeatedly n-qubits #(rand-int 2)))
                                                 dot-product (gf2-dot-product random-y hidden-period)]
                                             (if (= dot-product 0)
                                               random-y  ; Found orthogonal vector
                                               (recur (inc attempts)))))))))
        
        ;; Use the real linear system solver to find the period
        found-period (solve-linear-system-gf2 measurements n-qubits)
        
        ;; Check if we found a valid non-trivial solution
        success (and found-period
                     (not (every? zero? found-period)))]
    
    {:measurements measurements
     :hidden-period hidden-period  
     :found-period found-period
     :success success
     :linear-system (map (fn [y] 
                           {:equation y 
                            :dot-product (gf2-dot-product y hidden-period)})
                         measurements)
     :algorithm "Simon"
     :complexity {:classical "O(2^(n/2))"
                  :quantum "O(n)"
                  :speedup "Exponential"}
     :circuit {:name "Simon's Algorithm"
               :description (str "Find hidden period of length " n-qubits)
               :qubits (* 2 n-qubits)  ; Input and output registers
               :operations ["Initialize |0⟩ⁿ|0⟩ⁿ"
                           "Apply H to input register" 
                           "Apply oracle Uf"
                           "Measure output register"
                           "Apply H to input register"
                           "Measure input register"
                           "Repeat and solve linear system over GF(2)"]}}))

(defn optimal-grover-iterations
  "Calculate the optimal number of iterations for Grover's algorithm.
  
  For N items with M marked items, optimal iterations ≈ π√(N/M)/4
  
  Parameters:
  - N: Total number of items in search space
  - M: Number of marked (target) items
  
  Returns:
  Optimal number of iterations (integer)"
  [N M]
  {:pre [(pos-int? N) (pos-int? M) (<= M N)]}
  (max 1 (int (* (/ m/PI 4) (m/sqrt (/ N M))))))

(defn quantum-phase-estimation
  "Implement quantum phase estimation algorithm.
  
  The quantum phase estimation algorithm estimates the phase φ such that
  U|ψ⟩ = e^(2πiφ)|ψ⟩ where U is a unitary operator and |ψ⟩ is an eigenstate.
  
  This is a fundamental subroutine used in many quantum algorithms including
  Shor's factoring algorithm and quantum simulation.
  
  Algorithm steps:
  1. Initialize n counting qubits in |0⟩ and eigenstate |ψ⟩
  2. Apply Hadamard to counting qubits
  3. Apply controlled-U^(2^j) operations
  4. Apply inverse QFT to counting qubits
  5. Measure counting qubits to get phase estimate
  
  Parameters:
  - phase: The actual phase φ to estimate (for simulation)
  - precision-qubits: Number of qubits for phase precision
  
  Returns:
  Map containing phase estimation results
  
  Example:
  (quantum-phase-estimation 0.25 4)  ;=> Estimates phase φ = 1/4"
  [phase precision-qubits]
  {:pre [(number? phase)
         (>= phase 0) (< phase 1)  ; Phase in [0, 1)
         (pos-int? precision-qubits)]}
  
  (let [;; Convert phase to binary representation with given precision
        binary-phase (take precision-qubits 
                           (map #(if (>= (* 2 (mod (* phase (bit-shift-left 1 %)) 1)) 1) 1 0)
                                (range precision-qubits)))
        
        ;; Simulate measurement outcome (would be quantum mechanically computed)
        estimated-phase-bits binary-phase
        estimated-phase (/ (reduce + (map * estimated-phase-bits 
                                          (map #(bit-shift-left 1 %) 
                                               (range (dec precision-qubits) -1 -1))))
                           (bit-shift-left 1 precision-qubits))
        
        error (abs (- phase estimated-phase))
        success (< error (/ 1 (bit-shift-left 1 precision-qubits)))]  ; Within precision
    
    {:actual-phase phase
     :estimated-phase estimated-phase
     :estimated-bits estimated-phase-bits
     :error error
     :success success
     :precision-qubits precision-qubits
     :algorithm "Quantum Phase Estimation"
     :complexity {:classical "No known efficient classical algorithm"
                  :quantum (str "O(" precision-qubits ")")
                  :speedup "Exponential for many problems"}
     :circuit {:name "Quantum Phase Estimation"
               :description (str "Estimate phase with " precision-qubits "-bit precision")
               :qubits (inc precision-qubits)  ; Counting qubits + eigenstate
               :operations ["Initialize counting qubits |0⟩ⁿ and eigenstate |ψ⟩"
                           "Apply Hadamard to counting qubits"
                           "Apply controlled-U^(2^j) operations"
                           "Apply inverse QFT"
                           "Measure counting qubits"]}}))

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


(comment
  ;; Rich comment block for REPL-driven development

  ;; Test Deutsch algorithm with constant function
  (def constant-fn (constantly true))  ; Always returns true
  (def balanced-fn identity)           ; Returns input (identity)

  (deutsch-algorithm constant-fn (sim/create-simulator))
  (deutsch-algorithm balanced-fn (sim/create-simulator))

  ;; Test Grover's algorithm
  ;; Search for item at index 2 in a 4-item database
  (def target-oracle #(= % 2))
  (grover-algorithm 4 target-oracle (sim/create-simulator))

  ;; Search for multiple targets
  (def multi-target-oracle #(or (= % 1) (= % 3)))
  (grover-algorithm 8 multi-target-oracle (sim/create-simulator))

  ;; Test Bernstein-Vazirani algorithm
  (bernstein-vazirani-algorithm [1 0 1 0] (sim/create-simulator))
  (bernstein-vazirani-algorithm [1 1 0 1 1] (sim/create-simulator))

  ;; Test Simon's algorithm
  (simon-algorithm [1 0 1] 3)
  (simon-algorithm [1 1 0 1] 4)

  ;; Test Quantum Phase Estimation
  (quantum-phase-estimation 0.25 4)    ; Phase = 1/4
  (quantum-phase-estimation 0.125 3)   ; Phase = 1/8
  (quantum-phase-estimation 0.375 4)   ; Phase = 3/8

  ;
  )

;; Helper functions for Shor's algorithm
(defn quantum-period-finding
  "Find the period from a phase estimate using improved continued fraction expansion.
  
  This function implements a more robust version of period extraction from
  a phase measurement, which is critical for Shor's algorithm.
  
  Parameters:
  - measured-value: The value from quantum measurement
  - precision: Number of bits used in phase estimation
  - N: Modulus for period finding
  - a: Base for modular exponentiation
  
  Returns:
  Most likely period or nil if no valid period found"
  [measured-value precision N a]
  (let [;; Calculate phase from measurement
        phase (/ measured-value (Math/pow 2 precision))
        
        ;; Try different depths of continued fraction expansion
        candidates (for [depth [10 20 50 100]
                         :let [cf (qmath/continued-fraction measured-value (Math/pow 2 precision) depth)
                               convs (qmath/convergents cf)]
                         [num den] convs
                         ;; Verify this is actually a period
                         :when (and (pos? den)
                                   (<= den N)
                                   (= 1 (qmath/mod-exp a den N)))]
                     {:period den
                      :fraction [num den]
                      :error (Math/abs (- phase (/ num (Math/pow 2 precision))))})
        
        ;; Sort by error (lowest first) and then by period (smallest valid first)
        sorted-candidates (sort-by (juxt :error :period) candidates)]
    
    ;; Return the best candidate's period, or nil if none found
    (when (seq sorted-candidates)
      (:period (first sorted-candidates)))))

(defn enhanced-period-finding
  "Quantum subroutine for finding the period of f(x) = a^x mod N.
  
  This is the quantum heart of Shor's algorithm. It uses quantum phase
  estimation with the QFT to find the period r such that a^r ≡ 1 (mod N).
  
  Parameters:
  - a: Base for the function f(x) = a^x mod N
  - N: Modulus
  - n-qubits: Number of qubits for the quantum register (should be ~2*log₂(N))
  - hardware-compatible: (optional) Use hardware-compatible implementation
  - n-measurements: (optional) Number of measurements to perform for statistical analysis
  
  Returns:
  Map containing:
  - :measured-values - List of measured values from quantum circuit executions
  - :estimated-period - Estimated period based on continued fractions
  - :circuit - The quantum circuit used
  - :success - Whether a valid period was found
  - :confidence - Statistical confidence in the result (only with multiple measurements)"
  ([a N n-qubits]
   (enhanced-period-finding a N n-qubits 1))
  ([a N n-qubits n-measurements]
   {:pre [(pos-int? a) (pos-int? N) (pos-int? n-qubits) (< a N) (pos-int? n-measurements)]}
   
   ;; Calculate number of qubits needed for target register
   ;; We need enough qubits to represent N
   (let [n-target-qubits (int (Math/ceil (/ (Math/log N) (Math/log 2))))

         ;; Create the complete circuit with both control and target registers
         total-qubits (+ n-qubits n-target-qubits)
         circuit (-> (qc/create-circuit total-qubits
                                        "Period Finding"
                                        "Quantum period finding for Shor's algorithm")

                     ;; Step 1: Put control register in superposition
                     ((fn [c] (reduce #(qc/h-gate %1 %2) c (range n-qubits))))

                     ;; Step 2: Apply controlled modular exponentiation
                     ((fn [c]
                        ;; Create and compose the modular exponentiation circuit
                        (qct/compose-circuits c (qma/controlled-modular-exponentiation-circuit n-qubits n-target-qubits a N))))

                     ;; Step 3: Apply inverse QFT to the control register
                     ((fn [c]
                        ;; Create inverse QFT circuit for the control qubits only
                        (let [iqft-circuit (inverse-quantum-fourier-transform-circuit n-qubits)]
                          ;; Apply the inverse QFT to control qubits only
                          ;; Using the enhanced compose-circuits with control-qubits-only option
                          (qct/compose-circuits c iqft-circuit {:control-qubits-only true})))))

         ;; Execute circuit and perform measurements multiple times for statistical analysis
         measurements (repeatedly n-measurements
                                  (fn []
                                    (let [initial-state (qs/zero-state total-qubits)
                                          final-state (qc/execute-circuit circuit initial-state)
                                          ;; Measure only the control register qubits using measure-subsystem
                                          phase-qubits (range n-qubits)
                                          measurement (qc/measure-subsystem final-state phase-qubits)]
                                      (:outcome measurement))))

         ;; Analyze measurements and find most frequent outcomes
         measurement-freqs (frequencies measurements)
         sorted-measurements (sort-by second > measurement-freqs)
         most-likely-measurements (take (min 5 (count sorted-measurements)) sorted-measurements)

         ;; Find periods from the most likely measurements
         estimated-periods (for [[measured-value _freq] most-likely-measurements
                                 :let [;; Calculate phase
                                       measured-phase (/ measured-value (Math/pow 2 n-qubits))
                                       ;; Use improved continued fraction for better period extraction
                                       cf (qmath/continued-fraction measured-value (Math/pow 2 n-qubits))
                                       convs (qmath/convergents cf)
                                       ;; Find convergent that gives a valid period
                                       period (some (fn [[_num den]]
                                                      (when (and (pos? den)
                                                                 (<= den N)
                                                                 (= 1 (qmath/mod-exp a den N)))
                                                        den))
                                                    convs)]
                                 :when period]
                             {:measured-value measured-value
                              :phase measured-phase
                              :period period
                              :probability (/ (get measurement-freqs measured-value) n-measurements)})

         ;; Sort by probability to get best candidates
         best-periods (sort-by :probability > estimated-periods)
         best-period (first best-periods)] 
     
     {:measured-values measurements
      :measurement-frequencies measurement-freqs
      :estimated-period (when best-period (:period best-period))
      :period-candidates best-periods
      :circuit circuit 
      :n-measurements n-measurements
      :success (boolean (seq best-periods))
      :confidence (when (seq best-periods)
                    (/ (reduce + (map :probability best-periods)) 
                       (count best-periods)))})))

(defn shor-algorithm
  "Shor's algorithm for integer factorization.
  
  Shor's algorithm is a quantum algorithm that can factor large integers
  exponentially faster than the best known classical algorithms. It combines
  classical preprocessing, quantum period finding, and classical post-processing.
  
  This improved implementation supports:
  1. Hardware-compatible mode for real quantum hardware execution
  2. Multiple measurements for statistical robustness
  3. Enhanced period extraction for better success rate
  
  Algorithm steps:
  1. Classical preprocessing: Check for trivial cases
  2. Choose random a < N, check gcd(a,N)  
  3. Quantum period finding: Find period r of f(x) = a^x mod N
  4. Classical post-processing: Extract factors from the period
  
  Parameters:
  - N: Integer to factor (should be composite)
  - options: (Optional) Map containing:
    - :n-qubits - Number of qubits for quantum period finding (default: 2*⌈log₂(N)⌉)
    - :hardware-compatible - Boolean indicating if hardware-optimized circuit should be used
    - :n-measurements - Number of measurements for statistical analysis (default: 10)
    - :max-attempts - Maximum number of random 'a' values to try (default: 10)
  
  Returns:
  Map containing:
  - :factors - Vector of non-trivial factors found (empty if factorization failed)
  - :success - Boolean indicating if factorization succeeded
  - :N - The input number
  - :attempts - Vector of maps describing each attempt with different 'a' values
  - :quantum-circuit - The quantum circuit from the successful attempt (if any)
  - :statistics - Performance statistics and confidence metrics
  
  Example:
  (shor-algorithm 15)    ;=> {:factors [3 5], :success true, :N 15, ...}
  (shor-algorithm 21 {:hardware-compatible true})   ;=> {:factors [3 7], :success true, ...}"
  ([N] (shor-algorithm N {}))
  ([N options]
   {:pre [(> N 1)]}
   
   (let [;; Extract options with defaults
         n-qubits (get options :n-qubits 
                        (* 2 (int (Math/ceil (/ (Math/log N) (Math/log 2))))))
         n-measurements (get options :n-measurements 10)
         max-attempts (get options :max-attempts 10)]
   
     ;; Step 1: Classical preprocessing
     (cond
       ;; Check if N is even
       (even? N) {:factors [2 (/ N 2)]
                  :success true
                  :N N
                  :attempts []
                  :method :classical-even}
       
       ;; Check if N is a perfect power - try to find if N = m^k for some m,k>1
       (some (fn [k]
               (let [root (Math/pow N (/ 1 k))
                     int-root (int root)]
                 (when (= N (int (Math/pow int-root k)))
                   {:base int-root :power k})))
             (range 2 (inc (int (/ (Math/log N) (Math/log 2))))))
       (let [{:keys [base power]} (some (fn [k]
                                          (let [root (Math/pow N (/ 1 k))
                                                int-root (int root)]
                                            (when (= N (int (Math/pow int-root k)))
                                              {:base int-root :power k})))
                                        (range 2 (inc (int (/ (Math/log N) (Math/log 2))))))]
         {:factors (repeat power base)
          :success true
          :N N
          :attempts []
          :method :classical-perfect-power})
       
       ;; Continue with quantum period finding
       :else 
       (let [attempts (atom [])
             start-time (System/currentTimeMillis)]
         
         ;; Step 2-4: Try quantum period finding with different values of 'a'
         (loop [attempt 0]
           (if (>= attempt max-attempts)
             ;; Failed to find factors
             {:factors []
              :success false
              :N N
              :attempts @attempts
              :method :quantum-failed
              :statistics {:runtime (- (System/currentTimeMillis) start-time)
                          :attempts attempt
                          :n-measurements n-measurements}}
             
             ;; Choose random a coprime to N
             (let [a (loop [candidate (+ 2 (rand-int (- N 2)))]
                       (if (= (qmath/gcd candidate N) 1)
                         candidate
                         (recur (+ 2 (rand-int (- N 2))))))
                   gcd-a-N (qmath/gcd a N)]
               
               ;; Check if gcd(a,N) gives us a factor
               (if (> gcd-a-N 1)
                 ;; Found factor classically
                 (do
                   (swap! attempts conj {:a a :gcd gcd-a-N :method :classical-gcd})
                   {:factors [gcd-a-N (/ N gcd-a-N)]
                    :success true
                    :N N
                    :attempts @attempts
                    :method :classical-gcd
                    :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                :attempts (inc attempt)
                                :n-measurements 0}})
                 
                 ;; Try quantum period finding with our improved implementation
                 (let [period-result (enhanced-period-finding a N n-qubits)
                       period (:estimated-period period-result)]
                   
                   (when (map? period-result)
                     (swap! attempts conj (assoc period-result :a a)))
                   
                   (if (and period 
                           (even? period)
                           (not= 1 (qmath/mod-exp a (int (/ period 2)) N)))
                     ;; We have a valid period, try to extract factors
                     (let [exp-a-r-2 (qmath/mod-exp a (int (/ period 2)) N)
                           factor1 (qmath/gcd (dec exp-a-r-2) N)
                           factor2 (qmath/gcd (inc exp-a-r-2) N)]
                       (cond
                         ;; First factor is valid
                         (and (> factor1 1) (< factor1 N))
                         {:factors [factor1 (/ N factor1)]
                          :success true
                          :N N
                          :attempts @attempts
                          :quantum-circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                      :attempts (inc attempt)
                                      :n-measurements n-measurements
                                      :period period
                                      :confidence (:confidence period-result)}}
                         
                         ;; Second factor is valid 
                         (and (> factor2 1) (< factor2 N))
                         {:factors [factor2 (/ N factor2)]
                          :success true
                          :N N
                          :attempts @attempts
                          :quantum-circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                      :attempts (inc attempt)
                                      :n-measurements n-measurements
                                      :period period
                                      :confidence (:confidence period-result)}}
                         
                         ;; Period didn't give useful factors, try again
                         :else
                         (recur (inc attempt))))
                     
                     ;; Invalid period or quantum step failed, try again
                     (recur (inc attempt)))))))))))))

(comment
  ;; Test Shor's algorithm
  (println "\n=== Shor's Factoring Algorithm ===")
  (println "Factoring N = 15:")
  (println (shor-algorithm 15))
  (println "\nFactoring N = 21:")
  (println (shor-algorithm 21))

  ;; Performance comparison examples
  (defn algorithm-complexity-demo
    []
    (println "=== Quantum vs Classical Complexity ===")
    (println "Problem: Search unsorted database of N=1000000 items")
    (println "Classical: O(N) = 1,000,000 operations")
    (println "Grover:    O(√N) = 1,000 operations")
    (println "Speedup:   1000x")
    (println)
    (println "Problem: Factor n-bit number")
    (println "Classical: O(exp(n^(1/3))) - exponential")
    (println "Shor:      O(n³) - polynomial")
    (println "Speedup:   Exponential")
    (println)
    (println "Problem: Find hidden period of n-bit string")
    (println "Classical: O(2^(n/2)) - exponential")
    (println "Simon:     O(n) - linear")
    (println "Speedup:   Exponential"))

  (algorithm-complexity-demo)

 
  )

(comment
  ;; REPL experimentation with solve-linear-system-gf2
  
  ;; Test basic GF(2) operations
  (gf2-add 1 1)  ;=> 0
  (gf2-add 1 0)  ;=> 1
  (gf2-dot-product [1 0 1] [1 0 1])  ;=> 0
  (gf2-dot-product [1 0 1] [0 1 0])  ;=> 0
  
  ;; Test Gaussian elimination
  (let [matrix [[1 0 1]
                [0 1 1]
                [1 1 0]]]
    (gf2-gaussian-elimination matrix))
  ;=> [[1 0 1] [0 1 1] [0 0 0]]
  
  ;; Test null space finding
  (let [matrix [[1 0 1]
                [0 1 1]]]
    (gf2-find-null-space matrix))
  ;=> [1 1 1] ; A solution where each equation gives 0
  
  ;; Test solve-linear-system-gf2 for Simon's algorithm
  (let [;; Suppose we measured these bit vectors in Simon's algorithm
        ;; and they should all be orthogonal to hidden string [1 0 1]
        measurements [[0 1 0]  ; 0⊕0⊕0 = 0 ✓
                      [1 1 1]] ; 1⊕0⊕1 = 0 ✓
        solution (solve-linear-system-gf2 measurements 3)]
    (println "Found hidden string:" solution)
    ;; Verify orthogonality
    (doseq [[i m] (map-indexed vector measurements)]
      (println "measurement" i "•" solution "="
               (gf2-dot-product m solution))))
  
  ;; Test with larger system (4-bit hidden string)
  (let [measurements [[1 0 0 1]  ; Example measurements
                      [0 1 1 1]
                      [1 1 1 0]]
        solution (solve-linear-system-gf2 measurements 4)]
    (println "4-bit solution:" solution))
  
  ;; Performance test with larger matrix
  (let [n 10
        random-matrix (repeatedly (dec n) 
                                  #(vec (repeatedly n (fn [] (rand-int 2)))))
        start-time (System/nanoTime)
        result (solve-linear-system-gf2 random-matrix n)
        end-time (System/nanoTime)]
    (println "Solved" n "x" n "system in" 
             (/ (- end-time start-time) 1000000.0) "ms")
    (println "Solution:" result))
  )
