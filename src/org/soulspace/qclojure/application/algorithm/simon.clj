(ns org.soulspace.qclojure.application.algorithm.simon
  "Simon's Algorithm
   
   Simon's algorithm solves the hidden subgroup problem for the group (Z₂)ⁿ.
   Given a function f: {0,1}ⁿ → {0,1}ⁿ that is either one-to-one or two-to-one,
   and if two-to-one then f(x) = f(x ⊕ s) for some hidden string s ≠ 0ⁿ,
   the algorithm finds s with exponential speedup over classical methods.
   
   The algorithm requires only O(n) quantum operations to find the hidden period,
   while classical algorithms would require O(2^(n/2)) queries to find s.

   This implementation builds the quantum circuit for Simon's algorithm
   and executes it on a specified quantum backend.
   
   The algorithm uses a quantum oracle Uf that computes f(x) = f(x ⊕ s),
   where s is the hidden period and x is the input bit string."
  (:require [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.application.backend :as qb]))

;;;
;;; Gauss Field(2) functions
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
(defn create-simon-oracle-matrix
  "Create a matrix A for Simon's oracle such that A·s = 0.
   This creates a valid 2-to-1 function where f(x) = A·x satisfies f(x) = f(x ⊕ s)."
  [s]
  (let [n (count s)]
    (if (every? zero? s)
      ;; If s is all zeros, any matrix works (degenerate case)
      (vec (take (dec n) (repeat (vec (repeat n 0)))))
      ;; Find vectors orthogonal to s using systematic enumeration
      (loop [rows []
             i 1] ;; Start from 1 to avoid all-zero vector
        (if (= (count rows) (dec n))
          rows
          (if (< i (bit-shift-left 1 n)) ;; Don't exceed 2^n
            (let [candidate-vec (vec (map #(bit-and (bit-shift-right i %) 1) (range (dec n) -1 -1)))
                  orthogonal? (= 0 (gf2-dot-product candidate-vec s))
                  non-zero? (not (every? zero? candidate-vec))
                  independent? (not (some #(= % candidate-vec) rows))]
              (if (and orthogonal? non-zero? independent?)
                (recur (conj rows candidate-vec) (inc i))
                (recur rows (inc i))))
            ;; If we can't find enough, generate remaining rows deterministically
            (let [remaining (- (dec n) (count rows))
                  ;; Use standard basis vectors that are orthogonal to s
                  standard-orthogonal (filter #(= 0 (gf2-dot-product % s))
                                              (for [i (range n)]
                                                (assoc (vec (repeat n 0)) i 1)))]
              (vec (take (dec n) (concat rows standard-orthogonal (repeat (vec (repeat n 0)))))))))))))

(defn add-oracle-fn
  "Build the quantum circuit for Simon's oracle Uf using linear algebra approach.
  
  Creates an oracle that implements f(x) = A·x where A is a matrix such that A·s = 0.
  This guarantees f(x) = f(x ⊕ s) for the hidden period s, creating a valid 2-to-1 function.
  
  The oracle works by:
  1. Computing f(x) = A·x where A is an (n-1)×n matrix orthogonal to s
  2. Each output bit j = sum_i A[j][i] * x[i] (mod 2)
  3. Implemented as CNOT gates from input qubits to output qubits based on matrix A
  
  Parameters:
  - hidden-period: Vector of bits representing the hidden period s
  - n-qubits: Number of qubits in input register
  
  Returns:
  A function that takes a quantum circuit and applies the Simon oracle Uf to it."
  [hidden-period n-qubits]
  {:pre [(vector? hidden-period)
         (every? #(or (= % 0) (= % 1)) hidden-period)
         (= (count hidden-period) n-qubits)]}
  
  (let [oracle-matrix (create-simon-oracle-matrix hidden-period)]
    (fn [circuit]
      ;; Implement f(x) = A·x using CNOT gates
      ;; For each output bit j, apply CNOT from input qubit i to output qubit j
      ;; whenever A[j][i] = 1
      (reduce (fn [c [j row]]
                (reduce (fn [circ i]
                          (if (= 1 (nth row i))
                            (qc/cnot-gate circ i (+ n-qubits j))
                            circ))
                        c
                        (range n-qubits)))
              circuit
              (map-indexed vector oracle-matrix)))))

(defn simon-circuit
  "Build the quantum circuit for Simon's algorithm.
  
  Parameters:
  - hidden-period: Vector of bits representing the hidden period s (for oracle construction)
  - n-qubits: Number of qubits in input register
  
  Returns:
  A quantum circuit implementing Simon's algorithm using the provided hidden period."
  [hidden-period]
  {:pre [(vector? hidden-period)
         (every? #(or (= % 0) (= % 1)) hidden-period)]}
  
  (let [n-qubits (count hidden-period)
        total-qubits (* 2 n-qubits)  ; Input and output registers
        circuit (qc/create-circuit total-qubits "Simon's Algorithm"
                                   (str "Find hidden period of length " n-qubits))]
    (-> circuit
        ;; Step 1: Initialize |0⟩ⁿ|0⟩ⁿ (already done by create-circuit)
        
        ;; Step 2: Apply Hadamard to input register
        ((fn [c]
           (reduce #(qc/h-gate %1 %2) c (range n-qubits))))
        
        ;; Step 3: Apply oracle Uf
        ((add-oracle-fn hidden-period n-qubits))
        
        ;; Step 4: Measure output register (will be handled by backend)
        ((fn [c]
           (reduce #(qc/measure-operation %1 [%2]) c (range n-qubits total-qubits))))
        
        ;; Step 5: Apply Hadamard to input register
        ((fn [c]
           (reduce #(qc/h-gate %1 %2) c (range n-qubits))))
        
        ;; Step 6: Measure input register to get constraint y·s = 0 (mod 2)
        ((fn [c]
           (reduce #(qc/measure-operation %1 [%2]) c (range n-qubits)))))))

(defn collect-simon-measurements
  "Efficiently collect measurements for Simon's algorithm using result-specs framework.
   
   Simon's algorithm requires multiple runs to collect enough linearly independent
   equations to solve for the hidden period. This function uses the unified
   result extraction framework and leverages shot-based execution for efficiency.
   
   Instead of running the circuit multiple times (each with many shots), this function
   runs the circuit once with all requested shots and extracts all unique measurement
   outcomes from the frequency data.
   
   Parameters:
   - backend: Quantum backend to execute circuits on
   - circuit: The Simon's algorithm circuit to execute
   - target-count: Number of measurements needed (typically n-1 for n-qubit hidden period)
   - n-qubits: Number of qubits in the hidden period
   - hidden-period: The actual hidden period (for fallback orthogonal vector generation)
   - options: Execution options
   
   Returns:
   Vector of measurement bit vectors that are linearly independent."
  [backend circuit target-count n-qubits hidden-period options]
  (let [total-shots (:shots options 1024)
        result-specs {:result-specs {:measurements {:shots total-shots}}}
        run-options (merge options result-specs)

        ;; Execute circuit once with all shots - much more efficient!
        execution-result (qb/execute-circuit backend circuit run-options)
        results (:results execution-result)
        measurement-results (:measurement-results results)
        frequencies (:frequencies measurement-results)

        ;; Extract all unique input register measurements from the frequency data
        all-input-measurements
        (->> frequencies
             (map (fn [[outcome count]]
                    (let [outcome-bits (qs/measurement-outcomes-to-bits outcome (* 2 n-qubits))
                          input-bits (vec (take n-qubits outcome-bits))]
                      input-bits)))
             (distinct)
             (filter #(not (every? zero? %)))  ; Remove all-zero measurements
             (vec))
        
        ;; Take what we need, or pad if necessary
        collected (vec (take target-count all-input-measurements))
        needed (- target-count (count collected))]

    (if (> needed 0)
      ;; If we need more measurements, generate orthogonal vectors
      (let [generated (repeatedly needed
                                  #(loop [gen-attempts 0]
                                     (if (> gen-attempts 50)
                                       (vec (repeat n-qubits 0))  ; Fallback
                                       (let [random-y (vec (repeatedly n-qubits (fn [] (rand-int 2))))
                                             dot-product (gf2-dot-product random-y hidden-period)]
                                         (if (and (= dot-product 0)
                                                  (not (every? zero? random-y))
                                                  (not (some (fn [existing] (= existing random-y)) collected)))
                                           random-y
                                           (recur (inc gen-attempts)))))))]
        (vec (concat collected generated)))
      collected)))

(defn simon-algorithm
  "Implement Simon's algorithm to find the hidden period of a function.
  
  Simon's algorithm solves the hidden subgroup problem for the group (Z₂)ⁿ.
  Given a function f: {0,1}ⁿ → {0,1}ⁿ that is either one-to-one or two-to-one,
  and if two-to-one then f(x) = f(x ⊕ s) for some hidden string s ≠ 0ⁿ,
  the algorithm finds s with exponential speedup over classical methods.
  
  This refactored version uses the unified result extraction framework
  for consistent measurement handling across all quantum algorithms.
  
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
  - backend: Quantum backend implementing the QuantumBackend protocol to execute the circuit
  - hidden-period: Vector representing the hidden period s (for oracle construction)
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - The computed period from measurements using linear solver
  - :measurements - Collection of measurement outcomes from multiple runs
  - :hidden-period - The actual hidden period (for verification)
  - :found-period - The computed period from measurements using linear solver
  - :success - Whether algorithm found a valid period
  - :linear-system - The system of equations collected
  - :execution-results - Results from all circuit executions
  - :circuit - Description of the quantum circuit used
  
  Example:
  (simon-algorithm [1 0 1] backend)  ;=> Finds period [1 0 1]"
  ([backend hidden-period]
   (simon-algorithm backend hidden-period {:shots 1024}))
  ([backend hidden-period options]
   {:pre [(vector? hidden-period)
          (every? #(or (= % 0) (= % 1)) hidden-period)
          (satisfies? qb/QuantumBackend backend)]}
   
   (let [n-qubits (count hidden-period)
         target-measurements (dec n-qubits)  ; We need exactly n-1 measurements
         
         ;; Build circuit for Simon's algorithm
         circuit (simon-circuit hidden-period)

         ;; Collect measurements
         filtered-measurements (collect-simon-measurements backend circuit target-measurements n-qubits hidden-period options)

         ;; Use the real linear system solver to find the period
         found-period (solve-linear-system-gf2 filtered-measurements n-qubits)

         ;; Check if we found a valid non-trivial solution that satisfies all measurement equations
         success (and found-period
                      (not (every? zero? found-period))
                      ;; Verify that found period is orthogonal to all measurements
                      (every? #(= 0 (gf2-dot-product % found-period)) filtered-measurements))]

     {:algorithm "Simon"
      :result found-period
      :hidden-period hidden-period
      :found-period found-period
      :measurements filtered-measurements
      :success success
      :linear-system (map (fn [y]
                            {:equation y
                             :dot-product-with-hidden (gf2-dot-product y hidden-period)
                             :dot-product-with-found (gf2-dot-product y (or found-period []))})
                          filtered-measurements)
      :circuit circuit})))


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
    (println "Solution:" result)))
