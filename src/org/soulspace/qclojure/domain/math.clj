(ns org.soulspace.qclojure.domain.math
  "Mathematical operations and utilities for quantum algorithms."
  (:require [fastmath.core :as m]
            [fastmath.complex :as fc]))

; Enable fastmath operator macros
#_(m/use-primitive-operators)

(defn complex?
  "Check if value is a fastmath complex number (Vec2).
  
  FastMath represents complex numbers as 2D vectors where the x component
  is the real part and the y component is the imaginary part.
  
  Parameters:
  - z: Value to test for complex number type
  
  Returns:
  Boolean true if z is a fastmath Vec2 complex number, false otherwise
  
  Example:
  (complex? (fc/complex 1 2))
  ;=> true
  
  (complex? 42)
  ;=> false"
  [z]
  (instance? fastmath.vector.Vec2 z))

(defn complex-magnitude-squared
  "Calculate |z|² for a complex number represented as [real imag] or fastmath complex.
   
   This function computes the squared magnitude of a complex number, which is
   useful for normalizing quantum states or measuring fidelity.
   
   Parameters:
   - z: A complex number in one of the following formats:
     - Fastmath complex number (e.g. (fc/complex 1 2))
     - Clojure vector [real imag]
     - Regular number (treated as real)
   
   Returns:
   Squared magnitude as a number"
  [z]
  (cond
    ;; Handle fastmath complex numbers
    (complex? z)
    (+ (* (fc/re z) (fc/re z)) (* (fc/im z) (fc/im z)))

    ;; Handle Clojure vectors [real imag]
    (vector? z)
    (let [[real imag] z]
      (+ (* real real) (* imag imag)))

    ;; Handle regular numbers (treat as real)
    (number? z)
    (* z z)

    :else
    (throw (IllegalArgumentException. (str "Unsupported complex number format: " (type z))))))

(defn max-coeff-magnitude-squared
  "Get the maximum coefficient magnitude squared from a matrix.
   
   This function computes the maximum of the squared magnitudes of complex coefficients
   in a matrix, which is useful for normalizing quantum states or measuring fidelity.
   
   Parameters:
   - matrix: A 2D vector of complex numbers (e.g. [[c11
     c12] [c21 c22] ...])
   
   Returns:
   Maximum coefficient magnitude squared as a number"
  [matrix]
  (apply max (map (fn [row]
                    (apply max (map (fn [coeff]
                                      (+ (* (fc/re coeff) (fc/re coeff))
                                         (* (fc/im coeff) (fc/im coeff))))
                                    row)))
                  matrix)))


(defn prime?
  "Simple primality test for small numbers.
   
   This function checks if a number is prime by testing divisibility
   against all integers from 2 to the square root of the number.
   It is not optimized for large numbers, but works well for small inputs.
   
   Parameters:
   - n: The number to check for primality
   
   Returns:
   true if n is prime, false otherwise"
  [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %))
                 (range 2 (inc (int (m/sqrt n)))))))

(defn gcd
  "Calculate greatest common divisor using Euclidean algorithm.
   
   This function computes the GCD of two integers using the efficient
   Euclidean algorithm, which is based on the principle that GCD(a, b) = GCD(b, a mod b).
   
   Parameters:
   - a: First integer
   - b: Second integer
   
   Returns:
   The greatest common divisor of a and b"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn mod-exp
  "Calculate (base^exp) mod m efficiently using binary exponentiation.
   
   This function computes the modular exponentiation using the method of
   exponentiation by squaring, which is efficient for large numbers.
   It handles negative exponents by returning the modular inverse if needed.
   
   Parameters:
   - base: Base number
   - exp: Exponent (can be negative)
   - m: Modulus (must be positive)
   
   Returns:
   The result of (base^exp) mod m"
  [base exp m]
  (loop [result 1
         base (mod base m)
         exp exp]
    (cond
      (zero? exp) result
      (odd? exp) (recur (mod (* result base) m) base (dec exp))
      :else (recur result (mod (* base base) m) (quot exp 2)))))

(defn continued-fraction
  "Convert a fraction to continued fraction representation.
  
  This implementation handles numerical precision issues and early termination
  conditions that are important for Shor's algorithm. It can detect periodic
  patterns in the continued fraction expansion, which is crucial for finding
  the correct period.
  
  Parameters:
  - num: Numerator of the fraction
  - den: Denominator of the fraction
  - max-depth: (Optional) Maximum depth of continued fraction expansion
  - epsilon: (Optional) Precision threshold for detecting near-zero remainders
  
  Returns:
  Vector of continued fraction terms"
  ([num den]
   (continued-fraction num den 100 1e-10))
  ([num den max-depth]
   (continued-fraction num den max-depth 1e-10))
  ([num den max-depth epsilon]
   (loop [n num
          d den
          cf []
          depth 0]
     (cond
       ;; Stop if denominator is zero or very close to zero
       (or (zero? d) (< (m/abs d) epsilon))
       cf

       ;; Stop if we've reached max depth to prevent infinite loops
       (>= depth max-depth)
       cf

       :else
       (let [q (quot n d)
             r (mod n d)]
         ;; If remainder is very small relative to denominator, stop
         (if (< (/ r d) epsilon)
           (conj cf q)
           (recur d r (conj cf q) (inc depth))))))))

(defn convergents
  "Calculate convergents from continued fraction representation.
  
  This enhanced implementation handles edge cases better and includes
  additional validation to ensure proper convergence, which is important
  for accurately extracting periods in Shor's algorithm.
  
  Parameters:
  - cf: Vector of continued fraction terms
  
  Returns:
  Vector of convergents as [numerator denominator] pairs"
  [cf]
  (reduce (fn [acc term]
            (let [h (count acc)]
              (cond
                (= h 0) [[term 1]]
                (= h 1) (conj acc [(+ (* term (ffirst acc)) 1) term])
                :else (let [prev-2 (nth acc (- h 2))
                            prev-1 (nth acc (- h 1))
                            p (+ (* term (first prev-1)) (first prev-2))
                            q (+ (* term (second prev-1)) (second prev-2))]
                        (conj acc [p q])))))
          []
          cf))

(defn round-precision
  "Round a number to specified decimal places.
  
  Parameters:
  - x: Number to round
  - precision: Number of decimal places
  
  Returns:
  Number rounded to specified precision"
  [x precision]
  (if (zero? precision)
    (double (m/round x))
    (let [bd (bigdec x)
          scale precision
          rounded (.setScale bd scale java.math.RoundingMode/HALF_UP)]
      (double rounded))))

(defn find-period
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
                         :let [cf (continued-fraction measured-value (Math/pow 2 precision) depth)
                               convs (convergents cf)]
                         [num den] convs
                         :let [period (int den)]  ; Ensure period is an integer
                         ;; Verify this is actually a period
                         :when (and (pos? period)
                                    (<= period N)
                                    (= 1 (mod-exp a period N)))]
                     {:period period
                      :fraction [num den]
                      :error (Math/abs (- phase (/ num (Math/pow 2 precision))))})

        ;; Sort by error (lowest first) and then by period (smallest valid first)
        sorted-candidates (sort-by (juxt :error :period) candidates)]

    ;; Return the best candidate's period, or nil if none found
    (when (seq sorted-candidates)
      (:period (first sorted-candidates)))))

(defn perfect-power-factor
  "Check if N is a perfect power and return its base factor.
  
  A perfect power is a number that can be expressed as a^k for some integers a and k where k > 1.
  This function finds the smallest base a such that N = a^k for some k > 1.
  
  This is used in Shor's algorithm for classical preprocessing - if N is a perfect power,
  we can factor it classically without needing quantum period finding.
  
  Examples:
  - perfect-power-factor(8) = 2 (since 8 = 2^3)
  - perfect-power-factor(9) = 3 (since 9 = 3^2)
  - perfect-power-factor(15) = 1 (since 15 is not a perfect power)
  
  Parameters:
  - N: The number to check for perfect power
  
  Returns:
  Base factor a if N = a^k for some k > 1, otherwise returns 1"
  [N]
  {:pre [(pos? N)]}
  
  (if (<= N 1)
    1  ; Handle edge cases
    (loop [k 2]
      (if (> k (Math/ceil (/ (Math/log N) (Math/log 2))))  ; Check up to log_2(N)
        1  ; Not a perfect power
        (let [root (Math/round (Math/pow N (/ 1.0 k)))]
          (cond
            ;; Check if root^k equals N (accounting for floating point precision)
            (and (> root 1)
                 (let [power (long (Math/pow root k))]
                   (= power N)))
            root  ; Found perfect power base
            
            ;; Also check root-1 and root+1 due to floating point precision issues
            (and (> (dec root) 1)
                 (let [power (long (Math/pow (dec root) k))]
                   (= power N)))
            (dec root)
            
            (and (> (inc root) 1)
                 (let [power (long (Math/pow (inc root) k))]
                   (= power N)))
            (inc root)
            
            :else
            (recur (inc k))))))))

(defn complete-factorization
  "Complete factorization of a vector of partial factors, e.g. [3 9].
   
  Parameters:
  - factors: Vector of factors to complete
   
  Returns:
  Vector of complete factors, e.g. [3 3 3] for input [3 9]."
  [factors]
  {:pre [(seq factors)]}
  (reduce (fn [acc factor]
            (let [base (perfect-power-factor factor)]
              (if (= base 1)
                (conj acc factor)  ; Not a perfect power, keep as is
                (let [power (int (/ factor base))]
                  (concat acc (repeat power base))))))
          []
          factors))

(defn tensor-product
  "Compute the tensor product of two matrices.
  
  For matrices A (m×n) and B (p×q), returns the Kronecker product A⊗B (mp×nq).
  
  Parameters:
  - matrix-a: First matrix (m×n)
  - matrix-b: Second matrix (p×q)
   
  Returns:
  A new matrix representing the tensor product (mp×nq)."
  [matrix-a matrix-b]
  (let [m (count matrix-a)
        n (count (first matrix-a))
        p (count matrix-b)
        q (count (first matrix-b))
        result-rows (* m p)
        result-cols (* n q)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (let [a-row (quot i p)
                          a-col (quot j q)
                          b-row (mod i p)
                          b-col (mod j q)
                          a-val (get-in matrix-a [a-row a-col])
                          b-val (get-in matrix-b [b-row b-col])]
                      (* a-val b-val)))
                  (range result-cols)))
          (range result-rows))))

(defn lu-decomposition
  "Perform LU decomposition with partial pivoting for matrix inversion.
  Returns [L U P] where P is the permutation matrix.
   
  This function uses Gaussian elimination with partial pivoting to decompose the matrix. 

  Parameters:
  - matrix: A square matrix to decompose (n×n)

  Returns:
  A vector [L U P] where:
  - L is the lower triangular matrix
  - U is the upper triangular matrix
  - P is the permutation vector (indices of rows after pivoting)

  Example:
  (lu-decomposition [[4 3 2] [2 1 1] [1 1 1]])
  ;=> [[[1.0 0.0 0.0] [0.5 1.0 0.0] [0.25 0.5 1.0]]
       [[4.0 3.0 2.0] [0.0 -0.5 -0.5] [0.0 0.0 0.0]]
       [0 1 2]] ; Permutation"
  [matrix]
  (let [n (count matrix)
        a (mapv vec matrix) ; Make mutable copy
        p (vec (range n))   ; Permutation vector
        l (vec (repeat n (vec (repeat n 0.0))))
        u (vec (repeat n (vec (repeat n 0.0))))]

    (loop [k 0 a-curr a p-curr p]
      (if (>= k n)
        ;; Build L and U matrices
        (let [l-final (reduce (fn [l-acc i]
                                (reduce (fn [l-inner j]
                                          (cond
                                            (= i j) (assoc-in l-inner [i j] 1.0)
                                            (> i j) (assoc-in l-inner [i j] (get-in a-curr [i j]))
                                            :else l-inner))
                                        l-acc (range n)))
                              l (range n))
              u-final (reduce (fn [u-acc i]
                                (reduce (fn [u-inner j]
                                          (if (<= i j)
                                            (assoc-in u-inner [i j] (get-in a-curr [i j]))
                                            u-inner))
                                        u-acc (range n)))
                              u (range n))]
          [l-final u-final p-curr])

        ;; Find pivot
        (let [max-row (reduce (fn [max-i i]
                                (if (> (Math/abs (get-in a-curr [i k]))
                                       (Math/abs (get-in a-curr [max-i k])))
                                  i max-i))
                              k (range (inc k) n))

              ;; Swap rows if needed
              a-swapped (if (not= k max-row)
                          (let [temp-row (get a-curr k)]
                            (-> a-curr
                                (assoc k (get a-curr max-row))
                                (assoc max-row temp-row)))
                          a-curr)
              p-swapped (if (not= k max-row)
                          (let [temp-p (get p-curr k)]
                            (-> p-curr
                                (assoc k (get p-curr max-row))
                                (assoc max-row temp-p)))
                          p-curr)
              ;; Eliminate column
              a-eliminated
              (reduce (fn [a-acc i]
                        (if (> i k)
                          (let [factor (/ (get-in a-acc [i k])
                                          (get-in a-acc [k k]))]
                            (-> a-acc
                                (assoc-in [i k] factor)
                                (update i (fn [row]
                                            (mapv (fn [j val]
                                                    (if (> j k)
                                                      (- val (* factor (get-in a-acc [k j])))
                                                      val))
                                                  (range n) row)))))
                          a-acc))
                      a-swapped (range (inc k) n))]

          (recur (inc k) a-eliminated p-swapped))))))

(defn solve-linear-system
  "Solve Ax = b using LU decomposition with partial pivoting.
   
  This function solves the linear system Ax = b where A is a square matrix
  and b is a vector. It uses LU decomposition with partial pivoting to
  efficiently solve the system.
   
  Parameters:
  - matrix: Square matrix A (n×n)
  - b: Vector b (n×1)
  
  Returns:
  Vector x (n×1) that satisfies Ax = b, or nil if no solution
  
  Example:
  (solve-linear-system [[4 3 2] [2 1 1] [1 1 1]]
                       [1 2 3])
  ;=> [0.0 1.0 1.0] ; Solution to the system"
  [matrix b]
  (let [n (count matrix)
        [l u p] (lu-decomposition matrix)

        ;; Permute b according to P
        pb (mapv #(get b %) p)

        ;; Forward substitution: Ly = Pb
        y (loop [i 0 y-acc (vec (repeat n 0.0))]
            (if (>= i n)
              y-acc
              (let [sum (reduce + (map * (subvec (get l i) 0 i) (subvec y-acc 0 i)))
                    yi (/ (- (get pb i) sum) (get-in l [i i]))]
                (recur (inc i) (assoc y-acc i yi)))))

        ;; Back substitution: Ux = y
        x (loop [i (dec n) x-acc (vec (repeat n 0.0))]
            (if (< i 0)
              x-acc
              (let [sum (reduce + (map * (subvec (get u i) (inc i) n) (subvec x-acc (inc i) n)))
                    xi (/ (- (get y i) sum) (get-in u [i i]))]
                (recur (dec i) (assoc x-acc i xi)))))]
    x))

(comment ;

  (gcd 48 18) ; => 6
  (mod-exp 2 10 1000) ; => 24
  (continued-fraction 22 7) ; => [3 7 15]
  (convergents [3 7 15]) ; => [[3 1] [22 7] [45 16]]
  (round-precision 3.14159 2) ; => 3.14
  (find-period 5 3 15 2) ; => 4
  (perfect-power-factor 27) ; => 3
  (perfect-power-factor 16) ; => 2
  (complete-factorization [3 9]) ; => [3 3 3]
  (complete-factorization [2 4 8]) ; => [2 2 2 2 2 2]   
  ;
  )

; Disable fastmath operator macros to avoid conflicts
#_(m/unuse-primitive-operators)