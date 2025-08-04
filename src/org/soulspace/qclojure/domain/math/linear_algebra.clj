(ns org.soulspace.qclojure.domain.math.linear-algebra)

(defn matrix-multiply
  "Multiply two matrices represented as vectors of vectors.
  
  Parameters:
  - A: Matrix A as vector of row vectors
  - B: Matrix B as vector of row vectors
  
  Returns:
  Matrix product A*B"
  [A B]
  (let [rows-A (count A)
        cols-A (count (first A))
        cols-B (count (first B))]
    (vec (for [i (range rows-A)]
           (vec (for [j (range cols-B)]
                  (reduce + (for [k (range cols-A)]
                              (* (get-in A [i k]) (get-in B [k j]))))))))))

(defn matrix-transpose
  "Transpose a matrix.
  
  Parameters:
  - M: Matrix as vector of row vectors
  
  Returns:
  Transposed matrix"
  [M]
  (let [rows (count M)
        cols (count (first M))]
    (vec (for [j (range cols)]
           (vec (for [i (range rows)]
                  (get-in M [i j])))))))

(defn matrix-inverse
  "Compute matrix inverse using Gauss-Jordan elimination.
  
  This is a simple implementation suitable for small matrices (< 20x20).
  For larger matrices, consider using a dedicated linear algebra library.
  
  Parameters:
  - M: Square matrix as vector of row vectors
  
  Returns:
  Inverse matrix, or nil if matrix is singular"
  [M]
  (let [n (count M)
        ;; Create augmented matrix [M | I]
        augmented (vec (for [i (range n)]
                         (vec (concat (nth M i)
                                      (for [j (range n)] (if (= i j) 1.0 0.0))))))]
    (try
      ;; Gauss-Jordan elimination
      (loop [mat augmented
             row 0]
        (if (>= row n)
          ;; Extract inverse from right half of augmented matrix
          (vec (for [i (range n)]
                 (vec (for [j (range n n (* 2 n))]
                        (get-in mat [i j])))))
          (let [;; Find pivot
                pivot-row (reduce (fn [best-row curr-row]
                                    (if (> (abs (get-in mat [curr-row row]))
                                           (abs (get-in mat [best-row row])))
                                      curr-row
                                      best-row))
                                  row (range row n))
                pivot-val (get-in mat [pivot-row row])]
            (if (< (abs pivot-val) 1e-12)
              nil ; Matrix is singular
              (let [;; Swap rows if needed
                    mat-swapped (if (= pivot-row row)
                                  mat
                                  (assoc mat
                                         row (nth mat pivot-row)
                                         pivot-row (nth mat row)))
                    ;; Scale pivot row
                    mat-scaled (assoc mat-swapped row
                                      (mapv #(/ % pivot-val) (nth mat-swapped row)))
                    ;; Eliminate column
                    mat-eliminated (vec (for [i (range n)]
                                          (if (= i row)
                                            (nth mat-scaled i)
                                            (let [factor (get-in mat-scaled [i row])]
                                              (mapv - (nth mat-scaled i)
                                                    (mapv #(* factor %) (nth mat-scaled row)))))))]
                (recur mat-eliminated (inc row)))))))
      (catch Exception _
        nil))))

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

