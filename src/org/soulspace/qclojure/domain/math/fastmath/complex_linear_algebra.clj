(ns org.soulspace.qclojure.domain.math.fastmath.complex-linear-algebra
  (:require
   [fastmath.core :as fm]
   [fastmath.complex :as fc]
   [fastmath.matrix :as fmat]))

;;;
;;; Configuration and utilities
;;;
(def ^:const ^double default-tolerance 1.0e-12)

;;;
;;; Complex number utilities and predicates
;;;
(defn complex?
  "Test if x is a FastMath Vec2 complex number."
  [x]
  (instance? fastmath.vector.Vec2 x))

(defn complex-scalar?
  "Test if x represents a complex scalar (Vec2 or complex map)."
  [x]
  (or (complex? x)
      (and (map? x) (contains? x :real) (contains? x :imag)
           (number? (:real x)) (number? (:imag x)))))

(defn complex-vector?
  "Test if v represents a complex vector."
  [v]
  (and (vector? v) (every? complex-scalar? v)))

(defn complex-matrix?
  "Test if m represents a complex matrix."
  [m]
  (and (vector? m) (every? complex-vector? m)))

;;;
;;; Conversion utilities between representations
;;;
(defn ensure-complex
  "Ensure input is a complex number."
  [x]
  (cond
    (complex? x) x
    (number? x) (fc/complex x 0.0)
    (complex-scalar? x) (fc/complex (:real x) (:imag x))
    :else (throw (ex-info "Cannot convert to Vec2" {:value x}))))

;;;
;;; Matrix algebra helper functions
;;;
(defn real-matrix?
  "Check if a matrix contains only real numbers (zero imaginary parts)."
  [matrix]
  (every? (fn [row]
            (every? (fn [element]
                      (let [vec2-elem (ensure-complex element)]
                        (< (fm/abs (fc/im vec2-elem)) 1e-12)))
                    row))
          matrix))

(defn complex->real
  "Extract real part of a complex number, checking it's actually real."
  [z]
  (let [vec2-z (ensure-complex z)]
    (when-not (< (fm/abs (fc/im vec2-z)) 1e-12)
      (throw (ex-info "Complex number has non-zero imaginary part"
                      {:value z :imaginary (fc/im vec2-z)})))
    (fc/re vec2-z)))

(defn complex-matrix->real-matrix
  "Convert complex matrix to real matrix."
  [matrix]
  (let [data (into-array (map (fn [row]
                                (double-array (map complex->real row)))
                              matrix))]
    (fmat/mat data)))

(defn complex-matrix-shape
  "Get the shape of a matrix containing Complex elements."
  [matrix]
  (if (vector? matrix)
    [(count matrix) (if (vector? (first matrix))
                      (count (first matrix))
                      1)]
    [1 1]))

(defn complex-scale
  "Scale a complex number by a scalar."
  [v scalar]
  (let [s (ensure-complex scalar)]
    (fc/mult v s)))

(defn solve-linear
  "Solve linear system using Gaussian elimination for complex matrices."
  [A b]
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix must be square for solving" {:shape [n m]})))
    (when (not= n (count b))
      (throw (ex-info "Incompatible dimensions" {:matrix-rows n :vector-length (count b)})))

    ;; Create augmented matrix [A|b]
    (let [augmented (mapv (fn [i]
                            (conj (vec (get A i)) (get b i)))
                          (range n))]
      ;; Forward elimination with partial pivoting
      (loop [aug augmented
             i 0]
        (if (>= i n)
          ;; Back substitution
          (let [solution (vec (repeat n (fc/complex 0.0 0.0)))]
            (loop [sol solution
                   k (dec n)]
              (if (< k 0)
                sol
                (let [sum (reduce fc/add
                                  (fc/complex 0.0 0.0)
                                  (map (fn [j]
                                         (fc/mult (get-in aug [k j]) (get sol j)))
                                       (range (inc k) n)))
                      rhs (fc/sub (get-in aug [k n]) sum)
                      pivot (get-in aug [k k])]
                  (when (< (fc/abs pivot) 1e-14)
                    (throw (ex-info "Matrix is singular or nearly singular"
                                    {:pivot-magnitude (fc/abs pivot)})))
                  (recur (assoc sol k (fc/div rhs pivot))
                         (dec k))))))
          ;; Find pivot row
          (let [pivot-row (reduce (fn [best-row curr-row]
                                    (if (> (fc/abs (get-in aug [curr-row i]))
                                           (fc/abs (get-in aug [best-row i])))
                                      curr-row
                                      best-row))
                                  i
                                  (range i n))
                pivot-val (get-in aug [pivot-row i])]
            (when (< (fc/abs pivot-val) 1e-14)
              (throw (ex-info "Matrix is singular" {:row i :pivot-magnitude (fc/abs pivot-val)})))

            ;; Swap rows if needed
            (let [swapped-aug (if (= pivot-row i)
                                aug
                                (assoc aug
                                       i (get aug pivot-row)
                                       pivot-row (get aug i)))]
              ;; Eliminate below pivot
              (recur (mapv (fn [row-idx]
                             (if (<= row-idx i)
                               (get swapped-aug row-idx)
                               (let [factor (fc/div (get-in swapped-aug [row-idx i])
                                                    (get-in swapped-aug [i i]))]
                                 (mapv (fn [col-idx]
                                         (if (= col-idx i)
                                           (fc/complex 0.0 0.0)
                                           (fc/sub (get-in swapped-aug [row-idx col-idx])
                                                   (fc/mult factor (get-in swapped-aug [i col-idx])))))
                                       (range (inc n))))))
                           (range n))
                     (inc i)))))))))

(defn inverse
  "Compute matrix inverse using Gaussian elimination for complex matrices."
  [A]
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix must be square for inverse" {:shape [n m]})))

    ;; Create augmented matrix [A|I] 
    (let [identity (mapv (fn [i]
                           (mapv (fn [j]
                                   (if (= i j)
                                     (fc/complex 1.0 0.0)
                                     (fc/complex 0.0 0.0)))
                                 (range n)))
                         (range n))
          augmented (mapv (fn [i]
                            (vec (concat (get A i) (get identity i))))
                          (range n))]

      ;; Forward elimination with partial pivoting  
      (loop [aug augmented
             i 0]
        (if (>= i n)
          ;; Extract inverse from right half of augmented matrix
          (mapv (fn [i]
                  (mapv (fn [j]
                          (get-in aug [i (+ j n)]))
                        (range n)))
                (range n))
          ;; Find pivot row
          (let [pivot-row (reduce (fn [best-row curr-row]
                                    (if (> (fc/abs (get-in aug [curr-row i]))
                                           (fc/abs (get-in aug [best-row i])))
                                      curr-row
                                      best-row))
                                  i
                                  (range i n))
                pivot-val (get-in aug [pivot-row i])]
            (when (< (fc/abs pivot-val) 1e-14)
              (throw (ex-info "Matrix is singular" {:row i :pivot-magnitude (fc/abs pivot-val)})))

            ;; Swap rows if needed
            (let [swapped-aug (if (= pivot-row i)
                                aug
                                (assoc aug
                                       i (get aug pivot-row)
                                       pivot-row (get aug i)))
                  pivot (get-in swapped-aug [i i])
                  ;; Scale pivot row
                  scaled-aug (assoc swapped-aug i
                                    (mapv #(fc/div % pivot)
                                          (get swapped-aug i)))]

              ;; Eliminate column
              (recur (mapv (fn [row-idx]
                             (if (= row-idx i)
                               (get scaled-aug row-idx)
                               (let [factor (get-in scaled-aug [row-idx i])]
                                 (mapv (fn [col-idx]
                                         (if (= col-idx i)
                                           (fc/complex 0.0 0.0)
                                           (fc/sub (get-in scaled-aug [row-idx col-idx])
                                                   (fc/mult factor (get-in scaled-aug [i col-idx])))))
                                       (range (* 2 n))))))
                           (range n))
                     (inc i)))))))))

(defn matrix-add
  ""
  [A B]
  (mapv (fn [row-a row-b]
          (mapv fc/add row-a row-b))
        A B))

(defn matrix-subtract
  ""
  [A B]
  (mapv (fn [row-a row-b]
          (mapv fc/sub row-a row-b))
        A B))

(defn matrix-scale
  ""
  [A alpha]
  (let [alpha (ensure-complex alpha)]
    (mapv (fn [row]
            (mapv #(complex-scale % alpha) row))
          A)))

(defn matrix-negate
  ""
  [A]
  (matrix-scale A (fc/complex -1.0 0.0)))

(defn matrix-multiply
  ""
  [A B]
  (let [[rows-a cols-a] (complex-matrix-shape A)
        [rows-b cols-b] (complex-matrix-shape B)]
    (when (not= cols-a rows-b)
      (throw (ex-info "Matrix dimensions incompatible"
                      {:A-shape [rows-a cols-a] :B-shape [rows-b cols-b]})))
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce fc/add
                            (fc/complex 0.0 0.0)
                            (map (fn [k]
                                   (fc/mult (get-in A [i k])
                                            (get-in B [k j])))
                                 (range cols-a))))
                  (range cols-b)))
          (range rows-a))))

(defn matrix-vector-product
  ""
  [A x]
  (let [[rows cols] (complex-matrix-shape A)]
    (when (not= cols (count x))
      (throw (ex-info "Matrix-vector dimensions incompatible"
                      {:matrix-shape [rows cols] :vector-length (count x)})))
    (mapv (fn [i]
            (reduce fc/add
                    (fc/complex 0.0 0.0)
                    (map (fn [j]
                           (fc/mult (get-in A [i j])
                                    (get x j)))
                         (range cols))))
          (range rows))))

(defn outer-product
  "Compute the outer product x ⊗ y†."
  [x y]
  (mapv (fn [xi]
          (mapv (fn [yj]
                  (fc/mult xi (fc/conjugate yj)))
                y))
        x))

(defn hadamard-product
  "Compute the element-wise (Hadamard) product A ⊙ B."
  [A B]
  (mapv (fn [row-a row-b]
          (mapv fc/mult row-a row-b))
        A B))

(defn kronecker-product
  "Compute the Kronecker (tensor) product A ⊗ B."
  [A B]
  (let [[rows-a cols-a] (complex-matrix-shape A)
        [rows-b cols-b] (complex-matrix-shape B)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (let [ai (quot i rows-b)
                          aj (quot j cols-b)
                          bi (mod i rows-b)
                          bj (mod j cols-b)]
                      (fc/mult (get-in A [ai aj])
                               (get-in B [bi bj]))))
                  (range (* cols-a cols-b))))
          (range (* rows-a rows-b)))))

(defn transpose
  "Compute the transpose of a complex matrix Aᵀ."
  [A]
  (let [[rows cols] (complex-matrix-shape A)]
    (mapv (fn [j]
            (mapv (fn [i]
                    (get-in A [i j]))
                  (range rows)))
          (range cols))))

(defn conjugate-transpose
  "Compute the conjugate transpose Aᴴ (Hermitian adjoint)."
  [A]
  (let [[rows cols] (complex-matrix-shape A)]
    (mapv (fn [j]
            (mapv (fn [i]
                    (fc/conjugate (get-in A [i j])))
                  (range rows)))
          (range cols))))

(defn trace
  "Compute the trace Tr(A) = Σᵢ aᵢᵢ of a complex matrix A."
  [A]
  (let [[rows cols] (complex-matrix-shape A)]
    (when (not= rows cols)
      (throw (ex-info "Matrix must be square for trace" {:shape [rows cols]})))
    (reduce fc/add
            (fc/complex 0.0 0.0)
            (map #(get-in A [% %]) (range rows)))))

(defn inner-product
  "Compute the inner product ⟨x|y⟩ of two complex vectors."
  [x y]
  (when (not= (count x) (count y))
    (throw (ex-info "Vectors must have same length"
                    {:x-length (count x) :y-length (count y)})))
  (reduce fc/add
          (fc/complex 0.0 0.0)
          (map (fn [xi yi]
                 (fc/mult (fc/conjugate xi) yi))
               x y)))

(defn norm2
  "Compute the Euclidean (L2) norm ||x||₂ of a complex vector x."
  [x]
  (let [norm-squared (reduce fc/add
                             (fc/complex 0.0 0.0)
                             (map (fn [xi]
                                    (fc/mult (fc/conjugate xi) xi))
                                  x))]
    (fm/sqrt (fc/re norm-squared))))

(defn hermitian?
  "Check if a complex matrix A is Hermitian (A ≈ Aᴴ)."
  [A eps]
  (let [A-conj-transpose (conjugate-transpose  A)
        diff (matrix-subtract A A-conj-transpose)]
    ;; Check if all elements are within tolerance
    (every? (fn [row]
              (every? (fn [element]
                        (< (fc/abs element) eps))
                      row))
            diff)))

(defn diagonal?
  "Check if a complex matrix A is diagonal (off-diagonal elements ≈ 0)."
  [A eps]
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      false)
    ;; Check if all off-diagonal elements are within tolerance of zero
    (every? (fn [i]
              (every? (fn [j]
                        (if (= i j)
                          true  ; diagonal elements can be anything
                          (< (fc/abs (get-in A [i j])) eps)))
                      (range n)))
            (range n))))

(defn unitary?
  "Check if a complex matrix U is unitary (Uᴴ U ≈ I)."
  [U eps]
  (let [U-conj-transpose (conjugate-transpose U)
        product (matrix-multiply U-conj-transpose U)
        [rows cols] (complex-matrix-shape product)
        identity-matrix (mapv (fn [i]
                                (mapv (fn [j]
                                        (if (= i j)
                                          (fc/complex 1.0 0.0)
                                          (fc/complex 0.0 0.0)))
                                      (range cols)))
                              (range rows))
        diff (matrix-subtract product identity-matrix)]
    ;; Check if all elements are within tolerance
    (every? (fn [row]
              (every? (fn [element]
                        (< (fc/abs element) eps))
                      row))
            diff)))

(defn eigen-hermitian
  "Compute eigenvalues and eigenvectors of a Hermitian matrix A."
  [A]
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix must be square for eigendecomposition" {:shape [n m]})))

    ;; Extract real and imaginary parts
    (let [real-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/re (get-in A [i j])))
                                  (range n)))
                          (range n))
          imag-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/im (get-in A [i j])))
                                  (range n)))
                          (range n))]

      (if (real-matrix? A)
        ;; Real Hermitian matrix: use direct real eigendecomposition
        (let [real-matrix (let [data (into-array (map double-array real-part))]
                            (fmat/mat data))
              eigen-decomp (org.apache.commons.math3.linear.EigenDecomposition. real-matrix)
              eigenvals-array (.getRealEigenvalues eigen-decomp)
              eigenvecs-matrix (.getV eigen-decomp)
              
              ;; Convert to complex format
              complex-eigenvals (mapv #(fc/complex % 0.0) eigenvals-array)
              eigenvec-data (fmat/mat->array2d eigenvecs-matrix)
              complex-eigenvecs (mapv (fn [col-idx]
                                        (mapv (fn [row-idx]
                                                (fc/complex (get-in eigenvec-data [row-idx col-idx]) 0.0))
                                              (range n)))
                                      (range n))]
          
          {:eigenvalues complex-eigenvals
           :eigenvectors complex-eigenvecs})

        ;; Complex Hermitian matrix: use embedded matrix approach
        (let [;; Create 2n×2n real matrix: [[Re(A) -Im(A)] [Im(A) Re(A)]]
              embedded-matrix (concat
                               (mapv (fn [i]
                                       (concat (get real-part i)
                                               (mapv - (get imag-part i))))
                                     (range n))
                               (mapv (fn [i]
                                       (concat (get imag-part i)
                                               (get real-part i)))
                                     (range n)))

              ;; Convert to Apache Commons Math format
              embedded-real-matrix (let [data (into-array (map double-array embedded-matrix))]
                                     (fmat/mat data))

              ;; Compute eigendecomposition using Apache Commons Math directly
              eigen-decomp (org.apache.commons.math3.linear.EigenDecomposition. embedded-real-matrix)
              real-eigenvals-array (.getRealEigenvalues eigen-decomp)
              real-eigenvecs-matrix (.getV eigen-decomp)

              ;; Extract complex eigenvalues (they appear in pairs for Hermitian matrices)
              ;; Take only the first n eigenvalues (the rest are duplicates)
              complex-eigenvals (mapv (fn [i]
                                        ; Hermitian matrices have real eigenvalues
                                        (fc/complex (get real-eigenvals-array (* 2 i)) 0.0))
                                      (range n))

              ;; Extract complex eigenvectors
              eigenvec-data (fmat/mat->array2d real-eigenvecs-matrix)
              complex-eigenvecs (mapv (fn [i]
                                        (mapv (fn [j]
                                                (let [real-part (get-in eigenvec-data [j i])
                                                      imag-part (get-in eigenvec-data [(+ n j) i])]
                                                  (fc/complex real-part imag-part)))
                                              (range n)))
                                      (range n))

              ;; Transpose to get column vectors
              eigenvec-matrix (mapv (fn [i]
                                      (mapv #(get % i) complex-eigenvecs))
                                    (range n))]

          {:eigenvalues complex-eigenvals
           :eigenvectors eigenvec-matrix})))))

(defn positive-semidefinite?
  "Check if a complex matrix A is positive semidefinite."
  [A eps]
  (try
    ;; Use eigenvalue check
    (let [[n m] (complex-matrix-shape A)]
      (when (not= n m)
        (throw (ex-info "Matrix must be square for positive-semidefinite check" {:shape [n m]})))

      ;; For complex matrices, check if Hermitian first
      (when-not (hermitian? A eps)
        (throw (ex-info "Matrix must be Hermitian for positive-semidefinite check" {})))

      ;; Get eigenvalues of Hermitian matrix
      (let [eigen-result (eigen-hermitian A)
            eigenvals (:eigenvalues eigen-result)]
        ;; All eigenvalues should be non-negative
        (every? (fn [eigenval]
                  (let [real-part (fc/re eigenval)]
                    (>= real-part (- eps))))
                eigenvals)))
    (catch Exception e
      (throw (ex-info "Error checking positive semidefinite property" {:original-error (.getMessage e)})))))

(defn eigen-general
  "Compute eigenvalues and eigenvectors of a general matrix."
  [A]
  ;; use iterative methods or embedding 
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix must be square for eigendecomposition" {:shape [n m]})))

    ;; For general complex matrices, we'll use the real embedding approach
    ;; This embeds the complex matrix A into a 2n×2n real matrix
    (let [real-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/re (get-in A [i j])))
                                  (range n)))
                          (range n))
          imag-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/im (get-in A [i j])))
                                  (range n)))
                          (range n))

          ;; Create 2n×2n real matrix: [[Re(A) -Im(A)] [Im(A) Re(A)]]
          embedded-matrix (concat
                           (mapv (fn [i]
                                   (concat (get real-part i)
                                           (mapv - (get imag-part i))))
                                 (range n))
                           (mapv (fn [i]
                                   (concat (get imag-part i)
                                           (get real-part i)))
                                 (range n)))

          ;; Convert to Apache Commons Math format and compute eigendecomposition
          embedded-real-matrix (let [data (into-array (map double-array embedded-matrix))]
                                 (fmat/mat data))
          eigen-decomp (org.apache.commons.math3.linear.EigenDecomposition. embedded-real-matrix)
          real-eigenvals (vec (.getRealEigenvalues eigen-decomp))
          imag-eigenvals (vec (.getImagEigenvalues eigen-decomp))
          eigenvecs-matrix (.getV eigen-decomp)

          ;; Extract the complex eigenvalues - they come in conjugate pairs
          ;; Take the first n eigenvalues and combine real/imaginary parts
          complex-eigenvals (mapv (fn [i]
                                    (fc/complex (get real-eigenvals i)
                                                (get imag-eigenvals i)))
                                  (range n))

          ;; Extract complex eigenvectors 
          eigenvec-data (fmat/mat->array2d eigenvecs-matrix)
          complex-eigenvecs (mapv (fn [i]
                                    (mapv (fn [j]
                                            (let [real-part (get-in eigenvec-data [j i])
                                                  imag-part (get-in eigenvec-data [(+ n j) i])]
                                              (fc/complex real-part imag-part)))
                                          (range n)))
                                  (range n))

          ;; Transpose to get column vectors
          eigenvec-matrix (mapv (fn [i]
                                  (mapv #(get % i) complex-eigenvecs))
                                (range n))]

      {:eigenvalues complex-eigenvals
       :eigenvectors eigenvec-matrix})))

(defn svd
  "Compute Singular Value Decomposition A = U * S * V† for complex matrices."
  [A]
  ;; use eigendecomposition approach
  (let [[m n] (complex-matrix-shape A)
        A-conj-transpose (conjugate-transpose A)
        ATA (matrix-multiply A-conj-transpose A)

        ;; Get eigendecomposition of A†A for V and singular values
        ata-eigen (if (hermitian? ATA 1e-10)
                    (eigen-hermitian ATA)
                    (throw (ex-info "A†A should be Hermitian for SVD" {})))
        eigenvalues (:eigenvalues ata-eigen)
        V-matrix (:eigenvectors ata-eigen)

        ;; Singular values are sqrt of eigenvalues of A†A
        singular-values (mapv (fn [eigenval]
                                (let [real-part (fc/re eigenval)
                                      sqrt-val (fm/sqrt (fm/max 0.0 real-part))]
                                  sqrt-val))
                              eigenvalues)

        ;; Sort singular values in descending order with corresponding eigenvectors
        indexed-pairs (map vector (range) singular-values)
        sorted-pairs (sort-by second > indexed-pairs)
        sorted-indices (mapv first sorted-pairs)
        sorted-singular-values (mapv second sorted-pairs)

        ;; Reorder eigenvectors accordingly
        V-cols (mapv (fn [j]
                       (mapv #(get-in V-matrix [% j]) (range n)))
                     (range n))
        sorted-V-cols (mapv #(get V-cols %) sorted-indices)
        V-sorted (mapv (fn [i]
                         (mapv #(get % i) sorted-V-cols))
                       (range n))

        ;; Compute U = A * V * Σ^-1
        tolerance 1e-12
        U-cols (mapv (fn [j]
                       (let [v-col (mapv #(get-in V-sorted [% j]) (range n))
                             sigma-val (get sorted-singular-values j)]
                         (if (> sigma-val tolerance)
                           (let [av-col (matrix-vector-product A v-col)]
                             (mapv #(fc/mult % (fc/complex (/ 1.0 sigma-val) 0.0)) av-col))
                           ;; Zero singular value - use zero vector
                           (mapv (fn [_] (fc/complex 0.0 0.0)) (range m)))))
                     (range (min m n)))

        ;; If m > n, we need additional orthonormal columns for U
        U-complete (if (> m n)
                     ;; TODO: Use Gram-Schmidt to complete the basis
                     (let [extra-cols (mapv (fn [i]
                                              (let [ei (mapv (fn [j]
                                                               (if (= j (+ n i))
                                                                 (fc/complex 1.0 0.0)
                                                                 (fc/complex 0.0 0.0)))
                                                             (range m))]
                                                ei))
                                            (range (- m n)))]
                       (concat U-cols extra-cols))
                     U-cols)

        U-matrix (mapv (fn [i]
                         (mapv #(get % i) U-complete))
                       (range m))]

    {:U U-matrix
     :S (mapv #(fc/complex % 0.0) sorted-singular-values)
     :V† (conjugate-transpose V-sorted)}))

(defn lu-decomposition
  "Compute LU decomposition of a complex matrix A = P * L * U."
  [A]
  ;; use real embedding approach
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "LU decomposition requires square matrix" {:shape [n m]})))

    ;; Extract real and imaginary parts
    (let [real-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/re (get-in A [i j])))
                                  (range n)))
                          (range n))
          imag-part (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/im (get-in A [i j])))
                                  (range n)))
                          (range n))

          ;; Create 2n×2n real matrix: [[Re(A) -Im(A)] [Im(A) Re(A)]]
          embedded-matrix (vec (concat
                                (mapv (fn [i]
                                        (vec (concat (get real-part i)
                                                     (mapv - (get imag-part i)))))
                                      (range n))
                                (mapv (fn [i]
                                        (vec (concat (get imag-part i)
                                                     (get real-part i))))
                                      (range n))))

          ;; Convert to Apache Commons Math format and compute LU
          embedded-real-matrix (let [data (into-array (map double-array embedded-matrix))]
                                 (fmat/mat data))
          lu-decomp (org.apache.commons.math3.linear.LUDecomposition. embedded-real-matrix)
          L-embedded (.getL lu-decomp)
          U-embedded (.getU lu-decomp)
          P-embedded (.getP lu-decomp)

          ;; Extract complex matrices from 2n×2n real results
          L-data (fmat/mat->array2d L-embedded)
          U-data (fmat/mat->array2d U-embedded)
          P-data (fmat/mat->array2d P-embedded)

          ;; Reconstruct complex L matrix
          L-complex (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/complex (get-in L-data [i j])
                                                (get-in L-data [(+ n i) j])))
                                  (range n)))
                          (range n))

          ;; Reconstruct complex U matrix  
          U-complex (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/complex (get-in U-data [i j])
                                                (get-in U-data [(+ n i) j])))
                                  (range n)))
                          (range n))

          ;; Reconstruct complex P matrix
          P-complex (mapv (fn [i]
                            (mapv (fn [j]
                                    (fc/complex (get-in P-data [i j])
                                                (get-in P-data [(+ n i) j])))
                                  (range n)))
                          (range n))]

      {:L L-complex
       :U U-complex
       :P P-complex})))

(defn qr-decomposition
  "Compute QR decomposition of a complex matrix A = Q * R."
  [A]
  ;; use Modified Gram-Schmidt (stateless functional approach)
  (let [[m n] (complex-matrix-shape A)
        ;; Extract columns of A as complex vectors
        get-col (fn [j] (mapv #(get-in A [% j]) (range m)))]


    ;; Use loop/recur to build Q and R functionally
    (loop [j 0
           Q []
           R (mapv (fn [_] (mapv (fn [_] (fc/complex 0.0 0.0)) (range n))) (range n))]
      (if (= j n)
        {:Q (mapv (fn [i] (mapv #(nth % i) Q)) (range m))
         :R R}

        (let [v0 (get-col j)
              ;; Orthogonalize against all previous columns using reduce
              [v R] (reduce (fn [[vv R] i]
                              (let [qi (nth Q i)
                                    ;; R[i,j] = <qi, v> (complex inner product) 
                                    r_ij (inner-product qi vv)
                                    ;; v = v - R[i,j] * qi
                                    scaled-qi (mapv #(fc/mult % r_ij) qi)
                                    new-v (mapv fc/sub vv scaled-qi)]
                                [new-v (assoc-in R [i j] r_ij)]))
                            [v0 R]
                            (range j))

              ;; Normalize: R[j,j] = ||v||, q_j = v / R[j,j]
              norm-v (norm2 v)
              r_jj (fc/complex norm-v 0.0)

              ;; Handle near-zero vectors gracefully
              q_j (if (< norm-v 1e-12)
                    ;; Create standard basis vector
                    (mapv (fn [i] (if (= i j) (fc/complex 1.0 0.0) (fc/complex 0.0 0.0))) (range m))
                    ;; Normalize: q_j = v / ||v||
                    (mapv #(fc/div % r_jj) v))

              R (assoc-in R [j j] r_jj)
              Q (conj Q q_j)]

          (recur (inc j) Q R))))))

(defn cholesky-decomposition
  "Compute Cholesky decomposition A = L * L† for positive definite complex matrices."
  ([A] (cholesky-decomposition A  default-tolerance))
  ([A eps]
   (let [[n m] (complex-matrix-shape A)]
     (when (not= n m)
       (throw (ex-info "Cholesky decomposition requires square matrix" {:shape [n m]})))

     ;; Check if matrix is Hermitian
     (when-not (hermitian? A eps)
       (throw (ex-info "Cholesky decomposition requires Hermitian matrix" {})))

     ;; Direct Cholesky algorithm adapted from clojure-math
     (loop [i 0
            L (mapv (fn [_] (mapv (fn [_] (fc/complex 0.0 0.0)) (range n))) (range n))]
       (if (= i n)
         {:L L}
         (let [L (loop [j 0 L L]
                   (if (> j i)
                     L
                     (let [;; Compute sum: Σ(k=0 to j-1) L[i,k] * conj(L[j,k])
                           sum-of-products (reduce fc/add
                                                   (fc/complex 0.0 0.0)
                                                   (for [k (range j)]
                                                     (fc/mult (get-in L [i k])
                                                              (fc/conjugate (get-in L [j k])))))
                           ;; A[i,j] - sum
                           aij (get-in A [i j])
                           diff (fc/sub aij sum-of-products)]

                       (if (= i j)
                         ;; Diagonal case: L[i,i] = sqrt(A[i,i] - sum)
                         (let [real-part (fc/re diff)
                               imag-part (fc/im diff)]
                           ;; Check that diagonal is real and positive
                           (when (> (fm/abs imag-part) eps)
                             (throw (ex-info "Hermitian matrix diagonal must be real"
                                             {:i i :imaginary-part imag-part})))
                           (when (< real-part eps)
                             (throw (ex-info "Matrix is not positive definite"
                                             {:i i :diagonal-value real-part})))
                           (let [sqrt-val (fm/sqrt real-part)
                                 new-L (assoc-in L [i i] (fc/complex sqrt-val 0.0))]
                             (recur (inc j) new-L)))

                         ;; Off-diagonal case: L[i,j] = (A[i,j] - sum) / L[j,j]
                         (let [ljj (get-in L [j j])
                               lij (fc/div diff ljj)
                               new-L (assoc-in L [i j] lij)]
                           (recur (inc j) new-L))))))]
           (recur (inc i) L)))))))

(defn matrix-exp
  "Compute the matrix exponential exp(A) using eigendecomposition."
  [A]
  ;; use eigendecomposition approach
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix exponential requires square matrix" {:shape [n m]})))
    (if (= n 1)
      ;; 1x1 complex scalar case
      (let [z (get-in A [0 0])
            re (fc/re z)
            im (fc/im z)
            exp-re (fm/exp re)
            result-re (* exp-re (fm/cos im))
            result-im (* exp-re (fm/sin im))]
        [[(fc/complex result-re result-im)]])

      ;; General complex matrix - use eigendecomposition
      (try
        (let [eigen-result (if (hermitian? A 1e-10)
                             (eigen-hermitian A)
                             (eigen-general A))
              eigenvalues (:eigenvalues eigen-result)
              eigenvectors (:eigenvectors eigen-result)

              ;; Compute exp(lambda_i) for each eigenvalue  
              exp-eigenvals (mapv (fn [eigenval]
                                    (let [re (fc/re eigenval)
                                          im (fc/im eigenval)
                                          exp-re (fm/exp re)
                                          result-re (* exp-re (fm/cos im))
                                          result-im (* exp-re (fm/sin im))]
                                      (fc/complex result-re result-im)))
                                  eigenvalues)

              ;; Create diagonal matrix from exp(eigenvalues)
              exp-diag (mapv (fn [i]
                               (mapv (fn [j]
                                       (if (= i j)
                                         (get exp-eigenvals i)
                                         (fc/complex 0.0 0.0)))
                                     (range n)))
                             (range n))

              ;; Compute V * exp(Λ) * V^-1
              V eigenvectors
              V-inv (inverse V)
              temp (matrix-multiply exp-diag V-inv)]

          (matrix-multiply V temp))

        (catch Exception e
          ;; If eigendecomposition fails, fall back to series approximation for small matrices
          (if (< n 5)
            (let [;; Use Taylor series: exp(A) ≈ I + A + A²/2! + A³/3! + ...
                  identity-matrix (mapv (fn [i]
                                          (mapv (fn [j]
                                                  (if (= i j)
                                                    (fc/complex 1.0 0.0)
                                                    (fc/complex 0.0 0.0)))
                                                (range n)))
                                        (range n))
                  max-terms 20
                  tolerance 1e-12]
              (loop [result identity-matrix
                     term A
                     factorial 1.0
                     k 1]
                (if (or (>= k max-terms)
                        (every? (fn [row]
                                  (every? (fn [elem]
                                            (< (fc/abs elem) tolerance))
                                          row))
                                term))
                  result
                  (let [scaled-term (matrix-scale term (/ 1.0 factorial))
                        new-result (matrix-add result scaled-term)
                        new-term (matrix-multiply A term)]
                    (recur new-result new-term (* factorial (inc k)) (inc k))))))
            (throw (ex-info "Complex matrix exponential failed - matrix too large for series approximation"
                            {:original-error (.getMessage e)
                             :matrix-size n
                             :suggestion "Use smaller matrices or ensure matrix is diagonalizable"}))))))))

(defn matrix-log
  "Compute the principal matrix logarithm log(A) using eigendecomposition."
  [A]
  ;; implement via eigendecomposition  
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix logarithm requires square matrix" {:shape [n m]})))

    ;; Special case for diagonal matrices - compute directly
    (if (diagonal? A 1e-10)
      ;; For diagonal matrices, log(A) has log(A[i,i]) on the diagonal
      (mapv (fn [i]
              (mapv (fn [j]
                      (if (= i j)
                        (let [diag-elem (get-in A [i j])
                              r (fc/abs diag-elem)]
                          (when (< r 1e-14)
                            (throw (ex-info "Cannot compute log of zero diagonal element"
                                            {:element diag-elem :position [i j]})))
                          (fc/log diag-elem))
                        (fc/complex 0.0 0.0)))
                    (range n)))
            (range n))

      ;; General case: Use eigendecomposition: log(A) = V * log(Λ) * V^(-1)
      (try
        (let [eigen-result (if (hermitian? A 1e-10)
                             (eigen-hermitian A)
                             (eigen-general A))
              eigenvals (:eigenvalues eigen-result)
              eigenvecs (:eigenvectors eigen-result)

            ;; Compute log of eigenvalues - handle domain restrictions
            log-eigenvals (mapv (fn [lambda]
                                  (let [r (fc/abs lambda)
                                        theta (fc/arg lambda)]
                                    (when (< r 1e-14)
                                      (throw (ex-info "Cannot compute log of zero eigenvalue"
                                                      {:eigenvalue lambda})))
                                    ;; Principal branch: log(r*e^(iθ)) = log(r) + iθ
                                    (fc/complex (fm/log r) theta)))
                                eigenvals)

            ;; Create diagonal matrix of log eigenvalues
            log-diag (mapv (fn [i]
                             (mapv (fn [j]
                                     (if (= i j)
                                       (get log-eigenvals i)
                                       (fc/complex 0.0 0.0)))
                                   (range n)))
                           (range n))

            ;; Compute V^(-1)
            V-inv (inverse eigenvecs)

            ;; Result: V * log(Λ) * V^(-1)
            temp (matrix-multiply eigenvecs log-diag)]
        (matrix-multiply temp V-inv))
        (catch Exception e
          (throw (ex-info "Complex matrix logarithm failed"
                          {:original-error (.getMessage e)
                           :matrix-size n})))))))

(defn matrix-sqrt
  "Compute the principal matrix square root √A using eigendecomposition."
  [A]
  ;; implement via eigendecomposition
  (let [[n m] (complex-matrix-shape A)]
    (when (not= n m)
      (throw (ex-info "Matrix square root requires square matrix" {:shape [n m]})))

    ;; Check if matrix is diagonal - use direct approach
    (if (diagonal? A 1e-10)
      ;; For diagonal matrices, check positive semidefinite first
      (let [diagonal-elements (mapv #(get-in A [% %]) (range n))]
        ;; Check if all diagonal elements have non-negative real parts  
        (when (some (fn [elem] (< (fc/re elem) 0.0)) diagonal-elements)
          (throw (ex-info "Matrix square root requires positive semidefinite matrix"
                          {:diagonal-elements diagonal-elements})))
        
        ;; All diagonal elements are non-negative, compute sqrt
        (mapv (fn [i]
                (mapv (fn [j]
                        (if (= i j)
                          (let [diag-elem (get-in A [i j])
                                r (fc/abs diag-elem)
                                theta (fc/arg diag-elem)]
                            (when (< r 1e-14)
                              (throw (ex-info "Cannot compute sqrt of zero diagonal element"
                                              {:element diag-elem :position [i j]})))
                            ;; Principal branch: √(r*e^(iθ)) = √r * e^(iθ/2)
                            (let [sqrt-r (fm/sqrt r)
                                  half-theta (/ theta 2.0)]
                              (fc/complex (* sqrt-r (fm/cos half-theta))
                                          (* sqrt-r (fm/sin half-theta)))))
                          (fc/complex 0.0 0.0)))
                      (range n)))
              (range n)))
      
      ;; Use eigendecomposition: √A = V * √Λ * V^(-1)
      (try
        (let [eigen-result (eigen-general A)
              eigenvals (:eigenvalues eigen-result)
              eigenvecs (:eigenvectors eigen-result)

              ;; Compute square root of eigenvalues using principal branch
              sqrt-eigenvals (mapv (fn [lambda]
                                     (let [r (fc/abs lambda)
                                           theta (fc/arg lambda)]
                                       (when (< r 1e-14)
                                         (throw (ex-info "Cannot compute sqrt of zero eigenvalue"
                                                         {:eigenvalue lambda})))
                                       ;; Principal branch: √(r*e^(iθ)) = √r * e^(iθ/2)
                                       (let [sqrt-r (fm/sqrt r)
                                             half-theta (/ theta 2.0)]
                                         (fc/complex (* sqrt-r (fm/cos half-theta))
                                                     (* sqrt-r (fm/sin half-theta))))))
                                   eigenvals)

              ;; Create diagonal matrix of sqrt eigenvalues
              sqrt-diag (mapv (fn [i]
                                (mapv (fn [j]
                                        (if (= i j)
                                          (get sqrt-eigenvals i)
                                          (fc/complex 0.0 0.0)))
                                      (range n)))
                              (range n))

              ;; Compute V^(-1)
              V-inv (inverse eigenvecs)

              ;; Result: V * √Λ * V^(-1)
              temp (matrix-multiply eigenvecs sqrt-diag)]
          (matrix-multiply temp V-inv))
        (catch Exception e
          (throw (ex-info "Complex matrix square root failed"
                          {:original-error (.getMessage e)
                           :matrix-size n})))))))


(defn spectral-norm
  "Compute the spectral norm ||A||₂ (largest singular value) of a complex matrix A."
  [A]
  ;; implement via eigendecomposition of A†A
  (let [A-conj-transpose (conjugate-transpose A)
        ATA (matrix-multiply A-conj-transpose A)
        ;; Get eigenvalues of A†A
        eigenvals-result (if (real-matrix? ATA)
                           (let [real-ATA (complex-matrix->real-matrix ATA)]
                             ;; fmat/eigenvalues returns Vec2 even for real matrices, so extract real parts
                             (map fc/re (fmat/eigenvalues real-ATA)))
                           ;; Complex ATA case - use eigendecomposition
                           (let [eigen-result (eigen-hermitian ATA)]
                             (:eigenvalues eigen-result)))]
    ;; Spectral norm is sqrt of largest eigenvalue of A†A
    (fm/sqrt (apply max (map (fn [ev]
                               (if (instance? fastmath.vector.Vec2 ev)
                                 (fc/re ev)
                                 ev))
                             eigenvals-result)))))

(defn condition-number
  "Compute the condition number κ₂(A) = ||A||₂ * ||A⁻¹||₂ for a complex matrix A."
  [A]
  ;; implement via eigendecomposition of A†A
  (let [A-conj-transpose (conjugate-transpose A)
        ATA (matrix-multiply A-conj-transpose A)

        ;; Get eigenvalues to compute singular values
        ata-eigenvals (if (hermitian? ATA 1e-10)
                        (:eigenvalues (eigen-hermitian ATA))
                        (:eigenvalues (eigen-general ATA)))

        ;; Convert eigenvalues to singular values (sqrt of real parts)
        singular-values (mapv (fn [eigenval]
                                (let [real-part (fc/re eigenval)]
                                  (fm/sqrt (fm/max 0.0 real-part))))
                              ata-eigenvals)

        ;; Sort singular values in descending order
        sorted-svs (sort > singular-values)
        sigma-max (first sorted-svs)
        sigma-min (last sorted-svs)]

    (if (< sigma-min 1e-14)
      Double/POSITIVE_INFINITY  ; Matrix is singular
      (/ sigma-max sigma-min))))

;;;
;;; Quantum State Operation Functions (TODO: Move to separate namespace)
;;;
(defn state-normalize
  "Normalize a quantum state vector to unit norm."
  [state]
  (let [norm-squared (reduce fc/add
                             (fc/complex 0.0 0.0)
                             (map (fn [si]
                                    (fc/mult (fc/conjugate si) si))
                                  state))
        norm (fm/sqrt (fc/re norm-squared))]
    (if (< norm 1e-12)
      state  ; Return unchanged if zero vector
      (mapv #(fc/mult % (fc/complex (/ 1.0 norm) 0.0)) state))))

(defn projector-from-state
  "Create a projector matrix |ψ⟩⟨ψ| from a quantum state vector ψ."
  [psi]
  (let [normalized-psi (state-normalize psi)]
    (mapv (fn [psi-i]
            (mapv (fn [psi-j]
                    (fc/mult psi-i (fc/conjugate psi-j)))
                  normalized-psi))
          normalized-psi)))

(defn trace-one?
  "Check if a density matrix has trace equal to one (Tr(ρ) ≈ 1)."
  ([rho] (trace-one? rho default-tolerance))
  ([rho eps]
   (let [tr (trace rho)
         trace-value (fc/re tr)]  ; Extract real part
     (< (fm/abs (- trace-value 1.0)) eps))))


