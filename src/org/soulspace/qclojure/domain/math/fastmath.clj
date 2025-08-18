(ns org.soulspace.qclojure.domain.math.fastmath
  "FastMath 3 backend implementation for QClojure math protocols.
  
  Uses a hybrid approach:
  - FastMath Vec2 for complex numbers with conversion at API boundaries
  - Apache Commons Math3 RealMatrix for real matrix operations
  - Custom complex matrix operations using pairs of real matrices
  - Leverages FastMath optimizations where possible"
  (:require [org.soulspace.qclojure.domain.math.protocols :as proto]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [fastmath.vector :as fvec]
            [fastmath.matrix :as fmat]))

;;;
;;; Configuration and utilities
;;;

(def ^:const ^double default-tolerance 1.0e-12)

(defn tolerance* [backend]
  (double (or (:tolerance backend) (:tolerance (:config backend)) default-tolerance)))

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

(defn vec2->complex-map
  "Convert FastMath Vec2 to complex map representation."
  [v]
  (if (complex? v)
    {:real (fc/re v) :imag (fc/im v)}
    v))

(defn complex-map->vec2
  "Convert complex map to FastMath Vec2 representation."
  [c]
  (cond
    (complex? c) c
    (complex-scalar? c) (fc/complex (:real c) (:imag c))
    (number? c) (fc/complex c 0.0)
    :else (throw (ex-info "Cannot convert to Vec2" {:value c}))))

(defn ensure-complex
  "Ensure input is a complex number."
  [x]
  (cond
    (complex? x) x
    (number? x) (fc/complex x 0.0)
    (complex-scalar? x) (fc/complex (:real x) (:imag x))
    :else (throw (ex-info "Cannot convert to Vec2" {:value x}))))

(defn ensure-real
  "Extract real part or ensure it's a real number."
  [x]
  (cond
    (number? x) (double x)
    (complex? x) (fc/re x)
    (complex-scalar? x) (double (:real x))
    :else (throw (ex-info "Cannot extract real part" {:value x}))))

;;;
;;; Backend record
;;;

(defrecord FastMathBackend [tolerance])

;;;
;;; Complex protocol implementation
;;;

(extend-protocol proto/Complex
  fastmath.vector.Vec2
  (real [x] (fc/re x))
  (imag [x] (fc/im x))
  (conjugate [x] (fc/conjugate x))
  (complex? [_] true)
  
  java.lang.Number
  (real [x] (double x))
  (imag [_] 0.0)
  (conjugate [x] x)
  (complex? [_] false)
  
  clojure.lang.IPersistentMap
  (real [x] 
    (if (and (contains? x :real) (contains? x :imag))
      (:real x)
      (throw (ex-info "Not a complex map" {:value x}))))
  (imag [x]
    (if (and (contains? x :real) (contains? x :imag))
      (:imag x)
      (throw (ex-info "Not a complex map" {:value x}))))
  (conjugate [x]
    (if (and (contains? x :real) (contains? x :imag))
      {:real (:real x) :imag (- (:imag x))}
      (throw (ex-info "Not a complex map" {:value x}))))
  (complex? [x] 
    (and (contains? x :real) (contains? x :imag) 
         (number? (:real x)) (number? (:imag x))))
  
  clojure.lang.IPersistentVector
  (real [v] (mapv proto/real v))
  (imag [v] (mapv proto/imag v))
  (conjugate [v] (mapv proto/conjugate v))
  (complex? [v] (every? proto/complex? v)))

;;;
;;; BackendAdapter protocol implementation
;;;

(extend-protocol proto/BackendAdapter
  FastMathBackend
  (vector->backend [_ v]
    "Convert QClojure vector to FastMath representation."
    (cond
      ;; Already a vector of Vec2 complex numbers - pass through
      (and (vector? v) (every? complex? v)) v
      
      ;; Vector of complex maps - convert to Vec2
      (complex-vector? v) (mapv complex-map->vec2 v)
      
      ;; Vector of real numbers - convert to Vec2 with zero imaginary
      (and (vector? v) (every? number? v)) (mapv #(fc/complex % 0.0) v)
      
      ;; Single complex number - return as single-element vector
      (complex-scalar? v) [(ensure-complex v)]
      
      ;; Single real number - return as single-element vector
      (number? v) [(fc/complex v 0.0)]
      
      :else (throw (ex-info "Cannot convert to backend vector" {:value v}))))
  
  (backend->vector [_ v]
    "Convert FastMath vector to QClojure representation."
    (cond
      ;; Vector of Vec2 - convert to complex maps
      (and (vector? v) (every? complex? v)) (mapv vec2->complex-map v)
      
      ;; Already in QClojure format - pass through
      (vector? v) v
      
      :else (throw (ex-info "Cannot convert from backend vector" {:value v}))))
  
  (matrix->backend [_ m]
    "Convert QClojure matrix to FastMath representation."
    (cond
      ;; Matrix of Vec2 complex numbers - pass through
      (and (vector? m) (every? #(and (vector? %) (every? complex? %)) m)) m
      
      ;; Complex matrix - convert to Vec2
      (complex-matrix? m) (mapv #(mapv complex-map->vec2 %) m)
      
      ;; Real matrix - convert to Vec2 with zero imaginary
      (and (vector? m) (every? #(and (vector? %) (every? number? %)) m))
      (mapv #(mapv (fn [x] (fc/complex x 0.0)) %) m)
      
      :else (throw (ex-info "Cannot convert to backend matrix" {:value m}))))
  
  (backend->matrix [_ m]
    "Convert FastMath matrix to QClojure representation."
    (cond
      ;; Matrix of Vec2 - convert to complex maps
      (and (vector? m) (every? #(and (vector? %) (every? complex? %)) m))
      (mapv #(mapv vec2->complex-map %) m)
      
      ;; Already in QClojure format - pass through
      (vector? m) m
      
      :else (throw (ex-info "Cannot convert from backend matrix" {:value m}))))
  
  (scalar->backend [_ s]
    "Convert QClojure scalar to FastMath representation."
    (ensure-complex s))
  
  (backend->scalar [_ s]
    "Convert FastMath scalar to QClojure representation."
    (cond
      (complex? s) (vec2->complex-map s)
      (number? s) s
      :else (throw (ex-info "Cannot convert from backend scalar" {:value s})))))

;;;
;;; Matrix algebra helper functions
;;;

(defn- is-real-matrix?
  "Check if a matrix contains only real numbers (zero imaginary parts)."
  [matrix]
  (every? (fn [row]
            (every? (fn [element]
                      (let [vec2-elem (ensure-complex element)]
                        (< (Math/abs (fc/im vec2-elem)) 1e-12)))
                    row))
          matrix))

(defn- qcomplex->real
  "Extract real part of a QClojure complex number, checking it's actually real."
  [z]
  (let [vec2-z (ensure-complex z)]
    (when-not (< (Math/abs (fc/im vec2-z)) 1e-12)
      (throw (ex-info "Complex number has non-zero imaginary part" 
                      {:value z :imaginary (fc/im vec2-z)})))
    (fc/re vec2-z)))

(defn- qmatrix->real-matrix
  "Convert QClojure matrix to Apache Commons RealMatrix."
  [matrix]
  (let [data (into-array (map (fn [row]
                                (double-array (map qcomplex->real row)))
                              matrix))]
    (fmat/mat data)))

(defn- real-matrix->qmatrix
  "Convert Apache Commons RealMatrix to QClojure complex matrix."
  [real-matrix]
  (let [data (fmat/mat->array2d real-matrix)]
    (mapv (fn [row]
            (mapv #(fc/complex % 0.0) row))
          data)))

(defn- gaussian-elimination-solve
  "Solve linear system using Gaussian elimination for complex matrices."
  [_A _b]
  ;; For now, throw - we'll implement this if needed
  (throw (ex-info "Complex matrix solver not yet implemented" 
                  {:suggestion "Use real matrices when possible"})))

(defn- gaussian-elimination-inverse
  "Compute matrix inverse using Gaussian elimination for complex matrices."
  [_A]
  ;; For now, throw - we'll implement this if needed
  (throw (ex-info "Complex matrix inverse not yet implemented"
                  {:suggestion "Use real matrices when possible"})))

(defn- complex-matrix-shape
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

;;;
;;; MatrixAlgebra protocol implementation
;;;

(extend-protocol proto/MatrixAlgebra
  FastMathBackend
  
  (shape [_ A]
    "Return the dimensions of a matrix."
    (complex-matrix-shape A))
  
  (add [_ A B]
    "Perform matrix addition A + B."
    (mapv (fn [row-a row-b]
            (mapv fc/add row-a row-b))
          A B))
  
  (subtract [_ A B]
    "Perform matrix subtraction A - B."  
    (mapv (fn [row-a row-b]
            (mapv fc/sub row-a row-b))
          A B))
  
  (scale [_ A alpha]
    "Perform scalar multiplication alpha · A."
    (let [alpha-vec2 (ensure-complex alpha)]
      (mapv (fn [row]
              (mapv #(complex-scale % alpha-vec2) row))
            A)))
  
  (negate [backend A]
    "Compute the additive inverse -A."
    (proto/scale backend A -1.0))

  ;; Matrix multiplication operations
  (matrix-multiply [_ A B]
    "Perform matrix multiplication A × B."
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
  
  (matrix-vector-product [_ A x]
    "Perform matrix–vector multiplication A × x."
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
  
  (outer-product [_ x y]
    "Compute the outer product x ⊗ y†."
    (mapv (fn [xi]
            (mapv (fn [yj]
                    (fc/mult xi (fc/conjugate yj)))
                  y))
          x))
  
  (hadamard [_ A B]
    "Compute the element-wise (Hadamard) product A ⊙ B."
    (mapv (fn [row-a row-b]
            (mapv fc/mult row-a row-b))
          A B))
  
  (kronecker [_ A B]
    "Compute the Kronecker (tensor) product A ⊗ B."
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
  
  ;; Structural transforms
  (transpose [_ A]
    "Compute the transpose Aᵀ."
    (let [[rows cols] (complex-matrix-shape A)]
      (mapv (fn [j]
              (mapv (fn [i]
                      (get-in A [i j]))
                    (range rows)))
            (range cols))))
  
  (conjugate-transpose [_ A]
    "Compute the conjugate transpose Aᴴ (Hermitian adjoint)."
    (let [[rows cols] (complex-matrix-shape A)]
      (mapv (fn [j]
              (mapv (fn [i]
                      (fc/conjugate (get-in A [i j])))
                    (range rows)))
            (range cols))))
  
  ;; Reductions / scalar results
  (trace [_ A]
    "Compute the trace Tr(A) = Σᵢ aᵢᵢ."
    (let [[rows cols] (complex-matrix-shape A)]
      (when (not= rows cols)
        (throw (ex-info "Matrix must be square for trace" {:shape [rows cols]})))
      (reduce fc/add
              (fc/complex 0.0 0.0)
              (map #(get-in A [% %]) (range rows)))))
  
  (inner-product [_ x y]
    "Compute the vector inner product ⟨x|y⟩."
    (when (not= (count x) (count y))
      (throw (ex-info "Vectors must have same length" 
                      {:x-length (count x) :y-length (count y)})))
    (reduce fc/add
            (fc/complex 0.0 0.0)
            (map (fn [xi yi]
                   (fc/mult (fc/conjugate xi) yi))
                 x y)))
  
  (norm2 [_ x]
    "Compute the Euclidean (L2) norm ||x||₂."
    (let [norm-squared (reduce fc/add
                               (fc/complex 0.0 0.0)
                               (map (fn [xi]
                                      (fc/mult (fc/conjugate xi) xi))
                                    x))]
      (Math/sqrt (fc/re norm-squared))))
  
  ;; Linear solves / inverses
  (solve-linear-system [_ A b]
    "Solve the linear system A x = b."
    (if (is-real-matrix? A)
      ;; Real matrix case - use FastMath/Apache Commons
      (let [real-A (qmatrix->real-matrix A)
            real-b (if (vector? b)
                     (double-array (map qcomplex->real b))
                     (double-array (map qcomplex->real (flatten b))))
            solution-array (fmat/solve real-A real-b)]
        (mapv #(fc/complex % 0.0) (vec solution-array)))
      ;; Complex matrix case - implement Gaussian elimination
      (gaussian-elimination-solve A b)))
  
  (inverse [_ A]
    "Compute the matrix inverse A⁻¹."
    (if (is-real-matrix? A)
      ;; Real matrix case - use FastMath/Apache Commons
      (let [real-A (qmatrix->real-matrix A)
            inv-matrix (fmat/inverse real-A)]
        (real-matrix->qmatrix inv-matrix))
      ;; Complex matrix case - implement using Gaussian elimination
      (gaussian-elimination-inverse A)))
  
  ;; Predicates
  (hermitian? 
    ([backend A] (proto/hermitian? backend A (tolerance* backend)))
    ([backend A eps]
     "Test if a matrix is Hermitian (A ≈ Aᴴ)."
     (let [A-conj-transpose (proto/conjugate-transpose backend A)
           diff (proto/subtract backend A A-conj-transpose)]
       ;; Check if all elements are within tolerance
       (every? (fn [row]
                 (every? (fn [element]
                           (< (fc/abs element) eps))
                         row))
               diff))))
  
  (unitary? 
    ([backend U] (proto/unitary? backend U (tolerance* backend)))
    ([backend U eps]
     "Test if a matrix is unitary (Uᴴ U ≈ I)."
     (let [U-conj-transpose (proto/conjugate-transpose backend U)
           product (proto/matrix-multiply backend U-conj-transpose U)
           [rows cols] (proto/shape backend product)
           identity-matrix (mapv (fn [i]
                                   (mapv (fn [j]
                                           (if (= i j)
                                             (fc/complex 1.0 0.0)
                                             (fc/complex 0.0 0.0)))
                                         (range cols)))
                                 (range rows))
           diff (proto/subtract backend product identity-matrix)]
       ;; Check if all elements are within tolerance  
       (every? (fn [row]
                 (every? (fn [element]
                           (< (fc/abs element) eps))
                         row))
               diff))))
  
  (positive-semidefinite? [backend A]
    "Test if a matrix is positive semidefinite."
    (try
      (if (is-real-matrix? A)
        ;; Real matrix case - use eigenvalues
        (let [real-A (qmatrix->real-matrix A)
              eigenvals (fmat/eigenvalues real-A)]
          (every? #(>= (fc/re %) (- (tolerance* backend))) eigenvals))
        ;; Complex matrix case - not implemented yet
        (throw (ex-info "Complex matrix positive-semidefinite check not yet implemented" {})))
      (catch Exception e
        (throw (ex-info "Error checking positive semidefinite property" {:original-error (.getMessage e)}))))))

;;;
;;; MatrixFunctions protocol implementation
;;;

(extend-protocol proto/MatrixFunctions
  FastMathBackend
  
  (matrix-exp [_ A]
    "Compute the matrix exponential exp(A)."
    (if (is-real-matrix? A)
      ;; Real matrix case - use eigendecomposition and FastMath exp
      (let [real-A (qmatrix->real-matrix A)
            eigenvals (fmat/eigenvalues real-A)  
            eigenvecs (fmat/eigenvectors real-A)
            exp-eigenvals (mapv #(Math/exp (fc/re %)) eigenvals)
            diag-exp (fmat/diagonal exp-eigenvals)
            result (fmat/mulm eigenvecs (fmat/mulm diag-exp (fmat/transpose eigenvecs)))]
        (real-matrix->qmatrix result))
      ;; Complex matrix case - implement via eigendecomposition
      (throw (ex-info "Complex matrix exponential not yet implemented" {}))))
  
  (matrix-log [_ A]
    "Compute the principal matrix logarithm log(A)."
    (if (is-real-matrix? A)
      ;; Real matrix case - use eigendecomposition and FastMath log
      (let [real-A (qmatrix->real-matrix A)
            eigenvals (fmat/eigenvalues real-A)
            eigenvecs (fmat/eigenvectors real-A)
            log-eigenvals (mapv #(Math/log (fc/re %)) eigenvals)
            diag-log (fmat/diagonal log-eigenvals)
            result (fmat/mulm eigenvecs (fmat/mulm diag-log (fmat/transpose eigenvecs)))]
        (real-matrix->qmatrix result))
      ;; Complex matrix case - implement via eigendecomposition  
      (throw (ex-info "Complex matrix logarithm not yet implemented" {}))))
  
  (matrix-sqrt [_ A]
    "Compute the principal matrix square root √A."
    (if (is-real-matrix? A)
      ;; Real matrix case - use eigendecomposition and FastMath sqrt
      (let [real-A (qmatrix->real-matrix A)
            eigenvals (fmat/eigenvalues real-A)
            eigenvecs (fmat/eigenvectors real-A)
            sqrt-eigenvals (mapv #(Math/sqrt (fc/re %)) eigenvals)
            diag-sqrt (fmat/diagonal sqrt-eigenvals)
            result (fmat/mulm eigenvecs (fmat/mulm diag-sqrt (fmat/transpose eigenvecs)))]
        (real-matrix->qmatrix result))
      ;; Complex matrix case - implement via eigendecomposition
      (throw (ex-info "Complex matrix square root not yet implemented" {})))))

;;;
;;; MatrixAnalysis protocol implementation
;;;

(extend-protocol proto/MatrixAnalysis
  FastMathBackend
  
  (spectral-norm [_ A]
    "Compute the spectral norm ||A||₂ (largest singular value)."
    (if (is-real-matrix? A)
      ;; Real matrix case - use FastMath SVD 
      (let [real-A (qmatrix->real-matrix A)
            singular-vals (fmat/singular-values real-A)]
        (first singular-vals))  ; Largest singular value
      ;; Complex matrix case - implement via eigendecomposition of A†A
      (let [A-conj-transpose (mapv (fn [j]
                                     (mapv (fn [i]
                                             (fc/conjugate (get-in A [i j])))
                                           (range (count A))))
                                   (range (count (first A))))
            ATA (proto/matrix-multiply nil A-conj-transpose A)
            ;; Get eigenvalues of A†A
            eigenvals-result (if (is-real-matrix? ATA)
                               (let [real-ATA (qmatrix->real-matrix ATA)]
                                 (map #(fc/complex % 0.0) (fmat/eigenvalues real-ATA)))
                               (throw (ex-info "Complex ATA eigenvalues not implemented" {})))]
        ;; Spectral norm is sqrt of largest eigenvalue of A†A
        (Math/sqrt (apply max (map fc/re eigenvals-result))))))
  
  (condition-number [_ A]
    "Compute the 2-norm condition number κ₂(A)."
    (if (is-real-matrix? A)
      ;; Real matrix case - use FastMath SVD
      (let [real-A (qmatrix->real-matrix A)
            singular-vals (fmat/singular-values real-A)]
        (/ (first singular-vals) (last singular-vals)))  ; σ_max / σ_min
      ;; Complex matrix case - implement via SVD of complex matrix
      (throw (ex-info "Complex matrix condition number not yet implemented" {})))))

;;;
;;; QuantumStateOps protocol implementation  
;;;

(extend-protocol proto/QuantumStateOps
  FastMathBackend
  
  (state-normalize [_ state]
    "Normalize a quantum state vector to unit norm."
    (let [norm-squared (reduce fc/add
                               (fc/complex 0.0 0.0)
                               (map (fn [si]
                                      (fc/mult (fc/conjugate si) si))
                                    state))
          norm (Math/sqrt (fc/re norm-squared))]
      (if (< norm 1e-12)
        state  ; Return unchanged if zero vector
        (mapv #(fc/mult % (fc/complex (/ 1.0 norm) 0.0)) state))))
  
  (projector-from-state [_ psi]
    "Create a projector matrix |ψ⟩⟨ψ| from a quantum state."
    (let [normalized-psi (proto/state-normalize _ psi)]
      (mapv (fn [psi-i]
              (mapv (fn [psi-j]
                      (fc/mult psi-i (fc/conjugate psi-j)))
                    normalized-psi))
            normalized-psi)))
  
  (density-matrix [backend psi]
    "Create a density matrix ρ for a pure quantum state."
    (proto/projector-from-state backend psi))
  
  (trace-one? 
    ([_ rho] (proto/trace-one? _ rho default-tolerance))
    ([_ rho eps]
     "Test if a matrix has trace equal to one (Tr(ρ) ≈ 1)."
     (let [tr (proto/trace _ rho)
           trace-value (fc/re tr)]  ; Extract real part
       (< (Math/abs (- trace-value 1.0)) eps)))))

;;;
;;; MatrixDecompositions protocol implementation  
;;;

(extend-protocol proto/MatrixDecompositions
  FastMathBackend
  
  (eigen-hermitian [_ A]
    "Compute eigenvalues and eigenvectors of a Hermitian matrix."
    (if (is-real-matrix? A)
      ;; Real symmetric case - use FastMath
      (let [real-A (qmatrix->real-matrix A)
            eigenvals (fmat/eigenvalues real-A)
            eigenvecs (fmat/eigenvectors real-A)]
        {:eigenvalues (mapv vec2->complex-map eigenvals)
         :eigenvectors (real-matrix->qmatrix eigenvecs)})
      ;; Complex Hermitian case - not implemented
      (throw (ex-info "Complex Hermitian eigendecomposition not yet implemented"
                      {:suggestion "Use real symmetric matrices when possible"}))))
  
  (eigen-general [_ A]
    "Compute eigenvalues and eigenvectors of a general matrix."
    (if (is-real-matrix? A)
      ;; Real matrix case - use Apache Commons Math
      (let [real-A (qmatrix->real-matrix A)]
        (try
          (let [eigen-decomp (org.apache.commons.math3.linear.EigenDecomposition. real-A)
                real-eigenvals (vec (.getRealEigenvalues eigen-decomp))
                imag-eigenvals (vec (.getImagEigenvalues eigen-decomp))
                eigenvecs-matrix (.getV eigen-decomp)]
            {:eigenvalues (mapv (fn [re im] (fc/complex re im)) real-eigenvals imag-eigenvals)
             :eigenvectors (real-matrix->qmatrix eigenvecs-matrix)})
          (catch Exception e
            (throw (ex-info "General eigendecomposition failed" {:original-error (.getMessage e)})))))
      ;; Complex matrix case - not implemented
      (throw (ex-info "Complex general eigendecomposition not yet implemented" {}))))
  
  (svd [_ A]
    "Compute Singular Value Decomposition A = U * S * V†."
    (if (is-real-matrix? A)
      ;; Real matrix case - use Apache Commons Math
      (let [real-A (qmatrix->real-matrix A)]
        (try
          (let [svd-decomp (org.apache.commons.math3.linear.SingularValueDecomposition. real-A)
                U (.getU svd-decomp)
                S (vec (.getSingularValues svd-decomp))
                V (.getV svd-decomp)]
            {:U (real-matrix->qmatrix U)
             :S (mapv #(fc/complex % 0.0) S)
             :V† (real-matrix->qmatrix (.transpose V))})
          (catch Exception e
            (throw (ex-info "SVD decomposition failed" {:original-error (.getMessage e)})))))
      ;; Complex matrix case
      (throw (ex-info "Complex SVD not yet implemented" {}))))
  
  (lu-decomposition [_ A]
    "Compute LU decomposition A = P * L * U."
    (if (is-real-matrix? A)
      ;; Real matrix case - use Apache Commons Math
      (let [real-A (qmatrix->real-matrix A)]
        (try
          (let [lu-decomp (org.apache.commons.math3.linear.LUDecomposition. real-A)
                L (.getL lu-decomp)
                U (.getU lu-decomp)
                P-matrix (.getP lu-decomp)]
            {:L (real-matrix->qmatrix L)
             :U (real-matrix->qmatrix U)
             :P (real-matrix->qmatrix P-matrix)})
          (catch Exception e
            (throw (ex-info "LU decomposition failed" {:original-error (.getMessage e)})))))
      ;; Complex matrix case
      (throw (ex-info "Complex LU decomposition not yet implemented" {}))))
  
  (qr-decomposition [_ A]  
    "Compute QR decomposition A = Q * R."
    (if (is-real-matrix? A)
      ;; Real matrix case - use Apache Commons Math
      (let [real-A (qmatrix->real-matrix A)]
        (try
          (let [qr-decomp (org.apache.commons.math3.linear.QRDecomposition. real-A)
                Q (.getQ qr-decomp)
                R (.getR qr-decomp)]
            {:Q (real-matrix->qmatrix Q)
             :R (real-matrix->qmatrix R)})
          (catch Exception e
            (throw (ex-info "QR decomposition failed" {:original-error (.getMessage e)})))))
      ;; Complex matrix case
      (throw (ex-info "Complex QR decomposition not yet implemented" {}))))
  
  (cholesky-decomposition [_ A]
    "Compute Cholesky decomposition A = L * L† for positive definite A."
    (if (is-real-matrix? A)
      ;; Real matrix case - use FastMath
      (let [real-A (qmatrix->real-matrix A)
            chol-L (fmat/cholesky real-A)]
        {:L (real-matrix->qmatrix chol-L)})
      ;; Complex matrix case
      (throw (ex-info "Complex Cholesky decomposition not yet implemented" {})))))

