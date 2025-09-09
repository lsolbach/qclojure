(ns org.soulspace.qclojure.domain.math.fastmath.backend
  "Backend for linear algebra operations for complex numbers using the FastMath library.
   
   This namespace provides an implementation of the matrix algebra protocols
   defined in `org.soulspace.qclojure.domain.math.protocols` using the FastMath
   library for complex number support. It includes conversion utilities between
   QClojure's internal representations and FastMath's representations, as well
   as implementations of matrix operations, decompositions, and functions.
   
   FastMath Complex Backend:
   - Conversion between QClojure and FastMath representations
   - Matrix algebra operations (addition, multiplication, inversion, etc.)
   - Matrix decompositions (eigenvalue, SVD, LU, QR, Cholesky)
   - Matrix functions (exponential, logarithm, square root)
   - Matrix analysis (spectral norm, condition number)"
  (:require [org.soulspace.qclojure.domain.math.fastmath.complex-linear-algebra :as fcla]
            [org.soulspace.qclojure.domain.math.protocols :as proto]
            [fastmath.complex :as fc]))

;;;
;;; Configuration and utilities
;;;
(def ^:const ^double default-tolerance 1.0e-12)

(defn tolerance* [backend]
  (double (or (:tolerance backend) default-tolerance)))

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

;;;
;;; FastMath Complex Backend
;;;
(defrecord FastMathComplexBackend [tolerance config])

;;;
;;; BackendAdapter protocol implementation
;;;
(extend-protocol proto/BackendAdapter
  FastMathComplexBackend
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
      ;; Vector of Vec2 - return as-is (Vec2 is the external format)
      (and (vector? v) (every? complex? v)) v

      ;; Real number vector - convert to Vec2 with zero imaginary parts
      (and (vector? v) (every? number? v))
      (mapv #(fc/complex (double %) 0.0) v)

      ;; Already in QClojure format - pass through
      (vector? v) v

      :else (throw (ex-info "Cannot convert from backend vector" {:value v}))))

  (matrix->backend [_ m]
    "Convert QClojure matrix to FastMath representation."
    (cond
      ;; Matrix of Vec2 complex numbers - pass through
      (and (vector? m) (every? #(and (vector? %) (every? complex? %)) m)) m

      ;; SoA format matrix - convert to Vec2
      (and (map? m) (contains? m :real) (contains? m :imag)
           (vector? (:real m)) (vector? (:imag m)))
      (let [real-part (:real m)
            imag-part (:imag m)]
        (mapv (fn [real-row imag-row]
                (mapv #(fc/complex %1 %2) real-row imag-row))
              real-part imag-part))

      ;; Complex matrix - convert to Vec2
      (complex-matrix? m) (mapv #(mapv complex-map->vec2 %) m)

      ;; Real matrix - convert to Vec2 with zero imaginary
      (and (vector? m) (every? #(and (vector? %) (every? number? %)) m))
      (mapv #(mapv (fn [x] (fc/complex x 0.0)) %) m)

      :else (throw (ex-info "Cannot convert to backend matrix" {:value m}))))

  (backend->matrix [_ m]
    "Convert FastMath matrix to QClojure representation."
    (cond
      ;; Matrix of Vec2 - return as-is (Vec2 is the external format)
      (and (vector? m) (every? #(and (vector? %) (every? complex? %)) m)) m

      ;; Already in QClojure format - pass through
      (vector? m) m

      :else (throw (ex-info "Cannot convert from backend matrix" {:value m}))))

  (scalar->backend [_ s]
    "Convert QClojure scalar to FastMath representation."
    (ensure-complex s))

  (backend->scalar [_ s]
    "Convert FastMath scalar to QClojure representation."
    (cond
      (complex? s) s  ; Return Vec2 as-is (Vec2 is the external format)
      (number? s) s
      :else (throw (ex-info "Cannot convert from backend scalar" {:value s})))))

;;;
;;; MatrixAlgebra protocol implementation
;;;
(extend-protocol proto/MatrixAlgebra
  FastMathComplexBackend

  (shape [_ A] (fcla/complex-matrix-shape A))
  (add [_ A B] (fcla/matrix-add A B))
  (subtract [_ A B] (fcla/matrix-subtract A B))
  (scale [_ A alpha] (fcla/matrix-scale A alpha))
  (negate [_ A] (fcla/matrix-negate A))
  (matrix-multiply [_ A B] (fcla/matrix-multiply A B))
  (matrix-vector-product [_ A x] (fcla/matrix-vector-product A x))
  (inner-product [_ x y] (fcla/inner-product x y))
  (outer-product [_ x y] (fcla/outer-product x y))
  (hadamard-product [_ A B] (fcla/hadamard-product A B))
  (kronecker-product [_ A B] (fcla/kronecker-product A B))
  (transpose [_ A] (fcla/transpose A))
  (conjugate-transpose [_ A] (fcla/conjugate-transpose A))
  (trace [_ A] (fcla/trace A))
  (norm2 [_ x] (fcla/norm2 x))
  (solve-linear-system [_ A b] (fcla/solve-linear A b))
  (inverse [_ A] (fcla/inverse A))
  (hermitian?
    ([backend A] (fcla/hermitian? A (tolerance* backend)))
    ([_ A eps] (fcla/hermitian? A eps)))
  (diagonal?
    ([backend A] (fcla/diagonal? A (tolerance* backend)))
    ([_ A eps] (fcla/diagonal? A eps)))
  (unitary?
    ([backend U] (fcla/unitary? U (tolerance* backend)))
    ([_ U eps] (fcla/unitary? U eps)))
  (positive-semidefinite?
    ([backend A] (fcla/positive-semidefinite? A (tolerance* backend)))
    ([_ A eps] (fcla/positive-semidefinite? A eps))))

;;;
;;; MatrixDecompositions protocol implementation  
;;;
(extend-protocol proto/MatrixDecompositions
  FastMathComplexBackend
  (eigen-hermitian [_ A] (fcla/eigen-hermitian A))
  (eigen-general [_ A] (fcla/eigen-general A))
  (svd [_ A] (fcla/svd A))
  (lu-decomposition [_ A] (fcla/lu-decomposition A))
  (qr-decomposition [_ A] (fcla/qr-decomposition A))
  (cholesky-decomposition
    ([backend A] (fcla/cholesky-decomposition A (tolerance* backend)))
    ([_ A eps] (fcla/cholesky-decomposition A eps))))

;;;
;;; MatrixFunctions protocol implementation
;;;
(extend-protocol proto/MatrixFunctions
  FastMathComplexBackend

  (matrix-exp [_ A] (fcla/matrix-exp A))
  (matrix-log [_ A] (fcla/matrix-log A))
  (matrix-sqrt [_ A] (fcla/matrix-sqrt A)))

;;;
;;; MatrixAnalysis protocol implementation
;;;
(extend-protocol proto/MatrixAnalysis
  FastMathComplexBackend

  (spectral-norm [_ A] (fcla/spectral-norm A))
  (condition-number [_ A] (fcla/condition-number A)))
