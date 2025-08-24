(ns org.soulspace.qclojure.domain.math.clojure.backend
  "Backend for Clojure complex numbers."
  (:require [org.soulspace.qclojure.domain.math.protocols :as proto]
            [org.soulspace.qclojure.domain.math.clojure.complex-linear-algebra :as ccla]
            [fastmath.complex :as fc]))

;;;
;;; Configuration and utilities
;;;
(def ^:const ^double default-tolerance 1.0e-12)

(defn tolerance* [backend]
  (double (or (:tolerance backend) (:tolerance (:config backend)) default-tolerance)))

;;;
;;; Clojure Math Complex Backend
;;;
(defrecord ClojureMathComplexBackend [tolerance config])

;;;
;;; BackendAdapter protocol implementation
;;;
(extend-protocol proto/BackendAdapter
  ClojureMathComplexBackend

  (vector->backend [_ v]
    (cond
      ;; Already in SoA format - validate structure
      (ccla/complex-vector? v) v

      ;; Regular Clojure vector of numbers  
      (and (vector? v) (every? number? v))
      {:real (mapv double v) :imag (mapv (constantly 0.0) v)}

      ;; Vector of FastMath Vec2 complex numbers
      (and (vector? v) (every? #(instance? fastmath.vector.Vec2 %) v))
      (try
        {:real (mapv #(double (fc/re %)) v)
         :imag (mapv #(double (fc/im %)) v)}
        (catch Exception e
          (throw (ex-info "Failed to convert Vec2 vector to SoA format"
                          {:input-vector v :error (.getMessage e)}))))

      ;; Empty vector
      (and (vector? v) (empty? v))
      {:real [] :imag []}

      ;; Nil input
      (nil? v)
      (throw (ex-info "Cannot convert nil to backend vector format" {:input v}))

      ;; Pass through other formats unchanged
      :else v))

  (backend->vector [_ v]
    (cond
      ;; SoA format - convert to Vec2 complex numbers
      (ccla/complex-vector? v)
      (let [real-part (:real v)
            imag-part (:imag v)]
        (when (not= (count real-part) (count imag-part))
          (throw (ex-info "SoA vector real and imaginary parts have different lengths"
                          {:real-length (count real-part)
                           :imag-length (count imag-part)
                           :input v})))
        (try
          (mapv #(fc/complex %1 %2) real-part imag-part)
          (catch Exception e
            (throw (ex-info "Failed to convert SoA to Vec2 vector"
                            {:input-vector v :error (.getMessage e)})))))

      ;; Real vector - convert to Vec2 with zero imaginary parts
      (and (vector? v) (every? number? v))
      (try
        (mapv #(fc/complex (double %) 0.0) v)
        (catch Exception e
          (throw (ex-info "Failed to convert real vector to Vec2 format"
                          {:input-vector v :error (.getMessage e)}))))

      ;; Empty vector
      (and (vector? v) (empty? v))
      []

      ;; Nil input
      (nil? v)
      (throw (ex-info "Cannot convert nil from backend vector format" {:input v}))

      ;; Pass through other formats unchanged
      :else v))

  (matrix->backend [_ m]
    (cond
      ;; Already in SoA format - validate structure
      (ccla/complex-matrix? m)
      (let [real-matrix (:real m)
            imag-matrix (:imag m)]
        (when (not= (count real-matrix) (count imag-matrix))
          (throw (ex-info "SoA matrix real and imaginary parts have different row counts"
                          {:real-rows (count real-matrix)
                           :imag-rows (count imag-matrix)
                           :input m})))
        (when (and (not-empty real-matrix) (not-empty imag-matrix))
          (let [real-cols (map count real-matrix)
                imag-cols (map count imag-matrix)]
            (when (not= real-cols imag-cols)
              (throw (ex-info "SoA matrix real and imaginary parts have different column structure"
                              {:real-cols real-cols
                               :imag-cols imag-cols
                               :input m})))))
        m)

      ;; Matrix of FastMath Vec2 complex numbers
      (and (vector? m) (every? vector? m)
           (every? #(every? (fn [x] (instance? fastmath.vector.Vec2 x)) %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        {:real (mapv (fn [row] (mapv #(double (fc/re %)) row)) m)
         :imag (mapv (fn [row] (mapv #(double (fc/im %)) row)) m)}
        (catch Exception e
          (throw (ex-info "Failed to convert Vec2 matrix to SoA format"
                          {:input-matrix m :error (.getMessage e)}))))

      ;; Matrix of real numbers
      (and (vector? m) (every? vector? m) (every? #(every? number? %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        {:real (mapv (fn [row] (mapv double row)) m)
         :imag (mapv (fn [row] (mapv (constantly 0.0) row)) m)}
        (catch Exception e
          (throw (ex-info "Failed to convert real matrix to SoA format"
                          {:input-matrix m :error (.getMessage e)}))))

      ;; Empty matrix
      (and (vector? m) (empty? m))
      {:real [] :imag []}

      ;; Nil input
      (nil? m)
      (throw (ex-info "Cannot convert nil to backend matrix format" {:input m}))

      ;; Pass through other formats unchanged
      :else m))

  (backend->matrix [_ m]
    (cond
      ;; SoA format - convert to Vec2 complex numbers
      (ccla/complex-matrix? m)
      (let [real-matrix (:real m)
            imag-matrix (:imag m)]
        (when (not= (count real-matrix) (count imag-matrix))
          (throw (ex-info "SoA matrix real and imaginary parts have different row counts"
                          {:real-rows (count real-matrix)
                           :imag-rows (count imag-matrix)
                           :input m})))
        (when (and (not-empty real-matrix) (not-empty imag-matrix))
          (let [real-cols (map count real-matrix)
                imag-cols (map count imag-matrix)]
            (when (not= real-cols imag-cols)
              (throw (ex-info "SoA matrix real and imaginary parts have different column structure"
                              {:real-cols real-cols
                               :imag-cols imag-cols
                               :input m})))))
        (try
          (mapv (fn [real-row imag-row]
                  (mapv #(fc/complex %1 %2) real-row imag-row))
                real-matrix imag-matrix)
          (catch Exception e
            (throw (ex-info "Failed to convert SoA to Vec2 matrix"
                            {:input-matrix m :error (.getMessage e)})))))

      ;; Real matrix - convert to Vec2 with zero imaginary parts
      (and (vector? m) (every? vector? m) (every? #(every? number? %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        (mapv (fn [row] (mapv #(fc/complex (double %) 0.0) row)) m)
        (catch Exception e
          (throw (ex-info "Failed to convert real matrix to Vec2 format"
                          {:input-matrix m :error (.getMessage e)}))))

      ;; Empty matrix
      (and (vector? m) (empty? m))
      []

      ;; Nil input
      (nil? m)
      (throw (ex-info "Cannot convert nil from backend matrix format" {:input m}))

      ;; Pass through other formats unchanged
      :else m))

  (scalar->backend [_ s]
    (cond
      ;; Vec2 complex scalar
      (instance? fastmath.vector.Vec2 s)
      {:real (fc/re s) :imag (fc/im s)}

      ;; Real scalar
      (number? s)
      {:real (double s) :imag 0.0}

      ;; Already in SoA format
      (ccla/complex-scalar? s)
      s

      ;; Nil input
      (nil? s)
      (throw (ex-info "Cannot convert nil to backend scalar format" {:input s}))

      ;; Invalid input
      :else
      (throw (ex-info "Cannot convert to backend scalar format"
                      {:input s :type (type s)}))))

  (backend->scalar [_ s]
    (cond
      ;; SoA scalar format
      (ccla/complex-scalar? s)
      (try
        (fc/complex (:real s) (:imag s))
        (catch Exception e
          (throw (ex-info "Failed to convert SoA scalar to Vec2 format"
                          {:input-scalar s :error (.getMessage e)}))))

      ;; Real scalar
      (number? s)
      (fc/complex (double s) 0.0)

      ;; Already Vec2 format
      (instance? fastmath.vector.Vec2 s)
      s

      ;; Nil input
      (nil? s)
      (throw (ex-info "Cannot convert nil from backend scalar format" {:input s}))

      ;; Invalid input
      :else
      (throw (ex-info "Cannot convert from backend scalar format"
                      {:input s :type (type s)})))))

;;;
;;; MatrixAlgebra protocol implementation
;;;
(extend-protocol proto/MatrixAlgebra
  ClojureMathComplexBackend
  (shape [_ A] (ccla/matrix-shape A))
  (add [_ A B] (ccla/matrix-add A B))
  (subtract [_ A B] (ccla/matrix-subtract A B))
  (scale [_ A alpha] (ccla/matrix-scale A alpha))
  (negate [A] (ccla/matrix-scale  A -1.0))
  (matrix-multiply [_ A B] (ccla/matrix-multiply A B))
  (matrix-vector-product [_ A x] (ccla/matrix-vector-product A x))
  (inner-product [_ x y] (ccla/inner-product x y))
  (outer-product [_ x y] (ccla/outer-product x y))
  (hadamard-product [_ A B] (ccla/hadamard-product A B))
  (kronecker-product [_ A B] (ccla/kronecker-product A B))
  (transpose [_ A] (ccla/transpose A))
  (conjugate-transpose [_ A] (ccla/conjugate-transpose A))
  (trace [_ A] (ccla/trace A))
  (norm2 [_ x] (ccla/norm2 x))
  (solve-linear-system [_ A b] (ccla/solve-linear A b)) 
  (inverse [_ A] (ccla/inverse A)) 
  (hermitian?
    ([b A] (ccla/hermitian? A (tolerance* b)))
    ([_ A eps] (ccla/hermitian? A eps)))
  (unitary?
    ([b U] (ccla/unitary? U (tolerance* b)))
    ([_ U eps] (ccla/unitary? U eps)))
  (positive-semidefinite?
   ([b A] (ccla/positive-semidefinite? A (tolerance* b)))
   ([_ A eps] (ccla/positive-semidefinite? A eps)))
  )

;;;
;;; MatrixDecompositions protocol implementation  
;;;
(extend-protocol proto/MatrixDecompositions
  ClojureMathComplexBackend
  (eigen-hermitian [_ A] (ccla/eigen-hermitian A default-tolerance))
  (eigen-general [_ A] (ccla/eigen-general A default-tolerance))
  (svd [_ A] (ccla/svd A default-tolerance))
  (lu-decomposition [_ A] (ccla/lu-decomposition A default-tolerance))
  (qr-decomposition [_ A] (ccla/qr-decomposition A))
  (cholesky-decomposition [_ A] (ccla/cholesky-decomposition A)))

;;;
;;; MatrixFunctions protocol implementation
;;;
(extend-protocol proto/MatrixFunctions
  ClojureMathComplexBackend
  (matrix-exp [_ A] (ccla/matrix-exp A))
  (matrix-log [_ A] (ccla/matrix-log A))
  (matrix-sqrt [_ A] (ccla/matrix-sqrt A)))

;;;
;;; MatrixAnalysis protocol implementation
;;;
(extend-protocol proto/MatrixAnalysis
  ClojureMathComplexBackend
  (spectral-norm [_ A] (ccla/spectral-norm A))
  (condition-number [_ A] (ccla/condition-number A)))

;;
;; QuantumStateOps protocol implementation
;;
(extend-protocol proto/QuantumStateOps
  ClojureMathComplexBackend
  (state-normalize [_ state] (ccla/state-normalize state))
  (projector-from-state [_ psi] (ccla/projector-from-state psi))
  (density-matrix [_ psi] (ccla/density-matrix psi))
  (trace-one?
    ([b rho] (ccla/trace-one? rho (tolerance* b)))
    ([_ rho eps] (ccla/trace-one? rho eps))))

