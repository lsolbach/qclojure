(ns org.soulspace.qclojure.domain.math.neanderthal.neanderthal
  "High-performance math backend powered by Neanderthal (CPU-first, GPU-ready).

  This backend implements the math protocols using Neanderthal's BLAS/LAPACK
  operations for dense real linear algebra and SoA (Split-of-Arrays) representation
  for complex operations. For real matrices, it leverages high-performance native
  BLAS/LAPACK through Neanderthal. For complex matrices, it uses SoA representation
  and performs operations on the real and imaginary parts separately.

  Performance characteristics:
  - Real operations: 10-100x faster than pure Clojure (via BLAS/LAPACK)
  - Complex operations: SoA approach with Neanderthal acceleration on components
  - Memory efficient: Uses Neanderthal's native memory management
  - Thread-safe: Neanderthal operations are thread-safe
  
  Data representation:
  - Real matrices: Neanderthal GE (General) matrices
  - Real vectors: Neanderthal dense vectors
  - Complex matrices: {:real neanderthal-matrix :imag neanderthal-matrix}
  - Complex vectors: {:real neanderthal-vector :imag neanderthal-vector}
  - Complex scalars: {:real double :imag double}"
  (:require
   [org.soulspace.qclojure.domain.math.protocols :as proto]
   [org.soulspace.qclojure.domain.math.clojure.clojure-math :as cm]
   [uncomplicate.neanderthal.core :as nc]
   [uncomplicate.neanderthal.native :as nn]
   [uncomplicate.neanderthal.linalg :as la]
   [uncomplicate.neanderthal.math :as nm]))

;;
;; Configuration and defaults
;;
(def ^:const ^double default-tolerance 1.0e-12)

(defn tolerance*
  "Extract tolerance from backend configuration."
  [backend]
  (double (or (:tolerance backend) (:tolerance (:config backend)) default-tolerance)))

;;
;; Complex number helpers and predicates
;;
(defn complex-scalar?
  "Test if x is a complex scalar (map with :real and :imag keys)."
  [x]
  (and (map? x) (contains? x :real) (contains? x :imag) 
       (number? (:real x)) (number? (:imag x))))

(defn complex-vector?
  "Test if v is a complex vector (SoA with Neanderthal vectors)."
  [v]
  (and (map? v) (contains? v :real) (contains? v :imag)
       (nc/vctr? (:real v)) (nc/vctr? (:imag v))
       (= (nc/dim (:real v)) (nc/dim (:imag v)))))

(defn complex-matrix?
  "Test if m is a complex matrix (SoA with Neanderthal matrices)."
  [m]
  (and (map? m) (contains? m :real) (contains? m :imag)
       (nc/matrix? (:real m)) (nc/matrix? (:imag m))
       (= (nc/mrows (:real m)) (nc/mrows (:imag m)))
       (= (nc/ncols (:real m)) (nc/ncols (:imag m)))))

(defn make-complex-scalar
  "Create a complex scalar from real and imaginary parts."
  [r i]
  {:real (double r) :imag (double i)})

(defn ensure-complex-scalar
  "Ensure x is a complex scalar, promoting real numbers if needed."
  [x]
  (cond 
    (complex-scalar? x) x
    (number? x) (make-complex-scalar x 0.0)
    :else (throw (ex-info "Cannot convert to complex scalar" {:value x}))))

(defn ensure-complex-vector
  "Ensure v is a complex vector, promoting real vectors if needed."
  [v]
  (cond 
    (complex-vector? v) v
    (nc/vctr? v) {:real v :imag (nc/zero v)}
    (vector? v) {:real (nn/dv v) :imag (nc/zero (nn/dv v))}
    :else (throw (ex-info "Cannot convert to complex vector" {:value v}))))

(defn ensure-complex-matrix
  "Ensure m is a complex matrix, promoting real matrices if needed."
  [m]
  (cond 
    (complex-matrix? m) m
    (nc/matrix? m) {:real m :imag (nc/zero m)}
    (vector? m) (let [nm (nn/dge m)]
                  {:real nm :imag (nc/zero nm)})
    :else (throw (ex-info "Cannot convert to complex matrix" {:value m}))))

;;
;; Conversion helpers between Clojure and Neanderthal representations
;;
(defn clj-matrix->neanderthal
  "Convert Clojure matrix (vector of vectors) to Neanderthal matrix."
  [m]
  (if (vector? m)
    (nn/dge m)
    m))

(defn clj-vector->neanderthal
  "Convert Clojure vector to Neanderthal vector."
  [v]
  (if (vector? v)
    (nn/dv v)
    v))

(defn neanderthal-matrix->clj
  "Convert Neanderthal matrix to Clojure matrix (vector of vectors)."
  [m]
  (if (nc/matrix? m)
    (vec (for [i (range (nc/mrows m))]
           (vec (for [j (range (nc/ncols m))]
                  (nc/entry m i j)))))
    m))

(defn neanderthal-vector->clj
  "Convert Neanderthal vector to Clojure vector."
  [v]
  (if (nc/vctr? v)
    (vec (for [i (range (nc/dim v))]
           (nc/entry v i)))
    v))

;;
;; Complex arithmetic on SoA representations
;;
(defn complex-add-soa
  "Add two complex matrices/vectors in SoA representation."
  [A B]
  {:real (nc/axpy 1.0 (:real A) (nc/copy (:real B)))
   :imag (nc/axpy 1.0 (:imag A) (nc/copy (:imag B)))})

(defn complex-sub-soa
  "Subtract two complex matrices/vectors in SoA representation."
  [A B]
  {:real (nc/axpy -1.0 (:real B) (nc/copy (:real A)))
   :imag (nc/axpy -1.0 (:imag B) (nc/copy (:imag A)))})

(defn complex-scale-soa
  "Scale a complex matrix/vector by a complex scalar."
  [A alpha]
  (if (complex-scalar? alpha)
    (let [ar (:real alpha) ai (:imag alpha)
          Ar (:real A) Ai (:imag A)]
      {:real (nc/axpy (- ai) Ai (nc/scal ar (nc/copy Ar)))
       :imag (nc/axpy ar Ai (nc/scal ai (nc/copy Ar)))})
    {:real (nc/scal alpha (nc/copy (:real A)))
     :imag (nc/scal alpha (nc/copy (:imag A)))}))

(defn complex-mm-soa
  "Complex matrix multiplication using SoA representation: (Ar + i*Ai) * (Br + i*Bi)"
  [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)]
    {:real (nc/axpy -1.0 (nc/mm Ai Bi) (nc/mm Ar Br))
     :imag (nc/axpy 1.0 (nc/mm Ai Br) (nc/mm Ar Bi))}))

(defn complex-mv-soa
  "Complex matrix-vector multiplication using SoA representation."
  [A x]
  (let [Ar (:real A) Ai (:imag A) xr (:real x) xi (:imag x)]
    {:real (nc/axpy -1.0 (nc/mv Ai xi) (nc/mv Ar xr))
     :imag (nc/axpy 1.0 (nc/mv Ai xr) (nc/mv Ar xi))}))

(defn complex-outer-soa
  "Complex outer product x ⊗ y† using SoA representation."
  [x y]
  (let [xr (:real x) xi (:imag x) yr (:real y) yi (:imag y)]
    {:real (nc/axpy 1.0 (nc/rk xr yr) (nc/rk xi yi))
     :imag (nc/axpy -1.0 (nc/rk xi yr) (nc/rk xr yi))}))

(defn complex-transpose-soa
  "Transpose of complex matrix in SoA representation."
  [A]
  {:real (nc/trans (:real A))
   :imag (nc/trans (:imag A))})

(defn complex-conj-transpose-soa
  "Conjugate transpose of complex matrix in SoA representation."
  [A]
  {:real (nc/trans (:real A))
   :imag (nc/scal -1.0 (nc/trans (:imag A)))})

;;
;; Backend record definition
;;
(defrecord NeanderthalBackend [tolerance config])

;;
;; Complex scalar protocol implementation
;;
(extend-protocol proto/Complex
  Number
  (real [x] (double x))
  (imag [_] 0.0)
  (conjugate [x] x)
  (complex? [_] false)

  clojure.lang.IPersistentMap
  (real [m] 
    (if (contains? m :real) 
      (double (:real m)) 
      (throw (ex-info "Not a complex map" {:value m}))))
  (imag [m] 
    (if (contains? m :imag) 
      (double (:imag m)) 
      0.0))
  (conjugate [m] 
    (if (and (contains? m :real) (contains? m :imag)) 
      (update m :imag #(- (double %))) 
      m))
  (complex? [m] 
    (and (contains? m :real) (contains? m :imag))))

;;
;; BackendAdapter - handles conversion between representations
;;
(extend-protocol proto/BackendAdapter
  NeanderthalBackend
  
  (vector->backend [_ v]
    "Convert QClojure vector to Neanderthal representation."
    (cond
      (complex-vector? v) v  ; Already in SoA form
      (nc/vctr? v) v       ; Already Neanderthal vector
      (vector? v) (nn/dv v)  ; Convert Clojure vector to Neanderthal
      :else (throw (ex-info "Unsupported vector type" {:value v}))))

  (backend->vector [_ v]
    "Convert Neanderthal vector to QClojure representation."
    (cond
      (complex-vector? v) {:real (neanderthal-vector->clj (:real v))
                           :imag (neanderthal-vector->clj (:imag v))}
      (nc/vctr? v) (neanderthal-vector->clj v)
      :else v))

  (matrix->backend [_ m]
    "Convert QClojure matrix to Neanderthal representation."
    (cond
      (complex-matrix? m) m  ; Already in SoA form
      (nc/matrix? m) m       ; Already Neanderthal matrix
      (vector? m) (nn/dge m) ; Convert Clojure matrix to Neanderthal
      :else (throw (ex-info "Unsupported matrix type" {:value m}))))

  (backend->matrix [_ m]
    "Convert Neanderthal matrix to QClojure representation."
    (cond
      (complex-matrix? m) {:real (neanderthal-matrix->clj (:real m))
                           :imag (neanderthal-matrix->clj (:imag m))}
      (nc/matrix? m) (neanderthal-matrix->clj m)
      :else m)))

;;
;; MatrixAlgebra protocol implementation
;;
(extend-protocol proto/MatrixAlgebra
  NeanderthalBackend

  (shape [_ A]
    "Return matrix dimensions [rows cols]."
    (cond
      (complex-matrix? A) [(nc/mrows (:real A)) (nc/ncols (:real A))]
      (nc/matrix? A) [(nc/mrows A) (nc/ncols A)]
      (vector? A) [(count A) (count (first A))]
      :else (throw (ex-info "Unsupported matrix type for shape" {:value A}))))

  (add [_ A B]
    "Matrix addition A + B."
    (cond
      (and (complex-matrix? A) (complex-matrix? B)) 
      (complex-add-soa A B)
      
      (complex-matrix? A) 
      (complex-add-soa A (ensure-complex-matrix B))
      
      (complex-matrix? B) 
      (complex-add-soa (ensure-complex-matrix A) B)
      
      (and (nc/matrix? A) (nc/matrix? B))
      (nc/axpy 1.0 A (nc/copy B))
      
      :else 
      (let [nA (clj-matrix->neanderthal A)
            nB (clj-matrix->neanderthal B)]
        (neanderthal-matrix->clj (nc/axpy 1.0 nA (nc/copy nB))))))

  (subtract [_ A B]
    "Matrix subtraction A - B."
    (cond
      (and (complex-matrix? A) (complex-matrix? B)) 
      (complex-sub-soa A B)
      
      (complex-matrix? A) 
      (complex-sub-soa A (ensure-complex-matrix B))
      
      (complex-matrix? B) 
      (complex-sub-soa (ensure-complex-matrix A) B)
      
      (and (nc/matrix? A) (nc/matrix? B))
      (nc/axpy -1.0 B (nc/copy A))
      
      :else 
      (let [nA (clj-matrix->neanderthal A)
            nB (clj-matrix->neanderthal B)]
        (neanderthal-matrix->clj (nc/axpy -1.0 nB (nc/copy nA))))))

  (scale [_ A alpha]
    "Scalar multiplication alpha * A."
    (cond
      (complex-matrix? A) 
      (complex-scale-soa A alpha)
      
      (complex-scalar? alpha)
      (complex-scale-soa (ensure-complex-matrix A) alpha)
      
      (nc/matrix? A)
      (nc/scal alpha (nc/copy A))
      
      :else 
      (let [nA (clj-matrix->neanderthal A)]
        (neanderthal-matrix->clj (nc/scal alpha (nc/copy nA))))))

  (negate [backend A] 
    "Additive inverse -A."
    (proto/scale backend A -1.0))

  (matrix-multiply [_ A B]
    "Matrix multiplication A × B."
    (cond
      (and (complex-matrix? A) (complex-matrix? B)) 
      (complex-mm-soa A B)
      
      (complex-matrix? A) 
      (complex-mm-soa A (ensure-complex-matrix B))
      
      (complex-matrix? B) 
      (complex-mm-soa (ensure-complex-matrix A) B)
      
      (and (nc/matrix? A) (nc/matrix? B))
      (nc/mm A B)
      
      :else 
      (let [nA (clj-matrix->neanderthal A)
            nB (clj-matrix->neanderthal B)]
        (neanderthal-matrix->clj (nc/mm nA nB)))))

  (matrix-vector-product [_ A x]
    "Matrix-vector multiplication A × x."
    (cond
      (and (complex-matrix? A) (complex-vector? x)) 
      (complex-mv-soa A x)
      
      (complex-matrix? A) 
      (complex-mv-soa A (ensure-complex-vector x))
      
      (complex-vector? x) 
      (complex-mv-soa (ensure-complex-matrix A) x)
      
      (and (nc/matrix? A) (nc/vctr? x))
      (nc/mv A x)
      
      :else 
      (let [nA (clj-matrix->neanderthal A)
            nx (clj-vector->neanderthal x)]
        (neanderthal-vector->clj (nc/mv nA nx)))))

  (outer-product [_ x y]
    "Outer product x ⊗ y†."
    (cond
      (and (complex-vector? x) (complex-vector? y)) 
      (complex-outer-soa x y)
      
      (complex-vector? x) 
      (complex-outer-soa x (ensure-complex-vector y))
      
      (complex-vector? y) 
      (complex-outer-soa (ensure-complex-vector x) y)
      
      (and (nc/vctr? x) (nc/vctr? y))
      (nc/rk x y)
      
      :else 
      (let [nx (clj-vector->neanderthal x)
            ny (clj-vector->neanderthal y)]
        (neanderthal-matrix->clj (nc/rk nx ny)))))

  (hadamard [_ A B]
    "Element-wise (Hadamard) product A ⊙ B."
    ;; Neanderthal doesn't have direct Hadamard product, fall back to clojure-math
    (cond
      (and (complex-matrix? A) (complex-matrix? B))
      (let [Ar-clj (neanderthal-matrix->clj (:real A))
            Ai-clj (neanderthal-matrix->clj (:imag A))
            Br-clj (neanderthal-matrix->clj (:real B))
            Bi-clj (neanderthal-matrix->clj (:imag B))
            result-r (mapv (fn [ra ia rb ib]
                             (mapv (fn [a i b j] (- (* a b) (* i j))) ra ia rb ib)) 
                           Ar-clj Ai-clj Br-clj Bi-clj)
            result-i (mapv (fn [ra ia rb ib]
                             (mapv (fn [a i b j] (+ (* a j) (* i b))) ra ia rb ib)) 
                           Ar-clj Ai-clj Br-clj Bi-clj)]
        {:real (nn/dge result-r) :imag (nn/dge result-i)})
      
      :else
      ;; For real matrices, fall back to element-wise multiplication
      (let [A-clj (if (nc/matrix? A) (neanderthal-matrix->clj A) A)
            B-clj (if (nc/matrix? B) (neanderthal-matrix->clj B) B)
            result (mapv (fn [ra rb] 
                           (mapv #(* (double %1) (double %2)) ra rb)) A-clj B-clj)]
        (if (or (nc/matrix? A) (nc/matrix? B))
          (nn/dge result)
          result))))

  (kronecker [_ A B]
    "Kronecker (tensor) product A ⊗ B."
    ;; Complex implementation using SoA
    (cond
      (and (complex-matrix? A) (complex-matrix? B))
      (let [Ar (neanderthal-matrix->clj (:real A))
            Ai (neanderthal-matrix->clj (:imag A))
            Br (neanderthal-matrix->clj (:real B))
            Bi (neanderthal-matrix->clj (:imag B))
            [ar ac] [(count Ar) (count (first Ar))]
            [br bc] [(count Br) (count (first Br))]
            result-r (vec (for [i (range ar) bi (range br)]
                            (vec (for [j (range ac) bj (range bc)]
                                   (- (* (get-in Ar [i j]) (get-in Br [bi bj]))
                                      (* (get-in Ai [i j]) (get-in Bi [bi bj])))))))
            result-i (vec (for [i (range ar) bi (range br)]
                            (vec (for [j (range ac) bj (range bc)]
                                   (+ (* (get-in Ar [i j]) (get-in Bi [bi bj]))
                                      (* (get-in Ai [i j]) (get-in Br [bi bj])))))))]
        {:real (nn/dge result-r) :imag (nn/dge result-i)})
      
      :else
      ;; Real Kronecker product
      (let [A-clj (if (nc/matrix? A) (neanderthal-matrix->clj A) A)
            B-clj (if (nc/matrix? B) (neanderthal-matrix->clj B) B)
            [ar ac] [(count A-clj) (count (first A-clj))]
            [br bc] [(count B-clj) (count (first B-clj))]
            result (vec (for [i (range ar) bi (range br)]
                          (vec (for [j (range ac) bj (range bc)]
                                 (* (double (get-in A-clj [i j])) 
                                    (double (get-in B-clj [bi bj])))))))]
        (if (or (nc/matrix? A) (nc/matrix? B))
          (nn/dge result)
          result))))

  (transpose [_ A]
    "Matrix transpose Aᵀ."
    (cond
      (complex-matrix? A) 
      (complex-transpose-soa A)
      
      (nc/matrix? A)
      (nc/trans A)
      
      :else 
      (let [nA (clj-matrix->neanderthal A)]
        (neanderthal-matrix->clj (nc/trans nA)))))

  (conjugate-transpose [_ A]
    "Conjugate transpose Aᴴ."
    (cond
      (complex-matrix? A) 
      (complex-conj-transpose-soa A)
      
      (nc/matrix? A)
      (nc/trans A)  ; For real matrices, conjugate transpose = transpose
      
      :else 
      (let [nA (clj-matrix->neanderthal A)]
        (neanderthal-matrix->clj (nc/trans nA)))))

  (trace [_ A]
    "Matrix trace Tr(A)."
    (cond
      (complex-matrix? A)
      (let [n (nc/mrows (:real A))
            tr-real (reduce + (for [i (range n)] (nc/entry (:real A) i i)))
            tr-imag (reduce + (for [i (range n)] (nc/entry (:imag A) i i)))]
        (make-complex-scalar tr-real tr-imag))
      
      (nc/matrix? A)
      (let [n (nc/mrows A)]
        (reduce + (for [i (range n)] (nc/entry A i i))))
      
      :else
      (let [nA (clj-matrix->neanderthal A)
            n (nc/mrows nA)]
        (reduce + (for [i (range n)] (nc/entry nA i i))))))

  (inner-product [_ x y]
    "Vector inner product ⟨x|y⟩."
    (cond
      (and (complex-vector? x) (complex-vector? y))
      (let [xr (:real x) xi (:imag x) yr (:real y) yi (:imag y)
            re (+ (nc/dot xr yr) (nc/dot xi yi))
            im (- (nc/dot xr yi) (nc/dot xi yr))]
        (make-complex-scalar re im))
      
      (and (nc/vctr? x) (nc/vctr? y))
      (nc/dot x y)
      
      :else 
      (let [nx (clj-vector->neanderthal x)
            ny (clj-vector->neanderthal y)]
        (nc/dot nx ny))))

  (norm2 [backend x]
    "Euclidean norm ||x||₂."
    (let [ip (proto/inner-product backend x x)]
      (if (complex-scalar? ip)
        (Math/sqrt (:real ip))  ; For valid inner product, imaginary part should be ~0
        (Math/sqrt ip))))

  (solve-linear-system [_ A b]
    "Solve linear system Ax = b."
    ;; For now, delegate complex case to clojure-math backend
    ;; Real case uses Neanderthal's high-performance solvers
    (cond
      (or (complex-matrix? A) (complex-vector? b))
      ;; Fall back to clojure-math for complex systems
      (let [clj-backend (cm/make-backend)
            A-clj (if (complex-matrix? A) 
                    {:real (neanderthal-matrix->clj (:real A))
                     :imag (neanderthal-matrix->clj (:imag A))}
                    (neanderthal-matrix->clj A))
            b-clj (if (complex-vector? b)
                    {:real (neanderthal-vector->clj (:real b))
                     :imag (neanderthal-vector->clj (:imag b))}
                    (neanderthal-vector->clj b))]
        (proto/solve-linear-system clj-backend A-clj b-clj))
      
      :else
      ;; Real case: use Neanderthal's optimized solvers
      (try
        (let [nA (if (nc/matrix? A) A (clj-matrix->neanderthal A))
              nb (if (nc/vctr? b) b (clj-vector->neanderthal b))
              result (la/sv nA nb)]
          (if (or (nc/matrix? A) (nc/vctr? b))
            result
            (neanderthal-vector->clj result)))
        (catch Exception e
          (throw (ex-info "Linear system solve failed" {:error (.getMessage e)}))))))

  (inverse [_ A]
    "Matrix inverse A⁻¹."
    (cond
      (complex-matrix? A)
      ;; Fall back to clojure-math for complex matrices
      (let [clj-backend (cm/make-backend)
            A-clj {:real (neanderthal-matrix->clj (:real A))
                   :imag (neanderthal-matrix->clj (:imag A))}
            result (proto/inverse clj-backend A-clj)]
        (if result
          {:real (nn/dge (:real result))
           :imag (nn/dge (:imag result))}
          nil))
      
      :else
      ;; Real case: use Neanderthal
      (try
        (let [nA (if (nc/matrix? A) A (clj-matrix->neanderthal A))
              result (la/tri nA)]
          (if (nc/matrix? A)
            result
            (neanderthal-matrix->clj result)))
        (catch Exception e
          nil))))  ; Return nil for singular matrices

  (hermitian? 
    ([backend A] (proto/hermitian? backend A (tolerance* backend)))
    ([_ A eps]
     ;; Check if A ≈ Aᴴ within tolerance
     (cond
       (complex-matrix? A)
       (let [Ah (complex-conj-transpose-soa A)
             diff-r (nc/axpy -1.0 (:real Ah) (nc/copy (:real A)))
             diff-i (nc/axpy -1.0 (:imag Ah) (nc/copy (:imag A)))
             norm-diff (Math/sqrt (+ (Math/pow (nc/nrm2 diff-r) 2)
                                     (Math/pow (nc/nrm2 diff-i) 2)))]
         (< norm-diff eps))
       
       :else
       (let [nA (if (nc/matrix? A) A (clj-matrix->neanderthal A))
             At (nc/trans nA)
             diff (nc/axpy -1.0 At (nc/copy nA))
             norm-diff (nc/nrm2 diff)]
         (< norm-diff eps)))))

  (unitary? 
    ([backend U] (proto/unitary? backend U (tolerance* backend)))
    ([backend U eps]
     ;; Check if Uᴴ U ≈ I within tolerance
     (let [Uh (proto/conjugate-transpose backend U)
           UhU (proto/matrix-multiply backend Uh U)
           [n _] (proto/shape backend U)]
       (cond
         (complex-matrix? UhU)
         (let [I-real (nn/dge (vec (for [i (range n)] 
                                     (vec (for [j (range n)] 
                                            (if (= i j) 1.0 0.0))))))
               I-imag (nc/zero I-real)
               diff-r (nc/axpy -1.0 I-real (nc/copy (:real UhU)))
               diff-i (nc/axpy -1.0 I-imag (nc/copy (:imag UhU)))
               norm-diff (Math/sqrt (+ (Math/pow (nc/nrm2 diff-r) 2)
                                       (Math/pow (nc/nrm2 diff-i) 2)))]
           (< norm-diff eps))
         
         :else
         (let [I (nn/dge (vec (for [i (range n)] 
                                (vec (for [j (range n)] 
                                       (if (= i j) 1.0 0.0))))))
               diff (nc/axpy -1.0 I (nc/copy UhU))
               norm-diff (nc/nrm2 diff)]
           (< norm-diff eps))))))

  (positive-semidefinite? [backend A]
    "Test if matrix is positive semidefinite."
    ;; For now, delegate to clojure-math backend for eigenvalue computation
    (let [clj-backend (cm/make-backend)]
      (cond
        (complex-matrix? A)
        (let [A-clj {:real (neanderthal-matrix->clj (:real A))
                     :imag (neanderthal-matrix->clj (:imag A))}]
          (proto/positive-semidefinite? clj-backend A-clj))
        
        :else
        (let [A-clj (if (nc/matrix? A) (neanderthal-matrix->clj A) A)]
          (proto/positive-semidefinite? clj-backend A-clj))))))

;;
;; Factory function
;;
(defn make-backend
  "Create a new Neanderthal math backend.
  
  Options:
  - :tolerance - Numeric tolerance for predicates (default 1e-12)
  - :config - Additional configuration map"
  ([] (->NeanderthalBackend default-tolerance {:tolerance default-tolerance}))
  ([{:keys [tolerance] :as opts}]
   (->NeanderthalBackend 
    (or tolerance default-tolerance)
    (merge {:tolerance (or tolerance default-tolerance)} 
           (dissoc opts :tolerance)))))
