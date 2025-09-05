
(ns org.soulspace.qclojure.domain.math.complex-linear-algebra
  "Facade for linear algebra operations with pluggable backends.
  
   Goals:
    - Provide a single, meaningful API for matrix and vector operations.
    - Allow swapping implementations (pure Clojure Math, Fastmath, Neanderthal) without changing call sites.
    - Keep data shapes simple: matrices as vectors of row vectors; vectors as vectors of numbers.
    - Handle conversions between FastMath Vec2 format and backend-specific representations.
    
    Default backend: :pure, implemented via org.soulspace.qclojure.domain.math.clojure-math.
    
    Use `set-backend!` to change the global backend, or `with-backend` to override in a dynamic scope."
  (:require
   [org.soulspace.qclojure.domain.math.protocols :as proto]
   [org.soulspace.qclojure.domain.math.complex :as complex]
   [org.soulspace.qclojure.domain.math.clojure.backend :as cmath]
   [org.soulspace.qclojure.domain.math.fastmath.backend :as fmath]
   [fastmath.complex :as fc]))

;; Unified tolerance (can be overridden via backend config :tolerance). Defined after backend vars.
(def ^:dynamic *tolerance* 1.0e-12)

;; Public accessor for current numeric tolerance (backend or dynamic override)
(defn current-tolerance
  "Return the effective tolerance used for approximate numeric comparisons.
  
  This reads the dynamic var *tolerance*. Backend specific overrides (if any)
  should bind *tolerance* when performing computations so tests and client
  code can query a single source of truth.
  
  Returns:
  Current tolerance value as a double"
  [] *tolerance*)

;;;
;;; Backend selection
;;;
;; Backend registry: map key -> constructor (opts -> instance)
(def ^:private backend-constructors
  {:pure (fn [opts]
           (let [tol (or (:tolerance opts) 1.0e-12)]
             (cmath/->ClojureMathComplexBackend tol opts)))
   :fastmath (fn [opts]
               (let [tol (or (:tolerance opts) 1.0e-12)]
                 (fmath/->FastMathComplexBackend tol opts)))})

;; Current backend selection
(def ^:dynamic *backend*
  "Current math backend instance. Bind dynamically with `with-backend`.
    	Defaults to a FastMathComplexBackend instance."
  (let [tol 1.0e-12]
    (fmath/->FastMathComplexBackend tol {})))

(def ^:dynamic *backend-key*
  "Current math backend key for informational purposes."
  :fastmath)

;; Unified tolerance (can be overridden via backend config :tolerance)

(defn available-backends
  "Return the set of available backend keys.
  
  Returns:
  Set of backend keywords that can be used with set-backend! or create-backend"
  []
  (set (keys backend-constructors)))

(defn get-backend
  "Return the current backend key.
  
  Returns:
  Keyword identifying the currently active backend"
  []
  *backend-key*)

(defn get-backend-instance
  "Return the current backend instance implementing the real/complex protocols.
  
  Returns:
  Backend instance that implements the mathematical operation protocols"
  []
  *backend*)

(defn create-backend
  "Create a backend instance for the given key and optional configuration.
  
  Parameters:
  - backend-key: Keyword identifying the backend type (from available-backends)
  - opts: Optional map of backend-specific configuration options
  
  Returns:
  Backend instance implementing the mathematical operation protocols"
  ([backend-key]
   (create-backend backend-key {}))
  ([backend-key opts]
   (let [ctor (get backend-constructors backend-key)]
     (when-not ctor
       (throw (ex-info (str "Unknown backend: " backend-key)
                       {:available (available-backends)})))
     (ctor opts))))

(defn set-backend!
  "Set the global backend by key.
  
  Parameters:
  - backend-key: Keyword identifying the backend (use keys from available-backends)
  
  Returns:
  The backend key that was set
  
  Note:
  Prefer with-backend for scoped overrides instead of global mutation"
  [backend-key]
  (when-not (contains? backend-constructors backend-key)
    (throw (ex-info (str "Unknown backend: " backend-key) {:available (available-backends)})))
  (let [inst (create-backend backend-key)]
    (alter-var-root #'*backend* (constantly inst))
    (alter-var-root #'*backend-key* (constantly backend-key))
    backend-key))

(defmacro with-backend
  "Evaluate body with the backend temporarily bound to the specified backend.
  
  Parameters:
  - backend: Either a backend keyword or a backend instance
  - body: Forms to evaluate with the temporary backend binding
  
  Returns:
  Result of evaluating body with the temporary backend"
  [backend & body]
  `(let [bk# ~backend
         [inst# key#]
         (if (keyword? bk#)
           [(create-backend bk#) bk#]
           [bk# *backend-key*])]
     (binding [*backend* inst#
               *backend-key* key#]
       ~@body)))

;;;
;;; Conversion helpers for FastMath Vec2 format integration
;;;

(defn- to-backend-vector
  "Convert input vector from FastMath Vec2 format to backend representation."
  [v]
  (proto/vector->backend *backend* v))

(defn- from-backend-vector
  "Convert result vector from backend representation to FastMath Vec2 format."
  [v]
  (proto/backend->vector *backend* v))

(defn- to-backend-matrix
  "Convert input matrix from FastMath Vec2 format to backend representation."
  [m]
  (proto/matrix->backend *backend* m))

(defn- from-backend-matrix
  "Convert result matrix from backend representation to FastMath Vec2 format."
  [m]
  (proto/backend->matrix *backend* m))

(defn- to-backend-scalar
  "Convert input scalar from FastMath Vec2 format to backend representation."
  [s]
  (proto/scalar->backend *backend* s))

(defn- from-backend-scalar
  "Convert result scalar from backend representation to FastMath Vec2 format."
  [s]
  (proto/backend->scalar *backend* s))

;;;
;;; Public API
;;;

;;
;; Helper / constructors for complex data (Split-of-Arrays representation)
;;

(defn complex-vector
  "Create a complex vector from parallel real and imaginary part collections.

  Parameters:
  - real-part: sequence of real components
  - imag-part: sequence of imaginary components (same length)

  Returns:
  Complex vector as Vec2 format [#vec2 [...], #vec2 [...], ...].

  Example:
  (complex-vector [1 0] [0 1])  ; => [#vec2 [1.0, 0.0], #vec2 [0.0, 1.0]]"
  [real-part imag-part]
  {:pre [(= (count real-part) (count imag-part))]}
  (mapv #(fc/complex (double %1) (double %2)) real-part imag-part))

(defn complex-matrix
  "Create a complex matrix from real & imaginary part matrices.

  Parameters:
  - real-m: matrix (vector of row vectors) of real parts
  - imag-m: matrix of imaginary parts (same shape)

  Returns complex matrix as Vec2 format [[#vec2 [...], ...], ...]."
  [real-m imag-m]
  {:pre [(= (count real-m) (count imag-m))
         (every? true? (map (fn [r i] (= (count r) (count i))) real-m imag-m))]}
  (mapv (fn [real-row imag-row]
          (mapv #(fc/complex (double %1) (double %2)) real-row imag-row))
        real-m imag-m))

;;
;; Basic structural & arithmetic operations
;;
(defn shape
  "Return the shape of a matrix.
  
  Parameters:
  - A: Matrix
  
  Returns:
  Vector [rows cols] indicating the matrix dimensions"
  [A]
  (proto/shape *backend* (to-backend-matrix A)))

(defn add
  "Perform matrix addition A + B.
  
  Parameters:
  - A: First matrix
  - B: Second matrix with same dimensions as A
  
  Returns:
  Matrix representing A + B"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/add *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn subtract
  "Perform matrix subtraction A - B.
  
  Parameters:
  - A: First matrix
  - B: Second matrix with same dimensions as A
  
  Returns:
  Matrix representing A - B"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/subtract *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn scale
  "Perform scalar multiplication α · A.
  
  Parameters:
  - A: Matrix
  - alpha: Scalar value
  
  Returns:
  Matrix representing α · A"
  [A alpha]
  (let [backend-A (to-backend-matrix A)
        result (proto/scale *backend* backend-A alpha)]
    (from-backend-matrix result)))

(defn negate
  "Compute the additive inverse -A.
  
  Parameters:
  - A: Matrix
  
  Returns:
  Matrix representing -A"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/negate *backend* backend-A)]
    (from-backend-matrix result)))

;;
;; Products
;;
(defn matrix-multiply
  "Perform matrix multiplication A × B.
  
  Parameters:
  - A: Left matrix
  - B: Right matrix with compatible dimensions
  
  Returns:
  Matrix representing the product A × B"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/matrix-multiply *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn matrix-vector-product
  "Perform matrix–vector multiplication A × x.
  
  Parameters:
  - A: Matrix
  - x: Vector with compatible dimensions
  
  Returns:
  Vector representing the product A × x"
  [A x]
  (let [backend-A (to-backend-matrix A)
        backend-x (to-backend-vector x)
        result (proto/matrix-vector-product *backend* backend-A backend-x)]
    (from-backend-vector result)))

(defn inner-product
  "Compute the vector inner product ⟨x|y⟩.
  
  Parameters:
  - x: First vector
  - y: Second vector with same length as x
  
  Returns:
  Scalar representing the inner product
  
  Note:
  For complex vectors, computes ⟨x|y⟩ = Σ xᵢ* yᵢ (conjugate x)"
  [x y]
  (let [backend-x (to-backend-vector x)
        backend-y (to-backend-vector y)
        backend-result (proto/inner-product *backend* backend-x backend-y)]
    (from-backend-scalar backend-result)))

(defn outer-product
  "Compute the outer product x ⊗ y†.
  
  Parameters:
  - x: First vector
  - y: Second vector
  
  Returns:
  Matrix representing the outer product x ⊗ y† in Vec2 format (no conjugation of x)
  
  Note:
  For quantum states, this creates projector-like matrices"
  [x y]
  (let [backend-x (to-backend-vector x)
        backend-y (to-backend-vector y)
        result (proto/outer-product *backend* backend-x backend-y)]
    (from-backend-matrix result)))

(defn hadamard-product
  "Compute the element-wise (Hadamard) product A ⊙ B.
  
  Parameters:
  - A: First matrix
  - B: Second matrix with same dimensions as A
  
  Returns:
  Matrix with element-wise multiplication of A and B"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/hadamard-product *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn kronecker-product
  "Compute the Kronecker (tensor) product A ⊗ B.
  
  Parameters:
  - A: First matrix
  - B: Second matrix
   
  Returns:
  Matrix representing the Kronecker product A ⊗ B
  
  Note:
  Essential for quantum computing multi-qubit operations"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/kronecker-product *backend* backend-A backend-B)]
    (from-backend-matrix result)))

;;
;; Transformations
;;
(defn transpose
  "Compute the transpose Aᵀ.
  
  Parameters:
  - A: Matrix
  
  Returns:
  Matrix representing the transpose of A"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/transpose *backend* backend-A)]
    (from-backend-matrix result)))

(defn conjugate-transpose
  "Compute the conjugate transpose Aᴴ (Hermitian adjoint).
  
  Parameters:
  - A: Matrix
  
  Returns:
  Matrix representing the conjugate transpose of A
  
  Note:
  For real matrices, this is equivalent to transpose"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/conjugate-transpose *backend* backend-A)]
    (from-backend-matrix result)))

;;
;; Reductions / scalar results
;;
(defn trace
  "Compute the trace Tr(A) = Σ aᵢᵢ.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Scalar representing the sum of diagonal elements"
  [A]
  (let [backend-A (to-backend-matrix A)
        backend-result (proto/trace *backend* backend-A)]
    (from-backend-scalar backend-result)))

(defn norm2
  "Compute the Euclidean norm ||x||₂.
  
  Parameters:
  - x: Vector
  
  Returns:
  Non-negative real number representing the Euclidean norm"
  [x]
  (let [backend-x (to-backend-vector x)
        backend-result (proto/norm2 *backend* backend-x)]
    (from-backend-scalar backend-result)))

;;
;; Linear solves / inverse
;;
(defn matrix-inverse
  "Compute the matrix inverse A⁻¹.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Inverse matrix A⁻¹
  
  Throws:
  Exception if matrix is singular"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/inverse *backend* backend-A)]
    (from-backend-matrix result)))

(defn solve-linear-system
  "Solve the linear system Ax = b.
  
  Parameters:
  - A: Matrix (square)
  - b: Vector or matrix
  
  Returns:
  Solution vector(s) x such that Ax = b"
  [A b]
  (let [backend-A (to-backend-matrix A)
        backend-b (if (vector? (first b))
                    (to-backend-matrix b)  ; matrix
                    (to-backend-vector b)) ; vector
        result (proto/solve-linear-system *backend* backend-A backend-b)]
    (if (vector? (first b))
      (from-backend-matrix result)  ; return matrix
      (from-backend-vector result)))) ; return vector

;;
;; Predicates
;;
(defn hermitian?
  "Test if a matrix is Hermitian (A ≈ Aᴴ).
  
  Parameters:
  - A: Square matrix
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether A is Hermitian or real symmetric"
  ([A] (let [backend-A (to-backend-matrix A)]
         (proto/hermitian? *backend* backend-A)))
  ([A eps] (let [backend-A (to-backend-matrix A)]
             (proto/hermitian? *backend* backend-A eps))))

(defn unitary?
  "Test if a matrix is unitary (Uᴴ U ≈ I).
  
  Parameters:
  - U: Square matrix
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether U is unitary (or orthogonal for real matrices)"
  ([U] (let [backend-U (to-backend-matrix U)]
         (proto/unitary? *backend* backend-U)))
  ([U eps] (let [backend-U (to-backend-matrix U)]
             (proto/unitary? *backend* backend-U eps))))

(defn positive-semidefinite?
  "Test if a matrix is positive semidefinite.
  
  Parameters:
  - A: Square Hermitian matrix
  
  Returns:
  Boolean indicating whether all eigenvalues of A are ≥ -eps
  
  Note:
  Matrix must be Hermitian for meaningful results"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (proto/positive-semidefinite? *backend* backend-A)))

;;
;; Decompositions
;;
(defn eigen-hermitian
  "Compute the eigendecomposition of a Hermitian matrix.
  
  Parameters:
  - A: Square Hermitian matrix
  
  Returns:
  Map containing:
  - :eigenvalues - Vector of eigenvalues in ascending order
  - :eigenvectors - Vector of corresponding eigenvector columns [v0 v1 ...]
  (Both eigenvalues and eigenvectors returned in original format)"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/eigen-hermitian *backend* backend-A)]
    (-> result
        (update :eigenvalues from-backend-vector)
        (update :eigenvectors #(mapv from-backend-vector %)))))

(defn eigen-general
  "Compute eigenvalues for a general (possibly non-Hermitian) matrix.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Map containing:
  - :eigenvalues - Vector of eigenvalue approximations
  - :iterations - Number of iterations used in computation"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/eigen-general *backend* backend-A)]
    (update result :eigenvalues from-backend-vector)))

(defn svd
  "Compute the Singular Value Decomposition of a matrix.
  
  Parameters:
  - A: Matrix
  
  Returns:
  Map containing:
  - :U - Left singular vectors matrix
  - :singular-values - Vector of singular values [σ0≥σ1≥...] in descending order
  - :Vt - Right singular vectors transpose matrix
  
  Note:
  Renames backend keys (:S :V†) for consistency"
  [A]
  (let [backend-A (to-backend-matrix A)
        {:keys [U S V†] :as raw} (proto/svd *backend* backend-A)]
    (cond-> {:U (from-backend-matrix U)
             :singular-values (from-backend-vector S)}
      V† (assoc :Vt (from-backend-matrix V†))
      (nil? U) (into raw))))

(defn lu-decomposition
  "Compute the LU decomposition A = P L U.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Map containing:
  - :P - Row permutation matrix
  - :L - Lower triangular matrix
  - :U - Upper triangular matrix"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/lu-decomposition *backend* backend-A)]
    (-> result
        (update :P from-backend-matrix)
        (update :L from-backend-matrix)
        (update :U from-backend-matrix))))

(defn qr-decomposition
  "Compute the QR decomposition A = Q R.
  
  Parameters:
  - A: Matrix
  
  Returns:
  Map containing:
  - :Q - Orthogonal/unitary matrix
  - :R - Upper triangular matrix"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/qr-decomposition *backend* backend-A)]
    (-> result
        (update :Q from-backend-matrix)
        (update :R from-backend-matrix))))

(defn cholesky-decomposition
  "Compute the Cholesky decomposition A = L L†.
  
  Parameters:
  - A: Positive semidefinite matrix
  
  Returns:
  Map containing:
  - :L - Lower triangular matrix such that A = L L†
  
  Note:
  May throw or return nil if A is not positive semidefinite"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/cholesky-decomposition *backend* backend-A)]
    (update result :L from-backend-matrix)))

;;
;; Matrix functions
;;
(defn matrix-exp
  "Compute the matrix exponential exp(A).
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Matrix representing exp(A)
  
  Note:
  Important for quantum time evolution operators"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (from-backend-matrix (proto/matrix-exp *backend* backend-A))))

(defn matrix-log
  "Compute the principal matrix logarithm log(A).
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Matrix representing the principal branch of log(A)"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (from-backend-matrix (proto/matrix-log *backend* backend-A))))

(defn matrix-sqrt
  "Compute the principal matrix square root √A.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Matrix representing the principal square root of A"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (from-backend-matrix (proto/matrix-sqrt *backend* backend-A))))

;;
;; Analysis
;;
(defn spectral-norm
  "Compute the spectral norm ||A||₂ (largest singular value).
  
  Parameters:
  - A: Matrix
  
  Returns:
  Non-negative real number representing the largest singular value"
  [A]
  (let [backend-A (to-backend-matrix A)
        backend-result (proto/spectral-norm *backend* backend-A)]
    (from-backend-scalar backend-result)))

(defn condition-number
  "Compute the 2-norm condition number κ₂(A) = σ_max / σ_min.
  
  Parameters:
  - A: Square matrix
  
  Returns:
  Positive real number representing the condition number
  
  Note:
  Higher values indicate numerical instability in linear solves"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (proto/condition-number *backend* backend-A)))

;;
;; Convenience reconstruction helpers
;;
;; TODO: check result, convert from backend format
(defn matrix-from-eigen
  "Reconstruct (approximate) Hermitian matrix from eigenvalues & eigenvectors.

  Parameters:
  - eigenvalues   vector [λ₀ … λₙ₋₁]
  - eigenvectors  vector of eigenvector column vectors [v₀ …]

  Returns:
  Matrix Σ λᵢ vᵢ vᵢᵀ (real case only for now) in Vec2 format.
  NOTE: Complex support can be added when complex eigenvectors are exposed."
  [eigenvalues eigenvectors]
  {:pre [(= (count eigenvalues) (count eigenvectors))]}
  (let [n (count (first eigenvectors))
        zero (vec (repeat n (vec (repeat n 0.0))))]
    (reduce (fn [acc [λ v]]
              (let [outer (outer-product v v)
                    ;; Convert Vec2 eigenvalue to backend scalar representation
                    scaled-outer (scale outer λ)]
                (add acc scaled-outer)))
            zero
            (map vector eigenvalues eigenvectors))))
