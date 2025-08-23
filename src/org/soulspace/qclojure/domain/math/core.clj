
(ns org.soulspace.qclojure.domain.math.core
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
   [org.soulspace.qclojure.domain.math.clojure.backend :as cmath]
   [org.soulspace.qclojure.domain.math.fastmath.backend :as fmath]))

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
             (cmath/->ClojureMathComplexBackend tol (merge {:tolerance tol} opts))))
   :fastmath (fn [opts]
               (let [tol (or (:tolerance opts) 1.0e-12)]
                 (fmath/->FastMathComplexBackend tol (merge {:tolerance tol} opts))))})

;; Current backend selection
(def ^:dynamic *backend*
  "Current math backend instance. Bind dynamically with `with-backend`.
    	Defaults to a ->ClojureMathBackend instance."
  (let [tol 1.0e-12]
    (cmath/->ClojureMathComplexBackend tol {:tolerance tol})))

(def ^:dynamic *backend-key*
  "Current math backend key for informational purposes."
  :pure)

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
  Complex vector map {:real [...], :imag [...]}.

  Example:
  (complex-vector [1 0] [0 1])  ; => |ψ> = [1+0i, 0+1i]"
  [real-part imag-part]
  {:pre [(= (count real-part) (count imag-part))]}
  {:real (vec (map double real-part))
   :imag (vec (map double imag-part))})

(defn complex-matrix
  "Create a complex matrix from real & imaginary part matrices.

  Parameters:
  - real-m: matrix (vector of row vectors) of real parts
  - imag-m: matrix of imaginary parts (same shape)

  Returns complex matrix map {:real real-m' :imag imag-m'} with elements coerced to double."
  [real-m imag-m]
  {:pre [(= (count real-m) (count imag-m))
         (every? true? (map (fn [r i] (= (count r) (count i))) real-m imag-m))]}
  {:real (mapv (fn [row] (mapv double row)) real-m)
   :imag (mapv (fn [row] (mapv double row)) imag-m)})

;;
;; Basic structural & arithmetic operations
;;
(defn shape
  "Return the shape of a matrix.
  
  Parameters:
  - A: Matrix (vector of row vectors) or complex matrix map
  
  Returns:
  Vector [rows cols] indicating the matrix dimensions"
  [A]
  (proto/shape *backend* (to-backend-matrix A)))

(defn add
  "Perform matrix addition A + B.
  
  Parameters:
  - A: First matrix (real or complex, Vec2 or SoA format)
  - B: Second matrix (real or complex, Vec2 or SoA format) with same dimensions as A
  
  Returns:
  Matrix representing A + B in Vec2 format"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/add *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn subtract
  "Perform matrix subtraction A - B.
  
  Parameters:
  - A: First matrix (real or complex, Vec2 or SoA format)
  - B: Second matrix (real or complex, Vec2 or SoA format) with same dimensions as A
  
  Returns:
  Matrix representing A - B in Vec2 format"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/subtract *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn scale
  "Perform scalar multiplication α · A.
  
  Parameters:
  - A: Matrix (real or complex, Vec2 or SoA format)
  - alpha: Scalar value (real or complex number, Vec2 or SoA format)
  
  Returns:
  Matrix representing α · A in Vec2 format"
  [A alpha]
  (let [backend-A (to-backend-matrix A)
        result (proto/scale *backend* backend-A alpha)]
    (from-backend-matrix result)))

(defn negate
  "Compute the additive inverse -A.
  
  Parameters:
  - A: Matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Matrix representing -A in Vec2 format"
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
  - A: Left matrix (real or complex, Vec2 or SoA format)
  - B: Right matrix (real or complex, Vec2 or SoA format) with compatible dimensions
  
  Returns:
  Matrix representing the product A × B in Vec2 format"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/matrix-multiply *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn complex-matrix-multiply
  "Perform explicit complex matrix multiplication.
  
  Parameters:
  - A: Left complex matrix (Vec2 or SoA format)
  - B: Right complex matrix (Vec2 or SoA format) with compatible dimensions
  
  Returns:
  Complex matrix representing the product A × B in Vec2 format
  
  Note:
  This is an alias of matrix-multiply for clarity in complex contexts"
  [A B]
  (matrix-multiply A B))

(defn matrix-vector
  "Perform matrix–vector multiplication A × x.
  
  Parameters:
  - A: Matrix (real or complex, Vec2 or SoA format)
  - x: Vector (real or complex, Vec2 or SoA format) with compatible dimensions
  
  Returns:
  Vector representing the product A × x in Vec2 format"
  [A x]
  (let [backend-A (to-backend-matrix A)
        backend-x (to-backend-vector x)
        result (proto/matrix-vector-product *backend* backend-A backend-x)]
    (from-backend-vector result)))

(defn complex-matrix-vector
  "Perform complex matrix–vector multiplication.
  
  Parameters:
  - A: Complex matrix (Vec2 or SoA format)
  - x: Complex vector (Vec2 or SoA format) with compatible dimensions
  
  Returns:
  Complex vector representing the product A × x in Vec2 format
  
  Note:
  Automatically promotes real arguments to complex representation"
  [A x]
  (matrix-vector A x))

(defn outer-product
  "Compute the outer product x ⊗ y†.
  
  Parameters:
  - x: First vector (real or complex, Vec2 or SoA format)
  - y: Second vector (real or complex, Vec2 or SoA format)
  
  Returns:
  Matrix representing the outer product x ⊗ y† in Vec2 format (no conjugation of x)
  
  Note:
  For quantum states, this creates projector-like matrices"
  [x y]
  (let [backend-x (to-backend-vector x)
        backend-y (to-backend-vector y)
        result (proto/outer-product *backend* backend-x backend-y)]
    (from-backend-matrix result)))

(defn hadamard
  "Compute the element-wise (Hadamard) product A ⊙ B.
  
  Parameters:
  - A: First matrix (real or complex, Vec2 or SoA format)
  - B: Second matrix (real or complex, Vec2 or SoA format) with same dimensions as A
  
  Returns:
  Matrix with element-wise multiplication of A and B in Vec2 format"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/hadamard *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn kronecker
  "Compute the Kronecker (tensor) product A ⊗ B.
  
  Parameters:
  - A: First matrix (real or complex, Vec2 or SoA format)
  - B: Second matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Matrix representing the Kronecker product A ⊗ B in Vec2 format
  
  Note:
  Essential for quantum computing multi-qubit operations"
  [A B]
  (let [backend-A (to-backend-matrix A)
        backend-B (to-backend-matrix B)
        result (proto/kronecker *backend* backend-A backend-B)]
    (from-backend-matrix result)))

(defn complex-kronecker
  "Compute the complex Kronecker product.
  
  Parameters:
  - A: First complex matrix (Vec2 or SoA format)
  - B: Second complex matrix (Vec2 or SoA format)
  
  Returns:
  Complex matrix representing the Kronecker product A ⊗ B in Vec2 format
  
  Note:
  This is an alias of kronecker for clarity in complex contexts"
  [A B]
  (kronecker A B))

;;
;; Transforms
;;
(defn matrix-transpose
  "Compute the transpose Aᵀ.
  
  Parameters:
  - A: Matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Matrix representing the transpose of A in Vec2 format"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/transpose *backend* backend-A)]
    (from-backend-matrix result)))

(defn conjugate-transpose
  "Compute the conjugate transpose Aᴴ (Hermitian adjoint).
  
  Parameters:
  - A: Matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Matrix representing the conjugate transpose of A in Vec2 format
  
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
  - A: Square matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Scalar representing the sum of diagonal elements"
  [A]
  (let [backend-A (to-backend-matrix A)
        backend-result (proto/trace *backend* backend-A)]
    (from-backend-scalar backend-result)))

(defn inner-product
  "Compute the vector inner product ⟨x|y⟩.
  
  Parameters:
  - x: First vector (real or complex, Vec2 or SoA format)
  - y: Second vector (real or complex, Vec2 or SoA format) with same length as x
  
  Returns:
  Scalar representing the inner product
  
  Note:
  For complex vectors, computes ⟨x|y⟩ = Σ xᵢ* yᵢ (conjugate x)"
  [x y]
  (let [backend-x (to-backend-vector x)
        backend-y (to-backend-vector y)
        backend-result (proto/inner-product *backend* backend-x backend-y)]
    (from-backend-scalar backend-result)))

(defn norm2
  "Compute the Euclidean norm ||x||₂.
  
  Parameters:
  - x: Vector (real or complex, Vec2 or SoA format)
  
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
  - A: Square matrix (Vec2 or SoA format)
  
  Returns:
  Inverse matrix A⁻¹ (in original format)
  
  Throws:
  Exception if matrix is singular"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/inverse *backend* backend-A)]
    (from-backend-matrix result)))

(defn solve-linear-system
  "Solve the linear system Ax = b.
  
  Parameters:
  - A: Matrix (square, Vec2 or SoA format)
  - b: Vector or matrix (Vec2 or SoA format)
  
  Returns:
  Solution vector(s) x such that Ax = b (in original format)"
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
  - A: Square matrix (real or complex, Vec2 or SoA format)
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
  - U: Square matrix (real or complex, Vec2 or SoA format)
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
  - A: Square Hermitian matrix (real or complex, Vec2 or SoA format)
  
  Returns:
  Boolean indicating whether all eigenvalues of A are ≥ -eps
  
  Note:
  Matrix must be Hermitian for meaningful results"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (proto/positive-semidefinite? *backend* backend-A)))

(defn trace-one?
  "Test if a matrix has trace equal to one (Tr(ρ) ≈ 1).
  
  Parameters:
  - rho: Square matrix (typically a density matrix)
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether the trace is approximately 1
  
  Note:
  Useful for validating quantum density matrices"
  ([rho] (let [backend-rho (to-backend-matrix rho)]
           (proto/trace-one? *backend* backend-rho)))
  ([rho eps] (let [backend-rho (to-backend-matrix rho)]
               (proto/trace-one? *backend* backend-rho eps))))

;;
;; Decompositions
;;
(defn eigen-hermitian
  "Compute the eigendecomposition of a Hermitian matrix.
  
  Parameters:
  - A: Square Hermitian matrix (Vec2 or SoA format)
  
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
  - A: Square matrix (Vec2 or SoA format)
  
  Returns:
  Map containing:
  - :eigenvalues - Vector of eigenvalue approximations (in original format)
  - :iterations - Number of iterations used in computation"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/eigen-general *backend* backend-A)]
    (update result :eigenvalues from-backend-vector)))

(defn svd
  "Compute the Singular Value Decomposition of a matrix.
  
  Parameters:
  - A: Matrix (Vec2 or SoA format, may be rectangular)
  
  Returns:
  Map containing:
  - :U - Left singular vectors matrix (in original format)
  - :singular-values - Vector of singular values [σ0≥σ1≥...] in descending order (in original format)
  - :Vt - Right singular vectors transpose matrix (in original format)
  
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
  - A: Square matrix (Vec2 or SoA format)
  
  Returns:
  Map containing:
  - :P - Row permutation matrix (in original format)
  - :L - Lower triangular matrix (in original format)
  - :U - Upper triangular matrix (in original format)"
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
  - A: Matrix (Vec2 or SoA format)
  
  Returns:
  Map containing:
  - :Q - Orthogonal/unitary matrix (in original format)
  - :R - Upper triangular matrix (in original format)"
  [A]
  (let [backend-A (to-backend-matrix A)
        result (proto/qr-decomposition *backend* backend-A)]
    (-> result
        (update :Q from-backend-matrix)
        (update :R from-backend-matrix))))

(defn cholesky-decomposition
  "Compute the Cholesky decomposition A = L L†.
  
  Parameters:
  - A: Positive semidefinite matrix (Vec2 or SoA format)
  
  Returns:
  Map containing:
  - :L - Lower triangular matrix such that A = L L† (in original format)
  
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
  - A: Square matrix (real or complex)
  
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
  - A: Square matrix (real or complex)
  
  Returns:
  Matrix representing the principal branch of log(A)"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (from-backend-matrix (proto/matrix-log *backend* backend-A))))

(defn matrix-sqrt
  "Compute the principal matrix square root √A.
  
  Parameters:
  - A: Square matrix (real or complex)
  
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
  - A: Matrix (real or complex)
  
  Returns:
  Non-negative real number representing the largest singular value"
  [A]
  (let [backend-A (to-backend-matrix A)
        backend-result (proto/spectral-norm *backend* backend-A)]
    (from-backend-scalar backend-result)))

(defn condition-number
  "Compute the 2-norm condition number κ₂(A) = σ_max / σ_min.
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Positive real number representing the condition number
  
  Note:
  Higher values indicate numerical instability in linear solves"
  [A]
  (let [backend-A (to-backend-matrix A)]
    (proto/condition-number *backend* backend-A)))

;;
;; Quantum state helpers
;;
(defn state-normalize
  "Normalize a quantum state vector to unit norm.
  
  Parameters:
  - state: State vector (real or complex)
  
  Returns:
  Normalized state vector ψ/||ψ||₂ with unit norm"
  [state]
  (let [backend-state (to-backend-vector state)]
    (from-backend-vector (proto/state-normalize *backend* backend-state))))

(defn projector-from-state
  "Create a projector matrix from a quantum state.
  
  Parameters:
  - psi: State vector (real or complex, may be unnormalized)
  
  Returns:
  Projector matrix |ψ⟩⟨ψ| corresponding to the state
  
  Note:
  Automatically normalizes the state before creating the projector"
  [psi]
  (let [backend-psi (to-backend-vector psi)]
    (from-backend-matrix (proto/projector-from-state *backend* backend-psi))))

(defn density-matrix
  "Create a density matrix from a pure quantum state.
  
  Parameters:
  - psi: Pure state vector (real or complex)
  
  Returns:
  Density matrix ρ representing the pure state
  
  Note:
  This is an alias of projector-from-state for quantum contexts"
  [psi]
  (let [backend-psi (to-backend-vector psi)]
    (from-backend-matrix (proto/density-matrix *backend* backend-psi))))

;;
;; Convenience reconstruction helpers
;;
(defn matrix-from-eigen
  "Reconstruct (approximate) Hermitian matrix from eigenvalues & eigenvectors.

  Parameters:
  - eigenvalues   vector [λ₀ … λₙ₋₁]
  - eigenvectors  vector of eigenvector column vectors [v₀ …] (Vec2 or SoA format)

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
