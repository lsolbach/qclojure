
(ns org.soulspace.qclojure.domain.math.core
  "Facade for linear algebra operations with pluggable backends.
  
   Goals:
    - Provide a single, meaningful API for matrix and vector operations.
    - Allow swapping implementations (pure Clojure Math, Fastmath, Neanderthal) without changing call sites.
    - Keep data shapes simple: matrices as vectors of row vectors; vectors as vectors of numbers.
    
    Default backend: :pure, implemented via org.soulspace.qclojure.domain.math.linear-algebra.
    
    Use `set-backend!` to change the global backend, or `with-backend` to override in a dynamic scope."
  (:require
   [org.soulspace.qclojure.domain.math.protocols :as proto]
   [org.soulspace.qclojure.domain.math.clojure-math :as cmath]))

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
             (cmath/->ClojureMathBackend tol (merge {:tolerance tol} opts))))})

;; Current backend selection
(def ^:dynamic *backend*
  "Current math backend instance. Bind dynamically with `with-backend`.
    	Defaults to a ->ClojureMathBackend instance."
  (let [tol 1.0e-12]
    (cmath/->ClojureMathBackend tol {:tolerance tol})))

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
  (proto/shape *backend* A))

(defn add
  "Perform matrix addition A + B.
  
  Parameters:
  - A: First matrix (real or complex)
  - B: Second matrix (real or complex) with same dimensions as A
  
  Returns:
  Matrix representing A + B"
  [A B]
  (proto/add *backend* A B))

(defn subtract
  "Perform matrix subtraction A - B.
  
  Parameters:
  - A: First matrix (real or complex)
  - B: Second matrix (real or complex) with same dimensions as A
  
  Returns:
  Matrix representing A - B"
  [A B]
  (proto/subtract *backend* A B))

(defn scale
  "Perform scalar multiplication α · A.
  
  Parameters:
  - A: Matrix (real or complex)
  - alpha: Scalar value (real or complex number)
  
  Returns:
  Matrix representing α · A"
  [A alpha]
  (proto/scale *backend* A alpha))

(defn negate
  "Compute the additive inverse -A.
  
  Parameters:
  - A: Matrix (real or complex)
  
  Returns:
  Matrix representing -A"
  [A]
  (proto/negate *backend* A))

;;
;; Products
;;
(defn matrix-multiply
  "Perform matrix multiplication A × B.
  
  Parameters:
  - A: Left matrix (real or complex)
  - B: Right matrix (real or complex) with compatible dimensions
  
  Returns:
  Matrix representing the product A × B"
  [A B]
  (proto/matrix-multiply *backend* A B))

(defn complex-matrix-multiply
  "Perform explicit complex matrix multiplication.
  
  Parameters:
  - A: Left complex matrix
  - B: Right complex matrix with compatible dimensions
  
  Returns:
  Complex matrix representing the product A × B
  
  Note:
  This is an alias of matrix-multiply for clarity in complex contexts"
  [A B]
  (proto/matrix-multiply *backend* A B))

(defn matrix-vector
  "Perform matrix–vector multiplication A × x.
  
  Parameters:
  - A: Matrix (real or complex)
  - x: Vector (real or complex) with compatible dimensions
  
  Returns:
  Vector representing the product A × x"
  [A x]
  (proto/matrix-vector-product *backend* A x))

(defn complex-matrix-vector
  "Perform complex matrix–vector multiplication.
  
  Parameters:
  - A: Complex matrix
  - x: Complex vector with compatible dimensions
  
  Returns:
  Complex vector representing the product A × x
  
  Note:
  Automatically promotes real arguments to complex representation"
  [A x]
  (proto/matrix-vector-product *backend* A x))

(defn outer-product
  "Compute the outer product x ⊗ y†.
  
  Parameters:
  - x: First vector (real or complex)
  - y: Second vector (real or complex)
  
  Returns:
  Matrix representing the outer product x ⊗ y† (no conjugation of x)
  
  Note:
  For quantum states, this creates projector-like matrices"
  [x y]
  (proto/outer-product *backend* x y))

(defn hadamard
  "Compute the element-wise (Hadamard) product A ⊙ B.
  
  Parameters:
  - A: First matrix (real or complex)
  - B: Second matrix (real or complex) with same dimensions as A
  
  Returns:
  Matrix with element-wise multiplication of A and B"
  [A B]
  (proto/hadamard *backend* A B))

(defn kronecker
  "Compute the Kronecker (tensor) product A ⊗ B.
  
  Parameters:
  - A: First matrix (real or complex)
  - B: Second matrix (real or complex)
  
  Returns:
  Matrix representing the Kronecker product A ⊗ B
  
  Note:
  Essential for quantum computing multi-qubit operations"
  [A B]
  (proto/kronecker *backend* A B))

(defn complex-kronecker
  "Compute the complex Kronecker product.
  
  Parameters:
  - A: First complex matrix
  - B: Second complex matrix
  
  Returns:
  Complex matrix representing the Kronecker product A ⊗ B
  
  Note:
  This is an alias of kronecker for clarity in complex contexts"
  [A B]
  (proto/kronecker *backend* A B))

;;
;; Transforms
;;
(defn matrix-transpose
  "Compute the transpose Aᵀ.
  
  Parameters:
  - A: Matrix (real or complex)
  
  Returns:
  Matrix representing the transpose of A"
  [A]
  (proto/transpose *backend* A))

(defn conjugate-transpose
  "Compute the conjugate transpose Aᴴ (Hermitian adjoint).
  
  Parameters:
  - A: Matrix (real or complex)
  
  Returns:
  Matrix representing the conjugate transpose of A
  
  Note:
  For real matrices, this is equivalent to transpose"
  [A]
  (proto/conjugate-transpose *backend* A))

;;
;; Reductions / scalar results
;;
(defn trace
  "Compute the trace Tr(A) = Σ aᵢᵢ.
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Scalar representing the sum of diagonal elements"
  [A]
  (proto/trace *backend* A))

(defn inner-product
  "Compute the vector inner product ⟨x|y⟩.
  
  Parameters:
  - x: First vector (real or complex)
  - y: Second vector (real or complex) with same length as x
  
  Returns:
  Scalar representing the inner product
  
  Note:
  Conjugates the first argument if complex"
  [x y]
  (proto/inner-product *backend* x y))

(defn norm2
  "Compute the Euclidean norm ||x||₂.
  
  Parameters:
  - x: Vector (real or complex)
  
  Returns:
  Non-negative real number representing the Euclidean norm"
  [x]
  (proto/norm2 *backend* x))

;;
;; Linear solves / inverse
;;
(defn linear-solve
  "Solve the linear system A x = b.
  
  Parameters:
  - A: Square matrix (real or complex), must be non-singular
  - b: Right-hand side vector (real or complex)
  
  Returns:
  Solution vector x, or nil/throws if A is singular"
  [A b]
  (proto/solve-linear-system *backend* A b))

(defn matrix-inverse
  "Compute the matrix inverse A⁻¹.
  
  Parameters:
  - A: Square non-singular matrix (real or complex)
  
  Returns:
  Inverse matrix A⁻¹, or nil if A is singular"
  [A]
  (proto/inverse *backend* A))

;;
;; Predicates
;;
(defn hermitian?
  "Test if a matrix is Hermitian (A ≈ Aᴴ).
  
  Parameters:
  - A: Square matrix (real or complex)
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether A is Hermitian or real symmetric"
  ([A] (proto/hermitian? *backend* A))
  ([A eps] (proto/hermitian? *backend* A eps)))

(defn unitary?
  "Test if a matrix is unitary (Uᴴ U ≈ I).
  
  Parameters:
  - U: Square matrix (real or complex)
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether U is unitary (or orthogonal for real matrices)"
  ([U] (proto/unitary? *backend* U))
  ([U eps] (proto/unitary? *backend* U eps)))

(defn positive-semidefinite?
  "Test if a matrix is positive semidefinite.
  
  Parameters:
  - A: Square Hermitian matrix (real or complex)
  
  Returns:
  Boolean indicating whether all eigenvalues of A are ≥ -eps
  
  Note:
  Matrix must be Hermitian for meaningful results"
  [A]
  (proto/positive-semidefinite? *backend* A))

(defn trace-one?
  "Test if a matrix has trace equal to one (Tr(ρ) ≈ 1).
  
  Parameters:
  - rho: Square matrix (typically a density matrix)
  - eps: Optional tolerance for approximate equality (uses current tolerance if not provided)
  
  Returns:
  Boolean indicating whether the trace is approximately 1
  
  Note:
  Useful for validating quantum density matrices"
  ([rho] (proto/trace-one? *backend* rho))
  ([rho eps] (proto/trace-one? *backend* rho eps)))

;;
;; Decompositions
;;
(defn eigen-hermitian
  "Compute the eigendecomposition of a Hermitian matrix.
  
  Parameters:
  - A: Square Hermitian matrix (real symmetric or complex Hermitian)
  
  Returns:
  Map containing:
  - :eigenvalues - Vector of eigenvalues in ascending order
  - :eigenvectors - Vector of corresponding eigenvector columns [v0 v1 ...]"
  [A]
  (proto/eigen-hermitian *backend* A))

(defn eigen-general
  "Compute eigenvalues for a general (possibly non-Hermitian) matrix.
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Map containing:
  - :eigenvalues - Vector of eigenvalue approximations
  - :iterations - Number of iterations used in computation"
  [A]
  (proto/eigen-general *backend* A))

(defn svd
  "Compute the Singular Value Decomposition of a matrix.
  
  Parameters:
  - A: Matrix (real or complex, may be rectangular)
  
  Returns:
  Map containing:
  - :U - Left singular vectors matrix
  - :singular-values - Vector of singular values [σ0≥σ1≥...] in descending order
  - :Vt - Right singular vectors transpose matrix
  
  Note:
  Renames backend keys (:S :V†) for consistency"
  [A]
  (let [{:keys [U S V†] :as raw} (proto/svd *backend* A)]
    (cond-> {:U U :singular-values S}
      V† (assoc :Vt V†)
      (nil? U) (into raw))))

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
  (proto/matrix-exp *backend* A))

(defn matrix-log
  "Compute the principal matrix logarithm log(A).
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Matrix representing the principal branch of log(A)"
  [A]
  (proto/matrix-log *backend* A))

(defn matrix-sqrt
  "Compute the principal matrix square root √A.
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Matrix representing the principal square root of A"
  [A]
  (proto/matrix-sqrt *backend* A))

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
  (proto/spectral-norm *backend* A))

(defn condition-number
  "Compute the 2-norm condition number κ₂(A) = σ_max / σ_min.
  
  Parameters:
  - A: Square matrix (real or complex)
  
  Returns:
  Positive real number representing the condition number
  
  Note:
  Higher values indicate numerical instability in linear solves"
  [A]
  (proto/condition-number *backend* A))

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
  (proto/state-normalize *backend* state))

(defn projector-from-state
  "Create a projector matrix from a quantum state.
  
  Parameters:
  - psi: State vector (real or complex, may be unnormalized)
  
  Returns:
  Projector matrix |ψ⟩⟨ψ| corresponding to the state
  
  Note:
  Automatically normalizes the state before creating the projector"
  [psi]
  (proto/projector-from-state *backend* psi))

(defn density-matrix
  "Create a density matrix from a pure quantum state.
  
  Parameters:
  - psi: Pure state vector (real or complex)
  
  Returns:
  Density matrix ρ representing the pure state
  
  Note:
  This is an alias of projector-from-state for quantum contexts"
  [psi]
  (proto/density-matrix *backend* psi))

;;
;; Convenience reconstruction helpers
;;
(defn matrix-from-eigen
  "Reconstruct (approximate) Hermitian matrix from eigenvalues & eigenvectors.

  Parameters:
  - eigenvalues   vector [λ₀ … λₙ₋₁]
  - eigenvectors  vector of eigenvector column vectors [v₀ …]

  Returns:
  Matrix Σ λᵢ vᵢ vᵢᵀ (real case only for now).
  NOTE: Complex support can be added when complex eigenvectors are exposed."
  [eigenvalues eigenvectors]
  {:pre [(= (count eigenvalues) (count eigenvectors))]}
  (let [n (count (first eigenvectors))
        zero (vec (repeat n (vec (repeat n 0.0))))]
    (reduce (fn [acc [λ v]]
              (let [outer (mapv (fn [i]
                                  (mapv (fn [j] (* λ (double (nth v i)) (double (nth v j)))) (range n)))
                                (range n))]
                (proto/add *backend* acc outer)))
            zero
            (map vector eigenvalues eigenvectors))))
