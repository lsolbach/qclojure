
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
   code can query a single source of truth."
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
    (cmath/->ClojureMathBackend tol {:tolerance tol})) )

(def ^:dynamic *backend-key*
  "Current math backend key for informational purposes."
  :pure)

;; Unified tolerance (can be overridden via backend config :tolerance)

(defn available-backends
  "Returns the set of available backend keys."
  []
  (set (keys backend-constructors)))

(defn get-backend
  "Returns the current backend key."
  []
  *backend-key*)

(defn get-backend-instance
  "Returns the current backend instance implementing the real/complex protocols."
  []
  *backend*)

(defn create-backend
  "Create a backend instance for the given key and optional opts map."
  ([backend-key]
   (create-backend backend-key {}))
  ([backend-key opts]
   (let [ctor (get backend-constructors backend-key)]
     (when-not ctor
       (throw (ex-info (str "Unknown backend: " backend-key)
                       {:available (available-backends)})))
     (ctor opts))))

(defn set-backend!
  "Sets the global backend by key. Use keys from `(available-backends)`.
    	Prefer `with-backend` for scoped overrides."
  [backend-key]
  (when-not (contains? backend-constructors backend-key)
    (throw (ex-info (str "Unknown backend: " backend-key) {:available (available-backends)})))
  (let [inst (create-backend backend-key)]
    (alter-var-root #'*backend* (constantly inst))
    (alter-var-root #'*backend-key* (constantly backend-key))
    backend-key))

(defmacro with-backend
  "Evaluates body with the backend temporarily bound to `backend`.
    	`backend` can be a backend key or a backend instance."
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

;; ---------------------------------------------------------------------------
;; Helper / constructors for complex data (Split-of-Arrays representation)
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Basic structural & arithmetic operations
;; ---------------------------------------------------------------------------

(defn shape
  "Return matrix shape [rows cols]." [A]
  (proto/shape *backend* A))

(defn add
  "Matrix addition A + B." [A B]
  (proto/add *backend* A B))

(defn subtract
  "Matrix subtraction A - B." [A B]
  (proto/subtract *backend* A B))

(defn scale
  "Scalar multiplication α · A." [A alpha]
  (proto/scale *backend* A alpha))

(defn negate
  "Additive inverse -A." [A]
  (proto/negate *backend* A))

;; ---------------------------------------------------------------------------
;; Products
;; ---------------------------------------------------------------------------

(defn matrix-multiply
  "Matrix product A × B." [A B]
  (proto/matrix-multiply *backend* A B))

(defn complex-matrix-multiply
  "Explicit complex matrix multiply (alias of matrix-multiply)." [A B]
  (proto/matrix-multiply *backend* A B))

(defn matrix-vector
  "Matrix–vector product A × x." [A x]
  (proto/matrix-vector-product *backend* A x))

(defn complex-matrix-vector
  "Complex matrix–vector product (promotes real args automatically)." [A x]
  (proto/matrix-vector-product *backend* A x))

(defn outer-product
  "Outer product x ⊗ y† (no conjugation of x)." [x y]
  (proto/outer-product *backend* x y))

(defn hadamard
  "Element-wise (Hadamard) product A ⊙ B." [A B]
  (proto/hadamard *backend* A B))

(defn kronecker
  "Kronecker (tensor) product A ⊗ B." [A B]
  (proto/kronecker *backend* A B))

(defn complex-kronecker
  "Complex Kronecker product (alias of kronecker)." [A B]
  (proto/kronecker *backend* A B))

;; ---------------------------------------------------------------------------
;; Transforms
;; ---------------------------------------------------------------------------

(defn matrix-transpose
  "Transpose Aᵀ." [A]
  (proto/transpose *backend* A))

(defn conjugate-transpose
  "Conjugate transpose Aᴴ (Hermitian adjoint)." [A]
  (proto/conjugate-transpose *backend* A))

;; ---------------------------------------------------------------------------
;; Reductions / scalar results
;; ---------------------------------------------------------------------------

(defn trace
  "Trace Tr(A) = Σ aᵢᵢ." [A]
  (proto/trace *backend* A))

(defn inner-product
  "Vector inner product ⟨x|y⟩ (conjugates first arg if complex)." [x y]
  (proto/inner-product *backend* x y))

(defn norm2
  "Euclidean norm ||x||₂." [x]
  (proto/norm2 *backend* x))

;; ---------------------------------------------------------------------------
;; Linear solves / inverse
;; ---------------------------------------------------------------------------

(defn linear-solve
  "Solve linear system A x = b. Returns x or nil/throws if singular." [A b]
  (proto/solve-linear-system *backend* A b))

(defn matrix-inverse
  "Matrix inverse A⁻¹ or nil if singular." [A]
  (proto/inverse *backend* A))

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(defn hermitian?
  "Predicate: A ≈ Aᴴ (Hermitian / real symmetric). Optional tolerance." 
  ([A] (proto/hermitian? *backend* A))
  ([A eps] (proto/hermitian? *backend* A eps)))

(defn unitary?
  "Predicate: Uᴴ U ≈ I." 
  ([U] (proto/unitary? *backend* U))
  ([U eps] (proto/unitary? *backend* U eps)))

(defn positive-semidefinite?
  "Predicate: A Hermitian with eigenvalues ≥ -eps." [A]
  (proto/positive-semidefinite? *backend* A))

(defn trace-one?
  "Predicate: Tr(ρ) ≈ 1 for density matrices." 
  ([rho] (proto/trace-one? *backend* rho))
  ([rho eps] (proto/trace-one? *backend* rho eps)))

;; ---------------------------------------------------------------------------
;; Decompositions
;; ---------------------------------------------------------------------------

(defn eigen-hermitian
  "Hermitian (real symmetric / complex Hermitian) eigendecomposition.
  Returns {:eigenvalues [...ascending...] :eigenvectors [v0 v1 ...]}" [A]
  (proto/eigen-hermitian *backend* A))

(defn eigen-general
  "General (possibly non-Hermitian) eigenvalue approximation.
  Returns {:eigenvalues [...], :iterations k}." [A]
  (proto/eigen-general *backend* A))

(defn svd
  "Singular Value Decomposition wrapper.
  Returns {:U U :singular-values [σ0≥…] :Vt Vᴴ}. Renames backend keys (:S :V†)." [A]
  (let [{:keys [U S V†] :as raw} (proto/svd *backend* A)]
    (cond-> {:U U :singular-values S}
      V† (assoc :Vt V†)
  (nil? U) (into raw))))

;; ---------------------------------------------------------------------------
;; Matrix functions
;; ---------------------------------------------------------------------------

(defn matrix-exp
  "Matrix exponential exp(A)." [A]
  (proto/matrix-exp *backend* A))

(defn matrix-log
  "Principal matrix logarithm log(A)." [A]
  (proto/matrix-log *backend* A))

(defn matrix-sqrt
  "Principal matrix square root √A." [A]
  (proto/matrix-sqrt *backend* A))

;; ---------------------------------------------------------------------------
;; Analysis
;; ---------------------------------------------------------------------------

(defn spectral-norm
  "Spectral norm ||A||₂ (largest singular value)." [A]
  (proto/spectral-norm *backend* A))

(defn condition-number
  "2-norm condition number κ₂(A) = σ_max / σ_min." [A]
  (proto/condition-number *backend* A))

;; ---------------------------------------------------------------------------
;; Quantum state helpers
;; ---------------------------------------------------------------------------

(defn state-normalize
  "Return normalized state vector ψ/||ψ||₂ (real or complex)." [state]
  (proto/state-normalize *backend* state))

(defn projector-from-state
  "Projector |ψ⟩⟨ψ| for (optionally un-normalized) state ψ." [psi]
  (proto/projector-from-state *backend* psi))

(defn density-matrix
  "Density matrix ρ for pure state ψ (alias of projector)." [psi]
  (proto/density-matrix *backend* psi))

;; ---------------------------------------------------------------------------
;; Convenience reconstruction helpers
;; ---------------------------------------------------------------------------

(defn matrix-from-eigen
  "Reconstruct (approximate) Hermitian matrix from eigenvalues & eigenvectors.

  Parameters:
  - eigenvalues   vector [λ₀ … λₙ₋₁]
  - eigenvectors  vector of eigenvector column vectors [v₀ …]

  Returns matrix Σ λᵢ vᵢ vᵢᵀ (real case only for now).
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

