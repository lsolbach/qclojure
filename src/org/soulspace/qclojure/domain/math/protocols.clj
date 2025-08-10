(ns org.soulspace.qclojure.domain.math.protocols
  "Core mathematical protocols for matrix algebra, decompositions, and quantum state operations.
  
  Provides a contract for implementing backends to support linear algebra operations
  over numeric fields (ℝ or ℂ) with a focus on immutability and pure functions."
  (:require
   [fastmath.complex :as complex]))

;;;
;;; Complex number protocols
;;;

;; Provides a minimal protocol for complex numbers, allowing backends to define their own representations.
;; This protocol is used by matrix algebra operations that may involve complex numbers.
;; Backends can implement this protocol to provide access to the real and imaginary parts of complex numbers, vectors and matrices.
;; The protocol is designed to be flexible, allowing for different representations (e.g., maps, records, or custom types).
;; Implementations should ensure that the real and imaginary parts are accessible in a consistent manner.
(defprotocol Complex
  "Protocol for complex elements."
  (real [x]
    "Return the real part of x.")

  (imag [x]
    "Return the imaginary part of x.")

  (conjugate [x]
    "Return the complex conjugate of x.")

  (complex? [x]
    "Check if x is a complex element.")
  ;
  )

;;;
;;; Backend adapter
;;;
(defprotocol BackendAdapter
  "Protocol for converting between QClojure vectors/matrices and backend-specific representations."
  (vector->backend [backend v]
    "Convert a vector to the backend's representation. Returns a new vector in the backend's format.")

  (backend->vector [backend v]
    "Convert a backend vector to a Clojure vector. Returns a new Clojure vector.")

  (matrix->backend [backend m]
    "Convert a matrix to the backend's representation. Returns a new matrix in the backend's format.")

  (backend->matrix [backend m]
    "Convert a backend matrix to a Clojure matrix. Returns a new Clojure matrix.")

  ;
  )

;;;
;;; Matrix algebra protocol
;;;
(defprotocol MatrixAlgebra
  "Backend contract for core linear algebra operations over a numeric field.
    
  Rationale:
   - Backends decide representation (pure real, split-complex/SoA, AoS, native lib).
   - Public semantics expressed in mathematical terms, not data shape.
   - Mixed real/complex promotion is an implementation concern (outside protocol).

  Conventions used in docstrings below:
   - Matrix  : rectangular 2-D array (m x n) over a numeric field (ℝ or ℂ).
   - Vector  : 1-D array length n over same field.
   - Scalar  : element of the field.
   - Aᴴ      : conjugate transpose (Hermitian adjoint). For real matrices Aᴴ = Aᵀ.
   - ⊙       : Hadamard (element-wise) product.
   - ⊗       : Kronecker (tensor) product.
   
  Implementations may return concrete Clojure data (vectors of vectors, maps with
  :real/:imag parts, records, or opaque handles). Callers should treat returned
  values as opaque and use protocol functions for further manipulation.
 
  All functions are expected to be pure with respect to their arguments (i.e.
  no in-place mutation visible to the caller) unless the backend documents an
  optimization that preserves semantic immutability (copy-on-write, etc.)."

  ;; Structural & basic arithmetic 
  (shape [backend A]
    "Return the shape of matrix A as [rows cols]. May be O(1).")

  (add [backend A B]
    "Matrix addition: A + B. Shapes must match. Returns a new matrix.")

  (subtract [backend A B]
    "Matrix subtraction: A - B. Shapes must match. Returns a new matrix.")

  (scale [backend A alpha]
    "Scalar multiplication: alpha · A. Returns a new matrix.")

  (negate [backend A]
    "Additive inverse: -A. Equivalent to (scale backend A -1). Implement for speed.")

  ;; --- Multiplicative operations -----------------------------------------------------
  (matrix-multiply [backend A B]
    "Matrix product: A × B. If A is (m×k) and B is (k×n) returns (m×n).")

  (matrix-vector-product [backend A x]
    "Matrix–vector product: A × x. If A is (m×n) and x length n returns vector length m.")

  (outer-product [backend x y]
    "Outer product: x ⊗ y† producing (m×n) matrix (without conjugation of x; y† applies conjugate if complex). For real vectors reduces to x yᵀ.")

  (hadamard [backend A B]
    "Element-wise (Hadamard) product: A ⊙ B. Shapes must match.")

  (kronecker [backend A B]
    "Kronecker (tensor) product: A ⊗ B. If A (m×n), B (p×q) => (mp×nq).")

  ;; --- Structural transforms
  (transpose [backend A]
    "Transpose: Aᵀ. Swaps rows/cols without conjugation.")

  (conjugate-transpose [backend A]
    "Conjugate transpose: Aᴴ. For real A this MUST return (transpose* backend A).")

  ;; Reductions / scalar results
  (trace [backend A]
    "Trace: Tr(A) = Σᵢ aᵢᵢ. Requires square matrix. Returns scalar (ℝ or ℂ).")

  (inner-product [backend x y]
    "Vector inner product ⟨x|y⟩. Conjugates first argument if complex (Dirac bra-ket).
    Returns scalar over the field.")

  (norm2 [backend x]
    "Euclidean (L2) norm of vector: ||x||₂ = sqrt(⟨x|x⟩). Returns non-negative real.")

  ;; Linear solves / inverses
  (solve-linear-system [backend A b]
    "Solve linear system A x = b. A square (n×n), b length n (matrix or vector form).
    Returns solution vector x or may throw/return nil for singular/ill-conditioned A.")

  (inverse [backend A]
    "Matrix inverse A⁻¹. A square, non-singular. Returns matrix or nil if singular.")

  ;; Predicates (tolerance-based)
  (hermitian? [backend A] [backend A eps]
    "True if A ≈ Aᴴ within tolerance eps (default backend tolerance). Real symmetric => true.")

  (unitary? [backend U] [backend U eps]
    "True if Uᴴ U ≈ I within tolerance eps  (default backend tolerance). Square matrix.")

  (positive-semidefinite? [backend A]
    "True if A is Hermitian and all eigenvalues ≥ -eps (backend tolerance).")

  ;
  )

;; Decomposition protocol
;; Decompositions orthogonal to core algebra
(defprotocol MatrixDecompositions
  "Matrix decomposition operations.
  Returned data structures should be documented by each backend; suggested shapes:
  - eigen-hermitian*: {:eigenvalues (vector λ₀≤…≤λₙ₋₁) :eigenvectors [v₀ …]}
  - svd*: {:U U :S (vector singular-values descending) :V† V-hermitian}"

  (eigen-hermitian [backend A]
    "Eigendecomposition of Hermitian matrix A. Ascending eigenvalues preferred.")

  (eigen-general [backend A]
    "General (possibly complex) eigen decomposition. May throw if unsupported.")

  (svd [backend A]
    "Singular Value Decomposition A = U Σ Vᴴ. Σ returned as descending singular values.")

  (lu-decomposition [backend A]
    "LU decomposition A = P L U with row permutation P, lower triangular L, upper triangular U.")

  (qr-decomposition [backend A]
    "QR decomposition A = Q R with orthogonal Q, upper triangular R. May return
    (Q R) or (Q R Qᴴ) depending on backend conventions. Q is orthogonal/unitary, R upper triangular.")

  (cholesky-decomposition [backend A]
    "Cholesky decomposition A = L Lᴴ for positive semidefinite A.
    Returns lower triangular L (Lᴴ = Lᵀ for real A). If A is not positive semidefinite,
    may throw or return nil depending on backend semantics.")
  ;
  )

;; Matrix function protocol
(defprotocol MatrixFunctions
  "Analytic functions of matrices (functional calculus). Implementations may
  approximate using Pade, scaling & squaring, Schur decomposition, etc."

  (matrix-exp [backend A]
    "Matrix exponential exp(A).")

  (matrix-log [backend A]
    "Principal matrix logarithm log(A). Domain: matrices without negative/zero eigenvalues (unless handled).")

  (matrix-sqrt [backend A]
    "Principal matrix square root √A for positive semidefinite / suitable matrices."))

;; Analysis protocol
;; Additional analyses beyond predicates embedded in MatrixAlgebra
(defprotocol MatrixAnalysis
  "Higher-level scalar analyses / norms / conditioning metrics."

  (spectral-norm [backend A]
    "Spectral norm ||A||₂ = largest singular value σ₀.")

  (condition-number [backend A]
    "Condition number κ₂(A) = σ_max / σ_min (finite, σ_min>0).")

  ;
  )

;;;
;;; Quantum state operations protocol
;;;
(defprotocol QuantumStateOps
  "Quantum state / density matrix operations."
  ;; Quantum related helpers (optional fast-path overrides)
  (state-normalize [backend state]
    "Return normalized state vector (||ψ||₂ = 1). Zero vector returned unchanged.")

  (projector-from-state [backend psi]
    "Projector |ψ⟩⟨ψ| as matrix over field. Uses outer-product semantics.")

  (density-matrix [backend psi]
    "Density matrix ρ for pure state ψ (alias of projector). Returned matrix trace = 1.")

  (trace-one? [backend rho] [backend rho eps]
    "Predicate: Tr(ρ) ≈ 1 (within tolerance).")
    ;
  )
