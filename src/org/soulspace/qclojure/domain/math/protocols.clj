(ns org.soulspace.qclojure.domain.math.protocols
  "Core mathematical protocols for matrix algebra, decompositions, and quantum state operations.
  
  Provides a contract for implementing backends to support linear algebra operations
  over numeric fields (ℝ or ℂ) with a focus on immutability and pure functions."
  (:require
   [fastmath.complex :as complex]))

;;;
;;; Backend adapter
;;;
(defprotocol BackendAdapter
  "Protocol for converting between QClojure and backend-specific representations.
  
  Enables seamless interoperability between different mathematical backends
  while maintaining a consistent API at the QClojure level."

  (vector->backend [backend v]
    "Convert a QClojure vector to backend-specific representation.
    
    Parameters:
    - backend: Backend instance
    - v: QClojure vector (sequence of numbers or complex map)
    
    Returns:
    Vector in the backend's native format")

  (backend->vector [backend v]
    "Convert a backend vector to QClojure representation.
    
    Parameters:
    - backend: Backend instance  
    - v: Vector in backend's native format
    
    Returns:
    QClojure vector (sequence of numbers or complex map)")

  (matrix->backend [backend m]
    "Convert a QClojure matrix to backend-specific representation.
    
    Parameters:
    - backend: Backend instance
    - m: QClojure matrix (vector of row vectors or complex map)
    
    Returns:
    Matrix in the backend's native format")

  (backend->matrix [backend m]
    "Convert a backend matrix to QClojure representation.
    
    Parameters:
    - backend: Backend instance
    - m: Matrix in backend's native format
    
    Returns:
    QClojure matrix (vector of row vectors or complex map)")

  (scalar->backend [backend s]
    "Convert a QClojure scalar to backend-specific representation.
    
    Parameters:
    - backend: Backend instance
    - s: QClojure scalar (number or Vec2 complex)
    
    Returns:
    Scalar in the backend's native format")

  (backend->scalar [backend s]
    "Convert a backend scalar to QClojure representation.
    
    Parameters:
    - backend: Backend instance
    - s: Scalar in backend's native format
    
    Returns:
    QClojure scalar (number or Vec2 complex)"))

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
    "Return the dimensions of a matrix.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    Vector [rows cols] indicating matrix dimensions")

  (add [backend A B]
    "Perform matrix addition A + B.
    
    Parameters:
    - backend: Backend instance
    - A: First matrix (real or complex)
    - B: Second matrix (real or complex) with same dimensions as A
    
    Returns:
    New matrix representing A + B")

  (subtract [backend A B]
    "Perform matrix subtraction A - B.
    
    Parameters:
    - backend: Backend instance
    - A: First matrix (real or complex)
    - B: Second matrix (real or complex) with same dimensions as A
    
    Returns:
    New matrix representing A - B")

  (scale [backend A alpha]
    "Perform scalar multiplication alpha · A.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    - alpha: Scalar value (real or complex number)
    
    Returns:
    New matrix representing alpha · A")

  (negate [backend A]
    "Compute the additive inverse -A.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    New matrix representing -A
    
    Note:
    Equivalent to (scale backend A -1) but may be optimized")

  ;; --- Multiplicative operations -----------------------------------------------------
  (matrix-multiply [backend A B]
    "Perform matrix multiplication A × B.
    
    Parameters:
    - backend: Backend instance
    - A: Left matrix (m×k, real or complex)
    - B: Right matrix (k×n, real or complex) with compatible dimensions
    
    Returns:
    New matrix (m×n) representing the product A × B")

  (matrix-vector-product [backend A x]
    "Perform matrix–vector multiplication A × x.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (m×n, real or complex)
    - x: Vector (length n, real or complex) with compatible dimensions
    
    Returns:
    New vector (length m) representing the product A × x")

  (inner-product [backend x y]
               "Compute the vector inner product ⟨x|y⟩.
    
    Parameters:
    - backend: Backend instance
    - x: First vector (real or complex)
    - y: Second vector (real or complex) with same length as x
    
    Returns:
    Scalar representing the inner product
    
    Note:
    Conjugates the first argument if complex (follows Dirac bra-ket notation)")

  (outer-product [backend x y]
    "Compute the outer product x ⊗ y†.
    
    Parameters:
    - backend: Backend instance
    - x: First vector (length m, real or complex)
    - y: Second vector (length n, real or complex)
    
    Returns:
    New matrix (m×n) representing x ⊗ y†
    
    Note:
    For complex vectors, conjugates y but not x (follows Dirac bra-ket notation)")

  (hadamard-product [backend A B]
    "Compute the element-wise (Hadamard) product A ⊙ B.
    
    Parameters:
    - backend: Backend instance
    - A: First matrix (real or complex)
    - B: Second matrix (real or complex) with same dimensions as A
    
    Returns:
    New matrix with element-wise multiplication of A and B")

  (kronecker-product [backend A B]
    "Compute the Kronecker (tensor) product A ⊗ B.
    
    Parameters:
    - backend: Backend instance
    - A: First matrix (m×n, real or complex)
    - B: Second matrix (p×q, real or complex)
    
    Returns:
    New matrix (mp×nq) representing the Kronecker product A ⊗ B
    
    Note:
    Essential for quantum computing multi-qubit operations")

  ;; --- Structural transforms
  (transpose [backend A]
    "Compute the transpose Aᵀ.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    New matrix representing the transpose of A (swaps rows/cols without conjugation)")

  (conjugate-transpose [backend A]
    "Compute the conjugate transpose Aᴴ (Hermitian adjoint).
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    New matrix representing the conjugate transpose of A
    
    Note:
    For real matrices, this is equivalent to transpose")

  ;; Reductions / scalar results
  (trace [backend A]
    "Compute the trace Tr(A) = Σᵢ aᵢᵢ.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Scalar (real or complex) representing the sum of diagonal elements")

  (norm2 [backend x]
    "Compute the Euclidean (L2) norm ||x||₂.
    
    Parameters:
    - backend: Backend instance
    - x: Vector (real or complex)
    
    Returns:
    Non-negative real number representing ||x||₂ = sqrt(⟨x|x⟩)")

  ;; Linear solves / inverses
  (solve-linear-system [backend A b]
    "Solve the linear system A x = b.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (n×n, real or complex), must be non-singular
    - b: Right-hand side vector (length n) or matrix
    
    Returns:
    Solution vector x or matrix, or nil/throws if A is singular or ill-conditioned")

  (inverse [backend A]
    "Compute the matrix inverse A⁻¹.
    
    Parameters:
    - backend: Backend instance
    - A: Square non-singular matrix (real or complex)
    
    Returns:
    Inverse matrix A⁻¹, or nil if A is singular")

  ;; Predicates (tolerance-based)
  (hermitian? [backend A] [backend A eps]
    "Test if a matrix is Hermitian (A ≈ Aᴴ).
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    - eps: Optional tolerance for approximate equality (uses backend default if not provided)
    
    Returns:
    Boolean indicating whether A is Hermitian (or real symmetric)")

  (unitary? [backend U] [backend U eps]
    "Test if a matrix is unitary (Uᴴ U ≈ I).
    
    Parameters:
    - backend: Backend instance
    - U: Square matrix (real or complex)
    - eps: Optional tolerance for approximate equality (uses backend default if not provided)
    
    Returns:
    Boolean indicating whether U is unitary (or orthogonal for real matrices)")

  (positive-semidefinite? [backend A]
    "Test if a matrix is positive semidefinite.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex), should be Hermitian
    
    Returns:
    Boolean indicating whether all eigenvalues of A are ≥ -tolerance
    
    Note:
    Matrix should be Hermitian for meaningful results")

  ;
  )

;; Decomposition protocol
;; Decompositions orthogonal to core algebra
(defprotocol MatrixDecompositions
  "Protocol for matrix decomposition operations.
  
  Provides standard numerical linear algebra decompositions that are fundamental
  for many computational applications, especially in quantum computing."

  (eigen-hermitian [backend A]
    "Compute the eigendecomposition of a Hermitian matrix.
    
    Parameters:
    - backend: Backend instance
    - A: Square Hermitian matrix (real symmetric or complex Hermitian)
    
    Returns:
    Map containing:
    - :eigenvalues - Vector of eigenvalues in ascending order [λ₀≤...≤λₙ₋₁]
    - :eigenvectors - Vector of corresponding eigenvector columns [v₀ ...]")

  (eigen-general [backend A]
    "Compute eigenvalues for a general (possibly non-Hermitian) matrix.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Map containing eigenvalue information (backend-specific format)
    
    Note:
    May throw if the backend doesn't support general eigenvalue computation")

  (svd [backend A]
    "Compute the Singular Value Decomposition A = U Σ Vᴴ.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex, may be rectangular)
    
    Returns:
    Map containing:
    - :U - Left singular vectors matrix
    - :S - Vector of singular values in descending order
    - :V† - Right singular vectors conjugate transpose matrix")

  (lu-decomposition [backend A]
    "Compute the LU decomposition A = P L U.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Map containing:
    - :P - Row permutation matrix
    - :L - Lower triangular matrix
    - :U - Upper triangular matrix")

  (qr-decomposition [backend A]
    "Compute the QR decomposition A = Q R.
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    Map containing:
    - :Q - Orthogonal/unitary matrix
    - :R - Upper triangular matrix
    
    Note:
    Backend may return additional components depending on implementation")

  (cholesky-decomposition [backend A]
    "Compute the Cholesky decomposition A = L Lᴴ.
    
    Parameters:
    - backend: Backend instance
    - A: Positive semidefinite matrix (real or complex)
    
    Returns:
    Map containing:
    - :L - Lower triangular matrix such that A = L Lᴴ
    
    Note:
    May throw or return nil if A is not positive semidefinite"))

;; Matrix function protocol
(defprotocol MatrixFunctions
  "Protocol for analytic functions of matrices.
  
  Provides matrix-valued functions using functional calculus. Implementations may
  use various approximation methods such as Padé approximants, scaling & squaring,
  or Schur decomposition."

  (matrix-exp [backend A]
    "Compute the matrix exponential exp(A).
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Matrix representing exp(A)
    
    Note:
    Important for quantum time evolution operators and solving differential equations")

  (matrix-log [backend A]
    "Compute the principal matrix logarithm log(A).
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Matrix representing the principal branch of log(A)
    
    Note:
    Domain restrictions apply for matrices with negative or zero eigenvalues")

  (matrix-sqrt [backend A]
    "Compute the principal matrix square root √A.
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Matrix representing the principal square root of A
    
    Note:
    Best defined for positive semidefinite matrices"))

;; Analysis protocol
;; Additional analyses beyond predicates embedded in MatrixAlgebra
(defprotocol MatrixAnalysis
  "Protocol for matrix analysis and conditioning metrics.
  
  Provides scalar-valued analyses that measure various properties of matrices,
  particularly useful for numerical stability assessments."

  (spectral-norm [backend A]
    "Compute the spectral norm ||A||₂ (largest singular value).
    
    Parameters:
    - backend: Backend instance
    - A: Matrix (real or complex)
    
    Returns:
    Non-negative real number representing the largest singular value σ₀")

  (condition-number [backend A]
    "Compute the 2-norm condition number κ₂(A).
    
    Parameters:
    - backend: Backend instance
    - A: Square matrix (real or complex)
    
    Returns:
    Positive real number representing κ₂(A) = σ_max / σ_min
    
    Note:
    Higher values indicate numerical instability in linear system solutions"))

