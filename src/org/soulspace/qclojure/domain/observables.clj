(ns org.soulspace.qclojure.domain.observables
  "Domain for quantum observables - Hermitian operators representing measurable quantities.
   
   Observables in quantum mechanics are represented by Hermitian matrices.
   This namespace provides functions for creating and manipulating observables,
   calculating expectation values, and working with composite observables.
   
   Design Philosophy:
   - Observables are simple data (matrices) - no complex wrapping
   - Pure functions for all operations
   - Compose complex observables from simple primitives
   - Separate representation, computation, and measurement concerns"
  (:require [clojure.spec.alpha :as s]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.gate :as gate]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]))

;;;
;;; Specs for Observables
;;;
(s/def ::matrix
  (s/and vector?
         (s/coll-of (s/coll-of ::state/complex-amplitude))))

(s/def ::observable
  ::matrix)

(s/def ::pauli-string
  (s/and string?
         #(every? #{\I \X \Y \Z} %)))

;;;
;;; Pauli Operators and String Utilities
;;;
(def pauli-operators
  "The four single-qubit Pauli operators: Identity, X (bit-flip), Y (both), Z (phase-flip)"
  #{:I :X :Y :Z})

(s/def ::pauli-operator pauli-operators)

(defn pauli-string?
  "Check if a string is a valid Pauli string.
   
   A valid Pauli string contains only the characters I, X, Y, Z representing
   the Pauli operators.
   
   Parameters:
   - s: String to check
   
   Returns:
   Boolean indicating validity
   
   Example:
     (pauli-string? \"XYZII\") ; => true
     (pauli-string? \"ABC\")   ; => false"
  [s]
  (and (string? s)
       (every? (fn [c] (contains? #{\I \X \Y \Z} c)) s)))

(defn pauli-weight
  "Calculate the weight of a Pauli string (number of non-identity operators).
   
   The weight is the number of qubits on which non-trivial operations are performed.
   This is an important metric in quantum error correction and algorithm analysis.
   
   Parameters:
   - pauli-str: Pauli string like 'XYZII'
   
   Returns:
   Integer count of non-I operators
   
   Example:
     (pauli-weight \"XYZII\") ; => 3
     (pauli-weight \"IIIII\") ; => 0"
  [pauli-str]
  {:pre [(pauli-string? pauli-str)]}
  (count (filter #(not= \I %) pauli-str)))

(defn pauli-commute?
  "Check if two Pauli operators commute.
   
   Two Pauli strings commute if they differ on an even number of positions
   where both have non-identity, non-equal operators. This is crucial for:
   - Error correction syndrome measurements
   - Hamiltonian term grouping for simultaneous measurement
   - Quantum algorithm optimization
   
   Parameters:
   - p1: First Pauli string
   - p2: Second Pauli string (must be same length as p1)
   
   Returns:
   Boolean indicating if operators commute
   
   Examples:
     (pauli-commute? \"XII\" \"IXI\") ; => true (differ at 0 positions)
     (pauli-commute? \"XII\" \"ZII\") ; => false (differ at 1 position)
     (pauli-commute? \"XYZ\" \"ZYX\") ; => false (differ at 2 positions)"
  [p1 p2]
  {:pre [(pauli-string? p1)
         (pauli-string? p2)
         (= (count p1) (count p2))]}
  (let [diff-count (count (filter (fn [[c1 c2]]
                                    (and (not= c1 \I)
                                         (not= c2 \I)
                                         (not= c1 c2)))
                                  (map vector p1 p2)))]
    (even? diff-count)))

;;;
;;; Basic Single-Qubit Observables
;;;
(def pauli-x
  "Pauli-X observable (σₓ)"
  gate/pauli-x)

(def pauli-y
  "Pauli-Y observable (σᵧ)"
  gate/pauli-y)

(def pauli-z
  "Pauli-Z observable (σᵤ)"
  gate/pauli-z)

(def identity-op
  "Identity observable (I)"
  gate/pauli-i)

;;;
;;; Standard computational basis projectors
;;;
(def projector-0
  "Projector onto |0⟩ state"
  [[fc/ONE fc/ZERO]
   [fc/ZERO fc/ZERO]])

(def projector-1
  "Projector onto |1⟩ state"
  [[fc/ZERO fc/ZERO]
   [fc/ZERO fc/ONE]])

;;;
;;; Helper Functions for Matrix Operations
;;;
(defn zero-matrix
  "Create a zero matrix of complex numbers of given dimensions"
  [rows cols]
  (vec (repeat rows (vec (repeat cols fc/ZERO)))))

(defn matrix-equal?
  "Check if two matrices of complex numbers are equal within tolerance"
  [m1 m2]
  (let [tolerance 1e-10]
    (and (= (count m1) (count m2))
         (= (count (first m1)) (count (first m2)))
         (every? (fn [i]
                   (every? (fn [j]
                             (let [elem1 (get-in m1 [i j])
                                   elem2 (get-in m2 [i j])
                                   diff (fc/abs (fc/sub elem1 elem2))]
                               (< diff tolerance)))
                           (range (count (first m1)))))
                 (range (count m1))))))

;;;
;;; Observable Creation Functions
;;;
(defn linear-combination
  "Create a linear combination of observables: Σᵢ cᵢ Oᵢ
   
   Parameters:
   - coeffs-observables: sequence of [coefficient observable] pairs
   
   Returns:
     Matrix representing the linear combination
   
   Example:
     (linear-combination [[0.5 pauli-x] [0.5 pauli-z]])"
  [coeffs-observables]
  {:pre [(s/valid? (s/coll-of (s/tuple number? ::observable)) coeffs-observables)]}
  (reduce
   (fn [result [coeff obs]]
     (cla/add result (cla/scale obs coeff)))
   (zero-matrix (count (first (second (first coeffs-observables))))
                (count (second (first coeffs-observables))))
   coeffs-observables))

(defn tensor-product
  "Create tensor product of observables: O₁ ⊗ O₂ ⊗ ... ⊗ Oₙ
   
   Parameters:
   - observables: sequence of observable matrices
   
   Returns:
     Matrix representing the tensor product
   
   Example:
     (tensor-product [pauli-x pauli-z])"
  [observables]
  {:pre [(s/valid? (s/coll-of ::observable) observables)]}
  (reduce cla/kronecker-product observables))

;;;
;;; Pauli String Functions
;;;
(defn pauli-char->matrix
  "Convert a single Pauli character to its matrix representation"
  [pauli-char]
  (case pauli-char
    \I identity-op
    \X pauli-x
    \Y pauli-y
    \Z pauli-z))

(defn pauli-string->observable
  "Convert a Pauli string to an observable matrix
   
   Parameters:
   - pauli-str: string like 'XZYI' representing multi-qubit Pauli operator
   
   Returns:
     Matrix representing the tensor product of Pauli matrices
   
   Example:
     (pauli-string->observable 'XZ') ; creates X ⊗ Z"
  [pauli-str]
  {:pre [(s/valid? ::pauli-string pauli-str)]}
  (tensor-product (map pauli-char->matrix pauli-str)))

;;;
;;; Observable Measurement and Analysis
;;;
(defn expectation-value
  "Calculate expectation value ⟨ψ|O|ψ⟩ of observable O in state ψ
   
   Parameters:
   - observable: Hermitian matrix representing the observable
   - quantum-state: quantum state vector (normalized)
   
   Returns:
     Real number representing the expectation value
   
   Example:
     (expectation-value pauli-z |0⟩) ; returns 1.0"
  [observable quantum-state]
  {:pre [(s/valid? ::observable observable)
         (s/valid? ::state/state quantum-state)]}
  (let [state-vec (:state-vector quantum-state)
        obs-psi (cla/matrix-vector-product observable state-vec)]
    (fc/re (cla/inner-product state-vec obs-psi))))

(defn expectation-value-density-matrix
  "Calculate expectation value Tr(ρO) of observable O for density matrix ρ.
   
   For density matrices (mixed states), the expectation value is calculated as
   the trace of the product of the density matrix and the observable: Tr(ρO).
   
   This function supports both single density matrices and collections of density 
   matrices with optional weights for ensemble averaging.
   
   Parameters:
   - observable: Hermitian matrix representing the observable
   - density-matrix-or-collection: Either a single density matrix or collection of matrices
   - weights: (optional) Collection of weights for weighted averaging. If not provided,
             uniform weighting is used for collections.
   
   Returns:
     For single matrix: Real number representing the expectation value
     For collection: Map with :mean, :std-dev, :variance, :weights, :individual-values
   
   Examples:
     (expectation-value-density-matrix pauli-z single-density-matrix)
     (expectation-value-density-matrix pauli-z [dm1 dm2 dm3])
     (expectation-value-density-matrix pauli-z [dm1 dm2] [0.3 0.7])"
  ([observable density-matrix]
   {:pre [(s/valid? ::observable observable)]}
   (if (and (coll? density-matrix) (not (vector? (first density-matrix))))
     ;; Collection of density matrices - uniform weighting
     (expectation-value-density-matrix observable density-matrix nil)
     ;; Single density matrix
     (fc/re (cla/trace (cla/matrix-multiply density-matrix observable)))))
  ([observable density-matrices weights]
   {:pre [(s/valid? ::observable observable)
          (coll? density-matrices)
          (or (nil? weights) (and (coll? weights) (= (count weights) (count density-matrices))))]}
   (let [num-matrices (count density-matrices)
         actual-weights (or weights (repeat num-matrices (/ 1.0 num-matrices)))
         individual-values (mapv #(fc/re (cla/trace (cla/matrix-multiply % observable)))
                                 density-matrices)
         weighted-mean (reduce + (map * actual-weights individual-values))
         variance (if (> num-matrices 1)
                    (let [diff-squares (map #(* %1 (Math/pow (- %2 weighted-mean) 2))
                                            actual-weights individual-values)]
                      (/ (reduce + diff-squares) (- 1.0 (reduce + (map #(* % %) actual-weights)))))
                    0.0)
         std-dev (Math/sqrt variance)]
     {:mean weighted-mean
      :std-dev std-dev
      :variance variance
      :weights (vec actual-weights)
      :individual-values individual-values
      :count num-matrices})))

(defn variance
  "Calculate variance of observable: ⟨O²⟩ - ⟨O⟩²
   
   Parameters:
   - observable: Hermitian matrix representing the observable
   - quantum-state: quantum state vector (normalized)
   
   Returns:
     Real number representing the variance"
  [observable quantum-state]
  {:pre [(s/valid? ::observable observable)
         (s/valid? ::state/state quantum-state)]}
  (let [exp-val (expectation-value observable quantum-state)
        obs-squared (cla/matrix-multiply observable observable)
        exp-val-squared (expectation-value obs-squared quantum-state)]
    (- exp-val-squared (* exp-val exp-val))))

;;;
;;; Measurement Simulation
;;;
(defn measurement-probabilities
  "Calculate measurement probabilities for an observable's eigenvalues using eigendecomposition.
   
   This function computes the probability of measuring each eigenvalue of a Hermitian 
   observable when the system is in the given quantum state. The probabilities are 
   calculated as |⟨vᵢ|ψ⟩|² where vᵢ are the eigenvectors and ψ is the state.
   
   Parameters:
   - observable: Hermitian matrix representing the observable
   - quantum-state: quantum state vector (normalized)
   
   Returns:
     Map with keys representing eigenvalues and values representing probabilities
   
   Example:
     (measurement-probabilities pauli-z |+⟩) ; => {1.0 0.5, -1.0 0.5}"
  [observable quantum-state]
  {:pre [(s/valid? ::observable observable)
         (s/valid? ::state/state quantum-state)]}
  (let [state-vec (:state-vector quantum-state)
        {:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian observable)]
    ;; Calculate |⟨vᵢ|ψ⟩|² for each eigenvector vᵢ
    (into {}
          (map (fn [eigenval eigenvec]
                 (let [overlap (cla/inner-product eigenvec state-vec)
                       prob (fc/re (fc/mult overlap (fc/conjugate overlap)))
                       real-eigenval (fc/re eigenval)]
                   [real-eigenval prob]))
               eigenvalues eigenvectors))))

(comment

  ;; Basic Pauli observables
  pauli-x  ; σₓ (bit-flip)
  pauli-y  ; σᵧ (bit and phase flip)
  pauli-z  ; σᵤ (phase flip)
  identity-op  ; I (identity)

  ;; Single-qubit expectation values
  (expectation-value pauli-z state/|0⟩)  ; => 1.0
  (expectation-value pauli-z state/|1⟩)  ; => -1.0
  (expectation-value pauli-z state/|+⟩)  ; => 0.0
  (expectation-value pauli-x state/|+⟩)  ; => 1.0

  ;; Multi-qubit Pauli string observables
  (def xz-observable (pauli-string->observable "XZ"))  ; X ⊗ Z
  (def zzx-observable (pauli-string->observable "ZZX")) ; Z ⊗ Z ⊗ X

  ;; Linear combinations (custom Hamiltonians)
  (def custom-hamiltonian
    (linear-combination [[1.5 pauli-x]
                         [2.0 pauli-z]
                         [0.5 identity-op]]))

  ;; Bell measurement observable: (Z⊗I + I⊗Z)/2
  (def bell-measurement
    (linear-combination
     [[0.5 (tensor-product [pauli-z identity-op])]
      [0.5 (tensor-product [identity-op pauli-z])]]))

  ;; Expectation values and variances
  (expectation-value custom-hamiltonian state/|+⟩)
  (variance pauli-z state/|+⟩)  ; => 1.0 (maximum uncertainty)
  (variance pauli-z state/|0⟩)  ; => 0.0 (no uncertainty)

  ;; Measurement probabilities
  (measurement-probabilities pauli-z state/|+⟩)  ; => {1.0 0.5, -1.0 0.5}
  (measurement-probabilities pauli-z state/|0⟩)  ; => {1.0 1.0, -1.0 0.0}

  ;; Verify observables are Hermitian
  (cla/hermitian? pauli-x)        ; => true
  (cla/hermitian? custom-hamiltonian) ; => true

  ;; Test with Bell states
  (def bell-state (state/normalize-state
                   (state/multi-qubit-state
                    [(fc/complex 1.0) (fc/complex 0.0)
                     (fc/complex 0.0) (fc/complex 1.0)])))

  (expectation-value (pauli-string->observable "ZZ") bell-state)  ; => 1.0

  ;; Performance testing
  (time (pauli-string->observable "XYZIXYZIXYZ"))  ; Large multi-qubit observable
  )