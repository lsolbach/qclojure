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
            [org.soulspace.qclojure.domain.math.core :as mcore]))

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
      (mcore/add result (mcore/scale obs coeff)))
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
  (reduce mcore/kronecker-product observables))

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
         (s/valid? ::state/quantum-state quantum-state)]}
  (let [state-vec (:state-vector quantum-state)
        obs-psi (mcore/matrix-vector-product observable state-vec)]
    (fc/re (mcore/inner-product state-vec obs-psi))))

(defn variance
  "Calculate variance of observable: ⟨O²⟩ - ⟨O⟩²
   
   Parameters:
   - observable: Hermitian matrix representing the observable
   - quantum-state: quantum state vector (normalized)
   
   Returns:
     Real number representing the variance"
  [observable quantum-state]
  {:pre [(s/valid? ::observable observable)
         (s/valid? ::state/quantum-state quantum-state)]}
  (let [exp-val (expectation-value observable quantum-state)
        obs-squared (mcore/matrix-multiply observable observable)
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
         (s/valid? ::state/quantum-state quantum-state)]}
  (let [state-vec (:state-vector quantum-state)
        {:keys [eigenvalues eigenvectors]} (mcore/eigen-hermitian observable)]
    ;; Calculate |⟨vᵢ|ψ⟩|² for each eigenvector vᵢ
    (into {}
          (map (fn [eigenval eigenvec]
                 (let [overlap (mcore/inner-product eigenvec state-vec)
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
  (mcore/hermitian? pauli-x)        ; => true
  (mcore/hermitian? custom-hamiltonian) ; => true

  ;; Test with Bell states
  (def bell-state (state/normalize-state
                   (state/multi-qubit-state
                    [(fc/complex 1.0) (fc/complex 0.0)
                     (fc/complex 0.0) (fc/complex 1.0)])))

  (expectation-value (pauli-string->observable "ZZ") bell-state)  ; => 1.0

  ;; Performance testing
  (time (pauli-string->observable "XYZIXYZIXYZ"))  ; Large multi-qubit observable

  )