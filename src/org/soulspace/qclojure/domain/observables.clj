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
            [org.soulspace.qclojure.domain.math.core :as math]))

;;
;; Specs for Observables
;;
(s/def ::matrix
  (s/and vector?
         (s/coll-of (s/coll-of ::state/complex-amplitude))))

(s/def ::observable
  ::matrix)

(s/def ::pauli-string
  (s/and string?
         #(every? #{\I \X \Y \Z} %)))

;;
;; Basic Single-Qubit Observables
;;
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

;; Standard computational basis projectors
(def projector-0
  "Projector onto |0⟩ state"
  [[fc/ONE fc/ZERO]
   [fc/ZERO fc/ZERO]])

(def projector-1
  "Projector onto |1⟩ state"  
  [[fc/ZERO fc/ZERO]
   [fc/ZERO fc/ONE]])

;;
;; Helper Functions for Matrix Operations
;;
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

;;
;; Observable Creation Functions
;;
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
      (math/add result (math/scale obs coeff)))
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
  (reduce math/kronecker observables))

;;
;; Pauli String Functions
;;
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

;;
;; Observable Measurement and Analysis
;;
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
        obs-psi (math/matrix-vector observable state-vec)]
    (fc/re (math/inner-product state-vec obs-psi))))

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
        obs-squared (math/matrix-multiply observable observable)
        exp-val-squared (expectation-value obs-squared quantum-state)]
    (- exp-val-squared (* exp-val exp-val))))

(defn is-hermitian?
  "Check if a matrix is Hermitian (O = O†)
   
   Parameters:
   - matrix: matrix to check
   
   Returns:
     Boolean indicating if matrix is Hermitian"
  [matrix]
  {:pre [(s/valid? ::matrix matrix)]}
  (math/hermitian? matrix))

;;
;; Measurement Simulation
;;
(defn measurement-probabilities
  "Calculate measurement probabilities for an observable's eigenvalues
   
   For a two-level system observable, returns probabilities for +1 and -1 eigenvalues.
   This is a simplified version - full implementation would require eigendecomposition.
   
   Parameters:
   - observable: Hermitian matrix (currently supports Pauli observables)
   - quantum-state: quantum state vector
   
   Returns:
     Map with keys representing eigenvalues and values representing probabilities"
  [observable quantum-state]
  {:pre [(s/valid? ::observable observable)
         (s/valid? ::state/quantum-state quantum-state)]}
  (let [exp-val (expectation-value observable quantum-state)]
    (cond
      ;; For Pauli observables (eigenvalues ±1)
      (or (matrix-equal? observable pauli-x)
          (matrix-equal? observable pauli-y)
          (matrix-equal? observable pauli-z))
      {1.0 (/ (+ 1.0 exp-val) 2.0)
       -1.0 (/ (- 1.0 exp-val) 2.0)}
      
      ;; For identity (eigenvalue 1)
      (matrix-equal? observable identity-op)
      {1.0 1.0}
      
      ;; Default: return expectation value as single outcome
      :else
      {exp-val 1.0})))

(comment
  ;; Test basic observables
  (require '[org.soulspace.qclojure.domain.observables :as obs] :reload)
  
  ;; Basic Pauli observables
  obs/pauli-x  ; σₓ (bit-flip)
  obs/pauli-y  ; σᵧ (bit and phase flip)
  obs/pauli-z  ; σᵤ (phase flip)
  obs/identity-op  ; I (identity)
  
  ;; Single-qubit expectation values
  (obs/expectation-value obs/pauli-z state/|0⟩)  ; => 1.0
  (obs/expectation-value obs/pauli-z state/|1⟩)  ; => -1.0
  (obs/expectation-value obs/pauli-z state/|+⟩)  ; => 0.0
  (obs/expectation-value obs/pauli-x state/|+⟩)  ; => 1.0
  
  ;; Multi-qubit Pauli string observables
  (def xz-observable (obs/pauli-string->observable "XZ"))  ; X ⊗ Z
  (def zzx-observable (obs/pauli-string->observable "ZZX")) ; Z ⊗ Z ⊗ X
  
  ;; Linear combinations (custom Hamiltonians)
  (def custom-hamiltonian 
    (obs/linear-combination [[1.5 obs/pauli-x] 
                             [2.0 obs/pauli-z] 
                             [0.5 obs/identity-op]]))
  
  ;; Bell measurement observable: (Z⊗I + I⊗Z)/2
  (def bell-measurement
    (obs/linear-combination 
      [[0.5 (obs/tensor-product [obs/pauli-z obs/identity-op])]
       [0.5 (obs/tensor-product [obs/identity-op obs/pauli-z])]]))
  
  ;; Expectation values and variances
  (obs/expectation-value custom-hamiltonian state/|+⟩)
  (obs/variance obs/pauli-z state/|+⟩)  ; => 1.0 (maximum uncertainty)
  (obs/variance obs/pauli-z state/|0⟩)  ; => 0.0 (no uncertainty)
  
  ;; Measurement probabilities
  (obs/measurement-probabilities obs/pauli-z state/|+⟩)  ; => {1.0 0.5, -1.0 0.5}
  (obs/measurement-probabilities obs/pauli-z state/|0⟩)  ; => {1.0 1.0, -1.0 0.0}
  
  ;; Verify observables are Hermitian
  (obs/is-hermitian? obs/pauli-x)        ; => true
  (obs/is-hermitian? custom-hamiltonian) ; => true
  
  ;; Test with Bell states
  (def bell-state (state/normalize-state 
                    (state/multi-qubit-state 
                      [(fc/complex 1.0) (fc/complex 0.0) 
                       (fc/complex 0.0) (fc/complex 1.0)])))
  
  (obs/expectation-value (obs/pauli-string->observable "ZZ") bell-state)  ; => 1.0
  
  ;; Performance testing
  (time (obs/pauli-string->observable "XYZIXYZIXYZ"))  ; Large multi-qubit observable
  
  )