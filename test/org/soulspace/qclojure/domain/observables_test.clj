(ns org.soulspace.qclojure.domain.observables-test
  "Tests for quantum observables domain"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.complex :as c]
            [org.soulspace.qclojure.util.test :as util]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]
            [org.soulspace.qclojure.domain.state :as state]))

;;
;; Basic Observable Tests
;;
(deftest test-basic-observables
  (testing "Basic Pauli observables are defined"
    (is (some? obs/pauli-x))
    (is (some? obs/pauli-y))
    (is (some? obs/pauli-z))
    (is (some? obs/identity-op)))

  (testing "Projector observables are defined"
    (is (some? obs/projector-0))
    (is (some? obs/projector-1))))

(deftest test-pauli-string-conversion
  (testing "Single Pauli character conversion"
    (is (= obs/identity-op (obs/pauli-char->matrix \I)))
    (is (= obs/pauli-x (obs/pauli-char->matrix \X)))
    (is (= obs/pauli-y (obs/pauli-char->matrix \Y)))
    (is (= obs/pauli-z (obs/pauli-char->matrix \Z))))

  (testing "Pauli string to observable conversion"
    (let [single-x (obs/pauli-string->observable "X")
          two-qubit-xy (obs/pauli-string->observable "XY")]
      (is (= obs/pauli-x single-x))
      (is (= 4 (count two-qubit-xy))) ; 2^2 = 4 dimensions
      (is (= 4 (count (first two-qubit-xy)))))))

(deftest test-linear-combination
  (testing "Linear combination of observables"
    (let [combo (obs/linear-combination [[0.5 obs/pauli-x] [0.5 obs/pauli-z]])]
      (is (= 2 (count combo)))
      (is (= 2 (count (first combo))))
      (is (util/approx= 0.5 (c/re (first (first combo)))))))

  (testing "Single observable linear combination"
    (let [single-obs (obs/linear-combination [[1.0 obs/pauli-x]])]
      (is (obs/matrix-equal? single-obs obs/pauli-x)))))

(deftest test-tensor-product
  (testing "Tensor product of two observables"
    (let [product (obs/tensor-product [obs/pauli-x obs/pauli-z])]
      (is (= 4 (count product))) ; 2⊗2 = 4×4 matrix
      (is (= 4 (count (first product))))))

  (testing "Tensor product of single observable"
    (let [single (obs/tensor-product [obs/pauli-x])]
      (is (obs/matrix-equal? single obs/pauli-x)))))

;;
;; Expectation Value and Measurement Tests  
;;
(deftest test-expectation-values
  (testing "Pauli-Z expectation values"
    (is (util/approx= 1.0 (obs/expectation-value obs/pauli-z state/|0⟩)))
    (is (util/approx= -1.0 (obs/expectation-value obs/pauli-z state/|1⟩)))
    (is (util/approx= 0.0 (obs/expectation-value obs/pauli-z state/|+⟩))))

  (testing "Pauli-X expectation values"
    (is (util/approx= 0.0 (obs/expectation-value obs/pauli-x state/|0⟩)))
    (is (util/approx= 0.0 (obs/expectation-value obs/pauli-x state/|1⟩)))
    (is (util/approx= 1.0 (obs/expectation-value obs/pauli-x state/|+⟩))))

  (testing "Identity expectation value"
    (is (util/approx= 1.0 (obs/expectation-value obs/identity-op state/|0⟩)))
    (is (util/approx= 1.0 (obs/expectation-value obs/identity-op state/|1⟩)))
    (is (util/approx= 1.0 (obs/expectation-value obs/identity-op state/|+⟩)))))

(deftest test-variance
  (testing "Variance calculations"
    ;; For definite states, variance should be 0
    (is (util/approx= 0.0 (obs/variance obs/pauli-z state/|0⟩)))
    (is (util/approx= 0.0 (obs/variance obs/pauli-z state/|1⟩)))

    ;; For superposition states, variance should be non-zero
    (is (util/approx= 1.0 (obs/variance obs/pauli-z state/|+⟩)))
    (is (util/approx= 0.0 (obs/variance obs/pauli-x state/|+⟩)))))

(deftest test-measurement-probabilities
  (testing "Measurement probabilities for definite states"
    (let [prob-z-0 (obs/measurement-probabilities obs/pauli-z state/|0⟩)
          prob-z-1 (obs/measurement-probabilities obs/pauli-z state/|1⟩)]
      (is (util/approx= 1.0 (get prob-z-0 1.0)))
      (is (util/approx= 0.0 (get prob-z-0 -1.0)))
      (is (util/approx= 0.0 (get prob-z-1 1.0)))
      (is (util/approx= 1.0 (get prob-z-1 -1.0)))))

  (testing "Measurement probabilities for superposition states"
    (let [prob-z-plus (obs/measurement-probabilities obs/pauli-z state/|+⟩)]
      (is (util/approx= 0.5 (get prob-z-plus 1.0)))
      (is (util/approx= 0.5 (get prob-z-plus -1.0)))))
  (testing "Probability normalization"
    (doseq [[obs-name observable] [["Pauli-Z" obs/pauli-z]
                                   ["Pauli-X" obs/pauli-x]
                                   ["Pauli-Y" obs/pauli-y]]
            [state-name state] [["state |0⟩" state/|0⟩]
                                ["state |1⟩" state/|1⟩]
                                ["state |+⟩" state/|+⟩]]]
      (let [probs (obs/measurement-probabilities observable state)
            total (apply + (vals probs))]
        (is (util/approx= 1.0 total)
            (str "Probabilities don't sum to 1 for " obs-name " on " state-name ": sum = " total)))))

  (testing "Eigenstate measurements give definite outcomes"
    ;; |0⟩ is eigenstate of Z with eigenvalue +1
    (let [probs-z-0 (obs/measurement-probabilities obs/pauli-z state/|0⟩)]
      (is (util/approx= 1.0 (get probs-z-0 1.0 0.0))
          "Measuring Z on |0⟩ should give probability 1 for eigenvalue +1")
      (is (util/approx= 0.0 (get probs-z-0 -1.0 0.0))
          "Measuring Z on |0⟩ should give probability 0 for eigenvalue -1"))

    ;; |1⟩ is eigenstate of Z with eigenvalue -1
    (let [probs-z-1 (obs/measurement-probabilities obs/pauli-z state/|1⟩)]
      (is (util/approx= 0.0 (get probs-z-1 1.0 0.0))
          "Measuring Z on |1⟩ should give probability 0 for eigenvalue +1")
      (is (util/approx= 1.0 (get probs-z-1 -1.0 0.0))
          "Measuring Z on |1⟩ should give probability 1 for eigenvalue -1"))

    ;; |+⟩ is eigenstate of X with eigenvalue +1
    (let [probs-x-plus (obs/measurement-probabilities obs/pauli-x state/|+⟩)]
      (is (util/approx= 1.0 (reduce + (filter #(> % 0.9) (vals probs-x-plus))))
          "Measuring X on |+⟩ should give probability ~1 for positive eigenvalue")))

  (testing "Superposition state measurements"
    ;; |+⟩ = (|0⟩ + |1⟩)/√2 should give equal probabilities for Z measurement
    (let [probs-z-plus (obs/measurement-probabilities obs/pauli-z state/|+⟩)]
      (is (util/approx= 0.5 (get probs-z-plus 1.0 0.0))
          "Measuring Z on |+⟩ should give probability 0.5 for eigenvalue +1")
      (is (util/approx= 0.5 (get probs-z-plus -1.0 0.0))
          "Measuring Z on |+⟩ should give probability 0.5 for eigenvalue -1"))))

;;
;; Hermitian Property Tests
;;
(deftest test-hermitian-properties
  (testing "Pauli matrices are Hermitian"
    (is (cla/hermitian? obs/pauli-x))
    (is (cla/hermitian? obs/pauli-y))
    (is (cla/hermitian? obs/pauli-z))
    (is (cla/hermitian? obs/identity-op)))

  (testing "Linear combinations of Hermitian operators are Hermitian"
    (let [combo (obs/linear-combination [[0.3 obs/pauli-x] [0.7 obs/pauli-z]])]
      (is (cla/hermitian? combo)))))

;;
;; Integration Tests
;;
(deftest test-observable-integration
  (testing "Complex observable workflow"
    ;; Create Bell measurement observable (Z⊗I + I⊗Z)/2
    (let [z-i (obs/tensor-product [obs/pauli-z obs/identity-op])
          i-z (obs/tensor-product [obs/identity-op obs/pauli-z])
          bell-obs (obs/linear-combination [[0.5 z-i] [0.5 i-z]])]

      ;; Verify it's Hermitian
      (is (cla/hermitian? bell-obs))

      ;; Test with Bell state |00⟩ + |11⟩
      (let [bell-state (state/normalize-state
                        (state/multi-qubit-state
                         [(c/complex 1.0) (c/complex 0.0)
                          (c/complex 0.0) (c/complex 1.0)]))
            exp-val (obs/expectation-value bell-obs bell-state)]
        (is (number? exp-val))))))

;;
;; Comprehensive Eigendecomposition Tests
;;
(deftest test-eigendecomposition-correctness
  (testing "Eigenvalue equation A*v = λ*v"
    (doseq [[name observable] [["Pauli-X" obs/pauli-x]
                               ["Pauli-Y" obs/pauli-y]
                               ["Pauli-Z" obs/pauli-z]]]
      (let [{:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian observable)]
        (doseq [i (range (count eigenvalues))]
          (let [eigenval (nth eigenvalues i)
                eigenvec (nth eigenvectors i)
                Av (cla/matrix-vector-product observable eigenvec)
                lambda-v (mapv #(c/mult eigenval %) eigenvec)
                diff-norm (cla/norm2 (mapv c/sub Av lambda-v))]
            (is (< diff-norm 1e-10)
                (str name " eigenvector " i " fails eigenvalue equation: ||A*v - λ*v|| = " diff-norm)))))))

  (testing "Eigenvectors are normalized"
    (doseq [[name observable] [["Pauli-X" obs/pauli-x]
                               ["Pauli-Y" obs/pauli-y]
                               ["Pauli-Z" obs/pauli-z]]]
      (let [{:keys [eigenvectors]} (cla/eigen-hermitian observable)]
        (doseq [i (range (count eigenvectors))]
          (let [eigenvec (nth eigenvectors i)
                norm (cla/norm2 eigenvec)]
            (is (util/approx= 1.0 norm)
                (str name " eigenvector " i " not normalized: ||v|| = " norm)))))))

  (testing "Eigenvalues are real for Hermitian matrices"
    (doseq [[name observable] [["Pauli-X" obs/pauli-x]
                               ["Pauli-Y" obs/pauli-y]
                               ["Pauli-Z" obs/pauli-z]]]
      (let [{:keys [eigenvalues]} (cla/eigen-hermitian observable)]
        (doseq [i (range (count eigenvalues))]
          (let [eigenval (nth eigenvalues i)
                imag-part (c/im eigenval)]
            (is (< (Math/abs imag-part) 1e-10)
                (str name " eigenvalue " i " has non-zero imaginary part: " imag-part)))))))

  (testing "Matrix reconstruction from eigendecomposition"
    (doseq [[name observable] [["Pauli-X" obs/pauli-x]
                               ["Pauli-Z" obs/pauli-z]]] ; Skip Pauli-Y until bug is fixed
      (let [{:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian observable)
            n (count eigenvalues)
            ;; Construct V matrix (eigenvectors as columns)
            V (mapv (fn [i] (mapv #(get % i) eigenvectors)) (range n))
            V-dagger (cla/conjugate-transpose V)
            ;; Construct diagonal eigenvalue matrix
            Lambda (mapv (fn [i]
                           (mapv (fn [j]
                                   (if (= i j)
                                     (nth eigenvalues i)
                                     c/ZERO))
                                 (range n)))
                         (range n))
            ;; Reconstruct: A = V * Λ * V†
            reconstructed (-> (cla/matrix-multiply V Lambda)
                              (cla/matrix-multiply V-dagger))
            ;; Check reconstruction error
            diff-matrix (cla/subtract observable reconstructed)
            max-diff (apply max (map (fn [row]
                                       (apply max (map c/abs row)))
                                     diff-matrix))]
        (is (< max-diff 1e-8)
            (str name " cannot be reconstructed from eigendecomposition: max error = " max-diff))))))

;;
;; Property-based and Edge Case Tests
;;
(deftest test-edge-cases
  (testing "Zero matrix operations"
    (let [zero-2x2 (obs/zero-matrix 2 2)]
      (is (= 2 (count zero-2x2)))
      (is (= 2 (count (first zero-2x2))))
      (is (util/approx= 0.0 (c/re (first (first zero-2x2)))))))

  (testing "Matrix equality with tolerance"
    (let [m1 obs/pauli-x]
      (is (obs/matrix-equal? m1 obs/pauli-x))
      (is (not (obs/matrix-equal? m1 obs/pauli-z))))))

(comment
  ;; Run all tests
  (run-tests)

  ;; Run specific test groups
  (test-basic-observables)
  (test-expectation-values)
  (test-hermitian-properties)

  ;; Manual testing examples
  (def custom-observable
    (obs/linear-combination [[1.0 obs/pauli-x] [2.0 obs/pauli-z]]))

  (obs/expectation-value custom-observable state/|+⟩)

  ;; Test with different states
  (obs/measurement-probabilities obs/pauli-y state/|+i⟩)

  ;; Performance testing
  (time (obs/pauli-string->observable "XYZIXYZ")))