(ns org.soulspace.qclojure.domain.observables-test
  "Tests for quantum observables domain"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.complex :as c]
            [org.soulspace.qclojure.util.test :as util]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.math.core :as mcore]
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
      (is (util/approx= 0.5 (get prob-z-plus -1.0))))))

;;
;; Hermitian Property Tests
;;
(deftest test-hermitian-properties
  (testing "Pauli matrices are Hermitian"
    (is (mcore/hermitian? obs/pauli-x))
    (is (mcore/hermitian? obs/pauli-y))
    (is (mcore/hermitian? obs/pauli-z))
    (is (mcore/hermitian? obs/identity-op)))
  
  (testing "Linear combinations of Hermitian operators are Hermitian"
    (let [combo (obs/linear-combination [[0.3 obs/pauli-x] [0.7 obs/pauli-z]])]
      (is (mcore/hermitian? combo)))))

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
      (is (mcore/hermitian? bell-obs))
      
      ;; Test with Bell state |00⟩ + |11⟩
      (let [bell-state (state/normalize-state 
                         (state/multi-qubit-state 
                           [(c/complex 1.0) (c/complex 0.0) 
                            (c/complex 0.0) (c/complex 1.0)]))
            exp-val (obs/expectation-value bell-obs bell-state)]
        (is (number? exp-val))))))

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
  (time (obs/pauli-string->observable "XYZIXYZ"))
  
  )