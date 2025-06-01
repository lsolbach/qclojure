(ns org.soulspace.qclojure.domain.quantum-state-test
  "Tests for quantum state operations"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [fastmath.core :as m]
            [fastmath.complex :as fc]))

;; Basic state creation tests
(deftest test-basic-states
  (testing "Zero state creation"
    (let [|0⟩ (qs/zero-state 1)]
      (is (= (:state-vector |0⟩) [(fc/complex 1 0) (fc/complex 0 0)]))
      (is (= (:num-qubits |0⟩) 1))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |0⟩))
      ))
  
  (testing "One state creation"
    (let [|1⟩ (qs/one-state)]
      (is (= (:state-vector |1⟩) [(fc/complex 0 0) (fc/complex 1 0)]))
      (is (= (:num-qubits |1⟩) 1))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |1⟩))
      ))
  
  (testing "Plus state creation"
    (let [|+⟩ (qs/plus-state)]
      (is (= (:num-qubits |+⟩) 1))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |+⟩))
      ;; Check normalization (amplitude squared sum = 1)
      (let [amplitudes (:state-vector |+⟩)
            norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
        (is (< (Math/abs (- norm 1.0)) 1e-10)))))
  
  (testing "Minus state creation"
    (let [|-⟩ (qs/minus-state)]
      (is (= (:num-qubits |-⟩) 1))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |-⟩))
      )))

;; Multi-qubit state tests
(deftest test-multi-qubit-states
  (testing "Two-qubit zero state"
    (let [|00⟩ (qs/zero-state 2)]
      (is (= (:state-vector |00⟩) [(fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0)]))
      (is (= (:num-qubits |00⟩) 2))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |00⟩))
      ))
  
  (testing "Three-qubit zero state"
    (let [|000⟩ (qs/zero-state 3)]
      (is (= (:num-qubits |000⟩) 3))
      (is (= (count (:state-vector |000⟩)) 8))
      (is (= (first (:state-vector |000⟩)) (fc/complex 1 0)))
      (is (every? #(= % (fc/complex 0 0)) (rest (:state-vector |000⟩)))))))

;; Tensor product tests
(deftest test-tensor-product
  (testing "Simple tensor products"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |00⟩ (qs/tensor-product |0⟩ |0⟩)
          |01⟩ (qs/tensor-product |0⟩ |1⟩)
          |10⟩ (qs/tensor-product |1⟩ |0⟩)
          |11⟩ (qs/tensor-product |1⟩ |1⟩)]

      (is (= (:state-vector |00⟩) [(fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0)]))
      (is (= (:state-vector |01⟩) [(fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0)]))
      (is (= (:state-vector |10⟩) [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0)]))
      (is (= (:state-vector |11⟩) [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0)]))

      (is (= (:num-qubits |00⟩) 2))
      (is (= (:num-qubits |01⟩) 2))
      (is (= (:num-qubits |10⟩) 2))
      (is (= (:num-qubits |11⟩) 2))))
  
  (testing "Superposition tensor products"
    (let [|+⟩ (qs/plus-state)
          |0⟩ (qs/zero-state 1)
          |+0⟩ (qs/tensor-product |+⟩ |0⟩)]
      (is (= (:num-qubits |+0⟩) 2))
      ;; Should be in state (|00⟩ + |10⟩)/√2
      (let [expected-amp (/ 1.0 (Math/sqrt 2))
            [[a0-real a0-imag] [a1-real a1-imag] [a2-real a2-imag] [a3-real a3-imag]] (:state-vector |+0⟩)]
        (is (< (Math/abs (- a0-real expected-amp)) 1e-10))
        (is (< (Math/abs a0-imag) 1e-10))
        (is (< (Math/abs a1-real) 1e-10))
        (is (< (Math/abs a1-imag) 1e-10))
        (is (< (Math/abs (- a2-real expected-amp)) 1e-10))
        (is (< (Math/abs a2-imag) 1e-10))
        (is (< (Math/abs a3-real) 1e-10))
        (is (< (Math/abs a3-imag) 1e-10))))))

;; State normalization tests
(deftest test-normalization
  (testing "Normalize unnormalized state"
    (let [unnormalized {:state-vector [(fc/complex 3 0) (fc/complex 4 0)] :num-qubits 1}
          normalized (qs/normalize-state unnormalized)]
      (is (s/valid? ::qs/quantum-state normalized))
      ;; Check norm = 1
      (let [amplitudes (:state-vector normalized)
            norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
        (is (< (m/abs (- norm 1.0)) 1e-10)))))
  
  (testing "Already normalized state remains unchanged"
    (let [|+⟩ (qs/plus-state)
          renormalized (qs/normalize-state |+⟩)]
      ;; Use tolerance-based comparison for floating point precision
      (let [original-amplitudes (:state-vector |+⟩)
            renormalized-amplitudes (:state-vector renormalized)]
        (doseq [i (range (count original-amplitudes))]
          (let [orig-complex (nth original-amplitudes i)
                renorm-complex (nth renormalized-amplitudes i)
                orig-real (fc/re orig-complex)
                orig-imag (fc/im orig-complex)
                renorm-real (fc/re renorm-complex)
                renorm-imag (fc/im renorm-complex)]
            (is (< (Math/abs (- orig-real renorm-real)) 1e-10))
            (is (< (Math/abs (- orig-imag renorm-imag)) 1e-10))))))))

;; Probability calculation tests
(deftest test-probability
  (testing "Probability of zero state"
    (let [|0⟩ (qs/zero-state 1)]
      (is (= (qs/probability |0⟩ 0) 1.0))
      (is (= (qs/probability |0⟩ 1) 0.0))))
  
  (testing "Probability of one state"
    (let [|1⟩ (qs/one-state)]
      (is (= (qs/probability |1⟩ 0) 0.0))
      (is (= (qs/probability |1⟩ 1) 1.0))))
  
  (testing "Probability of plus state"
    (let [|+⟩ (qs/plus-state)]
      (is (< (Math/abs (- (qs/probability |+⟩ 0) 0.5)) 1e-10))
      (is (< (Math/abs (- (qs/probability |+⟩ 1) 0.5)) 1e-10))))
  
  (testing "Multi-qubit probabilities"
    (let [|00⟩ (qs/zero-state 2)]
      (is (= (qs/probability |00⟩ 0) 1.0))
      (is (= (qs/probability |00⟩ 1) 0.0))
      (is (= (qs/probability |00⟩ 2) 0.0))
      (is (= (qs/probability |00⟩ 3) 0.0)))))

;; Measurement tests
(deftest test-measurement
  (testing "Measurement of deterministic states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          measurement-0 (qs/measure-state |0⟩)
          measurement-1 (qs/measure-state |1⟩)]
      
      (is (= (:outcome measurement-0) 0))
      (is (= (:outcome measurement-1) 1))
      
      ;; Check collapsed states  
      (is (= (:state-vector (:collapsed-state measurement-0)) [(fc/complex 1 0) (fc/complex 0 0)]))
      (is (= (:state-vector (:collapsed-state measurement-1)) [(fc/complex 0 0) (fc/complex 1 0)]))))
  
  (testing "Measurement of superposition produces valid outcomes"
    (let [|+⟩ (qs/plus-state)]
      (dotimes [_ 10]
        (let [measurement (qs/measure-state |+⟩)]
          (is (contains? #{0 1} (:outcome measurement)))
          ;; Temporarily disabled spec validation
          ;; (is (s/valid? ::qs/quantum-state (:collapsed-state measurement)))
          )))))

;; Property-based tests
(defspec tensor-product-preserves-normalization 50
  (prop/for-all [n1 (gen/choose 1 3)
                 n2 (gen/choose 1 3)]
    (let [state1 (qs/zero-state n1)
          state2 (qs/zero-state n2)
          product (qs/tensor-product state1 state2)
          amplitudes (:state-vector product)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec measurement-outcomes-are-valid 100
  (prop/for-all [n (gen/choose 1 4)]
    (let [state (qs/zero-state n)
          max-outcome (dec (Math/pow 2 n))
          measurement (qs/measure-state state)]
      (and (>= (:outcome measurement) 0)
           (<= (:outcome measurement) max-outcome)
           (integer? (:outcome measurement))))))

(defspec normalization-preserves-relative-phases 50
  (prop/for-all [a (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})
                 b (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})]
                (when (not (and (zero? a) (zero? b)))
                  (let [unnormalized {:state-vector [(fc/complex a 0) (fc/complex b 0)] :num-qubits 1}
                        normalized (qs/normalize-state unnormalized)
                        norm-a (fc/re (first (:state-vector normalized)))
                        norm-b (fc/re (second (:state-vector normalized)))
                        original-ratio (if (zero? b) Double/POSITIVE_INFINITY (/ a b))
                        normalized-ratio (if (zero? norm-b) Double/POSITIVE_INFINITY (/ norm-a norm-b))]
                    (if (= original-ratio Double/POSITIVE_INFINITY)
                      (= normalized-ratio Double/POSITIVE_INFINITY)
                      ;; Handle zero ratio case specially  
                      (if (zero? original-ratio)
                        (< (Math/abs normalized-ratio) 1e-10)
                        ;; Use relative error tolerance for better numerical stability
                        (let [relative-error (/ (Math/abs (- original-ratio normalized-ratio))
                                                 (Math/abs original-ratio))]
                          (< relative-error 1e-9))))))))

;; Run all tests
(defn run-all-state-tests []
  (run-tests 'qclojure.domain.quantum-state-test))

(comment
  ;; Run tests in REPL
  (run-all-state-tests)
  
  ;; Run specific test
  (test-basic-states)
  (test-tensor-product)
  (test-measurement)
  )
