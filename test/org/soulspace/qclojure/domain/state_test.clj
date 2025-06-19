(ns org.soulspace.qclojure.domain.state-test
  "Tests for quantum state operations"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.domain.state :as qs]
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
        (is (< (m/abs (- norm 1.0)) 1e-10)))))

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
      (let [expected-amp (/ 1.0 (m/sqrt 2))
            [[a0-real a0-imag] [a1-real a1-imag] [a2-real a2-imag] [a3-real a3-imag]] (:state-vector |+0⟩)]
        (is (< (m/abs (- a0-real expected-amp)) 1e-10))
        (is (< (m/abs a0-imag) 1e-10))
        (is (< (m/abs a1-real) 1e-10))
        (is (< (m/abs a1-imag) 1e-10))
        (is (< (m/abs (- a2-real expected-amp)) 1e-10))
        (is (< (m/abs a2-imag) 1e-10))
        (is (< (m/abs a3-real) 1e-10))
        (is (< (m/abs a3-imag) 1e-10))))))

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

; Bits to index conversion tests
(deftest test-bits-to-index
  (testing "Single qubit bit patterns"
    (is (= (qs/bits-to-index [0]) 0))
    (is (= (qs/bits-to-index [1]) 1)))

  (testing "Two qubit bit patterns"
    (is (= (qs/bits-to-index [0 0]) 0))
    (is (= (qs/bits-to-index [0 1]) 1))
    (is (= (qs/bits-to-index [1 0]) 2))
    (is (= (qs/bits-to-index [1 1]) 3)))

  (testing "Three qubit bit patterns"
    (is (= (qs/bits-to-index [0 0 0]) 0))
    (is (= (qs/bits-to-index [0 0 1]) 1))
    (is (= (qs/bits-to-index [0 1 0]) 2))
    (is (= (qs/bits-to-index [0 1 1]) 3))
    (is (= (qs/bits-to-index [1 0 0]) 4))
    (is (= (qs/bits-to-index [1 0 1]) 5))
    (is (= (qs/bits-to-index [1 1 0]) 6))
    (is (= (qs/bits-to-index [1 1 1]) 7)))

  (testing "Four qubit example"
    (is (= (qs/bits-to-index [1 0 1 1]) 11))) ; 1*8 + 0*4 + 1*2 + 1*1 = 11

  (testing "Empty bit vector"
    (is (= (qs/bits-to-index []) 0))))

;; Computational basis state tests
(deftest test-computational-basis-state
  (testing "Single qubit computational basis states"
    (let [|0⟩ (qs/computational-basis-state 1 [0])
          |1⟩ (qs/computational-basis-state 1 [1])]

      (is (= (:num-qubits |0⟩) 1))
      (is (= (:num-qubits |1⟩) 1))

      (is (= (:state-vector |0⟩) [(fc/complex 1 0) (fc/complex 0 0)]))
      (is (= (:state-vector |1⟩) [(fc/complex 0 0) (fc/complex 1 0)]))))

  (testing "Two qubit computational basis states"
    (let [|00⟩ (qs/computational-basis-state 2 [0 0])
          |01⟩ (qs/computational-basis-state 2 [0 1])
          |10⟩ (qs/computational-basis-state 2 [1 0])
          |11⟩ (qs/computational-basis-state 2 [1 1])]

      ;; Check number of qubits
      (is (= (:num-qubits |00⟩) 2))
      (is (= (:num-qubits |01⟩) 2))
      (is (= (:num-qubits |10⟩) 2))
      (is (= (:num-qubits |11⟩) 2))

      ;; Check state vector lengths
      (is (= (count (:state-vector |00⟩)) 4))
      (is (= (count (:state-vector |01⟩)) 4))
      (is (= (count (:state-vector |10⟩)) 4))
      (is (= (count (:state-vector |11⟩)) 4))

      ;; Check that only the correct amplitude is 1
      (is (= (:state-vector |00⟩) [(fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0)]))
      (is (= (:state-vector |01⟩) [(fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0) (fc/complex 0 0)]))
      (is (= (:state-vector |10⟩) [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0) (fc/complex 0 0)]))
      (is (= (:state-vector |11⟩) [(fc/complex 0 0) (fc/complex 0 0) (fc/complex 0 0) (fc/complex 1 0)]))))

  (testing "Three qubit computational basis states"
    (let [|000⟩ (qs/computational-basis-state 3 [0 0 0])
          |101⟩ (qs/computational-basis-state 3 [1 0 1])
          |111⟩ (qs/computational-basis-state 3 [1 1 1])]

      (is (= (:num-qubits |000⟩) 3))
      (is (= (:num-qubits |101⟩) 3))
      (is (= (:num-qubits |111⟩) 3))

      (is (= (count (:state-vector |000⟩)) 8))
      (is (= (count (:state-vector |101⟩)) 8))
      (is (= (count (:state-vector |111⟩)) 8))

      ;; Check specific amplitudes
      (is (= (nth (:state-vector |000⟩) 0) (fc/complex 1 0)))
      (is (= (nth (:state-vector |101⟩) 5) (fc/complex 1 0))) ; bits [1 0 1] -> index 5
      (is (= (nth (:state-vector |111⟩) 7) (fc/complex 1 0))) ; bits [1 1 1] -> index 7

      ;; Check that all other amplitudes are zero
      (is (every? #(= % (fc/complex 0 0)) (take 7 (drop 1 (:state-vector |000⟩)))))
      (is (every? #(= % (fc/complex 0 0)) (concat (take 5 (:state-vector |101⟩)) (drop 6 (:state-vector |101⟩)))))
      (is (every? #(= % (fc/complex 0 0)) (take 7 (:state-vector |111⟩))))))

  (testing "Computational basis states are normalized"
    (doseq [n (range 1 4)]
      (let [bits (take n (repeatedly #(rand-int 2)))
            state (qs/computational-basis-state n (vec bits))
            amplitudes (:state-vector state)
            norm-squared (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
        (is (< (Math/abs (- norm-squared 1.0)) 1e-10)))))

  (testing "Error conditions"
    ;; Number of qubits doesn't match bits length
    (is (thrown? AssertionError (qs/computational-basis-state 2 [0])))
    (is (thrown? AssertionError (qs/computational-basis-state 1 [0 1])))

    ;; Invalid bit values
    (is (thrown? AssertionError (qs/computational-basis-state 2 [0 2])))
    (is (thrown? AssertionError (qs/computational-basis-state 2 [0 -1])))

    ;; Invalid number of qubits
    (is (thrown? AssertionError (qs/computational-basis-state 0 [])))
    (is (thrown? AssertionError (qs/computational-basis-state -1 [])))))

;; Integration tests with existing functions
(deftest test-computational-basis-state-integration
  (testing "Computational basis state matches tensor product construction"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)

          ;; Create |01⟩ using tensor product
          |01⟩-tensor (qs/tensor-product |0⟩ |1⟩)

          ;; Create |01⟩ using computational-basis-state
          |01⟩-computational (qs/computational-basis-state 2 [0 1])]

      (is (= (:state-vector |01⟩-tensor) (:state-vector |01⟩-computational)))
      (is (= (:num-qubits |01⟩-tensor) (:num-qubits |01⟩-computational)))))

  (testing "Computational basis state probabilities"
    (let [|101⟩ (qs/computational-basis-state 3 [1 0 1])]
      ;; Should have probability 1 for state |101⟩ (index 5) and 0 for all others
      (is (= (qs/probability |101⟩ 5) 1.0))
      (doseq [i (range 8)]
        (when (not= i 5)
          (is (= (qs/probability |101⟩ i) 0.0))))))

  (testing "Measurement of computational basis states is deterministic"
    (let [|110⟩ (qs/computational-basis-state 3 [1 1 0])
          measurement (qs/measure-state |110⟩)]
      (is (= (:outcome measurement) 6)) ; bits [1 1 0] -> index 6
      (is (= (:state-vector (:collapsed-state measurement)) (:state-vector |110⟩))))))

;; Additional tests for quantum mechanics properties
(deftest test-computational-basis-state-quantum-properties
  (testing "Computational basis states form a complete orthonormal set"
    (let [n 2
          all-basis-states (for [i (range (bit-shift-left 1 n))]
                             (let [bits (vec (map #(if (bit-test i (- n 1 %)) 1 0) (range n)))]
                               (qs/computational-basis-state n bits)))]

      ;; Test normalization - each state has norm 1
      (doseq [state all-basis-states]
        (let [amplitudes (:state-vector state)
              norm-squared (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
          (is (< (Math/abs (- norm-squared 1.0)) 1e-10))))

      ;; Test orthogonality - different states have zero inner product
      (doseq [i (range (count all-basis-states))
              j (range (count all-basis-states))
              :when (not= i j)]
        (let [state-i (nth all-basis-states i)
              state-j (nth all-basis-states j)
              amplitudes-i (:state-vector state-i)
              amplitudes-j (:state-vector state-j)
              inner-product (reduce + (map #(* (fc/abs %1) (fc/abs %2)) amplitudes-i amplitudes-j))]
          (is (< inner-product 1e-10))))

      ;; Test completeness - sum of all projectors equals identity
      ;; This is implicit in the fact that each amplitude position has exactly one 1 across all states
      (let [all-state-vectors (map :state-vector all-basis-states)
            num-positions (count (:state-vector (first all-basis-states)))
            total-amplitudes (for [i (range num-positions)]
                               (reduce fc/add (map #(nth % i) all-state-vectors)))]
        (is (every? #(< (Math/abs (- (fc/abs %) 1.0)) 1e-10) total-amplitudes)))))

  (testing "Bit order convention consistency"
    ;; Test that our bit ordering is consistent with standard quantum computing notation
    ;; |10⟩ should have qubit 0 in state |1⟩ and qubit 1 in state |0⟩
    (let [|10⟩ (qs/computational-basis-state 2 [1 0])
          |0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |1⟩⊗|0⟩ (qs/tensor-product |1⟩ |0⟩)]

      (is (= (:state-vector |10⟩) (:state-vector |1⟩⊗|0⟩)))

      ;; Also test the bits-to-index function aligns
      (is (= (qs/bits-to-index [1 0]) 2))
      (is (= (nth (:state-vector |10⟩) 2) (fc/complex 1 0)))))

  (testing "Superposition decomposition"
    ;; Any quantum state should be expressible as a superposition of computational basis states
    (let [|+⟩ (qs/plus-state)
          |0⟩ (qs/computational-basis-state 1 [0])
          |1⟩ (qs/computational-basis-state 1 [1])]

      ;; |+⟩ = (|0⟩ + |1⟩)/√2, so it should have equal amplitudes on |0⟩ and |1⟩
      (is (< (Math/abs (- (qs/probability |+⟩ 0) 0.5)) 1e-10))
      (is (< (Math/abs (- (qs/probability |+⟩ 1) 0.5)) 1e-10))

      ;; The computational basis states should have definite outcomes
      (is (= (qs/probability |0⟩ 0) 1.0))
      (is (= (qs/probability |0⟩ 1) 0.0))
      (is (= (qs/probability |1⟩ 0) 0.0))
      (is (= (qs/probability |1⟩ 1) 1.0)))))

(deftest test-computational-basis-state-round-trip
  (testing "Round-trip consistency between bits-to-index and computational-basis-state"
    (doseq [n (range 1 4)]
      (doseq [index (range (bit-shift-left 1 n))]
        (let [bits (vec (map #(if (bit-test index (- n 1 %)) 1 0) (range n)))
              state (qs/computational-basis-state n bits)
              converted-index (qs/bits-to-index bits)]
          (is (= converted-index index))
          (is (= (qs/probability state index) 1.0))
          (is (every? #(= (qs/probability state %) 0.0)
                      (filter #(not= % index) (range (bit-shift-left 1 n))))))))))
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

(defspec computational-basis-states-are-orthogonal 30
  (prop/for-all [n (gen/choose 1 3)]
                (let [all-states (for [i (range (bit-shift-left 1 n))]
                                   (let [bits (vec (map #(if (bit-test i (- n 1 %)) 1 0) (range n)))]
                                     (qs/computational-basis-state n bits)))]
                  ;; Check that different computational basis states have zero overlap
                  (every? identity
                          (for [i (range (count all-states))
                                j (range (count all-states))
                                :when (not= i j)]
                            (let [state-i (nth all-states i)
                                  state-j (nth all-states j)
                                  amplitudes-i (:state-vector state-i)
                                  amplitudes-j (:state-vector state-j)
                                  inner-product (reduce + (map #(* (fc/abs %1) (fc/abs %2)) amplitudes-i amplitudes-j))]
                              (< inner-product 1e-10)))))))

(comment
  ;; Run tests in REPL
  (run-tests)

  ;; Run specific test
  (test-basic-states)
  (test-tensor-product)
  (test-measurement))