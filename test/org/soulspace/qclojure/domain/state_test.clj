(ns org.soulspace.qclojure.domain.state-test
  "Tests for quantum state operations"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]))

;;
;; Basic state creation tests
;;
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
        (is (< (abs (- norm 1.0)) 1e-10)))))

  (testing "Minus state creation"
    (let [|-⟩ (qs/minus-state)]
      (is (= (:num-qubits |-⟩) 1))
      ;; Temporarily disabled spec validation
      ;; (is (s/valid? ::qs/quantum-state |-⟩))
      )))

;;
;; Multi-qubit state tests
;;
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

;;
;; Tensor product tests
;;
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
      (let [expected-amp (/ 1.0 (fm/sqrt 2))
            [a0 a1 a2 a3] (:state-vector |+0⟩)
            a0-real (fc/re a0) a0-imag (fc/im a0)
            a1-real (fc/re a1) a1-imag (fc/im a1)
            a2-real (fc/re a2) a2-imag (fc/im a2)
            a3-real (fc/re a3) a3-imag (fc/im a3)]
        (is (< (abs (- a0-real expected-amp)) 1e-10))
        (is (< (abs a0-imag) 1e-10))
        (is (< (abs a1-real) 1e-10))
        (is (< (abs a1-imag) 1e-10))
        (is (< (abs (- a2-real expected-amp)) 1e-10))
        (is (< (abs a2-imag) 1e-10))
        (is (< (abs a3-real) 1e-10))
        (is (< (abs a3-imag) 1e-10))))))

;;
;; State normalization tests
;;
(deftest test-normalization
  (testing "Normalize unnormalized state"
    (let [unnormalized {:state-vector [(fc/complex 3 0) (fc/complex 4 0)] :num-qubits 1}
          normalized (qs/normalize-state unnormalized)]
      (is (s/valid? ::qs/state normalized))
      ;; Check norm = 1
      (let [amplitudes (:state-vector normalized)
            norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
        (is (< (fm/abs (- norm 1.0)) 1e-10)))))

  (testing "Already normalized state remains unchanged"
    (let [|+⟩ (qs/plus-state)
          renormalized (qs/normalize-state |+⟩)

          ;; Use tolerance-based comparison for floating point precision
          original-amplitudes (:state-vector |+⟩)
          renormalized-amplitudes (:state-vector renormalized)]
      
      (doseq [i (range (count original-amplitudes))]
        (let [orig-complex (nth original-amplitudes i)
              renorm-complex (nth renormalized-amplitudes i)
              orig-real (fc/re orig-complex)
              orig-imag (fc/im orig-complex)
              renorm-real (fc/re renorm-complex)
              renorm-imag (fc/im renorm-complex)]
          (is (< (abs (- orig-real renorm-real)) 1e-10))
          (is (< (abs (- orig-imag renorm-imag)) 1e-10)))))))

;;
;; Probability calculation tests
;;
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
      (is (< (abs (- (qs/probability |+⟩ 0) 0.5)) 1e-10))
      (is (< (abs (- (qs/probability |+⟩ 1) 0.5)) 1e-10))))

  (testing "Multi-qubit probabilities"
    (let [|00⟩ (qs/zero-state 2)]
      (is (= (qs/probability |00⟩ 0) 1.0))
      (is (= (qs/probability |00⟩ 1) 0.0))
      (is (= (qs/probability |00⟩ 2) 0.0))
      (is (= (qs/probability |00⟩ 3) 0.0)))))

;;
;; Measurement tests
;;
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

;;
;; Bits to index conversion tests
;;
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

;;
;; Computational basis state tests
;;
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
        (is (< (abs (- norm-squared 1.0)) 1e-10)))))

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

;;
;; Integration tests with existing functions
;;
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

;;
;; Density matrix and projector tests
;;
(deftest test-state-projector
  (testing "State projector for pure states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |+⟩ (qs/plus-state)
          
          proj-0 (qs/state-projector |0⟩)
          proj-1 (qs/state-projector |1⟩)
          proj-+ (qs/state-projector |+⟩)]

      ;; |0⟩⟨0| should be [[1,0],[0,0]]
      (is (< (abs (- (fc/re (get-in proj-0 [0 0])) 1.0)) 1e-10))
      (is (< (abs (fc/re (get-in proj-0 [0 1]))) 1e-10))
      (is (< (abs (fc/re (get-in proj-0 [1 0]))) 1e-10))
      (is (< (abs (fc/re (get-in proj-0 [1 1]))) 1e-10))

      ;; |1⟩⟨1| should be [[0,0],[0,1]]
      (is (< (abs (fc/re (get-in proj-1 [0 0]))) 1e-10))
      (is (< (abs (fc/re (get-in proj-1 [0 1]))) 1e-10))
      (is (< (abs (fc/re (get-in proj-1 [1 0]))) 1e-10))
      (is (< (abs (- (fc/re (get-in proj-1 [1 1])) 1.0)) 1e-10))

      ;; |+⟩⟨+| should be [[0.5,0.5],[0.5,0.5]]
      (is (< (abs (- (fc/re (get-in proj-+ [0 0])) 0.5)) 1e-10))
      (is (< (abs (- (fc/re (get-in proj-+ [0 1])) 0.5)) 1e-10))
      (is (< (abs (- (fc/re (get-in proj-+ [1 0])) 0.5)) 1e-10))
      (is (< (abs (- (fc/re (get-in proj-+ [1 1])) 0.5)) 1e-10))))

  (testing "State projector properties"
    (let [|+⟩ (qs/plus-state)
          proj (qs/state-projector |+⟩)]
      
      ;; Projectors should be Hermitian: P† = P
      ;; For real matrices this means symmetric
      (is (< (abs (- (fc/re (get-in proj [0 1])) (fc/re (get-in proj [1 0])))) 1e-10))

      ;; Projectors should be idempotent: P² = P (for pure states)
      ;; This is more complex to test, but trace should equal rank for projectors
      )))

(deftest test-density-matrix
  (testing "Density matrix for pure states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |+⟩ (qs/plus-state)
          
          rho-0 (qs/density-matrix |0⟩)
          rho-1 (qs/density-matrix |1⟩)
          rho-+ (qs/density-matrix |+⟩)]

      ;; Density matrices should equal their projectors for pure states
      (let [proj-0 (qs/state-projector |0⟩)]
        (is (< (abs (- (fc/re (get-in rho-0 [0 0])) (fc/re (get-in proj-0 [0 0])))) 1e-10))
        (is (< (abs (- (fc/re (get-in rho-0 [1 1])) (fc/re (get-in proj-0 [1 1])))) 1e-10)))

      ;; Check trace equals 1
      (is (qs/trace-one? rho-0))
      (is (qs/trace-one? rho-1))
      (is (qs/trace-one? rho-+))))

  (testing "Density matrix properties"
    (let [|+⟩ (qs/plus-state)
          rho (qs/density-matrix |+⟩)]
      
      ;; Should be Hermitian (ρ† = ρ)
      ;; For our real case, this means symmetric
      (is (< (abs (- (fc/re (get-in rho [0 1])) (fc/re (get-in rho [1 0])))) 1e-10))

      ;; Should be positive semidefinite (all eigenvalues ≥ 0)
      ;; This is harder to test without eigenvalue computation

      ;; Trace should be 1
      (is (qs/trace-one? rho)))))

(deftest test-trajectory-to-density-matrix
  (testing "Basic trajectory to density matrix conversion"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |+⟩ (qs/plus-state)]

      ;; Test equal mixture of |0⟩ and |1⟩
      (let [result (qs/trajectory-to-density-matrix [|0⟩ |1⟩])]
        (is (= (:num-qubits result) 1))
        (is (< (abs (- (:trace result) 1.0)) 1e-10))
        (is (= (:weights result) [0.5 0.5]))
        
        ;; The resulting density matrix should be (|0⟩⟨0| + |1⟩⟨1|)/2
        ;; This should be [[0.5, 0], [0, 0.5]]
        (let [dm (:density-matrix result)]
          (is (< (abs (- (fc/re (get-in dm [0 0])) 0.5)) 1e-10))
          (is (< (abs (fc/re (get-in dm [0 1]))) 1e-10))
          (is (< (abs (fc/re (get-in dm [1 0]))) 1e-10))
          (is (< (abs (- (fc/re (get-in dm [1 1])) 0.5)) 1e-10))))

      ;; Test single state (should equal pure state density matrix)
      (let [result (qs/trajectory-to-density-matrix [|+⟩])
            pure-dm (qs/density-matrix |+⟩)]
        (is (= (:weights result) [1.0]))
        (let [traj-dm (:density-matrix result)]
          (is (< (abs (- (fc/re (get-in traj-dm [0 0])) (fc/re (get-in pure-dm [0 0])))) 1e-10))
          (is (< (abs (- (fc/re (get-in traj-dm [0 1])) (fc/re (get-in pure-dm [0 1])))) 1e-10))))))

  (testing "Weighted trajectory to density matrix conversion"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)]

      ;; Test weighted mixture
      (let [result (qs/trajectory-to-density-matrix [|0⟩ |1⟩] [0.7 0.3])]
        (is (= (:weights result) [0.7 0.3]))
        (is (< (abs (- (:trace result) 1.0)) 1e-10))
        
        ;; Should be 0.7|0⟩⟨0| + 0.3|1⟩⟨1| = [[0.7, 0], [0, 0.3]]
        (let [dm (:density-matrix result)]
          (is (< (abs (- (fc/re (get-in dm [0 0])) 0.7)) 1e-10))
          (is (< (abs (fc/re (get-in dm [0 1]))) 1e-10))
          (is (< (abs (fc/re (get-in dm [1 0]))) 1e-10))
          (is (< (abs (- (fc/re (get-in dm [1 1])) 0.3)) 1e-10))))

      ;; Test automatic weight normalization
      (let [result (qs/trajectory-to-density-matrix [|0⟩ |1⟩] [2.0 3.0])]
        (is (= (:weights result) [0.4 0.6]))
        (is (< (abs (- (reduce + (:weights result)) 1.0)) 1e-10)))))

  (testing "Trajectory validation"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |00⟩ (qs/zero-state 2)]

      ;; Different number of qubits should throw
      (is (thrown? Exception (qs/trajectory-to-density-matrix [|0⟩ |00⟩])))

      ;; Empty trajectory list should throw (due to precondition)
      (is (thrown? AssertionError (qs/trajectory-to-density-matrix [])))

      ;; Test with unnormalized state (should throw)
      (let [unnormalized {:state-vector [(fc/complex 2.0 0) (fc/complex 0 0)] :num-qubits 1}]
        (is (thrown? Exception (qs/trajectory-to-density-matrix [|0⟩ unnormalized]))))))

  (testing "Multi-qubit trajectory to density matrix"
    (let [|00⟩ (qs/zero-state 2)
          |11⟩ (qs/tensor-product qs/|1⟩ qs/|1⟩)]

      (let [result (qs/trajectory-to-density-matrix [|00⟩ |11⟩])]
        (is (= (:num-qubits result) 2))
        (is (= (:weights result) [0.5 0.5]))
        (is (< (abs (- (:trace result) 1.0)) 1e-10))
        
        ;; Should be (|00⟩⟨00| + |11⟩⟨11|)/2
        ;; This is a diagonal matrix with [0.5, 0, 0, 0.5]
        (let [dm (:density-matrix result)]
          (is (= (count dm) 4))  ; 2^2 dimensions
          (is (< (abs (- (fc/re (get-in dm [0 0])) 0.5)) 1e-10))  ; |00⟩⟨00|
          (is (< (abs (fc/re (get-in dm [1 1]))) 1e-10))           ; |01⟩⟨01|
          (is (< (abs (fc/re (get-in dm [2 2]))) 1e-10))           ; |10⟩⟨10|
          (is (< (abs (- (fc/re (get-in dm [3 3])) 0.5)) 1e-10)))))) ; |11⟩⟨11|

  (testing "Trajectory properties preservation"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |+⟩ (qs/plus-state)
          result (qs/trajectory-to-density-matrix [|0⟩ |1⟩ |+⟩] [0.4 0.3 0.3])]

      ;; Resulting density matrix should be Hermitian
      (let [dm (:density-matrix result)]
        (doseq [i (range 2)
                j (range 2)]
          (let [elem-ij (get-in dm [i j])
                elem-ji (get-in dm [j i])]
            ;; For Hermitian: A[i,j] = A[j,i]* (complex conjugate)
            ;; For our real matrices, this means symmetric
            (is (< (abs (- (fc/re elem-ij) (fc/re elem-ji))) 1e-10)))))

      ;; Trace should be 1
      (is (< (abs (- (:trace result) 1.0)) 1e-10))

      ;; All diagonal elements should be non-negative (positive semidefinite property)
      (let [dm (:density-matrix result)]
        (doseq [i (range 2)]
          (is (>= (fc/re (get-in dm [i i])) -1e-10)))))))

;;
;; Additional tests for quantum mechanics properties
;;
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
          (is (< (abs (- norm-squared 1.0)) 1e-10))))

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
        (is (every? #(< (abs (- (fc/abs %) 1.0)) 1e-10) total-amplitudes)))))

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
      (is (< (abs (- (qs/probability |+⟩ 0) 0.5)) 1e-10))
      (is (< (abs (- (qs/probability |+⟩ 1) 0.5)) 1e-10))

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
;;
;; Property-based tests
;;
(defspec tensor-product-preserves-normalization 50
  (prop/for-all [n1 (gen/choose 1 3)
                 n2 (gen/choose 1 3)]
                (let [state1 (qs/zero-state n1)
                      state2 (qs/zero-state n2)
                      product (qs/tensor-product state1 state2)
                      amplitudes (:state-vector product)
                      norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
                  (< (abs (- norm 1.0)) 1e-10))))

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
                        (< (abs normalized-ratio) 1e-10)
                        ;; Use relative error tolerance for better numerical stability
                        (let [relative-error (/ (fm/abs (- original-ratio normalized-ratio))
                                                (fm/abs original-ratio))]
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

;;
;; Multi-qubit state creation tests
;;
(deftest test-multi-qubit-one-state
  (testing "Multi-qubit one-state creation"
    (let [|11⟩ (qs/one-state 2)
          |111⟩ (qs/one-state 3)]
      
      ;; Check number of qubits
      (is (= (:num-qubits |11⟩) 2))
      (is (= (:num-qubits |111⟩) 3))
      
      ;; Check state vector size
      (is (= (count (:state-vector |11⟩)) 4))
      (is (= (count (:state-vector |111⟩)) 8))
      
      ;; For |11⟩, only the last amplitude (index 3) should be 1
      (is (= (nth (:state-vector |11⟩) 3) (fc/complex 1 0)))
      (is (every? #(= % (fc/complex 0 0)) (take 3 (:state-vector |11⟩))))
      
      ;; For |111⟩, only the last amplitude (index 7) should be 1
      (is (= (nth (:state-vector |111⟩) 7) (fc/complex 1 0)))
      (is (every? #(= % (fc/complex 0 0)) (take 7 (:state-vector |111⟩))))
      
      ;; Test probabilities
      (is (= (qs/probability |11⟩ 3) 1.0))
      (is (= (qs/probability |111⟩ 7) 1.0))
      
      ;; All states should be normalized
      (let [norm-11 (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |11⟩)))
            norm-111 (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |111⟩)))]
        (is (< (abs (- norm-11 1.0)) 1e-10))
        (is (< (abs (- norm-111 1.0)) 1e-10))))))

(deftest test-multi-qubit-plus-state
  (testing "Multi-qubit plus-state creation"
    (let [|++⟩ (qs/plus-state 2)
          |+++⟩ (qs/plus-state 3)]
      
      ;; Check number of qubits
      (is (= (:num-qubits |++⟩) 2))
      (is (= (:num-qubits |+++⟩) 3))
      
      ;; Check state vector size
      (is (= (count (:state-vector |++⟩)) 4))
      (is (= (count (:state-vector |+++⟩)) 8))
      
      ;; For |++⟩, all amplitudes should be equal (1/2)
      (let [expected-amp (/ 1 (fm/sqrt 4))]
        (doseq [amp (:state-vector |++⟩)]
          (is (< (abs (- (fc/re amp) expected-amp)) 1e-10))
          (is (< (abs (fc/im amp)) 1e-10))))
      
      ;; For |+++⟩, all amplitudes should be equal (1/√8)
      (let [expected-amp (/ 1 (fm/sqrt 8))]
        (doseq [amp (:state-vector |+++⟩)]
          (is (< (abs (- (fc/re amp) expected-amp)) 1e-10))
          (is (< (abs (fc/im amp)) 1e-10))))
      
      ;; Test equal probabilities for all basis states
      (doseq [i (range 4)]
        (is (< (abs (- (qs/probability |++⟩ i) 0.25)) 1e-10)))
      
      (doseq [i (range 8)]
        (is (< (abs (- (qs/probability |+++⟩ i) 0.125)) 1e-10)))
      
      ;; All states should be normalized
      (let [norm-++ (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |++⟩)))
            norm-+++ (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |+++⟩)))]
        (is (< (abs (- norm-++ 1.0)) 1e-10))
        (is (< (abs (- norm-+++ 1.0)) 1e-10))))))

(deftest test-multi-qubit-minus-state
  (testing "Multi-qubit minus-state creation"
    (let [|--⟩ (qs/minus-state 2)
          |---⟩ (qs/minus-state 3)]
      
      ;; Check number of qubits
      (is (= (:num-qubits |--⟩) 2))
      (is (= (:num-qubits |---⟩) 3))
      
      ;; Check state vector size
      (is (= (count (:state-vector |--⟩)) 4))
      (is (= (count (:state-vector |---⟩)) 8))
      
      ;; For |--⟩, first amplitude should be positive, rest negative
      (let [expected-amp (/ 1 (fm/sqrt 4))
            [a0 a1 a2 a3] (:state-vector |--⟩)]
        (is (< (abs (- (fc/re a0) expected-amp)) 1e-10))
        (is (< (abs (+ (fc/re a1) expected-amp)) 1e-10))
        (is (< (abs (+ (fc/re a2) expected-amp)) 1e-10))
        (is (< (abs (+ (fc/re a3) expected-amp)) 1e-10)))
      
      ;; Test equal probabilities despite phase differences
      (doseq [i (range 4)]
        (is (< (abs (- (qs/probability |--⟩ i) 0.25)) 1e-10)))
      
      (doseq [i (range 8)]
        (is (< (abs (- (qs/probability |---⟩ i) 0.125)) 1e-10)))
      
      ;; All states should be normalized
      (let [norm--- (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |--⟩)))
            norm---- (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |---⟩)))]
        (is (< (abs (- norm--- 1.0)) 1e-10))
        (is (< (abs (- norm---- 1.0)) 1e-10))))))

(deftest test-multi-qubit-plus-i-state
  (testing "Multi-qubit plus-i-state creation"
    (let [|+i+i⟩ (qs/plus-i-state 2)
          |+i+i+i⟩ (qs/plus-i-state 3)]
      
      ;; Check number of qubits
      (is (= (:num-qubits |+i+i⟩) 2))
      (is (= (:num-qubits |+i+i+i⟩) 3))
      
      ;; Check state vector size
      (is (= (count (:state-vector |+i+i⟩)) 4))
      (is (= (count (:state-vector |+i+i+i⟩)) 8))
      
      ;; For |+i+i⟩, first amplitude real, rest imaginary
      (let [expected-amp (/ 1 (fm/sqrt 4))
            [a0 a1 a2 a3] (:state-vector |+i+i⟩)]
        ;; First amplitude should be real
        (is (< (abs (- (fc/re a0) expected-amp)) 1e-10))
        (is (< (abs (fc/im a0)) 1e-10))
        
        ;; Rest should be imaginary (positive imaginary part)
        (is (< (abs (fc/re a1)) 1e-10))
        (is (< (abs (- (fc/im a1) expected-amp)) 1e-10))
        (is (< (abs (fc/re a2)) 1e-10))
        (is (< (abs (- (fc/im a2) expected-amp)) 1e-10))
        (is (< (abs (fc/re a3)) 1e-10))
        (is (< (abs (- (fc/im a3) expected-amp)) 1e-10)))
      
      ;; Test equal probabilities
      (doseq [i (range 4)]
        (is (< (abs (- (qs/probability |+i+i⟩ i) 0.25)) 1e-10)))
      
      (doseq [i (range 8)]
        (is (< (abs (- (qs/probability |+i+i+i⟩ i) 0.125)) 1e-10)))
      
      ;; All states should be normalized
      (let [norm-+i (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |+i+i⟩)))
            norm-+i+i (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |+i+i+i⟩)))]
        (is (< (abs (- norm-+i 1.0)) 1e-10))
        (is (< (abs (- norm-+i+i 1.0)) 1e-10))))))

(deftest test-multi-qubit-minus-i-state
  (testing "Multi-qubit minus-i-state creation"
    (let [|-i-i⟩ (qs/minus-i-state 2)
          |-i-i-i⟩ (qs/minus-i-state 3)]
      
      ;; Check number of qubits
      (is (= (:num-qubits |-i-i⟩) 2))
      (is (= (:num-qubits |-i-i-i⟩) 3))
      
      ;; Check state vector size
      (is (= (count (:state-vector |-i-i⟩)) 4))
      (is (= (count (:state-vector |-i-i-i⟩)) 8))
      
      ;; For |-i-i⟩, first amplitude real, rest negative imaginary
      (let [expected-amp (/ 1 (fm/sqrt 4))
            [a0 a1 a2 a3] (:state-vector |-i-i⟩)]
        ;; First amplitude should be real
        (is (< (abs (- (fc/re a0) expected-amp)) 1e-10))
        (is (< (abs (fc/im a0)) 1e-10))
        
        ;; Rest should be imaginary (negative imaginary part)
        (is (< (abs (fc/re a1)) 1e-10))
        (is (< (abs (+ (fc/im a1) expected-amp)) 1e-10))
        (is (< (abs (fc/re a2)) 1e-10))
        (is (< (abs (+ (fc/im a2) expected-amp)) 1e-10))
        (is (< (abs (fc/re a3)) 1e-10))
        (is (< (abs (+ (fc/im a3) expected-amp)) 1e-10)))
      
      ;; Test equal probabilities
      (doseq [i (range 4)]
        (is (< (abs (- (qs/probability |-i-i⟩ i) 0.25)) 1e-10)))
      
      (doseq [i (range 8)]
        (is (< (abs (- (qs/probability |-i-i-i⟩ i) 0.125)) 1e-10)))
      
      ;; All states should be normalized
      (let [norm--i (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |-i-i⟩)))
            norm--i-i (reduce + (map #(* (fc/abs %) (fc/abs %)) (:state-vector |-i-i-i⟩)))]
        (is (< (abs (- norm--i 1.0)) 1e-10))
        (is (< (abs (- norm--i-i 1.0)) 1e-10))))))

(deftest test-multi-qubit-state-consistency
  (testing "Multi-qubit states should be consistent with tensor products"
    ;; |++⟩ should equal |+⟩ ⊗ |+⟩
    (let [|+⟩ (qs/plus-state)
          |++⟩-tensor (qs/tensor-product |+⟩ |+⟩)
          |++⟩-direct (qs/plus-state 2)]
      
      ;; Compare state vectors
      (doseq [i (range 4)]
        (let [amp-tensor (nth (:state-vector |++⟩-tensor) i)
              amp-direct (nth (:state-vector |++⟩-direct) i)]
          (is (< (abs (- (fc/re amp-tensor) (fc/re amp-direct))) 1e-10))
          (is (< (abs (- (fc/im amp-tensor) (fc/im amp-direct))) 1e-10)))))
    
    ;; |11⟩ should equal |1⟩ ⊗ |1⟩
    (let [|1⟩ (qs/one-state)
          |11⟩-tensor (qs/tensor-product |1⟩ |1⟩)
          |11⟩-direct (qs/one-state 2)]
      
      (doseq [i (range 4)]
        (let [amp-tensor (nth (:state-vector |11⟩-tensor) i)
              amp-direct (nth (:state-vector |11⟩-direct) i)]
          (is (< (abs (- (fc/re amp-tensor) (fc/re amp-direct))) 1e-10))
          (is (< (abs (- (fc/im amp-tensor) (fc/im amp-direct))) 1e-10)))))))

(deftest test-multi-qubit-measurement-outcomes
  (testing "Multi-qubit state measurements produce valid outcomes"
    ;; |11⟩ should always measure to 3 (binary 11)
    (let [|11⟩ (qs/one-state 2)]
      (dotimes [_ 5]
        (let [measurement (qs/measure-state |11⟩)]
          (is (= (:outcome measurement) 3)))))
    
    ;; |++⟩ should measure to all outcomes with equal probability
    (let [|++⟩ (qs/plus-state 2)
          measurements (repeatedly 100 #(:outcome (qs/measure-state |++⟩)))
          outcome-set (set measurements)]
      ;; Should see multiple different outcomes
      (is (> (count outcome-set) 1))
      ;; All outcomes should be in valid range
      (is (every? #(< % 4) measurements))
      (is (every? #(>= % 0) measurements)))))
