(ns org.soulspace.qclojure.domain.measurement-test
  "Comprehensive tests for quantum measurement functionality"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [fastmath.complex :as fc]))

;;
;; Test basic quantum measurement functionality
;;
(deftest test-basic-measurement
  (testing "Measurement of computational basis states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)]
      ;; Test deterministic measurements
      (dotimes [_ 10]
        (let [measurement-0 (qs/measure-state |0⟩)
              measurement-1 (qs/measure-state |1⟩)]
          (is (= (:outcome measurement-0) 0) "Zero state should always measure 0")
          (is (= (:outcome measurement-1) 1) "One state should always measure 1")
          (is (> (:probability measurement-0) 0.99) "Zero state measurement probability should be ~1")
          (is (> (:probability measurement-1) 0.99) "One state measurement probability should be ~1")))))

  (testing "Measurement of superposition states"
    (let [|+⟩ (qs/plus-state)
          |-⟩ (qs/minus-state)]
      ;; Test probabilistic measurements
      (dotimes [_ 20]
        (let [measurement-plus (qs/measure-state |+⟩)
              measurement-minus (qs/measure-state |-⟩)]
          (is (contains? #{0 1} (:outcome measurement-plus)) 
              "Plus state should measure 0 or 1")
          (is (contains? #{0 1} (:outcome measurement-minus)) 
              "Minus state should measure 0 or 1")
          (is (< 0.4 (:probability measurement-plus) 0.6) 
              "Plus state measurement probability should be ~0.5")
          (is (< 0.4 (:probability measurement-minus) 0.6) 
              "Minus state measurement probability should be ~0.5")))))

  #_(testing "Statistical properties of measurements"
    (let [|+⟩ (qs/plus-state)
          num-measurements 1000
          statistics (qs/measure-state-statistics |+⟩ num-measurements)]
      ;; Check that we get approximately equal frequencies for 0 and 1
      (let [freq-0 (get (:frequencies statistics) 0 0)
            freq-1 (get (:frequencies statistics) 1 0)
            total (+ freq-0 freq-1)]
        (is (= total num-measurements) "Total measurements should match")
        (is (< 0.4 (/ freq-0 total) 0.6) "Frequency of 0 should be ~0.5")
        (is (< 0.4 (/ freq-1 total) 0.6) "Frequency of 1 should be ~0.5")))))

;;
;; Test multi-qubit measurement
;;
(deftest test-multi-qubit-measurement
  (testing "Two-qubit computational basis measurements"
    (let [|00⟩ (qs/zero-state 2)
          |01⟩ (qs/computational-basis-state 2 [0 1])
          |10⟩ (qs/computational-basis-state 2 [1 0])
          |11⟩ (qs/computational-basis-state 2 [1 1])]
      
      (dotimes [_ 5]
        (let [m00 (qs/measure-state |00⟩)
              m01 (qs/measure-state |01⟩)
              m10 (qs/measure-state |10⟩)
              m11 (qs/measure-state |11⟩)]
          (is (= (:outcome m00) 0) "|00⟩ should measure as outcome 0")
          (is (= (:outcome m01) 1) "|01⟩ should measure as outcome 1")
          (is (= (:outcome m10) 2) "|10⟩ should measure as outcome 2")
          (is (= (:outcome m11) 3) "|11⟩ should measure as outcome 3")))))

  (testing "Bell state measurements"
    ;; Create a Bell state (|00⟩ + |11⟩)/√2
    (let [bell-state (qs/normalize-state 
                       (qs/multi-qubit-state 
                         [(fc/complex (/ 1 (Math/sqrt 2)) 0)  ; |00⟩
                          (fc/complex 0 0)                    ; |01⟩
                          (fc/complex 0 0)                    ; |10⟩
                          (fc/complex (/ 1 (Math/sqrt 2)) 0)]))  ; |11⟩
          measurements (repeatedly 100 #(:outcome (qs/measure-state bell-state)))
          outcomes (set measurements)]
      ;; Bell state should only measure 0 (|00⟩) or 3 (|11⟩), never 1 or 2
      (is (every? #(contains? #{0 3} %) measurements) 
          "Bell state should only measure |00⟩ or |11⟩")
      (is (contains? outcomes 0) "Should sometimes measure |00⟩")
      (is (contains? outcomes 3) "Should sometimes measure |11⟩"))))

;;
;; Test partial measurement functionality  
;;
(deftest test-partial-measurement
  (testing "Single qubit measurement in multi-qubit system"
    (let [initial-state (qs/zero-state 2)
          partial-measurement (qs/measure-specific-qubits initial-state [0])]
      (is (= (:outcomes partial-measurement) [0]) 
          "Measuring qubit 0 of |00⟩ should give [0]")
      (is (map? (:probabilities partial-measurement)) 
          "Should return probability map")
      (is (map? (:collapsed-state partial-measurement)) 
          "Should return collapsed state")))

  (testing "Measurement preserves normalization"
    (let [|+0⟩ (qs/tensor-product (qs/plus-state) (qs/zero-state 1))
          measurement (qs/measure-specific-qubits |+0⟩ [1])
          collapsed-state (:collapsed-state measurement)
          amplitudes (:state-vector collapsed-state)
          norm-squared (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (is (< (Math/abs (- norm-squared 1.0)) 1e-10) 
          "State should remain normalized after partial measurement"))))

;;
;; Test circuit integration
;;
(deftest test-circuit-measurement
  (testing "Simple circuit with measurement"
    (let [circuit (-> (qc/create-circuit 1 "Test")
                     (qc/h-gate 0)
                     (qc/measure-operation [0]))
          initial-state (qs/zero-state 1)]
      ;; Execute circuit
      (dotimes [_ 10]
        (let [final-state (:final-state (qc/execute-circuit circuit initial-state))]
          (is (map? final-state) "Circuit execution should return a state")
          ;; The final state should be either |0⟩ or |1⟩ due to measurement
          (let [amplitudes (:state-vector final-state)
                non-zero-amps (filter #(> (fc/abs %) 1e-10) amplitudes)]
            (is (= (count non-zero-amps) 1) 
                "After measurement, only one amplitude should be non-zero"))))))

  (testing "Two-qubit circuit with partial measurement"
    (let [circuit (-> (qc/create-circuit 2 "Bell Circuit")
                     (qc/h-gate 0)
                     (qc/cnot-gate 0 1)
                     (qc/measure-operation [0]))  ; Measure only first qubit
          initial-state (qs/zero-state 2)]
      ;; Execute multiple times to see probabilistic behavior
      (dotimes [_ 10]
        (let [final-state (:final-state (qc/execute-circuit circuit initial-state))]
          (is (map? final-state) "Circuit execution should return a state")
          (is (= (:num-qubits final-state) 2) "Should still be a 2-qubit state")))))

  (testing "Measurement of all qubits"
    (let [circuit (-> (qc/create-circuit 2 "Full Measurement")
                     (qc/h-gate 0)
                     (qc/h-gate 1)
                     (qc/measure-all-operation))
          initial-state (qs/zero-state 2)]
      (dotimes [_ 5]
        (let [final-state (:final-state (qc/execute-circuit circuit initial-state))
              amplitudes (:state-vector final-state)
              non-zero-count (count (filter #(> (fc/abs %) 1e-10) amplitudes))]
          (is (= non-zero-count 1) 
              "After measuring all qubits, only one amplitude should be non-zero"))))))

;;
;; Test circuit integration with measurement gates
;;
(deftest test-circuit-measurement-integration
  (testing "Circuit execution with measurement gates"
    (let [bell-circuit (-> (qc/create-circuit 2 "Bell State with Measurement")
                           (qc/h-gate 0)
                           (qc/cnot-gate 0 1)
                           (qc/measure-operation [0 1]))
          initial-state (qs/zero-state 2)
          final-state (:final-state (qc/execute-circuit bell-circuit initial-state))
          state-vector (:state-vector final-state)
          num-qubits (:num-qubits final-state)
          non-zero-amplitudes (filter #(> (fc/abs %) 1e-10) state-vector)
          num-non-zero (count non-zero-amplitudes)]
      (is (= num-qubits 2))
      (is (= num-non-zero 1) "After measurement, only one amplitude should be non-zero")))

  (testing "Single qubit measurement in circuit"
    (let [circuit (-> (qc/create-circuit 2 "Partial Measurement")
                      (qc/h-gate 0)
                      (qc/h-gate 1)
                      (qc/measure-operation [0]))
          initial-state (qs/zero-state 2)
          final-state (:final-state (qc/execute-circuit circuit initial-state))
          state-vector (:state-vector final-state)
          non-zero-amplitudes (filter #(> (fc/abs %) 1e-10) state-vector)
          num-non-zero (count non-zero-amplitudes)]
      (is (= num-non-zero 2) "After partial measurement, two amplitudes should remain")))

  (testing "Measurement gate with superposition collapse"
    (let [circuit (-> (qc/create-circuit 1 "Plus State Measurement")
                      (qc/h-gate 0)
                      (qc/measure-operation [0]))
          initial-state (qs/zero-state 1)
          final-state (:final-state (qc/execute-circuit circuit initial-state))
          state-vector (:state-vector final-state)
          non-zero-amplitudes (filter #(> (fc/abs %) 1e-10) state-vector)
          num-non-zero (count non-zero-amplitudes)]
      (is (= num-non-zero 1) "After measurement, only one amplitude should be non-zero")
      (is (< (Math/abs (- (fc/abs (first non-zero-amplitudes)) 1.0)) 1e-10)
          "The non-zero amplitude should have magnitude 1"))))

;;
;; Test multi-qubit measurement with entanglement
;;
(deftest test-entangled-measurement
  (testing "Bell state measurement preserves correlations"
    (let [;; Test multiple executions to verify statistical correlations
          bell-circuit (-> (qc/create-circuit 2 "Bell State")
                           (qc/h-gate 0)
                           (qc/cnot-gate 0 1))
          initial-state (qs/zero-state 2)
          bell-state (:final-state (qc/execute-circuit bell-circuit initial-state))
          
          ;; Perform many measurements to check correlations
          num-trials 100
          measurement-results (repeatedly num-trials
                                          #(qs/measure-specific-qubits bell-state [0 1]))]
      
      ;; All measurements should show perfect correlation (both qubits same)
      (doseq [result measurement-results]
        (let [outcomes (:outcomes result)
              ;; outcomes is a vector like [0 0] or [1 1] for Bell state
              qubit0 (first outcomes)
              qubit1 (second outcomes)]
          (is (= qubit0 qubit1) "Bell state measurements should be perfectly correlated")))))

  (testing "GHZ state three-qubit correlations"
    (let [;; Create a 3-qubit GHZ state: (|000⟩ + |111⟩)/√2
          ghz-circuit (-> (qc/create-circuit 3 "GHZ State")
                          (qc/h-gate 0)
                          (qc/cnot-gate 0 1)
                          (qc/cnot-gate 0 2))
          initial-state (qs/zero-state 3)
          ghz-state (:final-state (qc/execute-circuit ghz-circuit initial-state))
          
          ;; Perform measurements
          num-trials 50
          measurement-results (repeatedly num-trials
                                          #(qs/measure-specific-qubits ghz-state [0 1 2]))]
      
      ;; All measurements should be either 000 or 111 (perfect correlation)
      (doseq [result measurement-results]
        (let [outcomes (:outcomes result)
              ;; Convert outcomes vector [0 0 0] or [1 1 1] to integer 0 or 7
              outcome (qs/bits-to-index outcomes)]
          (is (contains? #{0 7} outcome) "GHZ measurements should be either |000⟩ or |111⟩"))))))

;;
;; Test measurement probability calculations
;;
(deftest test-measurement-probabilities
  (testing "Probability calculation for basis states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          |+⟩ (qs/plus-state)]
      (is (= (qs/probability |0⟩ 0) 1.0) "P(|0⟩|0⟩) = 1")
      (is (= (qs/probability |0⟩ 1) 0.0) "P(|1⟩|0⟩) = 0")
      (is (= (qs/probability |1⟩ 0) 0.0) "P(|0⟩|1⟩) = 0")
      (is (= (qs/probability |1⟩ 1) 1.0) "P(|1⟩|1⟩) = 1")
      (is (< (Math/abs (- (qs/probability |+⟩ 0) 0.5)) 1e-10) "P(|0⟩|+⟩) = 0.5")
      (is (< (Math/abs (- (qs/probability |+⟩ 1) 0.5)) 1e-10) "P(|1⟩|+⟩) = 0.5")))

  (testing "Probability vector calculation"
    (let [|+⟩ (qs/plus-state)
          probs (qs/measurement-probabilities |+⟩)]
      (is (= (count probs) 2) "Single qubit should have 2 probabilities")
      (is (< (Math/abs (- (nth probs 0) 0.5)) 1e-10) "P(0) should be 0.5")
      (is (< (Math/abs (- (nth probs 1) 0.5)) 1e-10) "P(1) should be 0.5")
      (is (< (Math/abs (- (reduce + probs) 1.0)) 1e-10) "Probabilities should sum to 1"))))

;;
;; Test utility functions
;;
(deftest test-measurement-utilities
  (testing "Outcome to bits conversion"
    (is (= (qs/measurement-outcomes-to-bits 0 1) [0]) "0 in 1 bit = [0]")
    (is (= (qs/measurement-outcomes-to-bits 1 1) [1]) "1 in 1 bit = [1]")
    (is (= (qs/measurement-outcomes-to-bits 0 2) [0 0]) "0 in 2 bits = [0 0]")
    (is (= (qs/measurement-outcomes-to-bits 1 2) [0 1]) "1 in 2 bits = [0 1]")
    (is (= (qs/measurement-outcomes-to-bits 2 2) [1 0]) "2 in 2 bits = [1 0]")
    (is (= (qs/measurement-outcomes-to-bits 3 2) [1 1]) "3 in 2 bits = [1 1]")
    (is (= (qs/measurement-outcomes-to-bits 5 3) [1 0 1]) "5 in 3 bits = [1 0 1]"))

  (testing "Statistical measurement analysis"
    (let [|0⟩ (qs/zero-state 1)
          stats (qs/measure-state-statistics |0⟩ 100)]
      (is (= (:total-measurements stats) 100) "Should record correct total")
      (is (vector? (:outcomes stats)) "Should return outcomes vector")
      (is (map? (:frequencies stats)) "Should return frequencies map")
      (is (map? (:probabilities stats)) "Should return probabilities map")
      (is (map? (:expected-probabilities stats)) "Should return expected probabilities")
      ;; For |0⟩ state, all measurements should be 0
      (is (every? #(= % 0) (:outcomes stats)) "All outcomes should be 0 for |0⟩ state"))))

;;
;; Performance and edge case tests
;;
(deftest test-measurement-edge-cases
  (testing "Measurement of very small amplitudes"
    (let [;; Create a state with very small but non-zero amplitude
          tiny-amplitude 1e-10
          state {:state-vector [(fc/complex (Math/sqrt (- 1 (* tiny-amplitude tiny-amplitude))) 0)
                               (fc/complex tiny-amplitude 0)]
                 :num-qubits 1}
          measurements (repeatedly 1000 #(:outcome (qs/measure-state state)))
          outcome-1-count (count (filter #(= % 1) measurements))]
      ;; Should very rarely measure outcome 1
      (is (< outcome-1-count 50) "Should rarely measure the tiny amplitude state")))

  (testing "Normalization error handling"
    ;; This tests robustness - the system should handle near-normalized states
    (let [;; Create a state that's within tolerance (deviation < 1e-8)
          slightly-unnormalized {:state-vector [(fc/complex 0.7071067805519557 0) 
                                               (fc/complex 0.7071067818211393 0)]
                                :num-qubits 1}]
      ;; Should not throw an exception for states within normalization tolerance
      (is (map? (qs/measure-state slightly-unnormalized))
          "Should handle slightly unnormalized states gracefully"))))


(comment 
  (run-tests)
  ;
  )
