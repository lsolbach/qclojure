(ns org.soulspace.qclojure.domain.noise-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.noise :as noise]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.util.test :as util]))

;; Helper functions for testing
(defn state-norm
  "Calculate the norm of a quantum state."
  [state]
  (let [amplitudes (:state-vector state)]
    (fm/sqrt (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes)))))

(defn states-approximately-equal?
  "Check if two quantum states are approximately equal."
  ([state1 state2] (states-approximately-equal? state1 state2 1e-10))
  ([state1 state2 tolerance]
   (let [sv1 (:state-vector state1)
         sv2 (:state-vector state2)]
     (and (= (:num-qubits state1) (:num-qubits state2))
          (= (count sv1) (count sv2))
          (every? #(util/approx= (fc/abs (first %)) (fc/abs (second %)) tolerance)
                  (map vector sv1 sv2))))))

;; Tests for gate noise application
(deftest test-apply-gate-noise
  (testing "Gate noise application with different noise types"
    (testing "depolarizing noise"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.1}
            clean-state (qc/apply-operation-to-state initial-state h-gate)
            result-state (noise/apply-gate-noise clean-state h-gate noise-config)]

        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (vector? (:state-vector result-state)) "Should have valid state vector")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))

    (testing "amplitude damping noise"
      (let [initial-state (qs/one-state)
            x-gate {:operation-type :x :operation-params {:target 0}}
            noise-config {:noise-type :amplitude-damping
                          :noise-strength 0.1
                          :t1-time 100.0
                          :gate-time 20.0}
            clean-state (qc/apply-operation-to-state initial-state x-gate)
            result-state (noise/apply-gate-noise clean-state x-gate noise-config)]

        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))

    (testing "phase damping noise"
      (let [initial-state (qs/plus-state)
            z-gate {:operation-type :z :operation-params {:target 0}}
            noise-config {:noise-type :phase-damping
                          :noise-strength 0.1
                          :t2-time 50.0
                          :gate-time 20.0}
            clean-state (qc/apply-operation-to-state initial-state z-gate)
            result-state (noise/apply-gate-noise clean-state z-gate noise-config)]

        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))

    (testing "coherent error noise"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :coherent
                          :coherent-error {:rotation-angle 0.01 :rotation-axis :z}}
            clean-state (qc/apply-operation-to-state initial-state h-gate)
            result-state (noise/apply-gate-noise clean-state h-gate noise-config)]

        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))

    (testing "no noise case"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :unknown :noise-strength 0.1}
            clean-state (qc/apply-operation-to-state initial-state h-gate)
            result-state (noise/apply-gate-noise clean-state h-gate noise-config)]

        (is (states-approximately-equal? result-state clean-state 1e-10)
            "Unknown noise type should apply no noise")))))

;; Tests for readout noise application
(deftest test-apply-readout-noise
  (testing "Basic readout noise without correlations"
    (testing "no readout error configured"
      (let [state (qs/computational-basis-state 2 [1 0])
            noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.01}}}
            result (noise/apply-readout-noise state 2 noise-model)]
        (is (= result "10") "Should return clean measurement when no readout error configured")))

    (testing "readout error without correlations"
      (let [state (qs/computational-basis-state 2 [1 0])
            noise-model {:readout-error {:prob-0-to-1 0.0 :prob-1-to-0 0.0}}
            result (noise/apply-readout-noise state 2 noise-model)]
        (is (= result "10") "Should return clean measurement with zero error probabilities")))

    (testing "deterministic readout errors"
      (let [state (qs/computational-basis-state 2 [1 0])
            noise-model {:readout-error {:prob-0-to-1 1.0 :prob-1-to-0 1.0}}
            results (repeatedly 10 #(noise/apply-readout-noise state 2 noise-model))
            flipped-count (count (filter #(not= % "10") results))]
        (is (> flipped-count 5) "Should have some flipped results with high error probabilities"))))

  (testing "Readout noise with correlated errors"
    (testing "positive correlation increases error probability"
      (let [state (qs/computational-basis-state 3 [1 0 0])
            ;; High correlation: error on qubit 0 strongly affects qubit 1
            noise-model {:readout-error {:prob-0-to-1 0.5 :prob-1-to-0 0.5
                                         :correlated-errors {0 {1 3.0}}}}
            results (repeatedly 50 #(noise/apply-readout-noise state 3 noise-model))

            ;; Count results where both qubits 0 and 1 have errors
            both-errors (count (filter (fn [result]
                                         (and (= (nth result 0) \0)  ; qubit 0 flipped from 1->0
                                              (= (nth result 1) \1))) ; qubit 1 flipped from 0->1
                                       results))

            ;; Count results where only qubit 0 has error
            only-q0-error (count (filter (fn [result]
                                           (and (= (nth result 0) \0)  ; qubit 0 flipped
                                                (= (nth result 1) \0))) ; qubit 1 not flipped
                                         results))]

        ;; With positive correlation, we expect more cases where both qubits have errors
        (is (> both-errors 5) "Should have some cases with correlated errors")
        (is (> (+ both-errors only-q0-error) 10) "Should have reasonable number of errors on qubit 0")))

    (testing "anti-correlation decreases error probability"
      (let [state (qs/computational-basis-state 2 [1 0])
            ;; Anti-correlation: error on qubit 0 reduces error probability on qubit 1
            noise-model {:readout-error {:prob-0-to-1 0.8 :prob-1-to-0 0.8
                                         :correlated-errors {0 {1 0.1}}}}
            results (repeatedly 30 #(noise/apply-readout-noise state 2 noise-model))

            ;; Count cases where qubit 0 has error but qubit 1 doesn't
            q0-error-q1-clean (count (filter (fn [result]
                                               (and (= (nth result 0) \0)  ; qubit 0 flipped
                                                    (= (nth result 1) \0))) ; qubit 1 not flipped
                                             results))

            ;; Count cases where both have errors (should be rare with anti-correlation)
            both-errors (count (filter (fn [result]
                                         (and (= (nth result 0) \0)
                                              (= (nth result 1) \1)))
                                       results))]

        ;; With anti-correlation, when qubit 0 has error, qubit 1 should rarely have error
        (is (> q0-error-q1-clean both-errors) "Anti-correlation should reduce concurrent errors")))

    (testing "complex multi-qubit correlations"
      (let [state (qs/computational-basis-state 3 [1 1 0])
            noise-model {:readout-error {:prob-0-to-1 0.3 :prob-1-to-0 0.3
                                         :correlated-errors {0 {1 1.5 2 2.0}
                                                             1 {2 0.5}}}}
            results (repeatedly 20 #(noise/apply-readout-noise state 3 noise-model))]

        ;; Just test that the function runs without errors and produces valid bitstrings
        (is (every? string? results) "Should produce string results")
        (is (every? #(= 3 (count %)) results) "Should maintain correct qubit count")
        (is (every? #(every? (fn [c] (or (= c \0) (= c \1))) %) results) "Should contain only 0s and 1s")))

    (testing "Edge cases and validation"
      (testing "single qubit system"
        (let [state (qs/one-state)
              noise-model {:readout-error {:prob-0-to-1 0.1 :prob-1-to-0 0.1}}
              result (noise/apply-readout-noise state 1 noise-model)]
          (is (= 1 (count result)) "Should handle single qubit systems")
          (is (or (= result "0") (= result "1")) "Should produce valid single-qubit result")))

      (testing "correlation with out-of-bounds qubits"
        (let [state (qs/computational-basis-state 2 [1 0])
              ;; Correlation references non-existent qubit 5
              noise-model {:readout-error {:prob-0-to-1 0.5 :prob-1-to-0 0.5
                                           :correlated-errors {0 {5 2.0}}}}
              result (noise/apply-readout-noise state 2 noise-model)]
          ;; Should handle gracefully without crashing
          (is (= 2 (count result)) "Should handle out-of-bounds correlations gracefully")))

      (testing "self-correlation is ignored"
        (let [state (qs/computational-basis-state 2 [1 0])
              ;; Self-correlation should be ignored
              noise-model {:readout-error {:prob-0-to-1 0.5 :prob-1-to-0 0.5
                                           :correlated-errors {0 {0 10.0}}}}
              results (repeatedly 10 #(noise/apply-readout-noise state 2 noise-model))]
          ;; Should not crash and produce valid results
          (is (every? #(= 2 (count %)) results) "Should handle self-correlation gracefully")))

      (testing "empty correlations"
        (let [state (qs/computational-basis-state 2 [1 0])
              noise-model {:readout-error {:prob-0-to-1 0.1 :prob-1-to-0 0.1
                                           :correlated-errors {}}}
              result (noise/apply-readout-noise state 2 noise-model)]
          (is (= 2 (count result)) "Should handle empty correlations")))

      (testing "correlation factor edge cases"
        (let [state (qs/computational-basis-state 2 [1 0])]
          ;; Test with very large correlation factor
          (let [noise-model {:readout-error {:prob-0-to-1 0.1 :prob-1-to-0 0.1
                                             :correlated-errors {0 {1 100.0}}}}
                result (noise/apply-readout-noise state 2 noise-model)]
            (is (= 2 (count result)) "Should handle large correlation factors"))

          ;; Test with very small correlation factor  
          (let [noise-model {:readout-error {:prob-0-to-1 0.9 :prob-1-to-0 0.9
                                             :correlated-errors {0 {1 0.001}}}}
                result (noise/apply-readout-noise state 2 noise-model)]
            (is (= 2 (count result)) "Should handle small correlation factors")))))))

;; Edge cases and error handling
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "zero noise strength"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.0}
            clean-state (qc/apply-operation-to-state initial-state h-gate)
            result-state (noise/apply-gate-noise clean-state h-gate noise-config)]

        (is (states-approximately-equal? result-state clean-state 1e-10)
            "Zero noise should be equivalent to no noise")))

    (testing "maximum valid noise strength"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.75} ; Max for depolarizing
            clean-state (qc/apply-operation-to-state initial-state h-gate)
            result-state (noise/apply-gate-noise clean-state h-gate noise-config)]

        (is (= (:num-qubits result-state) 1) "Should handle maximum noise strength")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))))

(comment
  ;; Run all tests in this namespace
  (run-tests)
  ;
  )