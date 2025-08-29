(ns org.soulspace.qclojure.application.noise-test
  "Comprehensive tests for quantum noise application functions."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.noise :as noise]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;; Helper functions for testing
;; Test fixtures and data
(def test-noise-model
  {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.001
                    :t1-time 125.0 :t2-time 89.0 :gate-time 35.6}
                :x {:noise-type :amplitude-damping :noise-strength 0.0005
                    :t1-time 125.0 :t2-time 89.0 :gate-time 35.6}
                :cnot {:noise-type :depolarizing :noise-strength 0.006
                       :t1-time 125.0 :t2-time 89.0 :gate-time 476.0}}
   :readout-error {:prob-0-to-1 0.013 :prob-1-to-0 0.028}})

;; Tests for circuit fidelity estimation
(deftest test-estimate-circuit-fidelity
  (testing "Circuit fidelity estimation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0)
                        (qc/cnot-gate 0 1))
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]

        (is (contains? fidelity-data :estimated-fidelity) "Should contain fidelity estimate")
        (is (contains? fidelity-data :total-estimated-error) "Should contain total error")
        (is (contains? fidelity-data :gate-counts) "Should contain gate counts")
        (is (contains? fidelity-data :dominant-error-sources) "Should contain error sources")
        (is (> (:estimated-fidelity fidelity-data) 0.9) "Should have high fidelity for simple circuit")
        (is (< (:total-estimated-error fidelity-data) 0.1) "Should have low total error")))

    (testing "complex circuit"
      (let [circuit (-> (qc/create-circuit 3)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)
                        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2) (qc/cnot-gate 0 2))
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]

        (is (< (:estimated-fidelity fidelity-data)
               (:estimated-fidelity (noise/estimate-circuit-fidelity (qc/create-circuit 1) test-noise-model)))
            "Complex circuit should have lower fidelity than simple circuit")))

    (testing "empty circuit"
      (let [circuit (qc/create-circuit 1)
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]

        (is (= (:estimated-fidelity fidelity-data) 1.0) "Empty circuit should have perfect fidelity")
        (is (= (:total-estimated-error fidelity-data) 0.0) "Empty circuit should have no error")))))

;; Tests for platform comparison
(deftest test-compare-hardware-platforms
  (testing "Hardware platform comparison"
    (let [circuit (qc/ghz-state-circuit 3)
          platforms {:ibm-test {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.005}
                                             :cnot {:noise-type :depolarizing :noise-strength 0.01}}
                                :readout-error {:prob-0-to-1 0.02 :prob-1-to-0 0.03}}
                     :ionq-test {:gate-noise {:h {:noise-type :coherent :coherent-error {:rotation-angle 0.001 :rotation-axis :y}}
                                              :cnot {:noise-type :depolarizing :noise-strength 0.003}}
                                 :readout-error {:prob-0-to-1 0.001 :prob-1-to-0 0.002}}}
          comparison (noise/compare-hardware-platforms circuit platforms)]

      (is (map? comparison) "Should return comparison map")
      (is (contains? comparison :ibm-test) "Should contain IBM test platform")
      (is (contains? comparison :ionq-test) "Should contain IonQ test platform")

      (let [ibm-data (:ibm-test comparison)
            ionq-data (:ionq-test comparison)]
        (is (contains? ibm-data :estimated-fidelity) "Should contain fidelity for IBM")
        (is (contains? ionq-data :estimated-fidelity) "Should contain fidelity for IonQ")
        (is (contains? ibm-data :platform-type) "Should identify platform type")
        (is (= :superconducting (:platform-type ibm-data)) "Should identify IBM as superconducting")
        (is (= :trapped-ion (:platform-type ionq-data)) "Should identify IonQ as trapped ion")))))

;; Tests for utility functions
(deftest test-noise-aware-circuit-depth
  (testing "Noise-aware circuit depth calculation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0)
                        (qc/cnot-gate 0 1))
            depth (noise/noise-aware-circuit-depth circuit test-noise-model)]

        (is (number? depth) "Should return a number")
        (is (> depth 2) "Should be greater than basic gate count due to noise penalty")))

    (testing "empty circuit"
      (let [circuit (qc/create-circuit 1)
            depth (noise/noise-aware-circuit-depth circuit test-noise-model)]

        (is (= depth 0) "Empty circuit should have zero depth")))))

(deftest test-recommend-error-mitigation
  (testing "Error mitigation recommendations"
    (testing "high fidelity circuit"
      (let [circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            low-noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.0001}}}
            recommendations (noise/recommend-error-mitigation circuit low-noise-model)]

        (is (vector? recommendations) "Should return vector of recommendations")
        (is (every? map? recommendations) "Each recommendation should be a map")))

    (testing "low fidelity circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0) (qc/cnot-gate 0 1)
                        (qc/h-gate 0) (qc/cnot-gate 0 1))
            high-noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.1}
                                           :cnot {:noise-type :depolarizing :noise-strength 0.2}}}
            recommendations (noise/recommend-error-mitigation circuit high-noise-model)]

        (is (vector? recommendations) "Should return vector of recommendations")
        (is (pos? (count recommendations)) "Should have some recommendations for noisy circuit")))

    (testing "deep circuit"
      (let [circuit (reduce #(qc/h-gate %1 (mod %2 2)) (qc/create-circuit 2) (range 25))
            recommendations (noise/recommend-error-mitigation circuit test-noise-model)]

        (is (some #(= (:strategy %) :circuit-cutting) recommendations)
            "Should recommend circuit cutting for deep circuits")))))

;; Edge cases and error handling
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "circuit with unsupported gate types"
      (let [circuit {:num-qubits 1 :operations [{:operation-type :custom-gate :operation-params {:target 0}}]}
            noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.01}}}
            fidelity-data (noise/estimate-circuit-fidelity circuit noise-model)]

        (is (= (:estimated-fidelity fidelity-data) 1.0) "Unsupported gates should contribute no error")
        (is (= (:total-estimated-error fidelity-data) 0.0) "Should handle unsupported gates gracefully")))))

(comment
  ; Run all tests in this namespace
  (run-tests)
  ;
  )