(ns org.soulspace.qclojure.application.error-mitigation.protocol-integration-test
  "Comprehensive tests for error mitigation protocol integration.
  
  These tests validate that all error mitigation functions work correctly
  with the QuantumBackend protocol across different backend implementations."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation :as em]
            [org.soulspace.qclojure.application.error-mitigation.zero-noise :as zne]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as noisy]
            [org.soulspace.qclojure.application.backend :as qb]))

;; Test fixtures
(def test-circuit (qc/bell-state-circuit))
(def test-noise-model
  {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.02}
                :cnot {:noise-type :depolarizing :noise-strength 0.05}}
   :readout-error {:prob-0-to-1 0.1 :prob-1-to-0 0.05}})

(def clean-backend (sim/create-simulator))
(def noisy-backend (noisy/create-noisy-simulator test-noise-model))

(deftest test-error-mitigation-protocol-integration
  (testing "Error mitigation with protocol-compliant backends"
    (testing "ZNE with noisy backend"
      (let [config {:strategies [:zero-noise-extrapolation]
                    :num-shots 100}
            result (em/mitigate-errors test-circuit noisy-backend config)]
        
        (is (map? result) "Should return result map")
        (is (contains? result :mitigation-applied) "Should indicate mitigation was applied")
        (is (contains? result :backend-info) "Should include backend info")
        (is (contains? (:improvements result) :zero-noise-extrapolation) "Should contain ZNE results")
        (is (number? (:total-execution-time-ms result)) "Should record execution time")
        (is (satisfies? qb/QuantumBackend noisy-backend) "Backend should be protocol compliant")))

    (testing "Multiple strategies with noisy backend"
      (let [config {:strategies [:readout-error-mitigation :zero-noise-extrapolation]
                    :num-shots 100}
            result (em/mitigate-errors test-circuit noisy-backend config)]
        
        (is (contains? (:improvements result) :zero-noise-extrapolation) "Should apply ZNE")
        (is (contains? (:improvements result) :readout-error-mitigation) "Should apply REM")
        (is (number? (:overall-improvement-factor result)) "Should calculate overall improvement")))

    (testing "Clean backend integration"
      (let [config {:strategies [:zero-noise-extrapolation]
                    :num-shots 100}
            result (em/mitigate-errors test-circuit clean-backend config)]
        
        (is (map? result) "Should work with clean backend")
        (is (contains? result :backend-info) "Should include clean backend info")
        (is (satisfies? qb/QuantumBackend clean-backend) "Clean backend should be protocol compliant")))

    (testing "Circuit optimization integration"
      (let [config {:strategies [:circuit-optimization :zero-noise-extrapolation]
                    :num-shots 100}
            result (em/mitigate-errors test-circuit noisy-backend config)]
        
        (is (contains? (:improvements result) :circuit-optimization) "Should apply circuit optimization")
        (is (contains? (:improvements result) :zero-noise-extrapolation) "Should apply ZNE after optimization")))))

(deftest test-backend-protocol-compliance
  (testing "Backend protocol methods work correctly"
    (testing "Noisy backend protocol methods"
      (is (qb/is-available? noisy-backend) "Should be available")
      (is (set? (qb/get-supported-gates noisy-backend)) "Should return supported gates")
      (is (map? (qb/get-backend-info noisy-backend)) "Should return backend info")
      
      ;; Test circuit execution through protocol
      (let [job-id (qb/submit-circuit noisy-backend test-circuit {:shots 50})]
        (is (string? job-id) "Should return job ID")
        
        ;; Wait for completion
        (Thread/sleep 100)
        (let [status (qb/get-job-status noisy-backend job-id)]
          (is (keyword? status) "Should return status keyword")
          
          (when (= status :completed)
            (let [result (qb/get-job-result noisy-backend job-id)]
              (is (map? result) "Should return result map")
              (is (contains? result :measurement-results) "Should contain measurements"))))))

    (testing "Clean backend protocol methods"
      (is (qb/is-available? clean-backend) "Should be available")
      (is (set? (qb/get-supported-gates clean-backend)) "Should return supported gates")
      (is (map? (qb/get-backend-info clean-backend)) "Should return backend info"))))

(deftest test-zne-protocol-integration
  (testing "ZNE functions with protocol backends"
    (testing "execute-circuit-with-backend"
      (let [result (zne/execute-circuit-with-backend 
                     noisy-backend test-circuit test-noise-model 1.5 50)]
        
        (is (= :completed (:job-status result)) "Should complete successfully")
        (is (map? (:measurement-results result)) "Should return measurements")
        (is (= 1.5 (:noise-scale-factor result)) "Should record noise scale")
        (is (contains? result :backend-info) "Should include backend info")))

    (testing "zero-noise-extrapolation end-to-end"
      (let [result (zne/zero-noise-extrapolation 
                     test-circuit noisy-backend [1.0 1.5 2.0] ["00" "11"] 100)]
        
        (is (number? (:extrapolated-value result)) "Should extrapolate value")
        (is (number? (:improvement-factor result)) "Should calculate improvement")
        (is (= 3 (count (:data-points result))) "Should have data for all noise scales")
        (is (contains? result :backend-info) "Should include backend information")))))

(deftest test-error-handling
  (testing "Error handling with protocol backends"
    (testing "Backend availability validation"
      ;; Create a mock unavailable backend
      (let [unavailable-backend (reify qb/QuantumBackend
                                  (is-available? [_] false)
                                  (get-backend-info [_] {:error "Backend unavailable"}))]
        (is (not (qb/is-available? unavailable-backend)) "Should detect unavailable backend")))

    (testing "Graceful degradation"
      ;; Test with very small shot counts
      (let [config {:strategies [:zero-noise-extrapolation]
                    :num-shots 1}  ; Very small for edge case testing
            result (em/mitigate-errors test-circuit noisy-backend config)]
        
        (is (map? result) "Should handle small shot counts gracefully")))))

(deftest test-performance-characteristics
  (testing "Performance with protocol backends"
    (testing "Execution time bounds"
      (let [config {:strategies [:zero-noise-extrapolation]
                    :num-shots 100}
            start-time (System/currentTimeMillis)
            result (em/mitigate-errors test-circuit noisy-backend config)
            end-time (System/currentTimeMillis)
            total-time (- end-time start-time)]
        
        (is (< total-time 5000) "Should complete within reasonable time")
        (is (pos? (:total-execution-time-ms result)) "Should record positive execution time")))

    (testing "Resource utilization"
      ;; Test multiple concurrent executions don't interfere
      (let [config {:strategies [:zero-noise-extrapolation]
                    :num-shots 50}
            futures (repeatedly 3 #(future (em/mitigate-errors test-circuit noisy-backend config)))
            results (map deref futures)]
        
        (is (= 3 (count results)) "Should handle concurrent executions")
        (is (every? map? results) "All executions should succeed")))))

(comment
  ;; Run all tests
  (run-tests)
  ;
  )
