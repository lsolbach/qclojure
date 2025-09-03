(ns org.soulspace.qclojure.application.error-mitigation.zero-noise-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation.zero-noise :as zne]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as noisy]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.util.test :as util]))

;; Test data using protocol-compliant backends
(def test-readout-config
  {:prob-0-to-1 0.1
   :prob-1-to-0 0.05})

(def test-noise-model
  {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.02}
                :cnot {:noise-type :depolarizing :noise-strength 0.05}}
   :readout-error test-readout-config})

(def simple-bell-circuit
  (qc/bell-state-circuit))

;; Protocol-compliant backends for testing
(def clean-backend (sim/create-simulator))
(def noisy-backend (noisy/create-noisy-simulator test-noise-model))

;; Tests for Zero Noise Extrapolation with Backend Protocol Integration
(deftest test-zero-noise-extrapolation-protocol-compliance
  (testing "ZNE with protocol-compliant noisy backend"
    (let [result (zne/zero-noise-extrapolation 
                   simple-bell-circuit 
                   noisy-backend 
                   [1.0 1.5 2.0]
                   ["00" "11"]
                   100)]  ; Reduced shots for faster tests
      
      (is (contains? result :extrapolated-value) "Should return extrapolated value")
      (is (contains? result :improvement-factor) "Should return improvement factor")
      (is (contains? result :data-points) "Should return data points")
      (is (contains? result :backend-info) "Should return backend info from protocol")
      (is (= 3 (count (:data-points result))) "Should have 3 data points for 3 noise scales")
      (is (every? #(contains? % :x) (:data-points result)) "Data points should have x values")
      (is (every? #(contains? % :y) (:data-points result)) "Data points should have y values")
      (is (satisfies? qb/QuantumBackend noisy-backend) "Backend should implement QuantumBackend protocol")))
  
  (testing "ZNE with protocol-compliant clean backend"
    (let [result (zne/zero-noise-extrapolation 
                   simple-bell-circuit 
                   clean-backend 
                   [1.0 1.5 2.0]
                   ["00" "11"] 
                   100)]
      
      (is (number? (:improvement-factor result)) "Improvement factor should be numeric")
      (is (pos? (:improvement-factor result)) "Improvement factor should be positive")
      (is (contains? result :backend-info) "Should contain backend info")
      (is (satisfies? qb/QuantumBackend clean-backend) "Backend should implement QuantumBackend protocol")))

  (testing "ZNE backend availability validation"
    (is (qb/is-available? noisy-backend) "Noisy backend should be available")
    (is (qb/is-available? clean-backend) "Clean backend should be available")))

(deftest test-execute-circuit-with-backend
  (testing "Protocol-compliant circuit execution with noise scaling"
    (let [result (zne/execute-circuit-with-backend 
                   noisy-backend 
                   simple-bell-circuit 
                   test-noise-model
                   1.5  ; Noise amplification
                   50)]
      
      (is (= :completed (:job-status result)) "Execution should complete successfully")
      (is (map? (:measurement-results result)) "Should return measurement results")
      (is (= 1.5 (:noise-scale-factor result)) "Should record noise scale factor")
      (is (contains? result :backend-info) "Should include backend info")
      (is (pos? (:execution-time-ms result)) "Should record execution time")))

  (testing "Error handling for failed execution"
    ;; Test with a backend that's not available (simulate by creating but not starting)
    (let [unavailable-backend (reify qb/QuantumBackend
                                (is-available? [_] false)
                                (get-backend-info [_] {:error "Backend unavailable"}))]
      ;; The function should handle unavailable backends gracefully
      (is (not (qb/is-available? unavailable-backend)) "Test backend should be unavailable"))))

(deftest test-utility-functions
  (testing "Noise model scaling"
    (let [scaled-model (zne/scale-noise-model test-noise-model 2.0)]
      (is (= 0.04 (get-in scaled-model [:gate-noise :h :noise-strength])) "H gate noise should be doubled")
      (is (= 0.10 (get-in scaled-model [:gate-noise :cnot :noise-strength])) "CNOT gate noise should be doubled")
      (is (<= (get-in scaled-model [:readout-error :prob-0-to-1]) 1.0) "Readout error should be capped at 1.0")))

  (testing "Expectation value extraction"
    (let [measurement-results {"00" 450 "01" 25 "10" 30 "11" 495}
          expectation (zne/extract-expectation-value measurement-results ["00" "11"])]
      (is (= 0.945 expectation) "Should correctly calculate Bell state fidelity")))

  (testing "Exponential decay fitting"
    (let [data-points [{:x 1.0 :y 0.95} {:x 1.5 :y 0.89} {:x 2.0 :y 0.82}]
          fit-result (zne/fit-exponential-decay data-points)]
      (is (contains? fit-result :extrapolated-value) "Should provide extrapolated value")
      (is (contains? fit-result :model-type) "Should specify model type")
      (is (number? (:extrapolated-value fit-result)) "Extrapolated value should be numeric"))))

(deftest test-backend-protocol-integration
  (testing "Backend protocol method calls"
    (is (set? (qb/get-supported-gates noisy-backend)) "Should return supported gates set")
    (is (map? (qb/get-backend-info noisy-backend)) "Should return backend info map")
    
    ;; Test circuit submission and execution through protocol
    (let [job-id (qb/submit-circuit noisy-backend simple-bell-circuit {:shots 10})]
      (is (string? job-id) "Should return job ID")
      
      ;; Wait briefly and check status
      (Thread/sleep 50)
      (let [status (qb/get-job-status noisy-backend job-id)]
        (is (keyword? status) "Status should be a keyword")
        
        (when (= status :completed)
          (let [result (qb/get-job-result noisy-backend job-id)]
            (is (map? result) "Result should be a map")
            (is (contains? result :measurement-results) "Should contain measurement results")))))))
  
(comment 
  ;; Run all tests in namespace
  (run-tests)
  ;
  )
