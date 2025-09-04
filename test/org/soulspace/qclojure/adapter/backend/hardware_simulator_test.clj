(ns org.soulspace.qclojure.adapter.backend.hardware-simulator-test
  "Comprehensive tests for the local noisy simulator backend."
  (:require [clojure.test :refer [deftest is testing use-fixtures run-tests]]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as noisy]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.application.noise-analysis :as noise]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.util.test :as util]))

;; Test fixtures
(defn reset-simulator-state-fixture
  "Reset noisy simulator state before each test."
  [f]
  (noisy/reset-simulator-state!)
  (f))

(use-fixtures :each reset-simulator-state-fixture)

;; Helper functions for testing
;; Tests for simulator creation and basic functionality
(deftest test-create-noisy-simulator
  (testing "Simulator creation"
    (testing "with noise model"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))]
        (is (satisfies? qb/QuantumBackend simulator) "Should implement QuantumBackend protocol")
        (is (qb/is-available? simulator) "Should be available")
        (is (contains? (qb/get-supported-gates simulator) :h) "Should support H gate")
        (is (contains? (qb/get-supported-gates simulator) :cnot) "Should support CNOT gate")))
    
    (testing "backend info"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))
            info (qb/get-backend-info simulator)]
        (is (= :quantum-hardware-simulator (:backend-type info)) "Should have correct backend type")
        (is (= (noisy/noise-model-for :ibm-lagos) (:noise-model info)) "Should contain noise model")
        (is (contains? (:capabilities info) :depolarizing-noise) "Should support depolarizing noise")
        (is (contains? (:capabilities info) :readout-errors) "Should support readout errors")))))

;; Tests for circuit execution
(deftest test-circuit-execution
  (testing "Circuit execution with noise"
    (testing "single qubit circuit"
      (let [simulator (noisy/create-noisy-simulator {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.01}}})
            circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            job-id (qb/submit-circuit simulator circuit {:shots 100})]
        (is (string? job-id) "Should return job ID")

        ; Wait for completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should complete")

        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Result should be completed")
          (is (= 100 (:shots-executed result)) "Should execute correct number of shots")
          (is (true? (:noise-applied result)) "Should indicate noise was applied")
          (is (map? (:measurement-results result)) "Should contain measurement results")
          (is (= 100 (reduce + (vals (:measurement-results result)))) "Shot counts should sum to total"))))

    (testing "GHZ circuit with realistic noise"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))
            circuit (qc/ghz-state-circuit 3)
            job-id (qb/submit-circuit simulator circuit {:shots 1000})]

        ; FIXME flaky test, sometimes fails
        ; Wait for completion
        (Thread/sleep 1000)
        (let [result (qb/get-job-result simulator job-id)
              measurements (:measurement-results result)
              ideal-states (+ (get measurements "000" 0) (get measurements "111" 0))
              total-shots (reduce + (vals measurements))
              ideal-percentage (/ ideal-states total-shots)]

          (is (> ideal-percentage 0.85) "Should preserve most entanglement with realistic noise")
          (is (< ideal-percentage 1.0) "Should show some noise effects")
          (is (>= (count measurements) 2) "Should have at least ideal states")
          (is (<= (count measurements) 8) "Should not exceed possible outcomes"))))))

;; Tests for job management
(deftest test-job-management
  (testing "Job management functionality"
    (testing "job status tracking"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 10})]
        
        (is (contains? #{:queued :running :completed} (qb/get-job-status simulator job-id))
            "Job should have valid status")
        
        ; Wait and check completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should complete")))
    
    (testing "queue status"
      (let [simulator (noisy/create-noisy-simulator {})
            queue-status (qb/get-queue-status simulator)]
        (is (map? queue-status) "Should return queue status map")
        (is (contains? queue-status :total-jobs) "Should contain total jobs")
        (is (contains? queue-status :active-jobs) "Should contain active jobs")
        (is (contains? queue-status :completed-jobs) "Should contain completed jobs")))
    
    (testing "nonexistent job"
      (let [simulator (noisy/create-noisy-simulator {})]
        (is (= :not-found (qb/get-job-status simulator "fake-job-id")) "Should return not-found")
        (is (= :not-found (:job-status (qb/get-job-result simulator "fake-job-id")))
            "Should return not-found for result")))))

;; Tests for utility functions
(deftest test-utility-functions
  (testing "Simulator state management"
    (testing "reset state"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            _ (qb/submit-circuit simulator circuit {:shots 10})
            initial-stats (noisy/get-simulator-stats)
            _ (noisy/reset-simulator-state!)
            reset-stats (noisy/get-simulator-stats)]
        
        (is (> (:total-jobs initial-stats) 0) "Should have jobs before reset")
        (is (= 0 (:total-jobs reset-stats)) "Should have no jobs after reset")))
    
    (testing "cleanup completed jobs"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 5})]
        
        ; Wait for completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should be completed")
        
        ; Cleanup with very short max age (should remove the job)
        (let [removed-count (noisy/cleanup-completed-jobs! 1)]
          (is (>= removed-count 0) "Should return number of removed jobs"))))))

;; Tests for circuit fidelity estimation
(deftest test-circuit-fidelity-estimation
  (testing "Circuit fidelity estimation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2) (qc/h-gate 0) (qc/cnot-gate 0 1))
            fidelity-data (noise/estimate-circuit-fidelity circuit (noisy/noise-model-for :ibm-lagos))]
        
        (is (contains? fidelity-data :estimated-fidelity) "Should contain fidelity estimate")
        (is (contains? fidelity-data :total-estimated-error) "Should contain total error")
        (is (contains? fidelity-data :gate-counts) "Should contain gate counts")
        (is (> (:estimated-fidelity fidelity-data) 0.99) "Should have high fidelity for simple circuit")
        (is (< (:total-estimated-error fidelity-data) 0.01) "Should have low total error")))
    
    (testing "complex circuit"
      (let [circuit (-> (qc/create-circuit 3)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)
                        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2) (qc/cnot-gate 0 2)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2))
            fidelity-data (noise/estimate-circuit-fidelity circuit (noisy/noise-model-for :ibm-lagos))]
        
        (is (< (:estimated-fidelity fidelity-data) 
               (:estimated-fidelity (noise/estimate-circuit-fidelity (qc/create-circuit 1) (noisy/noise-model-for :ibm-lagos))))
            "Complex circuit should have lower fidelity than simple circuit")))))

;; Tests for platform comparison
(deftest test-platform-comparison
  (testing "Hardware platform comparison"
    (let [circuit (qc/ghz-state-circuit 3)
          platforms {:ibm-lagos (noisy/noise-model-for :ibm-lagos)
                     :rigetti-aspen (noisy/noise-model-for :rigetti-aspen)}
          comparison (noise/compare-hardware-platforms circuit platforms)]
      
      (is (map? comparison) "Should return comparison map")
      (is (contains? comparison :ibm-lagos) "Should contain IBM Lagos")
      (is (contains? comparison :rigetti-aspen) "Should contain Rigetti Aspen")
      
      (let [ibm-data (:ibm-lagos comparison)
            rigetti-data (:rigetti-aspen comparison)]
        (is (contains? ibm-data :estimated-fidelity) "Should contain fidelity for IBM")
        (is (contains? rigetti-data :estimated-fidelity) "Should contain fidelity for Rigetti")
        (is (contains? ibm-data :platform-type) "Should identify platform type")
        (is (= :superconducting (:platform-type ibm-data)) "Should identify IBM as superconducting")))))

;; Edge cases and error handling
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "empty circuit"
      (let [simulator (noisy/create-noisy-simulator (:ibm-lagos qb/devices))
            circuit (qc/create-circuit 1) ; Empty circuit
            job-id (qb/submit-circuit simulator circuit {:shots 10})]
        
        (Thread/sleep 200)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Empty circuit should execute successfully")
          (is (= 10 (reduce + (vals (:measurement-results result)))) "Should measure all shots"))))
    
    (testing "invalid noise parameters"
      ; Test that very high noise parameters don't break the simulator
      (let [simulator (noisy/create-noisy-simulator
                       {:noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.7}}}}) ; High but valid
            circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            job-id (qb/submit-circuit simulator circuit {:shots 100})]
        
        (Thread/sleep 200)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Should handle high noise parameters")
          (is (= 100 (reduce + (vals (:measurement-results result)))) "Should count all shots"))))
    
    (testing "zero shots"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 0})]
        
        (Thread/sleep 50)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Should handle zero shots")
          (is (= 0 (:shots-executed result)) "Should execute zero shots"))))))

(comment
  ; Run all tests in this namespace
  (run-tests)
  ;
  )