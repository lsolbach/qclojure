(ns org.soulspace.qclojure.adapter.backend.hardware-simulator-test
  "Comprehensive tests for the local noisy simulator backend."
  (:require [clojure.test :refer [deftest is testing use-fixtures run-tests]]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as sim]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.domain.circuit :as circuit]))

;;;
;;; Test fixtures
;;;
(defn reset-simulator-state-fixture
  "Reset noisy simulator state before each test."
  [f]
  (sim/reset-simulator-state!)
  (f))

(use-fixtures :each reset-simulator-state-fixture)

;; Tests for simulator creation and basic functionality
(deftest test-create-hardware-simulator
  (testing "Simulator creation"
    (testing "with noise model"
      (let [simulator (sim/create-hardware-simulator)]
        (is (satisfies? backend/QuantumBackend simulator) "Should implement QuantumBackend protocol")
        (is (backend/available? simulator) "Should be available")))

    (testing "backend info"
      (let [simulator (sim/create-hardware-simulator)
            info (backend/backend-info simulator)
            _ (println "Hardware Simulator Info:" info)]
        
        (is (= :hardware-simulator (:backend-type info)) "Should have correct backend type")))))

;; Tests for circuit execution
(deftest test-circuit-execution
  (testing "Circuit execution with noise"
    (testing "single qubit circuit"
      (let [simulator (sim/create-hardware-simulator)
            circuit (-> (circuit/create-circuit 1) (circuit/h-gate 0))
            job-id (backend/submit-circuit simulator circuit {:shots 100})]
        (is (string? job-id) "Should return job ID")

        ; Wait for completion
        (Thread/sleep 500)
        (is (= :completed (backend/job-status simulator job-id)) "Job should complete")

        (let [result (backend/job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Result should be completed")
          (is (= 100 (:shots-executed result)) "Should execute correct number of shots")
          (is (true? (:noise-applied result)) "Should indicate noise was applied")
          (is (map? (:measurement-results result)) "Should contain measurement results")
          (is (= 100 (reduce + (vals (:measurement-results result)))) "Shot counts should sum to total"))))

    (testing "GHZ circuit with realistic noise"
      (let [simulator (sim/create-hardware-simulator)
            _ (backend/select-device simulator (:ibm-kyoto sim/device-map))
            circuit (circuit/ghz-state-circuit 3)
            job-id (backend/submit-circuit simulator circuit {:shots 1000})]

        ; Wait for completion
        (Thread/sleep 2000)
        (let [result (backend/job-result simulator job-id)
              _ (println result)
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
      (let [simulator (sim/create-hardware-simulator)
            circuit (circuit/ghz-state-circuit 2)
            job-id (backend/submit-circuit simulator circuit {:shots 10})]

        (is (contains? #{:queued :running :completed} (backend/job-status simulator job-id))
            "Job should have valid status")

        ; Wait and check completion
        (Thread/sleep 500)
        (is (= :completed (backend/job-status simulator job-id)) "Job should complete")))

    (testing "queue status"
      (let [simulator (sim/create-hardware-simulator)
            queue-status (backend/queue-status simulator)]
        (is (map? queue-status) "Should return queue status map")
        (is (contains? queue-status :total-jobs) "Should contain total jobs")
        (is (contains? queue-status :active-jobs) "Should contain active jobs")
        (is (contains? queue-status :completed-jobs) "Should contain completed jobs")))

    (testing "nonexistent job"
      (let [simulator (sim/create-hardware-simulator)]
        (is (= :not-found (backend/job-status simulator "fake-job-id")) "Should return not-found")
        (is (= :not-found (:job-status (backend/job-result simulator "fake-job-id")))
            "Should return not-found for result")))))

;; Tests for utility functions
(deftest test-utility-functions
  (testing "Simulator state management"
    (testing "reset state"
      (let [simulator (sim/create-hardware-simulator)
            circuit (circuit/ghz-state-circuit 2)
            _ (backend/submit-circuit simulator circuit {:shots 10})
            initial-stats (sim/simulator-statistics)
            _ (sim/reset-simulator-state!)
            reset-stats (sim/simulator-statistics)]

        (is (> (:total-jobs initial-stats) 0) "Should have jobs before reset")
        (is (= 0 (:total-jobs reset-stats)) "Should have no jobs after reset")))

    (testing "cleanup completed jobs"
      (let [simulator (sim/create-hardware-simulator)
            circuit (circuit/ghz-state-circuit 2)
            job-id (backend/submit-circuit simulator circuit {:shots 5})]

        ; Wait for completion
        (Thread/sleep 500)
        (is (= :completed (backend/job-status simulator job-id)) "Job should be completed")

        ; Cleanup with very short max age (should remove the job)
        (let [removed-count (sim/cleanup-completed-jobs! 1)]
          (is (>= removed-count 0) "Should return number of removed jobs"))))))


;;;
;;; Edge cases and error handling
;;;
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "empty circuit"
      (let [simulator (sim/create-hardware-simulator)
            circuit (circuit/create-circuit 1) ; Empty circuit
            job-id (backend/submit-circuit simulator circuit {:shots 10})]
        
        (Thread/sleep 500)
        (let [result (backend/job-result simulator job-id)]
          (is (= :failed (:job-status result)) "Empty circuit result in failed optimization"))))))

  (comment
  ; Run all tests in this namespace
    (run-tests)
    ;
    )