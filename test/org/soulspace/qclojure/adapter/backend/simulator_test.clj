(ns org.soulspace.qclojure.adapter.backend.simulator-test
  "Tests for the local quantum simulator backend.
  
   These tests verify that the quantum simulator correctly implements the
   QuantumBackend protocol and accurately simulates quantum circuits."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;; Test fixtures
(defn reset-simulator-fixture
  "Test fixture to reset simulator state before and after each test."
  [f]
  (sim/reset-simulator-state!)
  (f)
  (sim/reset-simulator-state!))

;; Apply fixture to all tests in this namespace
(use-fixtures :each reset-simulator-fixture)

;; Helper functions for tests
(defn- bell-circuit
  "Create a Bell state preparation circuit (entangled state)"
  []
  (qc/bell-state-circuit))

(defn- ghz-circuit
  "Create a GHZ state preparation circuit for n qubits"
  [n]
  (qc/ghz-state-circuit n))

;; Test cases for the quantum simulator backend
(deftest test-simulator-creation
  (testing "Basic simulator creation"
    (let [sim1 (sim/create-simulator)]
      (is (satisfies? qb/QuantumBackend sim1))
      (is (instance? org.soulspace.qclojure.adapter.backend.simulator.LocalQuantumSimulator sim1))))
  
  (testing "Simulator creation with config"
    (let [config {:max-qubits 10 :seed 42}
          sim2 (sim/create-simulator config)
          info (qb/get-backend-info sim2)]
      (is (= 10 (:max-qubits info)))
      (is (= 42 (get-in info [:backend-config :seed]))))))

(deftest test-backend-protocol-implementation
  (testing "Backend info"
    (let [simulator (sim/create-simulator)
          info (qb/get-backend-info simulator)]
      (is (= :simulator (:backend-type info)))
      (is (string? (:backend-name info)))
      (is (contains? info :capabilities))
      (is (contains? info :supported-gates))
      (is (contains? info :max-qubits))))
  
  (testing "Supported gates"
    (let [simulator (sim/create-simulator)
          supported-gates (qb/get-supported-gates simulator)]
      (is (set? supported-gates))
      (is (>= (count supported-gates) 10))
      (is (contains? supported-gates :h))
      (is (contains? supported-gates :cnot))
      (is (contains? supported-gates :x))))
  
  (testing "Availability check"
    (let [simulator (sim/create-simulator)]
      (is (true? (qb/is-available? simulator))))))

(deftest test-job-submission-and-tracking
  (testing "Circuit submission"
    (let [simulator (sim/create-simulator)
          circuit (bell-circuit)
          job-id (qb/submit-circuit simulator circuit {:shots 100})]
      (is (string? job-id))
      
      ;; Wait a bit for the job to complete (since it runs in a future)
      (Thread/sleep 100)
      
      (testing "Job status retrieval"
        (let [status (qb/get-job-status simulator job-id)]
          (is (contains? #{:queued :running :completed} status))))
      
      (testing "Job result retrieval"
        (let [_ (Thread/sleep 100) ;; Ensure job completes
              result (qb/get-job-result simulator job-id)]
          (is (= job-id (:job-id result)))
          (is (contains? result :measurement-results))
          
          (let [measurements (:measurement-results result)]
            (is (map? measurements))
            (is (= 2 (count (keys measurements))))
            (is (contains? measurements "00"))
            (is (contains? measurements "11")))))))
  
  (testing "Circuit execution convenience function"
    (let [simulator (sim/create-simulator)
          circuit (bell-circuit)
          result (qb/execute-circuit simulator circuit {:shots 500})]
      (is (= :completed (:job-status result)))
      (is (map? (:measurement-results result)))
      (is (contains? (:measurement-results result) "00"))
      (is (contains? (:measurement-results result) "11")))))

(deftest test-circuit-simulations
  (testing "Hadamard gate simulation"
    (let [simulator (sim/create-simulator)
          h-circuit (-> (qc/create-circuit 1 "Hadamard Test")
                         (qc/h-gate 0))
          result (qb/execute-circuit simulator h-circuit {:shots 1000})
          measurements (:measurement-results result)]
      ;; Hadamard should create superposition of |0⟩ and |1⟩ with ~50% probability each
      (is (contains? measurements "0"))
      (is (contains? measurements "1"))
      (let [count-0 (get measurements "0" 0)
            count-1 (get measurements "1" 0)
            ratio (/ count-0 (+ count-0 count-1))]
        ;; Allow for some statistical variation (should be close to 0.5)
        (is (< 0.40 ratio 0.60)))))
  
  (testing "Bell state simulation"
    (let [simulator (sim/create-simulator)
          bell (bell-circuit)
          result (qb/execute-circuit simulator bell {:shots 1000})
          measurements (:measurement-results result)]
      ;; Bell state should only measure |00⟩ and |11⟩ with ~50% probability each
      (is (contains? measurements "00"))
      (is (contains? measurements "11"))
      (is (not (contains? measurements "01")))
      (is (not (contains? measurements "10")))
      (let [count-00 (get measurements "00" 0)
            count-11 (get measurements "11" 0)
            ratio (/ count-00 (+ count-00 count-11))]
        ;; Allow for some statistical variation
        (is (< 0.45 ratio 0.55)))))
  
  (testing "Multi-qubit GHZ state"
    (let [simulator (sim/create-simulator)
          ghz (ghz-circuit 3)  ;; 3-qubit GHZ state
          result (qb/execute-circuit simulator ghz {:shots 1000})
          measurements (:measurement-results result)]
      ;; GHZ state should only measure |000⟩ and |111⟩
      (is (contains? measurements "000"))
      (is (contains? measurements "111"))
      (is (not (some #(contains? measurements %) ["001" "010" "011" "100" "101" "110"])))
      (let [count-000 (get measurements "000" 0)
            count-111 (get measurements "111" 0)
            ratio (/ count-000 (+ count-000 count-111))]
        ;; Allow for some statistical variation
        (is (< 0.45 ratio 0.55))))))

(deftest test-multiple-gate-circuits
  (testing "X-gate followed by measurement"
    (let [simulator (sim/create-simulator)
          x-circuit (-> (qc/create-circuit 1 "X Gate Test")
                         (qc/x-gate 0))
          result (qb/execute-circuit simulator x-circuit {:shots 100})
          measurements (:measurement-results result)]
      ;; X gate should flip |0⟩ to |1⟩
      (is (contains? measurements "1"))
      (is (= 100 (get measurements "1" 0)))
      (is (not (contains? measurements "0")))))
  
  (testing "Controlled-X gate test"
    (let [simulator (sim/create-simulator)
          cx-circuit (-> (qc/create-circuit 2 "CNOT Test")
                         ;; Control=1, Target=0
                         (qc/x-gate 0)
                         (qc/cnot-gate 0 1))
          result (qb/execute-circuit simulator cx-circuit {:shots 100})
          measurements (:measurement-results result)]
      ;; Initial state |00⟩, apply X to get |10⟩, CNOT gives |11⟩
      (is (contains? measurements "11"))
      (is (= 100 (get measurements "11" 0))))))

(deftest test-job-cancellation-and-queue-status
  (testing "Job cancellation"
    (let [simulator (sim/create-simulator)
          circuit (ghz-circuit 5)  ;; Create a larger circuit
          job-id (qb/submit-circuit simulator circuit {:shots 5000})
          cancel-result (qb/cancel-job simulator job-id)]
      ;; Might already be completed depending on timing
      (is (contains? #{:cancelled :cannot-cancel} cancel-result))))
  
  (testing "Queue status"
    (let [simulator (sim/create-simulator)
          _ (qb/submit-circuit simulator (bell-circuit) {})
          _ (qb/submit-circuit simulator (ghz-circuit 3) {})
          queue-status (qb/get-queue-status simulator)]
      (is (map? queue-status))
      (is (contains? queue-status :total-jobs))
      (is (<= 2 (:total-jobs queue-status)))
      (is (contains? queue-status :queued))
      (is (contains? queue-status :running))
      (is (contains? queue-status :completed)))))

(deftest test-simulator-statistics
  (testing "Simulator stats"
    (let [simulator (sim/create-simulator)
          _ (qb/execute-circuit simulator (bell-circuit) {:shots 100})
          stats (sim/get-simulator-stats)]
      (is (map? stats))
      (is (contains? stats :total-jobs))
      (is (contains? stats :completed-jobs))
      (is (contains? stats :average-execution-time))
      (is (contains? stats :job-counter)))))

(deftest test-error-handling
  (testing "Not found job"
    (let [simulator (sim/create-simulator)
          result (qb/get-job-result simulator "non-existent-id")]
      (is (= :not-found (:job-status result)))
      (is (= "non-existent-id" (:job-id result)))
      (is (contains? result :error-message))))
  
  (testing "Missing job ID for cancel"
    (let [simulator (sim/create-simulator)
          result (qb/cancel-job simulator "non-existent-id")]
      (is (= :not-found result)))))
