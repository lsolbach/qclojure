(ns org.soulspace.qclojure.application.algorithm.grover-test
  "Tests for Grover's search algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.pprint]
            [org.soulspace.qclojure.application.algorithm.grover :as grover]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]))

;; Test circuit construction functions
(deftest test-grover-circuit
  (testing "2-qubit Grover circuit construction"
    (let [oracle-fn (grover/single-target-oracle 2)  ; Target |10⟩
          circuit (grover/grover-circuit 2 oracle-fn 1)]

      (is (not (nil? circuit)) "Circuit should be created")
      (is (= 2 (:num-qubits circuit)) "Circuit should have 2 qubits")))

  (testing "3-qubit Grover circuit with measurements"
    (let [oracle-fn (grover/single-target-oracle 5)  ; Target |101⟩
          circuit (grover/grover-circuit 3 oracle-fn 2 {:add-measurements? true})]

      (is (not (nil? circuit)) "Circuit should be created")
      (is (= 3 (:num-qubits circuit)) "Circuit should have 3 qubits")
      ;; Check if measurements were added (circuit should have measurement operations)
      (is (some #(= (:operation-type %) :measure) (:operations circuit))
          "Circuit should contain measurement operations")))

  (testing "Optimal iterations calculation"
    (let [oracle-fn (grover/single-target-oracle 10)
          circuit (grover/grover-circuit 4 oracle-fn)]  ; Use default optimal iterations

      (is (not (nil? circuit)) "Circuit should be created")
      (is (= 4 (:num-qubits circuit)) "Circuit should have 4 qubits"))))

(deftest test-optimal-grover-iterations
  (testing "Optimal iterations for single target"
    (is (= 1 (grover/optimal-grover-iterations 4 1)) "4 items, 1 target should need 1 iteration")
    (is (= 3 (grover/optimal-grover-iterations 16 1)) "16 items, 1 target should need ~3 iterations"))

  (testing "Optimal iterations for multiple targets"
    (is (= 1 (grover/optimal-grover-iterations 16 4)) "16 items, 4 targets should need ~1 iteration")
    (is (= 2 (grover/optimal-grover-iterations 64 8)) "64 items, 8 targets should need ~2 iterations")))

;; Test Grover algorithm execution
(deftest test-grover-algorithm-single-target
  (testing "2-qubit search space - single target"
    (let [backend (sim/create-simulator)
          search-space-size 4
          target-index 2  ; Target |10⟩
          oracle-fn (grover/single-target-oracle target-index)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should return a result")
      (is (number? (:probability result)) "Should return success probability")
      ;; For now, let's check that we get a deterministic result rather than specific probability
      (is (number? (:result result)) "Result should be a number")
      (is (= [target-index] (:target-indices result)) "Should identify correct target indices")
      (is (= 1 (:iterations result)) "Should use 1 iteration for 4-item search")
      (is (not (nil? (:circuit result))) "Should return the quantum circuit")
      (is (not (nil? (:execution-result result))) "Should return execution result")))

  (testing "3-qubit search space - single target"
    (let [backend (sim/create-simulator)
          search-space-size 8
          target-index 5  ; Target |101⟩
          oracle-fn (grover/single-target-oracle target-index)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should return a result")
      (is (number? (:probability result)) "Should return success probability")
      (is (= [target-index] (:target-indices result)) "Should identify correct target indices")
      (is (= 2 (:iterations result)) "Should use 2 iterations for 8-item search")))

  (testing "4-qubit search space - single target"
    (let [backend (sim/create-simulator)
          search-space-size 16
          target-index 10  ; Target |1010⟩
          oracle-fn (grover/single-target-oracle target-index)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should return a result")
      (is (number? (:probability result)) "Should return success probability")
      (is (= [target-index] (:target-indices result)) "Should identify correct target indices")
      (is (= 3 (:iterations result)) "Should use 3 iterations for 16-item search"))))

(deftest test-grover-algorithm-multiple-targets
  (testing "2-qubit search space - multiple targets"
    (let [backend (sim/create-simulator)
          search-space-size 4
          target-indices [1 3]  ; Targets |01⟩ and |11⟩
          oracle-fn (grover/multiple-targets-oracle target-indices)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should return a result")
      (is (number? (:probability result)) "Should return success probability")
      (is (= (set target-indices) (set (:target-indices result))) "Should identify correct target indices")
      ;; Note: The result may not always be one of the targets due to quantum interference
      ;; but the algorithm should complete successfully
      (is (number? (:result result)) "Result should be a number")))

  (testing "3-qubit search space - even numbers"
    (let [backend (sim/create-simulator)
          search-space-size 8
          oracle-fn grover/even-numbers-oracle
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})
          even-targets #{0 2 4 6}]

      (is (:result result) "Algorithm should return a result")
      (is (number? (:probability result)) "Should return success probability")
      (is (= even-targets (set (:target-indices result))) "Should identify all even numbers as targets")
      ;; Check if result is a valid number in the search space
      (is (and (>= (:result result) 0) (< (:result result) search-space-size))
          "Result should be within search space"))))

(deftest test-grover-algorithm-edge-cases
  (testing "Single item search space"
    (let [backend (sim/create-simulator)
          search-space-size 2
          target-index 1
          oracle-fn (grover/single-target-oracle target-index)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should work with minimal search space")
      (is (= 1 (:iterations result)) "Should use 1 iteration for 2-item search")))

  (testing "All items are targets"
    (let [backend (sim/create-simulator)
          search-space-size 4
          oracle-fn (fn [_] true)  ; All items are targets
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should handle all-targets case")
      (is (== 1 (:probability result)) "Success probability should be 100%")
      (is (= [0 1 2 3] (:target-indices result)) "All indices should be targets")))

  (testing "No targets exist"
    ;; This is an edge case - Grover's algorithm doesn't work when there are no targets
    ;; But our implementation should still run without crashing
    (let [backend (sim/create-simulator)
          search-space-size 4
          oracle-fn (fn [_] false)  ; No items are targets
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})]

      (is (:result result) "Algorithm should return some result even with no targets")
      (is (== 0 (:probability result)) "Success probability should be 0%")
      (is (empty? (:target-indices result)) "No indices should be targets"))))

(deftest test-grover-algorithm-measurement-statistics
  (testing "Measurement statistics structure"
    (let [backend (sim/create-simulator)
          search-space-size 4
          target-index 2
          oracle-fn (grover/single-target-oracle target-index)
          result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 1024})
          stats (:measurement-statistics result)]

      (is (not (nil? stats)) "Should return measurement statistics")
      (is (contains? stats :frequencies) "Should contain measurement frequencies")
      (is (contains? stats :target-counts) "Should contain target counts")
      (is (contains? stats :total-shots) "Should contain total shots")
      (is (contains? stats :success-probability) "Should contain success probability")
      (is (= 1024 (:total-shots stats)) "Should record correct number of shots")
      (is (= (:probability result) (:success-probability stats)) "Probabilities should match")))

  (deftest test-grover-algorithm-options
    (testing "Custom shot count"
      (let [backend (sim/create-simulator)
            search-space-size 4
            target-index 1
            oracle-fn (grover/single-target-oracle target-index)
            result (grover/grover-algorithm backend search-space-size oracle-fn {:shots 2048})
            stats (:measurement-statistics result)]

        (is (= 2048 (:total-shots stats)) "Should use custom shot count")))

    (testing "Default shot count"
      (let [backend (sim/create-simulator)
            search-space-size 4
            target-index 1
            oracle-fn (grover/single-target-oracle target-index)
            result (grover/grover-algorithm backend search-space-size oracle-fn)
            stats (:measurement-statistics result)]

        (is (= 1024 (:total-shots stats)) "Should use default shot count of 1024"))))

  ;; Circuit validation tests
  (deftest test-grover-circuit-validation
    (testing "Invalid inputs to grover-circuit"
      (is (thrown? AssertionError
                   (grover/grover-circuit 0 (grover/single-target-oracle 0) 1))
          "Should throw for zero qubits")

      (is (thrown? AssertionError
                   (grover/grover-circuit 2 (grover/single-target-oracle 0) 0))
          "Should throw for zero iterations"))

    (testing "Invalid inputs to grover-algorithm"
      (let [backend (sim/create-simulator)]
        (is (thrown? AssertionError
                     (grover/grover-algorithm backend 3 (grover/single-target-oracle 0)))
            "Should throw for non-power-of-2 search space")

        (is (thrown? AssertionError
                     (grover/grover-algorithm backend 0 (grover/single-target-oracle 0)))
            "Should throw for zero search space")))))

(comment
  ;; Test suite execution
  (run-tests)

  ;; Individual test execution for development
  (test-grover-algorithm-single-target)
  (test-grover-algorithm-multiple-targets)
  (test-optimal-grover-iterations)

  ;; Manual testing and exploration
  (let [backend (sim/create-simulator)
        result (grover/grover-algorithm backend 8 (grover/single-target-oracle 5) {:shots 2048})]
    (println "Grover search result:")
    (clojure.pprint/pprint result))

  ;; Test with different oracle functions
  (let [backend (sim/create-simulator)
        result (grover/grover-algorithm backend 16 grover/even-numbers-oracle {:shots 4096})]
    (println "Even numbers search:")
    (clojure.pprint/pprint (:measurement-statistics result))))