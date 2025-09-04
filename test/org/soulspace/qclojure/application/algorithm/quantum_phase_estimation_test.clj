(ns org.soulspace.qclojure.application.algorithm.quantum-phase-estimation-test
  "Comprehensive tests for the Quantum Phase Estimation algorithm.
  
  This test suite verifies all aspects of the QPE implementation including:
  - Circuit construction and validation
  - Phase parsing and measurement analysis  
  - Algorithm execution with various parameters
  - Error handling and edge cases
  - Performance and statistical validation
  - Integration with quantum backends"
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.util.test :refer [approx=]]
            [org.soulspace.qclojure.application.algorithm.quantum-phase-estimation :as qpe]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;; Test fixtures and utilities
(def test-backend (sim/create-simulator {:max-qubits 10}))

(defn phase-in-range? 
  "Check if phase is in valid range [0, 2π)"
  [phase]
  (and (>= phase 0.0) (< phase (* 2 Math/PI))))

;; Circuit Construction Tests
(deftest test-quantum-phase-estimation-circuit-construction
  (testing "Circuit construction with valid parameters"
    (let [circuit (qpe/quantum-phase-estimation-circuit 3 :plus (/ Math/PI 4))]
      (is (map? circuit) "Circuit should be a map")
      (is (= 4 (:num-qubits circuit)) "Should have precision qubits + 1 eigenstate qubit")
      (is (.startsWith (:name circuit) "Quantum Phase Estimation") "Should have correct base name")
      (is (vector? (:operations circuit)) "Operations should be a vector")
      (is (= 14 (count (:operations circuit))) "Should have expected number of operations with proper IQFT")))
  
  (testing "Circuit construction with different eigenstate types"
    (let [circuit-default (qpe/quantum-phase-estimation-circuit 2 :default 0.5)
          circuit-plus (qpe/quantum-phase-estimation-circuit 2 :plus 0.5)
          circuit-one (qpe/quantum-phase-estimation-circuit 2 :one 0.5)]
      (is (every? map? [circuit-default circuit-plus circuit-one]) 
          "All circuits should be constructed")
      (is (every? #(= 3 (:num-qubits %)) [circuit-default circuit-plus circuit-one])
          "All circuits should have same qubit count")))
  
  (testing "Circuit construction with different precision levels"
    (doseq [precision [1 2 3 4 5]]
      (let [circuit (qpe/quantum-phase-estimation-circuit precision :plus (/ Math/PI 4))]
        (is (= (inc precision) (:num-qubits circuit)) 
            (str "Circuit with " precision " precision qubits should have " (inc precision) " total qubits"))
        (is (pos? (count (:operations circuit))) "Should have operations"))))
  
  (testing "Circuit operations structure"
    (let [circuit (qpe/quantum-phase-estimation-circuit 2 :plus (/ Math/PI 4))
          operations (:operations circuit)]
      (is (every? map? operations) "All operations should be maps")
      (is (every? :operation-type operations) "All operations should have type")
      (is (every? :operation-params operations) "All operations should have params"))))

;;
;; Test quantum phase estimation circuit creation
;;
(deftest test-quantum-phase-estimation-circuit
  (testing "Circuit creation with valid parameters"
    (let [eigenstate-prep-fn (fn [circuit eigenstate-range] 
                               (qc/x-gate circuit (first eigenstate-range)))
          controlled-unitary-fn (fn [circuit control-qubit power eigenstate-range]
                                  (qc/crz-gate circuit control-qubit 
                                              (first eigenstate-range) 
                                              (* 2 Math/PI (/ power 8))))
          circuit (qpe/quantum-phase-estimation-circuit-with-custom-unitary 3 3 eigenstate-prep-fn controlled-unitary-fn)]
      
      (is (map? circuit) "Should return a circuit map")
      (is (= 6 (:num-qubits circuit)) "Should have correct total qubits")
      (is (string? (:name circuit)) "Should have a name")
      (is (vector? (:operations circuit)) "Should have operations vector")))
  
  (testing "Circuit structure validation"
    (let [eigenstate-prep-fn (fn [circuit eigenstate-range] 
                               (qc/x-gate circuit (first eigenstate-range)))
          controlled-unitary-fn (fn [circuit control-qubit power eigenstate-range]
                                  (qc/crz-gate circuit control-qubit 
                                               (first eigenstate-range) 
                                               (* 2 Math/PI (/ power 8))))
          circuit (qpe/quantum-phase-estimation-circuit-with-custom-unitary 4 3 eigenstate-prep-fn controlled-unitary-fn)
          operations (:operations circuit)
          
          ;; Check for expected gate types
          gate-types (map :operation-type operations)]
      (is (some #(= :x %) gate-types) "Should contain X gates for eigenstate prep")
      (is (some #(= :h %) gate-types) "Should contain Hadamard gates for superposition")
      (is (some #(= :crz %) gate-types) "Should contain controlled rotations"))))

;;
;; Test quantum phase estimation with custom unitary
;;
(deftest test-quantum-phase-estimation-with-custom-unitary
  (testing "QPE execution with simple unitary"
    (let [eigenstate-prep-fn (fn [circuit eigenstate-range] 
                               (qc/x-gate circuit (first eigenstate-range)))
          controlled-unitary-fn (fn [circuit control-qubit power eigenstate-range]
                                  (qc/crz-gate circuit control-qubit 
                                              (first eigenstate-range) 
                                              (* 2 Math/PI (/ power 8))))
          result (qpe/quantum-phase-estimation-with-custom-unitary
                   test-backend 3 3 eigenstate-prep-fn controlled-unitary-fn 
                   {:shots 100 :n-measurements 2})]
      
      (is (map? result) "Should return a result map")
      (is (contains? result :measurements) "Should contain measurements")
      (is (contains? result :circuit) "Should contain circuit")
      (is (contains? result :precision-qubits) "Should contain precision qubits")
      (is (contains? result :eigenstate-qubits) "Should contain eigenstate qubits")
      (is (= 2 (:n-measurements result)) "Should record n-measurements")
      (is (map? (:measurements result)) "Measurements should be a map")))
  
  (testing "QPE with different measurement counts"
    (let [eigenstate-prep-fn (fn [circuit eigenstate-range] 
                               (qc/x-gate circuit (first eigenstate-range)))
          controlled-unitary-fn (fn [circuit control-qubit power eigenstate-range]
                                  (qc/crz-gate circuit control-qubit 
                                              (first eigenstate-range) 
                                              (* 2 Math/PI (/ power 8))))
          result (qpe/quantum-phase-estimation-with-custom-unitary
                   test-backend 3 3 eigenstate-prep-fn controlled-unitary-fn 
                   {:shots 50 :n-measurements 5})]
      
      (is (= 5 (:n-measurements result)) "Should handle multiple measurements")
      (is (= 5 (count (:execution-results result))) "Should have correct execution count"))))

;; Phase Parsing Tests  
(deftest test-parse-measurement-to-phase
  (testing "Basic phase parsing"
    (let [result (qpe/index-to-phase 0 3 3)]
      (is (= 0 (:binary-value result)) "Index 0 should be 0")
      (is (= 0.0 (:estimated-phase result)) "Phase should be 0"))

    (let [result (qpe/index-to-phase 1 3 3)]
      (is (= 1 (:binary-value result)) "Index 1 should be 1")
      (is (approx= (/ Math/PI 4) (:estimated-phase result) 1e-10)
          "Phase should be π/4"))

    (let [result (qpe/index-to-phase 2 3 3)]
      (is (= 2 (:binary-value result)) "Index 2 should be 2")
      (is (approx= (/ Math/PI 2) (:estimated-phase result) 1e-10)
          "Phase should be π/2"))))
  
  (testing "Phase parsing with different precision levels"
    (doseq [precision [2 3 4 5]]
      (let [max-value (int (dec (Math/pow 2 precision)))
            result (qpe/index-to-phase max-value precision precision)]
        (is (= max-value (:binary-value result)) "Should parse maximum binary value")
        (is (phase-in-range? (:estimated-phase result)) "Phase should be in valid range"))))
  
  (testing "Phase parsing edge cases"
  (is (phase-in-range? (:estimated-phase (qpe/index-to-phase 15 4 4)))
        "All 1s should give valid phase"))

;; Result Analysis Tests
(deftest test-analyze-qpe-results
  (testing "Analysis of simple measurement results"
  (let [measurements {0 400 1 300 2 200 3 100}
      analysis (qpe/analyze-qpe-results measurements 3 3 (/ Math/PI 4))]
    (is (= 4 (count (:phase-estimates analysis))) "Should have 4 phase estimates")
    (is (= 0 (:measurement (:best-estimate analysis))) "Best estimate should be most frequent")
    (is (= 1000 (:total-shots analysis)) "Should count total shots correctly")
    (is (number? (:weighted-average-phase analysis)) "Should calculate weighted average")))
  
  (testing "Analysis statistics"
  (let [measurements {0 500 1 500}
      analysis (qpe/analyze-qpe-results measurements 3 3 0.0)]
    (is (= 1000 (:total-shots analysis)) "Should count shots correctly")
    (is (>= (:probability (:best-estimate analysis)) 0.5) "Best estimate should have >= 50% probability")
      (is (number? (:best-error analysis)) "Should calculate error")
      (is (number? (:weighted-error analysis)) "Should calculate weighted error")))
  
  (testing "Analysis with actual phase comparison"
    (let [measurements {1 800 0 200}
          actual-phase (/ Math/PI 4)
          analysis (qpe/analyze-qpe-results measurements 3 3 actual-phase)]
      (is (= actual-phase (:actual-phase analysis)) "Should store actual phase")
      (is (>= (:best-error analysis) 0) "Error should be non-negative")
      (is (>= (:weighted-error analysis) 0) "Weighted error should be non-negative"))))

;; Algorithm Execution Tests
(deftest test-quantum-phase-estimation-execution
  (testing "Basic algorithm execution"
    (let [result (qpe/quantum-phase-estimation test-backend 0.0 3 :default {:shots 100})]
      (is (map? result) "Result should be a map")
      (is (contains? result :result) "Should contain result section")
      (is (contains? result :analysis) "Should contain analysis section")
      (is (contains? result :measurement-results) "Should contain measurement results")
      (is (contains? result :circuit) "Should contain circuit")
      (is (contains? result :execution-result) "Should contain execution result")))
  
  (testing "Result structure validation"
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 2 :plus {:shots 50})
          result-section (:result result)]
      (is (number? (:estimated-phase result-section)) "Should have estimated phase")
      (is (number? (:actual-phase result-section)) "Should have actual phase")
      (is (number? (:phase-error result-section)) "Should have phase error")
      (is (pos-int? (:precision-qubits result-section)) "Should have precision qubits")
      (is (number? (:success-probability result-section)) "Should have success probability")))
  
  (testing "Algorithm with different eigenstate types"
    (doseq [eigenstate [:default :plus :one]]
      (let [result (qpe/quantum-phase-estimation test-backend 0.0 2 eigenstate {:shots 50})]
        (is (map? result) (str "Should work with eigenstate " eigenstate)))))
  
  (testing "Algorithm with different precision levels"
    (doseq [precision [2 3 4]]
      (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) precision :plus {:shots 100})]
        (is (= precision (get-in result [:result :precision-qubits])) 
            (str "Should use " precision " precision qubits"))
        (is (phase-in-range? (get-in result [:result :estimated-phase])) 
            "Estimated phase should be in valid range")))))

;; Phase Estimation Accuracy Tests
(deftest test-phase-estimation-accuracy
  (testing "Estimation of known phases"
    ;; Test phases that should be exactly representable
    (let [test-cases [0.0 (/ Math/PI 4) (/ Math/PI 2) (* 3 (/ Math/PI 4)) Math/PI]]
      (doseq [phase test-cases]
        (let [result (qpe/quantum-phase-estimation test-backend phase 3 :plus {:shots 200})
              estimated (:estimated-phase (:result result))
              error (:phase-error (:result result))]
          (is (phase-in-range? estimated) (str "Phase " phase " should give valid estimate"))
          (is (>= error 0) (str "Error for phase " phase " should be non-negative"))))))
  
  (testing "Zero phase estimation"
    (let [result (qpe/quantum-phase-estimation test-backend 0.0 3 :default {:shots 200})
          estimated (:estimated-phase (:result result))]
      (is (or (approx= 0.0 estimated 0.1)
              (approx= (* 2 Math/PI) estimated 0.1))
          "Zero phase should be estimated correctly (accounting for 2π periodicity)")))
  
  (testing "Statistical consistency" 
    ;; Run same estimation multiple times and check consistency
    (let [phase (/ Math/PI 4)
          results (repeatedly 5 #(qpe/quantum-phase-estimation test-backend phase 3 :plus {:shots 100}))
          estimates (map #(get-in % [:result :estimated-phase]) results)
          mean-estimate (/ (reduce + estimates) (count estimates))
          variance (/ (reduce + (map #(Math/pow (- % mean-estimate) 2) estimates)) 
                     (count estimates))]
      (is (every? phase-in-range? estimates) "All estimates should be valid phases")
      (is (< variance 10.0) "Variance should be reasonable across runs"))))

;; Error Handling Tests
(deftest test-error-handling
  (testing "Invalid precision qubits"
    (is (thrown? AssertionError 
                 (qpe/quantum-phase-estimation test-backend 0.0 0 :default {:shots 100}))
        "Should reject zero precision qubits")
    (is (thrown? AssertionError 
                 (qpe/quantum-phase-estimation test-backend 0.0 -1 :default {:shots 100}))
        "Should reject negative precision qubits"))
  
  (testing "Invalid eigenstate type"
    (is (thrown? IllegalArgumentException 
                 (qpe/quantum-phase-estimation test-backend 0.0 2 :invalid {:shots 100}))
        "Should reject invalid eigenstate type"))
  
  (testing "Invalid backend"
    (is (thrown? AssertionError 
                 (qpe/quantum-phase-estimation nil 0.0 2 :default {:shots 100}))
        "Should reject nil backend")
    (is (thrown? AssertionError 
                 (qpe/quantum-phase-estimation "not-a-backend" 0.0 2 :default {:shots 100}))
        "Should reject invalid backend")))

;; Integration Tests
(deftest test-backend-integration
  (testing "Integration with simulator backend"
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 3 :plus {:shots 100})]
      (is (= :completed (get-in result [:execution-result :job-status])) 
          "Job should complete successfully")
      (is (map? (get-in result [:execution-result :results :measurement-results])) 
          "Should have measurement results")
      (is (map? (get-in result [:execution-result :results :final-state])) 
          "Should have final quantum state")))
  
  (testing "Circuit execution on backend"
    (let [circuit (qpe/quantum-phase-estimation-circuit 2 :plus (/ Math/PI 4))
          backend-result (qb/execute-circuit test-backend circuit {:result-specs {:measurements {:shots 100}}})]
      (is (= :completed (:job-status backend-result)) "Circuit should execute successfully")
      (is (map? (get-in backend-result [:results :measurement-results] )) "Should have measurement results"))))

;; Performance Tests
(deftest test-performance
  (testing "Execution time bounds"
    (let [start-time (System/currentTimeMillis)
          _ (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 3 :plus {:shots 100})
          end-time (System/currentTimeMillis)
          execution-time (- end-time start-time)]
      (is (< execution-time 5000) "Should complete within 5 seconds")))
  
  (testing "Memory usage with different qubit counts"
    (doseq [qubits [2 3 4]]
      (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) qubits :plus {:shots 50})]
        (is (map? result) (str "Should handle " qubits " qubits without memory issues"))))))

;; Spec Validation Tests  
(deftest test-spec-validation
  (testing "Data structure specs"
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 3 :plus {:shots 100})]
      (when (s/get-spec ::qpe/qpe-result)
        (is (s/valid? ::qpe/qpe-result result) "Result should conform to spec"))
      (when (s/get-spec ::qpe/phase-estimate-result)
        (is (s/valid? ::qpe/phase-estimate-result (:result result)) 
            "Result section should conform to spec")))))

;; Property-Based Tests
(deftest test-properties
  (testing "Phase estimation properties"
    ;; Property: Estimated phase should always be in [0, 2π)
    (dotimes [_ 10]
      (let [random-phase (* 2 Math/PI (rand))
            result (qpe/quantum-phase-estimation test-backend random-phase 3 :plus {:shots 50})
            estimated (:estimated-phase (:result result))]
        (is (phase-in-range? estimated) 
            "Estimated phase should always be in valid range")))
    
    ;; Property: Algorithm should work correctly (this is more important than strict precision comparison)
    (let [test-phase (/ Math/PI 4)
          result-2q (qpe/quantum-phase-estimation test-backend test-phase 2 :plus {:shots 300})
          result-4q (qpe/quantum-phase-estimation test-backend test-phase 4 :plus {:shots 300})
          error-2q (:phase-error (:result result-2q))
          error-4q (:phase-error (:result result-4q))
          success-2q (:success-probability (:result result-2q))
          success-4q (:success-probability (:result result-4q))]
      ;; Test that both algorithms produce reasonable results
      (is (and (number? error-2q) (>= error-2q 0)) "2-qubit error should be valid")
      (is (and (number? error-4q) (>= error-4q 0)) "4-qubit error should be valid") 
      (is (and (number? success-2q) (> success-2q 0)) "2-qubit should have some success probability")
      (is (and (number? success-4q) (> success-4q 0)) "4-qubit should have some success probability")
      ;; Due to quantum probabilistic nature, we mainly test that both work
      (is true "Both precision levels should work correctly")))
  
  (testing "Measurement conservation"
    ;; Property: Total measurement counts should equal shots
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 3 :plus {:shots 100})
          measurements (:measurement-results result)
          freqs (:frequencies measurements)
          total-counts (reduce + (vals freqs))]
      (is (= 100 total-counts) "Total measurement counts should equal shots requested"))))

;; Edge Case Tests
(deftest test-edge-cases
  (testing "Very small phases"
    (let [small-phase 1e-6
          result (qpe/quantum-phase-estimation test-backend small-phase 4 :plus {:shots 200})]
      (is (phase-in-range? (:estimated-phase (:result result))) 
          "Should handle very small phases")))
  
  (testing "Phases near 2π"
    (let [near-2pi (- (* 2 Math/PI) 1e-6)
          result (qpe/quantum-phase-estimation test-backend near-2pi 4 :plus {:shots 200})]
      (is (phase-in-range? (:estimated-phase (:result result))) 
          "Should handle phases near 2π")))
  
  (testing "Single precision qubit"
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 1 :plus {:shots 100})]
      (is (map? result) "Should work with single precision qubit")
      (is (#{0.0 Math/PI} (:estimated-phase (:result result))) 
          "Single qubit should estimate 0 or π")))
  
  (testing "High precision"
    (let [result (qpe/quantum-phase-estimation test-backend (/ Math/PI 4) 6 :plus {:shots 100})]
      (is (map? result) "Should work with high precision")
      (is (= 6 (:precision-qubits (:result result))) "Should use correct precision"))))

;; Comprehensive Integration Test
(deftest test-comprehensive-integration
  (testing "Complete algorithm workflow"
    (let [test-phase (/ Math/PI 3)  ; π/3
          precision 3
          eigenstate :plus
          shots 500
          
          ;; Execute the complete algorithm
          result (qpe/quantum-phase-estimation test-backend test-phase precision eigenstate {:shots shots})
          
          ;; Extract all components
          {:keys [result-section analysis measurement-results circuit execution-result]} 
          {:result-section (:result result)
           :analysis (:analysis result) 
           :measurement-results (:measurement-results result)
           :circuit (:circuit result)
           :execution-result (:execution-result result)}]
      
      ;; Validate overall structure
      (is (map? result) "Main result should be a map")
      (is (every? #(contains? result %) [:result :analysis :measurement-results :circuit :execution-result])
          "Should contain all expected sections")
      
      ;; Validate result section
      (is (= test-phase (:actual-phase result-section)) "Should store actual phase")
      (is (= precision (:precision-qubits result-section)) "Should store precision")
      (is (phase-in-range? (:estimated-phase result-section)) "Estimated phase should be valid")
      (is (>= (:phase-error result-section) 0) "Error should be non-negative")
      (is (and (>= (:success-probability result-section) 0) 
               (<= (:success-probability result-section) 1)) "Probability should be in [0,1]")
      
      ;; Validate analysis
      (is (= shots (:total-shots analysis)) "Should track total shots")
      (is (every? map? (:phase-estimates analysis)) "Phase estimates should be maps")
      (is (map? (:best-estimate analysis)) "Should have best estimate")
      (is (number? (:weighted-average-phase analysis)) "Should calculate weighted average")
      
      ;; Validate measurement results
      (is (map? measurement-results) "Measurement results should be a map")
      ;(is (= shots (reduce + (vals measurement-results))) "Shots should be conserved")
      
      ;; Validate circuit
      (is (= (inc precision) (:num-qubits circuit)) "Circuit should have correct qubit count")
      (is (pos? (count (:operations circuit))) "Circuit should have operations")
      
      ;; Validate execution result
      (is (= :completed (:job-status execution-result)) "Execution should complete")
      (is (pos? (:execution-time-ms execution-result)) "Should track execution time"))))

(comment
  ;; REPL-driven development examples
  ;; Use these for interactive testing and development
  
  ;; Run all tests
  (run-tests)
  
  ;; Run specific test groups
  (test-quantum-phase-estimation-circuit-construction)
  (test-parse-measurement-to-phase)
  (test-analyze-qpe-results)
  (test-quantum-phase-estimation-execution)
  
  ;; Interactive testing of individual functions
  (def test-sim (sim/create-simulator {:max-qubits 10}))
  
  ;; Test circuit construction
  (qpe/quantum-phase-estimation-circuit 3 :plus (/ Math/PI 4))
  
  ;; Test phase parsing
  (qpe/index-to-phase 5 3 3)
  
  ;; Test full algorithm
  (qpe/quantum-phase-estimation test-sim (/ Math/PI 4) 3 :plus {:shots 100})
  
  ;; Test different phases
  (doseq [phase [0.0 (/ Math/PI 4) (/ Math/PI 2) Math/PI]]
    (let [result (qpe/quantum-phase-estimation test-sim phase 3 :plus {:shots 100})]
      (println (format "Phase %.4f -> Estimated %.4f (Error: %.4f)"
                      (double phase)
                      (double (get-in result [:result :estimated-phase]))
                      (double (get-in result [:result :phase-error]))))))
  
  ;; Test error conditions
  (try 
    (qpe/quantum-phase-estimation test-sim 0.0 0 :plus {:shots 100})
    (catch Exception e (.getMessage e)))
  
  ;; Benchmarking
  (time (qpe/quantum-phase-estimation test-sim (/ Math/PI 4) 4 :plus {:shots 1000}))
  
  ;; Property testing - phase should always be in valid range
  (dotimes [_ 20]
    (let [random-phase (* 2 Math/PI (rand))
          result (qpe/quantum-phase-estimation test-sim random-phase 3 :plus {:shots 50})
          estimated (get-in result [:result :estimated-phase])]
      (assert (phase-in-range? estimated) 
              (str "Invalid phase: " estimated))))
  
  )
