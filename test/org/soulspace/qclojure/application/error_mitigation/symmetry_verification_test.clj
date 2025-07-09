(ns org.soulspace.qclojure.application.error-mitigation.symmetry-verification-test
  "Tests for enhanced production-ready symmetry verification in error mitigation."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation.symmetry-verification :as sv]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.util.test :as util]))

(deftest test-enhanced-symmetry-verification-basic
  (testing "Symmetric measurement results should pass"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :h :qubits [0]}
                                {:operation-type :cnot :qubits [0 1]}]}
          symmetric-results {"00" 450 "11" 450 "01" 50 "10" 50}
          analysis (sv/apply-symmetry-verification circuit symmetric-results)]
      
      (is (> (:symmetry-score analysis) 0.8) "Symmetric data should have high symmetry score")
      (is (:symmetry-passed analysis) "Symmetric data should pass verification")
      (is (empty? (:symmetry-violations analysis)) "Symmetric data should have no violations")
      (is (empty? (:corrective-actions analysis)) "Symmetric data should need no corrective actions")))
  
  (testing "Asymmetric measurement results should fail with production config"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :h :qubits [0]}
                                {:operation-type :cnot :qubits [0 1]}]}
          asymmetric-results {"00" 600 "11" 300 "01" 80 "10" 20}
          analysis (sv/apply-symmetry-verification circuit asymmetric-results)]
      
      (is (< (:symmetry-score analysis) 0.9) "Asymmetric data should have lower symmetry score")
      (is (not (:symmetry-passed analysis)) "Asymmetric data should fail with production config")
      (is (seq (:corrective-actions analysis)) "Failed verification should provide corrective actions")))
  
  (testing "Statistical validation should reject insufficient shots"
    (let [circuit {:num-qubits 2 :operations []}
          insufficient-results {"00" 50 "11" 30}]
      
      (is (thrown-with-msg? 
           clojure.lang.ExceptionInfo 
           #"Invalid measurement results"
           (sv/apply-symmetry-verification circuit insufficient-results))
          "Should reject insufficient shots for statistical significance"))))

(deftest test-chi-squared-statistical-analysis
  (testing "Chi-squared test should detect statistically significant violations"
    (let [circuit {:num-qubits 2 :operations [{:operation-type :h :qubits [0 1]}]}
          violation-results {"00" 200 "11" 200 "01" 400 "10" 200}
          analysis (sv/apply-symmetry-verification circuit violation-results)]
      
      (is (get-in analysis [:reflection-analysis :statistically-significant]) 
          "Large violations should be statistically significant")
      (is (> (get-in analysis [:reflection-analysis :chi-squared :chi-squared]) 3.841)
          "Chi-squared value should exceed critical value for significance")))
  
  (testing "Chi-squared test should handle edge cases properly"
    (let [observed [100 50 25]
          expected [75 75 75]
          result (sv/compute-chi-squared-test observed expected 2)]
      
      (is (> (:chi-squared result) 0) "Chi-squared should be positive")
      (is (contains? result :p-value) "Should include p-value estimate")
      (is (contains? result :effect-size) "Should include effect size"))))

(deftest test-permutation-symmetry-analysis
  (testing "Permutation violations should be detected"
    (let [circuit {:num-qubits 3 :operations [{:operation-type :h :qubits [0 1 2]}]}
          permutation-violation {"000" 200 "001" 300 "010" 50 "011" 50
                                 "100" 50 "101" 50 "110" 50 "111" 250}
          analysis (sv/apply-symmetry-verification circuit permutation-violation)]
      
      (is (< (get-in analysis [:permutation-analysis :score]) 0.9)
          "Permutation violations should reduce permutation score")
      (is (seq (get-in analysis [:permutation-analysis :violations]))
          "Should detect permutation violations")))
  
  (testing "Large systems should use sampling approach"
    (let [circuit {:num-qubits 5 :operations [{:operation-type :h :qubits [0 1 2 3 4]}]}
          large-results (zipmap (for [i (range 32)] (format "%05d" (Integer/parseInt (Integer/toBinaryString i))))
                                (repeat 32 30))
          analysis (sv/apply-symmetry-verification circuit large-results)]
      
      (is (contains? (:permutation-analysis analysis) :analysis)
          "Large systems should include analysis description")
      (is (re-find #"Sampled" (get-in analysis [:permutation-analysis :analysis]))
          "Large systems should use sampling approach"))))

(deftest test-configuration-management
  (testing "Production configuration should be stricter than development"
    (let [circuit {:num-qubits 2 :operations []}
          marginal-results {"00" 400 "11" 350 "01" 150 "10" 100}
          
          production-analysis (sv/apply-symmetry-verification circuit marginal-results)
          
          dev-config {:thresholds {:parity-tolerance 0.1
                                   :reflection-symmetry-min 0.7
                                   :overall-symmetry-min 0.6}}
          dev-analysis (sv/apply-symmetry-verification circuit marginal-results dev-config)]
      
      ;; Same data should potentially pass with dev config but fail with production
      (is (>= (:symmetry-score production-analysis) (:symmetry-score dev-analysis))
          "Symmetry scores should be comparable")
      (is (or (:symmetry-passed dev-analysis) (not (:symmetry-passed production-analysis)))
          "Development config should be more lenient than production")))
  
  (testing "Custom configuration should override defaults properly"
    (let [circuit {:num-qubits 2 :operations []}
          results {"00" 500 "11" 500}
          custom-config {:statistical {:min-shots 200}}
          analysis (sv/apply-symmetry-verification circuit results custom-config)]
      
      (is (:symmetry-passed analysis) "Should pass with custom lower shot requirement")
      (is (= 1000 (get-in analysis [:statistical-validation :total-shots]))
          "Should properly validate against custom shot requirement"))))

(deftest test-corrective-actions
  (testing "Different violation types should trigger appropriate actions"
    (let [circuit {:num-qubits 2 :operations [{:operation-type :h :qubits [0]}]}
          reflection-violation {"00" 800 "11" 100 "01" 80 "10" 20}
          analysis (sv/apply-symmetry-verification circuit reflection-violation)]
      
      (is (not (:symmetry-passed analysis)) "Should fail verification")
      (is (some #(re-find #"hardware calibration" %) (:corrective-actions analysis))
          "Reflection violations should suggest hardware calibration checks")))
  
  (testing "High severity violations should trigger urgent actions"
    (let [circuit {:num-qubits 3 :operations []}
          severe-violation {"000" 900 "001" 10 "010" 10 "011" 10
                            "100" 10 "101" 10 "110" 10 "111" 40}
          analysis (sv/apply-symmetry-verification circuit severe-violation)]
      
      (is (some #(= (:severity %) :high) (:symmetry-violations analysis))
          "Severe asymmetry should create high-severity violations")
      (is (some #(re-find #"URGENT" %) (:corrective-actions analysis))
          "High-severity violations should trigger urgent actions"))))

(deftest test-production-readiness
  (testing "Performance metrics should be included"
    (let [circuit {:num-qubits 2 :operations []}
          results {"00" 500 "11" 500}
          analysis (sv/apply-symmetry-verification circuit results)]
      
      (is (contains? analysis :execution-metrics) "Should include execution metrics")
      (is (contains? (:execution-metrics analysis) :analysis-time) "Should track analysis time")
      (is (contains? (:execution-metrics analysis) :total-shots) "Should track total shots analyzed")))
  
  (testing "Recommendation confidence should be provided"
    (let [circuit {:num-qubits 2 :operations []}
          good-results {"00" 500 "11" 500}
          analysis (sv/apply-symmetry-verification circuit good-results)]
      
      (is (contains? analysis :recommendation-confidence) "Should include confidence rating")
      (is (> (:recommendation-confidence analysis) 0.8) "High-quality results should have high confidence")))
  
  (testing "Default configuration should be production-appropriate"
    (let [config sv/default-config]
      
      (is (>= (get-in config [:statistical :min-shots]) 500) "Should require sufficient shots for production")
      (is (>= (get-in config [:thresholds :reflection-symmetry-min]) 0.9) "Should have strict reflection thresholds")
      (is (>= (get-in config [:thresholds :overall-symmetry-min]) 0.8) "Should have strict overall thresholds")
      (is (<= (get-in config [:thresholds :max-violations]) 2) "Should limit acceptable violations")
      (is (contains? config :production-mode) "Should include production-mode settings"))))

(comment
  (run-tests)
  ;
  )