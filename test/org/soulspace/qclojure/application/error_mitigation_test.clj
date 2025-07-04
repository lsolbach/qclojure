(ns org.soulspace.qclojure.application.error-mitigation-test
  "Comprehensive tests for error mitigation strategies."
  (:require [clojure.test :refer :all]
            [org.soulspace.qclojure.application.error-mitigation :as em]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.util.test :as util]))

;; Test data
(def test-readout-config
  {:prob-0-to-1 0.1
   :prob-1-to-0 0.05})

(def test-noise-model
  {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.02}
                :cnot {:noise-type :depolarizing :noise-strength 0.05}}
   :readout-error test-readout-config})

(def simple-bell-circuit
  (qc/bell-state-circuit))

(def test-backend
  {:noise-model test-noise-model
   :supported-gates #{:h :x :z :cnot}})

;; Tests for readout error mitigation
(deftest test-readout-error-mitigation
  (testing "Calibration matrix creation"
    (testing "2-qubit calibration matrix"
      (let [cal-matrix (em/create-calibration-matrix 2 test-readout-config)]
        (is (= 4 (count cal-matrix)) "Should create 4x4 matrix for 2 qubits")
        (is (every? #(= 4 (count %)) cal-matrix) "All rows should have 4 elements")
        (is (every? #(and (>= % 0) (<= % 1)) (flatten cal-matrix)) "All probabilities should be in [0,1]")))
    
    (testing "1-qubit calibration matrix"
      (let [cal-matrix (em/create-calibration-matrix 1 test-readout-config)]
        (is (= 2 (count cal-matrix)) "Should create 2x2 matrix for 1 qubit")
        (is (every? #(= 2 (count %)) cal-matrix) "All rows should have 2 elements"))))
  
    (testing "High noise rates"
      (let [high-noise {:prob-0-to-1 0.4 :prob-1-to-0 0.3}
            cal-matrix (em/create-calibration-matrix 1 high-noise)]
        (is (not (nil? cal-matrix)) "Should handle high noise rates"))))

;; Tests for Zero Noise Extrapolation(deftest test-zero-noise-extrapolation
  (testing "ZNE with valid noise scales"
    (let [result (em/zero-noise-extrapolation 
                   simple-bell-circuit 
                   test-backend 
                   [1.0 1.5 2.0]
                   ["00" "11"]
                   1000)]
      
      (is (contains? result :extrapolated-value) "Should return extrapolated value")
      (is (contains? result :improvement-factor) "Should return improvement factor")
      (is (contains? result :data-points) "Should return data points")
      (is (= 3 (count (:data-points result))) "Should have 3 data points for 3 noise scales")
      (is (every? #(contains? % :x) (:data-points result)) "Data points should have x values")
      (is (every? #(contains? % :y) (:data-points result)) "Data points should have y values")))
  
  (testing "ZNE improvement calculation"
    (let [result (em/zero-noise-extrapolation 
                   simple-bell-circuit 
                   test-backend 
                   [1.0 2.0]
                   ["00" "11"] 
                   500)]
      
      (is (number? (:improvement-factor result)) "Improvement factor should be numeric")
      (is (pos? (:improvement-factor result)) "Improvement factor should be positive")))
  

;; Tests for circuit analysis
(deftest test-circuit-analysis
  (testing "Circuit noise profile analysis"
    (let [analysis (em/analyze-circuit-noise-profile simple-bell-circuit test-noise-model)]
      
      (is (contains? analysis :circuit-depth) "Should analyze circuit depth")
      (is (contains? analysis :gate-counts) "Should count gate types")
      (is (contains? analysis :total-gate-noise) "Should calculate total gate noise")
      (is (contains? analysis :recommended-strategies) "Should recommend strategies")
      
      (is (= 2 (:circuit-depth analysis)) "Should correctly count operations")
      (is (= {:h 1, :cnot 1} (:gate-counts analysis)) "Should correctly count gate types")
      (is (sequential? (:recommended-strategies analysis)) "Strategies should be a sequence")))
  
  (testing "Strategy selection"
    (let [constraints {:resource-limit :moderate :priority :fidelity}
          selection (em/select-mitigation-strategies simple-bell-circuit test-noise-model constraints)]
      
      (is (contains? selection :analysis) "Should include circuit analysis")
      (is (contains? selection :selected-strategies) "Should return selected strategies")
      (is (contains? selection :constraints) "Should include constraints")
      (is (sequential? (:selected-strategies selection)) "Selected strategies should be a sequence"))))

;; Tests for complete mitigation pipeline
(deftest test-mitigation-pipeline
  (testing "Basic mitigation pipeline"
    (let [config {:strategies [:readout-error-mitigation]
                  :num-shots 1000
                  :constraints {:resource-limit :moderate}}
          result (em/apply-error-mitigation simple-bell-circuit test-backend config)]
      
      (is (contains? result :measurement-counts) "Should return measurement counts")
      (is (contains? result :mitigation-applied) "Should track applied strategies")
      (is (contains? result :overall-improvement-factor) "Should calculate overall improvement")
      (is (contains? result :execution-time-ms) "Should track execution time")
      
      (is (sequential? (:mitigation-applied result)) "Applied strategies should be a sequence")
      (is (number? (:overall-improvement-factor result)) "Overall improvement should be numeric")
      (is (number? (:execution-time-ms result)) "Execution time should be numeric")))
  
  (testing "Multiple strategy application"
    (let [config {:strategies [:readout-error-mitigation :symmetry-verification]
                  :num-shots 500}
          result (em/apply-error-mitigation simple-bell-circuit test-backend config)]
      
      (is (<= 1 (count (:mitigation-applied result))) "Should apply at least one strategy")
      (is (contains? (:improvements result) :readout-error-mitigation) 
          "Should include readout mitigation improvements")))
  
  (testing "Strategy constraint handling"
    (let [config {:strategies [:readout-error-mitigation :zero-noise-extrapolation]
                  :constraints {:resource-limit :minimal :priority :speed}}
          result (em/apply-error-mitigation simple-bell-circuit test-backend config)]
      
      (is (not (nil? result)) "Should handle resource constraints")
      (is (contains? result :strategy-selection) "Should include strategy selection info"))))

;; Tests for strategy metadata and utilities
(deftest test-utility-functions
  (testing "Strategy metadata access"
    (let [strategies (em/list-available-strategies)]
      (is (map? strategies) "Should return strategy metadata map")
      (is (contains? strategies :zero-noise-extrapolation) "Should include ZNE")
      (is (contains? strategies :readout-error-mitigation) "Should include readout mitigation"))
    
    (let [zne-info (em/get-strategy-info :zero-noise-extrapolation)]
      (is (map? zne-info) "Should return strategy info")
      (is (contains? zne-info :type) "Should include strategy type")
      (is (contains? zne-info :overhead) "Should include overhead info")))
  
  (testing "Noise model scaling"
    (let [scaled-model (em/scale-noise-model test-noise-model 2.0)]
      (is (map? scaled-model) "Should return scaled noise model")
      (is (= 0.04 (get-in scaled-model [:gate-noise :h :noise-strength])) 
          "Should scale gate noise correctly")
      (is (= 0.2 (get-in scaled-model [:readout-error :prob-0-to-1]))
          "Should scale readout error correctly"))))

;; Performance and robustness tests
(deftest test-performance-and-robustness
  #_(testing "Large circuit handling"
    (let [large-circuit {:num-qubits 3
                         :operations (repeatedly 20 #(hash-map :operation-type :h 
                                                                :operation-params {:target (rand-int 3)}))}
          config {:strategies [:readout-error-mitigation]
                  :num-shots 100}]
      (is (not (nil? (em/apply-error-mitigation large-circuit test-backend config)))
          "Should handle larger circuits")))
  
  #_(testing "Error resilience"
    (let [invalid-config {:strategies [:nonexistent-strategy]
                          :num-shots 100}
          result (em/apply-error-mitigation simple-bell-circuit test-backend invalid-config)]
      (is (not (nil? result)) "Should handle invalid strategies gracefully")))
  
  (testing "Edge case noise models"
    (let [zero-noise-model {:gate-noise {} :readout-error {:prob-0-to-1 0.0 :prob-1-to-0 0.0}}
          config {:strategies [:readout-error-mitigation]}
          result (em/apply-error-mitigation simple-bell-circuit 
                                            {:noise-model zero-noise-model} 
                                            config)]
      (is (not (nil? result)) "Should handle zero-noise scenarios"))))

(comment
  (run-tests)
  ;
  )