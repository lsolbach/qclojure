(ns org.soulspace.qclojure.application.error-mitigation.zero-noise-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation.zero-noise :as zne]
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

;; Tests for Zero Noise Extrapolation
(deftest test-zero-noise-extrapolation
  (testing "ZNE with valid noise scales"
    (let [result (zne/zero-noise-extrapolation 
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
    (let [result (zne/zero-noise-extrapolation 
                   simple-bell-circuit 
                   test-backend 
                   [1.0 2.0]
                   ["00" "11"] 
                   500)]
      
      (is (number? (:improvement-factor result)) "Improvement factor should be numeric")
      (is (pos? (:improvement-factor result)) "Improvement factor should be positive"))))
  

