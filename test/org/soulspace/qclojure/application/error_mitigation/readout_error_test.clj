(ns org.soulspace.qclojure.application.error-mitigation.readout-error-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation.readout-error :as rem]
            [org.soulspace.qclojure.util.test :as util]))

;; Test data
(def test-readout-config
  {:prob-0-to-1 0.1
   :prob-1-to-0 0.05})

;; Tests for readout error mitigation
(deftest test-readout-error-mitigation
  (testing "Calibration matrix creation"
    (testing "2-qubit calibration matrix"
      (let [cal-matrix (rem/create-calibration-matrix 2 test-readout-config)]
        (is (= 4 (count cal-matrix)) "Should create 4x4 matrix for 2 qubits")
        (is (every? #(= 4 (count %)) cal-matrix) "All rows should have 4 elements")
        (is (every? #(and (>= % 0) (<= % 1)) (flatten cal-matrix)) "All probabilities should be in [0,1]")))
    
    (testing "1-qubit calibration matrix"
      (let [cal-matrix (rem/create-calibration-matrix 1 test-readout-config)]
        (is (= 2 (count cal-matrix)) "Should create 2x2 matrix for 1 qubit")
        (is (every? #(= 2 (count %)) cal-matrix) "All rows should have 2 elements"))))
  
    (testing "High noise rates"
      (let [high-noise {:prob-0-to-1 0.4 :prob-1-to-0 0.3}
            cal-matrix (rem/create-calibration-matrix 1 high-noise)]
        (is (not (nil? cal-matrix)) "Should handle high noise rates"))))

