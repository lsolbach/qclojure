(ns org.soulspace.qclojure.application.shors-algorithm-test
  "Tests for Shor's factoring algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.quantum-algorithms :as qa]
            [org.soulspace.qclojure.domain.math :as qmath]))

;; Test Enhanced Period Finding
(deftest test-enhanced-period-finding
  (testing "Enhanced period finding for various inputs"
    (let [test-cases [{:a 7 :N 15 :period 4}
                      {:a 11 :N 21 :period 6}
                      {:a 2 :N 35 :period 12}]
          precision 8]  ;; Using 8 qubits precision for simulation
      
      (doseq [{:keys [a N period]} test-cases]
        ;; Calculate expected phase from period
        (let [phase-fraction (/ 1 period)
              measured-value (int (* phase-fraction (Math/pow 2 precision)))
              found-period (qa/enhanced-period-finding measured-value precision N a)]
          (is (= period found-period)
              (str "Period finding failed for a=" a ", N=" N 
                   ". Expected period " period ", got " found-period)))))))

;; Test Quantum Period Finding
(deftest test-quantum-period-finding
  (testing "Quantum period finding routine"
    ;; Test with a=7, N=15, expected period=4
    (let [a 7
          N 15
          expected-period 4
          ;; Use small number of qubits for faster tests
          n-qubits 4
          result (qa/quantum-period-finding a N n-qubits)]
      
      (is (map? result) "Result should be a map")
      (is (contains? result :estimated-period) "Result should contain estimated period")
      
      (when-let [period (:estimated-period result)]
        (is (= 1 (qmath/mod-exp a period N))
            (str "a^period mod N should equal 1: " a "^" period " mod " N))))))

;; Test Shor's Algorithm with small composite numbers
(deftest test-shors-algorithm
  (testing "Factoring N=15"
    (let [N 15
          expected-factors #{3 5}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 5")))
  
  (testing "Factoring N=21"
    (let [N 21
          expected-factors #{3 7}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 7")))
  
  (testing "Factoring even number N=14"
    (let [N 14
          expected-factors #{2 7}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 2 and 7")
      (is (= :classical-even (:method result)) "Should use classical method for even numbers")))
  
  (testing "Perfect power factorization N=27"
    (let [N 27  ; 3^3
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= [3 3 3] (:factors result)) "Should find factors [3 3 3]"))))

;; Test hardware-compatible mode
(deftest test-hardware-compatible-mode
  (testing "Shor's algorithm with hardware-compatible circuits"
    (let [N 15
          options {:hardware-compatible true
                   :n-measurements 2  ; Lower count for faster tests
                   :n-qubits 4}       ; Use smaller circuit for tests
          result (qa/shor-algorithm N options)]
      
      ;; We mainly test that it runs without errors since results may vary
      (is (map? result) "Should return a map")
      (is (contains? result :success) "Should include success flag")
      (is (contains? result :statistics) "Should include statistics"))))
