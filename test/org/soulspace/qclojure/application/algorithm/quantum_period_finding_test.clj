(ns org.soulspace.qclojure.application.algorithm.quantum-period-finding-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]))

;; Test data and helper functions
(def test-backend (sim/create-simulator))

(defn test-modular-period
  "Classical function to verify quantum period finding results"
  [a N]
  (loop [r 1]
    (if (= 1 (qmath/mod-exp a r N))
      r
      (recur (inc r)))))

(defn valid-period?
  "Check if r is a valid period for a^r ≡ 1 (mod N)"
  [a r N]
  (= 1 (qmath/mod-exp a r N)))

;;
;; Test phase-to-period conversion function
;;
(deftest test-phase-to-period
  (testing "Phase to period conversion for known cases"
    ;; Test with known phase values that should produce valid periods
    (let [result (qpf/phase-to-period 0.0 3 15 7)]
      (is (or (nil? result) (map? result)) "Should return nil or a map"))
    
    ;; Test with π phase (half rotation)
    (let [result (qpf/phase-to-period Math/PI 4 15 7)]
      (is (or (nil? result) (and (map? result) (contains? result :period))) 
          "Should return nil or map with period"))
    
    ;; Test with 2π phase (full rotation)
    (let [result (qpf/phase-to-period (* 2 Math/PI) 4 15 7)]
      (is (or (nil? result) (map? result)) "Should handle full rotation")))
  
  (testing "Phase to period edge cases"
    ;; Zero phase
    (let [result (qpf/phase-to-period 0.0 3 8 3)]
      (is (or (nil? result) (map? result)) "Should handle zero phase"))
    
    ;; Very small phase
    (let [result (qpf/phase-to-period 1e-10 4 15 2)]
      (is (or (nil? result) (map? result)) "Should handle very small phase"))
    
    ;; Invalid parameters
    (is (nil? (qpf/phase-to-period 1.0 3 2 5)) "Should return nil for a >= N")))

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
          circuit (qpf/quantum-phase-estimation-circuit 3 3 eigenstate-prep-fn controlled-unitary-fn)]
      
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
          circuit (qpf/quantum-phase-estimation-circuit 4 3 eigenstate-prep-fn controlled-unitary-fn)
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
          result (qpf/quantum-phase-estimation-with-custom-unitary
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
          result (qpf/quantum-phase-estimation-with-custom-unitary
                   test-backend 3 3 eigenstate-prep-fn controlled-unitary-fn 
                   {:shots 50 :n-measurements 5})]
      
      (is (= 5 (:n-measurements result)) "Should handle multiple measurements")
      (is (= 5 (count (:execution-results result))) "Should have correct execution count"))))

;;
;; Test quantum period finding main function
;;
(deftest test-quantum-period-finding
  (testing "Period finding with known cases"
    ;; Test with a=7, N=15 (known period = 4)
    (let [result (qpf/quantum-period-finding test-backend 7 15 4 3 {:shots 100})]
      
      (is (map? result) "Should return a result map")
      (is (contains? result :estimated-period) "Should contain estimated period")
      (is (contains? result :period-candidates) "Should contain period candidates")
      (is (contains? result :qpe-result) "Should contain QPE result")
      (is (contains? result :success) "Should contain success flag")
      (is (contains? result :confidence) "Should contain confidence")
      (is (contains? result :circuit) "Should contain circuit")
      (is (boolean? (:success result)) "Success should be boolean")
      (is (number? (:confidence result)) "Confidence should be number")
      (is (= 3 (:n-measurements result)) "Should record n-measurements"))
    
    ;; Test with a=2, N=15 (known period = 4)
    (let [result (qpf/quantum-period-finding test-backend 2 15 4 2 {:shots 50})]
      (is (map? result) "Should return result for a=2, N=15")
      (is (boolean? (:success result)) "Should have success flag")))
  
  (testing "Period finding with smaller cases"
    ;; Test with a=3, N=8 (known period = 2)
    (let [result (qpf/quantum-period-finding test-backend 3 8 3 2 {:shots 100})]
      (is (map? result) "Should handle smaller modulus")
      (is (contains? result :estimated-period) "Should contain estimated period")
      (is (>= (:confidence result) 0.0) "Confidence should be non-negative")
      (is (<= (:confidence result) 1.0) "Confidence should not exceed 1.0")))
  
  (testing "Period finding parameter validation"
    ;; Test parameter constraints
    (is (thrown? java.lang.AssertionError
                 (qpf/quantum-period-finding test-backend 15 7 3 1 {}))
        "Should fail when a >= N")
    
    (is (thrown? java.lang.AssertionError
                 (qpf/quantum-period-finding test-backend 0 15 3 1 {}))
        "Should fail when a <= 1")
    
    (is (thrown? java.lang.AssertionError
                 (qpf/quantum-period-finding test-backend 7 15 0 1 {}))
        "Should fail when precision-qubits <= 0")))

;;
;; Test function overloads
;;
(deftest test-quantum-period-finding-overloads
  (testing "Four argument overload"
    (let [result (qpf/quantum-period-finding test-backend 7 15 4)]
      (is (map? result) "Should work with 4 arguments")
      (is (= 1 (:n-measurements result)) "Should default to 1 measurement")))
  
  (testing "Five argument overload"
    (let [result (qpf/quantum-period-finding test-backend 7 15 4 3)]
      (is (map? result) "Should work with 5 arguments")
      (is (= 3 (:n-measurements result)) "Should use specified n-measurements")))
  
  (testing "Six argument overload"
    (let [result (qpf/quantum-period-finding test-backend 7 15 4 2 {:shots 200})]
      (is (map? result) "Should work with 6 arguments")
      (is (= 2 (:n-measurements result)) "Should use specified parameters"))))

;;
;; Integration tests with classical verification
;;
(deftest test-classical-verification
  (testing "Classical period verification for known cases"
    (is (= 4 (test-modular-period 7 15)) "7^4 ≡ 1 (mod 15)")
    (is (= 4 (test-modular-period 2 15)) "2^4 ≡ 1 (mod 15)")
    (is (= 2 (test-modular-period 3 8)) "3^2 ≡ 1 (mod 8)")
    (is (= 2 (test-modular-period 4 15)) "4^2 ≡ 1 (mod 15)")
    (is (= 1 (test-modular-period 1 5)) "1^1 ≡ 1 (mod 5)"))
  
  (testing "Period validation function"
    (is (valid-period? 7 4 15) "4 is valid period for 7 mod 15")
    (is (valid-period? 2 4 15) "4 is valid period for 2 mod 15")
    (is (not (valid-period? 7 3 15)) "3 is not valid period for 7 mod 15")
    (is (valid-period? 3 2 8) "2 is valid period for 3 mod 8")))

;;
;; Test result structure and consistency
;;
(deftest test-result-structure
  (testing "Result structure consistency"
    (let [result (qpf/quantum-period-finding test-backend 7 15 4 2 {:shots 100})]
      
      ;; Test required keys
      (is (contains? result :estimated-period) "Should have estimated-period")
      (is (contains? result :period-candidates) "Should have period-candidates")
      (is (contains? result :qpe-result) "Should have qpe-result")
      (is (contains? result :success) "Should have success")
      (is (contains? result :confidence) "Should have confidence")
      (is (contains? result :circuit) "Should have circuit")
      (is (contains? result :n-measurements) "Should have n-measurements")
      
      ;; Test data types
      (is (or (nil? (:estimated-period result)) 
              (pos-int? (:estimated-period result))) "Estimated period should be nil or positive integer")
      (is (vector? (:period-candidates result)) "Period candidates should be vector")
      (is (map? (:qpe-result result)) "QPE result should be map")
      (is (boolean? (:success result)) "Success should be boolean")
      (is (number? (:confidence result)) "Confidence should be number")
      (is (map? (:circuit result)) "Circuit should be map")
      
      ;; Test period candidates structure
      (doseq [candidate (:period-candidates result)]
        (is (map? candidate) "Each candidate should be a map")
        (is (contains? candidate :period) "Candidate should have period")
        (is (contains? candidate :probability) "Candidate should have probability")
        (is (pos-int? (:period candidate)) "Period should be positive integer")
        (is (number? (:probability candidate)) "Probability should be number"))))
  
  (testing "Success and confidence correlation"
    (let [result (qpf/quantum-period-finding test-backend 7 15 4 3 {:shots 100})]
      (if (:success result)
        (is (pos? (:confidence result)) "If successful, confidence should be positive")
        (is (zero? (:confidence result)) "If not successful, confidence should be zero")))))

;;
;; Performance and edge case tests
;;
(deftest test-edge-cases
  (testing "Edge cases for period finding"
    ;; Test with minimal precision
    (let [result (qpf/quantum-period-finding test-backend 3 8 2 1 {:shots 50})]
      (is (map? result) "Should handle minimal precision qubits"))
    
    ;; Test with higher precision
    (let [result (qpf/quantum-period-finding test-backend 7 15 6 1 {:shots 50})]
      (is (map? result) "Should handle higher precision qubits"))
    
    ;; Test with single measurement
    (let [result (qpf/quantum-period-finding test-backend 2 15 4 1 {:shots 10})]
      (is (= 1 (:n-measurements result)) "Should handle single measurement")))
  
  #_(testing "Different shot counts"
    (doseq [shots [10 50 100 500]]
      (let [result (qpf/quantum-period-finding test-backend 7 15 4 2 {:shots shots})]
        (is (map? result) (str "Should work with " shots " shots"))
        (is (>= (:confidence result) 0.0) "Confidence should be non-negative")))))

;; Run all tests
(comment
  (run-tests)
  
  ;; Individual test examples
  (test-phase-to-period)
  (test-quantum-phase-estimation-circuit)
  (test-quantum-period-finding)
  (test-classical-verification)
  
  ;; Test with specific parameters
  (qpf/quantum-period-finding test-backend 7 15 4 3 {:shots 1000})
  (qpf/phase-to-period 0.5 4 15 7)
  
  ;
  )

