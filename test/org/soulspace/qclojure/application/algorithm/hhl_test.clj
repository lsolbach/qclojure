(ns org.soulspace.qclojure.application.algorithm.hhl-test
  "Tests for HHL (Harrow-Hassidim-Lloyd) Algorithm"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.string :as str]
            [org.soulspace.qclojure.application.algorithm.hhl :as hhl]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;;
;; Test Matrix Validation
;;
(deftest test-validate-hermitian-matrix
  (testing "Validates symmetric matrices correctly"
    (is (true? (hhl/validate-hermitian-matrix [[1 2] [2 3]])))
    (is (true? (hhl/validate-hermitian-matrix [[1 0 0] [0 2 0] [0 0 3]])))
    (is (true? (hhl/validate-hermitian-matrix [[2 1 3] [1 4 2] [3 2 5]]))))
  
  (testing "Rejects non-symmetric matrices"
    (is (false? (hhl/validate-hermitian-matrix [[1 2] [3 4]])))
    (is (false? (hhl/validate-hermitian-matrix [[1 2 3] [4 5 6] [7 8 9]]))))
  
  (testing "Handles edge cases"
    (is (true? (hhl/validate-hermitian-matrix [[5]]))) ; 1x1 matrix
    (is (true? (hhl/validate-hermitian-matrix [[0 0] [0 0]]))) ; Zero matrix
    (is (true? (hhl/validate-hermitian-matrix [[-1 2] [2 -3]]))) ; Negative elements
    ))

;;
;; Test Condition Number Estimation
;;
(deftest test-estimate-condition-number
  (testing "Estimates condition number for well-conditioned matrices"
    (let [identity-2x2 [[1 0] [0 1]]
          diagonal-2x2 [[2 0] [0 2]]
          condition-id (hhl/estimate-condition-number identity-2x2)
          condition-diag (hhl/estimate-condition-number diagonal-2x2)]
      (is (>= condition-id 1.0))
      (is (<= condition-id 2.0)) ; Should be close to 1 for identity
      (is (>= condition-diag 1.0))
      (is (<= condition-diag 3.0))))
  
  (testing "Handles ill-conditioned matrices"
    (let [ill-conditioned [[1 0.999] [0.999 1]]
          condition-num (hhl/estimate-condition-number ill-conditioned)]
      (is (>= condition-num 1.0))
      (is (< condition-num 1000.0)))) ; Should be reasonable but > 1
  
  (testing "Returns valid numbers for all test matrices"
    (let [test-matrices [[[1 0] [0 1]]
                         [[2 1] [1 2]]
                         [[3 0 0] [0 2 0] [0 0 1]]
                         [[1 2 3] [2 4 5] [3 5 6]]]
          condition-numbers (map hhl/estimate-condition-number test-matrices)]
      (doseq [κ condition-numbers]
        (is (number? κ))
        (is (>= κ 1.0))
        (is (not (Double/isInfinite κ)))
        (is (not (Double/isNaN κ)))))))

;;
;; Test Vector Preparation Circuit
;;
(deftest test-vector-preparation-circuit
  (testing "Creates circuit with correct number of qubits"
    (let [circuit-2q (hhl/vector-preparation-circuit [1 0] 2)
          circuit-3q (hhl/vector-preparation-circuit [1 2 3] 3)]
      (is (= (:num-qubits circuit-2q) 2))
      (is (= (:num-qubits circuit-3q) 3))))
  
  (testing "Circuit has appropriate structure"
    (let [circuit (hhl/vector-preparation-circuit [1 1] 2)]
      (is (contains? circuit :operations))
      (is (contains? circuit :name))
      (is (= (:name circuit) "Vector Preparation"))
      (is (vector? (:operations circuit)))))
  
  (testing "Handles various vector sizes"
    (let [vectors [[1]
                   [1 0] 
                   [1 1]
                   [1 2 3]
                   [0.5 0.5 0.5 0.5]]
          qubits [1 2 2 3 3]]
      (doseq [[vec qubits] (map vector vectors qubits)]
        (let [circuit (hhl/vector-preparation-circuit vec qubits)]
          (is (= (:num-qubits circuit) qubits))
          (is (>= (count (:operations circuit)) 0)))))) ; May have 0 ops for simple cases
  
  (testing "Handles zero vector gracefully"
    (let [circuit (hhl/vector-preparation-circuit [0 0] 2)]
      (is (= (:num-qubits circuit) 2))
      (is (vector? (:operations circuit))))))

;;
;; Test Matrix Encoding
;;
(deftest test-matrix-encoding-unitary
  (testing "Creates unitary function for valid matrices"
    (let [matrix [[1 0] [0 1]]
          unitary-fn (hhl/matrix-encoding-unitary matrix 1.0)]
      (is (fn? unitary-fn))))
  
  (testing "Unitary function operates on circuits correctly"
    (let [matrix [[2 1] [1 2]]
          unitary-fn (hhl/matrix-encoding-unitary matrix 1.0)
          test-circuit {:num-qubits 3 :operations []}
          n (count matrix)
          vector-qubits (max 1 (int (Math/ceil (/ (Math/log n) (Math/log 2)))))
          result-circuit (unitary-fn test-circuit 0 1 (vec (range vector-qubits)))]
      (is (map? result-circuit))
      (is (contains? result-circuit :operations))
      (is (>= (count (:operations result-circuit)) 0))))
  
  (testing "Handles different matrix sizes"
    (let [matrices [[[1 0] [0 2]]
                    [[1 0 0] [0 2 0] [0 0 3]]
                    [[2 1] [1 2]]]
          time-scales [0.5 1.0 2.0]]
      (doseq [[matrix time] (map vector matrices time-scales)]
        (let [unitary-fn (hhl/matrix-encoding-unitary matrix time)]
          (is (fn? unitary-fn)))))))

;;
;; Test Conditional Rotation Circuit
;;
(deftest test-conditional-rotation-circuit
  (testing "Creates conditional rotation function"
    (let [cond-rot-fn (hhl/conditional-rotation-circuit 3 2 (/ Math/PI 4))]
      (is (fn? cond-rot-fn))))
  
  (testing "Conditional rotation function operates on circuits"
    (let [cond-rot-fn (hhl/conditional-rotation-circuit 2 3 0.5)
          test-circuit {:num-qubits 4 :operations []}
          result-circuit (cond-rot-fn test-circuit)]
      (is (map? result-circuit))
      (is (contains? result-circuit :operations))
      (is (>= (count (:operations result-circuit)) 2)))) ; Should add rotations
  
  (testing "Handles different parameter ranges"
    (let [precision-qubits [2 3 4]
          ancilla-qubits [2 3 4] 
          C-values [0.1 (/ Math/PI 4) 1.0]]
      (doseq [[p a c] (map vector precision-qubits ancilla-qubits C-values)]
        (let [cond-rot-fn (hhl/conditional-rotation-circuit p a c)]
          (is (fn? cond-rot-fn)))))))

;;
;; Test HHL Circuit Construction
;;
(deftest test-hhl-circuit
  (testing "Creates complete HHL circuit"
    (let [matrix [[2 1] [1 2]]
          vector [1 1]
          circuit (hhl/hhl-circuit matrix vector 3 1)]
      (is (map? circuit))
      (is (contains? circuit :num-qubits))
      (is (contains? circuit :operations))
      (is (contains? circuit :name))
      (is (str/includes? (:name circuit) "HHL Algorithm"))))
  
  (testing "Circuit has correct number of qubits"
    (let [matrix-2x2 [[1 0] [0 2]]
          vector-2 [1 1]
          circuit-2x2 (hhl/hhl-circuit matrix-2x2 vector-2 3 1)
          
          matrix-4x4 [[1 0 0 0] [0 2 0 0] [0 0 3 0] [0 0 0 4]]
          vector-4 [1 1 1 1]
          circuit-4x4 (hhl/hhl-circuit matrix-4x4 vector-4 3 1)]
      
      ;; 2x2 matrix: 1 vector qubit + 3 precision + 1 ancilla = 5 qubits
      (is (= (:num-qubits circuit-2x2) 5))
      
      ;; 4x4 matrix: 2 vector qubits + 3 precision + 1 ancilla = 6 qubits
      (is (= (:num-qubits circuit-4x4) 6))))
  
  (testing "Circuit contains meaningful operations"
    (let [circuit (hhl/hhl-circuit [[1 0] [0 2]] [1 0] 2 1)]
      (is (vector? (:operations circuit)))
      (is (> (count (:operations circuit)) 0))))
  
  (testing "Handles different precision settings"
    (let [matrix [[2 1] [1 2]]
          vector [1 1]
          precisions [2 3 4 5]]
      (doseq [p precisions]
        (let [circuit (hhl/hhl-circuit matrix vector p 1)]
          (is (= (:num-qubits circuit) (+ 1 p 1))) ; 1 vector + p precision + 1 ancilla
          (is (> (count (:operations circuit)) 0)))))))

;;
;; Test Full HHL Algorithm
;;
(deftest test-hhl-algorithm
  (testing "Executes HHL algorithm successfully"
    (let [simulator (sim/create-simulator {:max-qubits 8})
          matrix [[2 1] [1 2]]
          vector [1 1]
          result (hhl/hhl-algorithm simulator matrix vector 
                                    {:precision-qubits 3
                                     :ancilla-qubits 1
                                     :shots 100})]
      (is (map? result))
      (is (contains? result :success))
      (is (contains? result :solution-vector))
      (is (contains? result :circuit))
      (is (contains? result :measurements))
      (is (contains? result :condition-number))))
  
  (testing "Returns proper result structure"
    (let [simulator (sim/create-simulator {:max-qubits 6})
          result (hhl/hhl-algorithm simulator [[1 0] [0 2]] [1 0]
                                    {:precision-qubits 2
                                     :shots 50})]
      (is (boolean? (:success result)))
      (is (vector? (:solution-vector result)))
      (is (number? (:condition-number result)))
      (is (>= (:condition-number result) 1.0))
      (is (number? (:success-probability result)))
      (is (>= (:success-probability result) 0.0))
      (is (<= (:success-probability result) 1.0))
      (is (nat-int? (:total-shots result)))
      (is (nat-int? (:successful-shots result)))))
  
  (testing "Handles different matrix sizes"
    (let [simulator (sim/create-simulator {:max-qubits 10})
          matrices [[[1 0] [0 2]]
                    [[1 0 0] [0 2 0] [0 0 3]]
                    [[2 1] [1 3]]]
          vectors [[1 0]
                   [1 1 1]
                   [1 1]]]
      (doseq [[matrix vector] (map clojure.core/vector matrices vectors)]
        (let [result (hhl/hhl-algorithm simulator matrix vector
                                        {:precision-qubits 2
                                         :shots 20})]
          (is (contains? result :success))
          (is (= (count (:solution-vector result)) (count vector)))))))
  
  (testing "Algorithm completes in reasonable time"
    (let [simulator (sim/create-simulator {:max-qubits 6})
          start-time (System/currentTimeMillis)
          result (hhl/hhl-algorithm simulator [[1 0] [0 1]] [1 0]
                                    {:precision-qubits 2
                                     :shots 10})
          end-time (System/currentTimeMillis)
          duration (- end-time start-time)]
      (is (< duration 5000)) ; Should complete within 5 seconds
      (is (contains? result :success))))
  
  (testing "Handles edge cases gracefully"
    (let [simulator (sim/create-simulator {:max-qubits 6})]
      ;; Single qubit case
      (let [result (hhl/hhl-algorithm simulator [[2]] [1]
                                      {:precision-qubits 2
                                       :shots 10})]
        (is (contains? result :success))
        (is (= (count (:solution-vector result)) 1)))
      
      ;; Zero vector case (should not crash)
      (let [result (hhl/hhl-algorithm simulator [[1 0] [0 1]] [0 0]
                                      {:precision-qubits 2
                                       :shots 10})]
        (is (contains? result :success))))))

;;
;; Property-based tests using test.check
;;
(def hermitian-matrix-2x2-gen
  "Generator for 2x2 Hermitian matrices"
  (gen/let [a (gen/choose -5 5)
            b (gen/choose -5 5)
            c (gen/choose -5 5)]
    [[a b] [b c]]))

(def unit-vector-2d-gen
  "Generator for 2D unit vectors"
  (gen/let [x (gen/double* {:min -2 :max 2 :NaN? false :infinite? false})
            y (gen/double* {:min -2 :max 2 :NaN? false :infinite? false})]
    (let [norm (Math/sqrt (+ (* x x) (* y y)))]
      (if (> norm 1e-10)
        [(/ x norm) (/ y norm)]
        [1.0 0.0]))))

(def hhl-algorithm-properties
  "Property: HHL algorithm always returns valid result structure"
  (prop/for-all [matrix hermitian-matrix-2x2-gen
                 vector unit-vector-2d-gen]
    (when (hhl/validate-hermitian-matrix matrix)
      (let [simulator (sim/create-simulator {:max-qubits 8})
            result (hhl/hhl-algorithm simulator matrix vector
                                      {:precision-qubits 2
                                       :shots 10})]
        (and (map? result)
             (contains? result :success)
             (contains? result :solution-vector)
             (contains? result :condition-number)
             (boolean? (:success result))
             (vector? (:solution-vector result))
             (number? (:condition-number result))
             (>= (:condition-number result) 1.0))))))

(def hermitian-validation-properties
  "Property: Hermitian validation works correctly"
  (prop/for-all [matrix hermitian-matrix-2x2-gen]
    (let [validation-result (hhl/validate-hermitian-matrix matrix)]
      (boolean? validation-result))))

(deftest test-hhl-properties
  (testing "Property-based testing of HHL algorithm"
    (let [result (tc/quick-check 20 hhl-algorithm-properties)]
      (is (:result result) (str "Property failed: " (:shrunk result)))))
  
  (testing "Property-based testing of Hermitian validation"
    (let [result (tc/quick-check 50 hermitian-validation-properties)]
      (is (:result result) (str "Property failed: " (:shrunk result))))))

;;
;; Integration tests
;;
(deftest test-hhl-integration
  (testing "HHL algorithm integrates with different backends"
    (let [matrix [[2 1] [1 2]]
          vector [1 1]
          simulator (sim/create-simulator {:max-qubits 8})
          result (hhl/hhl-algorithm simulator matrix vector
                                    {:precision-qubits 3
                                     :shots 100})]
      (is (contains? result :circuit))
      (is (contains? (:circuit result) :num-qubits))
      (is (>= (:num-qubits (:circuit result)) 5))))
  
  (testing "Algorithm metadata consistency"
    (let [results [(hhl/hhl-algorithm (sim/create-simulator {:max-qubits 6}) [[1 0] [0 1]] [1 0]
                                      {:precision-qubits 2
                                       :shots 10})
                   (hhl/hhl-algorithm (sim/create-simulator {:max-qubits 6}) [[2 1] [1 2]] [1 1]
                                      {:precision-qubits 2
                                       :shots 10})]]
      
      ;; All results should have consistent structure
      (doseq [result results]
        (is (contains? result :precision-qubits))
        (is (contains? result :ancilla-qubits))
        (is (contains? result :total-shots))
        (is (contains? result :successful-shots))))))

;;
;; Performance and benchmark tests
;;
(deftest test-hhl-performance
  (testing "Algorithm scales reasonably with matrix size"
    (let [simulator (sim/create-simulator {:max-qubits 10})
          matrices [[[1 0] [0 2]]                    ; 2x2
                    [[1 0 0] [0 2 0] [0 0 3]]]       ; 3x3
          vectors [[1 0] [1 1 1]]
          
          results (for [[matrix vector] (map clojure.core/vector matrices vectors)]
                    (let [start (System/currentTimeMillis)
                          result (hhl/hhl-algorithm simulator matrix vector
                                                    {:precision-qubits 2
                                                     :shots 20})
                          end (System/currentTimeMillis)]
                      {:size (count matrix)
                       :duration (- end start)
                       :qubits (:num-qubits (:circuit result))}))]
      
      ;; All should complete reasonably quickly
      (doseq [{:keys [duration]} results]
        (is (< duration 3000))) ; < 3 seconds
      
      ;; Qubit count should scale logarithmically
      (let [qubit-counts (map :qubits results)]
        (is (every? #(>= % 4) qubit-counts))
        (is (every? #(<= % 10) qubit-counts)))))
  
  (testing "Success probability is reasonable for well-conditioned matrices"
    (let [simulator (sim/create-simulator {:max-qubits 8})
          ;; Use well-conditioned diagonal matrix
          matrix [[2 0] [0 2]]
          vector [1 0]
          result (hhl/hhl-algorithm simulator matrix vector
                                    {:precision-qubits 3
                                     :shots 100})]
      (is (>= (:success-probability result) 0.0))
      (is (<= (:success-probability result) 1.0))
      ;; Well-conditioned matrices should have reasonable condition numbers
      (is (<= (:condition-number result) 5.0)))))

(comment
  ;; REPL testing examples for HHL algorithm

  ;; Run all HHL tests
  (run-tests)

  ;; Run specific test categories
  (test-validate-hermitian-matrix)
  (test-hhl-algorithm)
  (test-hhl-properties)

  ;; Manual testing
  
  ;; Test basic functionality
  (let [simulator (sim/create-simulator {:max-qubits 8})]
    (hhl/hhl-algorithm simulator [[2 1] [1 2]] [1 1]
                       {:precision-qubits 3
                        :shots 100}))

  ;; Test condition number estimation
  (hhl/estimate-condition-number [[1 0] [0 1]])
  (hhl/estimate-condition-number [[2 1] [1 2]])
  (hhl/estimate-condition-number [[1 0.999] [0.999 1]])

  ;; Test matrix validation
  (hhl/validate-hermitian-matrix [[1 2] [2 3]]) ; true
  (hhl/validate-hermitian-matrix [[1 2] [3 4]]) ; false

  ;; Run property-based tests
  (tc/quick-check 30 hhl-algorithm-properties)
  (tc/quick-check 50 hermitian-validation-properties)

  ;; Performance testing
  (time (hhl/hhl-algorithm (sim/create-simulator {:max-qubits 6}) [[1 0] [0 2]] [1 0]
                           {:precision-qubits 2
                            :shots 50}))
  )
