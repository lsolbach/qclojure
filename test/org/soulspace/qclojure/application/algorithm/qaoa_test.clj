(ns org.soulspace.qclojure.application.algorithm.qaoa-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.application.algorithm.qaoa :as qaoa]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;;
;; Test data
;;
(def triangle-graph [[0 1 1.0] [1 2 1.0] [0 2 1.0]])
(def line-graph [[0 1 1.0] [1 2 1.0]])

;;
;; Problem Hamiltonian Tests  
;;
(deftest test-max-cut-hamiltonian
  (testing "MaxCut Hamiltonian construction for triangle graph"
    (let [hamiltonian (qaoa/max-cut-hamiltonian triangle-graph 3)]
      (is (coll? hamiltonian) "Should return a collection")
      (is (every? map? hamiltonian) "Each term should be a map")
      (is (every? #(contains? % :coefficient) hamiltonian) "Each term should have coefficient")
      (is (every? #(contains? % :pauli-string) hamiltonian) "Each term should have Pauli string")
      (is (= 6 (count hamiltonian)) "Triangle graph should have 6 terms (3 edges * 2 terms per edge)")
      (is (ham/validate-hamiltonian hamiltonian) "Should be a valid Hamiltonian")))

  (testing "MaxCut Hamiltonian coefficients"
    (let [hamiltonian (qaoa/max-cut-hamiltonian [[0 1 2.0]] 2)]
      (is (= 2 (count hamiltonian)) "Single edge should produce 2 terms")
      ;; Check coefficients are correct (weight/2 and -weight/2)
      (let [coeffs (map :coefficient hamiltonian)]
        (is (= #{1.0 -1.0} (set coeffs)) "Should have coefficients 1.0 and -1.0 for weight 2.0")))))

(deftest test-standard-mixer-hamiltonian
  (testing "Standard X mixer Hamiltonian"
    (let [hamiltonian (qaoa/standard-mixer-hamiltonian 3)]
      (is (= 3 (count hamiltonian)) "Should have one X term per qubit")
      (is (every? #(= (:coefficient %) 1.0) hamiltonian) "All coefficients should be 1.0")
      (is (every? #(= 1 (count (remove #{\I} (:pauli-string %)))) hamiltonian) 
          "Each term should have exactly one X")
      (is (ham/validate-hamiltonian hamiltonian) "Should be a valid Hamiltonian"))))

(deftest test-xy-mixer-hamiltonian  
  (testing "XY mixer Hamiltonian without periodic boundary"
    (let [hamiltonian (qaoa/xy-mixer-hamiltonian 3 false)]
      (is (= 4 (count hamiltonian)) "Should have 2 pairs * 2 terms (XX + YY) = 4 terms")
      (is (ham/validate-hamiltonian hamiltonian) "Should be a valid Hamiltonian")))

  (testing "XY mixer Hamiltonian with periodic boundary"
    (let [hamiltonian (qaoa/xy-mixer-hamiltonian 3 true)]
      (is (= 6 (count hamiltonian)) "Should have 3 pairs * 2 terms (XX + YY) = 6 terms")
      (is (ham/validate-hamiltonian hamiltonian) "Should be a valid Hamiltonian"))))

(deftest test-custom-ising-hamiltonian
  (testing "Custom Ising model Hamiltonian"
    (let [h-fields [0.5 -0.3 0.2]
          j-couplings [[0 1 0.1] [1 2 -0.2]]
          hamiltonian (qaoa/custom-ising-hamiltonian h-fields j-couplings)]
      (is (= 5 (count hamiltonian)) "Should have 3 field terms + 2 coupling terms")
      (is (ham/validate-hamiltonian hamiltonian) "Should be a valid Hamiltonian")
      ;; Check that field terms are single Z operators
      (let [field-terms (take 3 hamiltonian)]
        (is (every? #(= 1 (count (remove #{\I} (:pauli-string %)))) field-terms)
            "Field terms should have single Z operators")))))

;;
;; Circuit Construction Tests
;;
(deftest test-hamiltonian-evolution-circuit
  (testing "Single Pauli term evolution"
    (let [hamiltonian [{:coefficient 1.0 :pauli-string "XII"}]
          circuit (qaoa/hamiltonian-evolution-circuit hamiltonian 0.1 3)]
      (is (map? circuit) "Should return a circuit map")
      (is (= 3 (:num-qubits circuit)) "Should have correct number of qubits")
      (is (pos? (count (:operations circuit))) "Should have at least one operation")))

  (testing "ZZ interaction evolution"
    (let [hamiltonian [{:coefficient 1.0 :pauli-string "ZZI"}]
          circuit (qaoa/hamiltonian-evolution-circuit hamiltonian 0.1 3)]
      (is (map? circuit) "Should return a circuit map")
      (is (= 3 (:num-qubits circuit)) "Should have correct number of qubits")
      ;; ZZ interaction should create CNOT-RZ-CNOT pattern
      (is (>= (count (:operations circuit)) 3) "Should have at least 3 operations for ZZ"))))

(deftest test-qaoa-ansatz-circuit
  (testing "QAOA ansatz circuit construction"
    (let [problem-ham (qaoa/max-cut-hamiltonian triangle-graph 3)
          mixer-ham (qaoa/standard-mixer-hamiltonian 3)
          parameters [0.1 0.2]  ; γ=0.1, β=0.2 for 1 layer
          circuit (qaoa/qaoa-ansatz-circuit problem-ham mixer-ham parameters 3)]
      (is (map? circuit) "Should return a circuit map")
      (is (= 3 (:num-qubits circuit)) "Should have correct number of qubits")
      (is (pos? (count (:operations circuit))) "Should have operations")
      ;; Should start with Hadamard gates for equal superposition
      (let [first-ops (take 3 (:operations circuit))]
        (is (every? #(= (:operation-type %) :h) first-ops)
            "Should start with Hadamard gates on all qubits")))))

;;
;; Analysis and Utility Tests
;;
(deftest test-estimate-classical-optimum
  (testing "Classical optimum estimation for MaxCut"
    (let [optimum (qaoa/estimate-classical-optimum :max-cut triangle-graph 3)]
      (is (number? optimum) "Should return a number")
      (is (= 2.0 optimum) "Triangle graph should have classical optimum of 2.0")))

  (testing "Classical optimum estimation for larger graphs"
    (let [large-graph (for [i (range 10) j (range (inc i) 10)] [i j 1.0])
          optimum (qaoa/estimate-classical-optimum :max-cut large-graph 13)]
      (is (number? optimum) "Should return a number")
      (is (pos? optimum) "Should be positive"))))

(deftest test-qaoa-config-validation
  (testing "QAOA configuration validation"
    (let [valid-config {:problem-hamiltonian (qaoa/max-cut-hamiltonian triangle-graph 3)
                       :num-qubits 3
                       :num-layers 2}]
      (is (s/valid? 
           ::qaoa/qaoa-config 
           valid-config)
          "Valid config should pass validation"))))

;;
;; Integration Tests
;;
(deftest test-qaoa-objective-function
  (testing "QAOA objective function creation and evaluation"
    (let [problem-ham (qaoa/max-cut-hamiltonian line-graph 3) 
          mixer-ham (qaoa/standard-mixer-hamiltonian 3)
          objective-fn (qaoa/create-qaoa-objective problem-ham mixer-ham 3 (sim/create-simulator) {})]
      (is (fn? objective-fn) "Should return a function")
      ;; Test with simple parameters
      (let [energy (objective-fn [0.1 0.2])]
        (is (number? energy) "Should return a numeric energy")
        (is (not (Double/isNaN energy)) "Energy should not be NaN")))))

;; Run tests when file is loaded for quick feedback
(comment
  (run-tests))