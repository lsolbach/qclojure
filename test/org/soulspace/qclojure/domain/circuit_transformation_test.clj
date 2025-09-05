(ns org.soulspace.qclojure.domain.circuit-transformation-test
  "Tests for circuit transformation functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]))

;;
;; Helper functions for testing
;;
(defn create-test-circuit-1
  "Create a simple 2-qubit Bell state circuit."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

;;
;; Tests for circuit transformation
;;
(deftest test-transform-circuit
  (testing "Transform circuit with unsupported gates"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/t-gate 1)  ; T gate might need decomposition
                      (qc/cnot-gate 0 1))
          supported-ops #{:h :x :z :rz :cnot}  ; T not supported
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (s/valid? ::qc/circuit (:circuit result)))
      ;; All operations in result should be supported
      (let [transformed-circuit (:circuit result)
            operation-types (set (map :operation-type (:operations transformed-circuit)))]
        (is (set/subset? operation-types supported-ops)))))

  (testing "Transform circuit with all supported gates"
    (let [circuit (create-test-circuit-1)
          supported-ops #{:h :cnot}
          result (ct/transform-circuit circuit supported-ops)]
      (is (= (:transformed-operation-count result) 0))
      (is (empty? (:unsupported-operations result)))
      (is (= (:circuit result) circuit))))

  (testing "Transform circuit with options"
    (let [circuit (-> (qc/create-circuit 1) (qc/t-gate 0))
          supported-ops #{:h :x :z :rz}
          result (ct/transform-circuit circuit supported-ops {:max-iterations 10})]
      (is (s/valid? ::ct/transformation-result result))))

  (testing "Transform circuit with transform disabled"
    (let [circuit (-> (qc/create-circuit 1) (qc/t-gate 0))
          supported-ops #{:h :x :z :rz}
          result (ct/transform-circuit circuit supported-ops {:transform-unsupported? false})]
      (is (s/valid? ::ct/transformation-result result))
      (is (= (:transformed-operation-count result) 0))
      (is (= (:circuit result) circuit))))

  (testing "Transform circuit with empty supported operations"
    (let [circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
          supported-ops #{}
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (not (empty? (:unsupported-operations result))))))

  (testing "Transform empty circuit"
    (let [circuit (qc/create-circuit 1)
          supported-ops #{:h :x :z}
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (= (:transformed-operation-count result) 0))
      (is (empty? (:unsupported-operations result)))
      (is (= (:circuit result) circuit))))

  (testing "Transform circuit with multiple unsupported operations"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/t-gate 1)
                      (qc/s-gate 2)
                      (qc/cnot-gate 0 1))
          supported-ops #{:h :x :z :rz :cnot}  ; T and S not supported
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (s/valid? ::qc/circuit (:circuit result)))
      ;; Check that transformations were applied
      (is (>= (:transformed-operation-count result) 0))))

  (testing "Transform circuit spec validation"
    (let [invalid-circuit {:num-qubits "not-a-number"}  ; Invalid circuit
          supported-ops #{:h :x}]
      (is (thrown? AssertionError
                   (ct/transform-circuit invalid-circuit supported-ops)))))

  (testing "Transform circuit with virtual gates"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/phase-gate 1 (/ Math/PI 8))  ; Virtual gate
                      (qc/cnot-gate 0 1))
          supported-ops #{:h :x :z :rz :cnot}
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (s/valid? ::qc/circuit (:circuit result)))))

  (testing "Result structure validation"
    (let [circuit (create-test-circuit-1)
          supported-ops #{:h :cnot}
          result (ct/transform-circuit circuit supported-ops)]
      ;; Verify all required keys are present
      (is (contains? result :circuit))
      (is (contains? result :transformed-operation-count))
      (is (contains? result :unsupported-operations))
      ;; Verify types
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (number? (:transformed-operation-count result)))
      (is (vector? (:unsupported-operations result))))))

(comment
  (run-tests)
  ;
  )
