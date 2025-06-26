(ns org.soulspace.qclojure.domain.qubit-optimization-test
  (:require [clojure.test :refer :all]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.spec.alpha :as s]))

;;
;; Helper functions for testing
;;
(defn create-test-circuit-1
  "Create a simple 2-qubit Bell state circuit."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

(defn create-test-circuit-with-gaps
  "Create a circuit that uses qubits with gaps (0, 2, 5)."
  []
  (-> (qc/create-circuit 6 "Sparse Circuit")
      (qc/h-gate 0)
      (qc/x-gate 2)
      (qc/cnot-gate 0 5)))

;;
;; Tests for qubit usage analysis and optimization
;;
(deftest test-analyze-qubit-usage
  (testing "Full qubit usage"
    (let [circuit (create-test-circuit-1)
          analysis (qo/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{0 1}))
      (is (= (:total-qubits analysis) 2))
      (is (= (:unused-qubits analysis) #{}))
      (is (= (:max-qubit-id analysis) 1))
      (is (= (:qubit-usage-efficiency analysis) 1.0))))

  (testing "Sparse qubit usage"
    (let [circuit (create-test-circuit-with-gaps)
          analysis (qo/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{0 2 5}))
      (is (= (:total-qubits analysis) 6))
      (is (= (:unused-qubits analysis) #{1 3 4}))
      (is (= (:max-qubit-id analysis) 5))
      (is (= (:qubit-usage-efficiency analysis) 0.5))))

  (testing "Empty circuit"
    (let [circuit (qc/create-circuit 3)
          analysis (qo/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{}))
      (is (= (:total-qubits analysis) 3))
      (is (= (:unused-qubits analysis) #{0 1 2}))
      (is (= (:max-qubit-id analysis) -1))
      (is (= (:qubit-usage-efficiency analysis) 0.0)))))

(deftest test-optimize-qubit-usage
  (testing "Optimization of sparse circuit"
    (let [circuit (create-test-circuit-with-gaps)
          result (qo/optimize-qubit-usage circuit)]
      (is (= (:optimized-qubits result) 3))
      (is (= (:qubits-saved result) 3))
      (is (= (:original-qubits result) 6))
      (is (= (:qubit-mapping result) {0 0, 2 1, 5 2}))))

  (testing "Optimization of already efficient circuit"
    (let [circuit (create-test-circuit-1)
          result (qo/optimize-qubit-usage circuit)]
      (is (= (:optimized-qubits result) 2))
      (is (= (:qubits-saved result) 0))
      (is (= (:qubit-mapping result) {0 0, 1 1}))))

  (testing "Optimized circuit operations are remapped correctly"
    (let [circuit (create-test-circuit-with-gaps)
          result (qo/optimize-qubit-usage circuit)
          optimized-circuit (:quantum-circuit result)]
      ; Original H gate on qubit 0 should stay on qubit 0
      (is (= (get-in optimized-circuit [:operations 0 :operation-params :target]) 0))
      ; Original X gate on qubit 2 should be on qubit 1
      (is (= (get-in optimized-circuit [:operations 1 :operation-params :target]) 1))
      ; Original CNOT from 0 to 5 should be from 0 to 2
      (let [cnot-op (nth (:operations optimized-circuit) 2)]
        (is (= (get-in cnot-op [:operation-params :control]) 0))
        (is (= (get-in cnot-op [:operation-params :target]) 2))))))

(comment
  (run-tests)
  ;
  )
