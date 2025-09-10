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

(deftest test-edge-cases-and-comprehensive-scenarios
  (testing "Circuit that becomes empty after qubit optimization"
    ;; Create a circuit where operations exist but use no actual qubits
    ;; This is a theoretical edge case
    (let [empty-circuit (qc/create-circuit 1)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (qo/optimize-qubit-usage {:circuit empty-circuit :options {:optimize-qubits? true}})))))

  (testing "Successful optimization maintains circuit validity"
    (let [circuit (create-test-circuit-with-gaps)
          result (qo/optimize-qubit-usage {:circuit circuit :options {:optimize-qubits? true}})
          optimized-circuit (:circuit result)]
      (is (s/valid? ::qc/circuit optimized-circuit))
      (is (> (:num-qubits optimized-circuit) 0))
      (is (= (count (:operations optimized-circuit)) 
             (count (:operations circuit))))))

  (testing "Qubit mapping consistency"
    (let [circuit (create-test-circuit-with-gaps)
          result (qo/optimize-qubit-usage {:circuit circuit :options {:optimize-qubits? true}})
          mapping (:qubit-mapping result)]
      (is (= (count mapping) (:num-qubits (:circuit result))))
      (is (= (set (vals mapping)) (set (range (:num-qubits (:circuit result)))))))))

(comment
  ;; Run all tests in this namespace
  (run-tests)
  ;
  )
