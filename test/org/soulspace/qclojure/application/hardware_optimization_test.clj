(ns org.soulspace.qclojure.application.hardware-optimization-test
  "Tests for hardware optimization functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.application.hardware-optimization :as ho]
            [org.soulspace.qclojure.application.topology :as topo]))

;;;
;;; Tests for main optimization function
;;;
(deftest test-optimize
  (testing "Full optimization pipeline"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/t-gate 1)  ; Virtual gate
                      (qc/cnot-gate 0 2))  ; Non-adjacent
          supported-gates #{:h :x :z :rz :cnot}
          topology (topo/linear-coupling 3)
          result (ho/optimize circuit supported-gates topology {:optimize-topology? true})]
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (contains? result :pipeline-order))
      (is (contains? result :qubit-optimization-result))
      (is (contains? result :topology-optimization-result))
      (is (contains? result :gate-decomposition-result))
      (is (contains? result :all-gates-supported?))
      (is (contains? result :optimization-summary))))

  (testing "Optimization without topology"
    (let [circuit (-> (qc/create-circuit 2) (qc/t-gate 0))
          supported-gates #{:h :x :z :rz}
          result (ho/optimize circuit supported-gates)]
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (:all-gates-supported? result)))))

(comment
  (run-tests)
  ;
  )