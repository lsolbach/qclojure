(ns org.soulspace.qclojure.application.hardware-optimization-test
  "Tests for hardware optimization functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.application.hardware-optimization :as ho]
            [org.soulspace.qclojure.domain.topology :as topo]))

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
          coupling (topo/linear-coupling 3)
          result (ho/optimize {:circuit circuit 
                               :device {:supported-operations supported-gates
                                        :coupling coupling}
                               :options {:optimize-topology? true}})]
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (contains? result :pipeline-order))
      (is (contains? result :all-gates-supported?))
      (is (contains? result :final-unsupported-gates))
      (is (contains? result :logical-to-physical))
      (is (contains? result :physical-to-logical))))

  (testing "Optimization without topology"
    (let [circuit (-> (qc/create-circuit 2) (qc/t-gate 0))
          supported-gates #{:h :x :z :rz}
          result (ho/optimize {:circuit circuit
                               :device {:supported-operations supported-gates}})]
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (contains? result :all-gates-supported?)))))

(comment
  (run-tests)
  ;
  )