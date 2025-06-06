(ns org.soulspace.qclojure.domain.circuit-test
  "Tests for quantum circuit operations and composition"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]))

;; Test circuit composition functionality
(deftest test-circuit-composition
  (testing "Basic circuit composition"
    (let [c1 (qc/h-gate (qc/create-circuit 1 "C1") 0)
          c2 (qc/x-gate (qc/create-circuit 1 "C2") 0)
          composed (qc/compose-circuits c1 c2)]
      (is (= (:num-qubits composed) 1))
      (is (= (count (:operations composed)) 2))))
  
  (testing "Composition with different qubit counts"
    (let [c1 (qc/h-gate (qc/create-circuit 1 "C1") 0)
          c2 (qc/cnot-gate (qc/create-circuit 2 "C2") 0 1)
          composed (qc/compose-circuits c1 c2)]
      (is (= (:num-qubits composed) 2))
      (is (= (count (:operations composed)) 2))))

  (testing "Composition with offset mapping"
    (let [c1 (qc/create-circuit 3 "C1")
          c2 (qc/h-gate (qc/create-circuit 1 "C2") 0)
          composed (qc/compose-circuits c1 c2 {:offset 1})]
      (is (= (:num-qubits composed) 3))
      ;; Check that the H gate now targets qubit 1 instead of 0
      (is (= (get-in (nth (:operations composed) 0) [:operation-params :target]) 1))))

  (testing "Composition with control-qubits-only option"
    (let [total-qubits 5
          control-qubits 2
          c1 (qc/create-circuit total-qubits "Main Circuit")
          c2 (qc/h-gate (qc/h-gate (qc/create-circuit control-qubits "QFT") 0) 1)
          composed (qc/compose-circuits c1 c2 {:control-qubits-only true})]
      (is (= (:num-qubits composed) total-qubits))
      ;; Check that the H gates still target the original qubit indices
      (is (= (get-in (nth (:operations composed) 0) [:operation-params :target]) 0))
      (is (= (get-in (nth (:operations composed) 1) [:operation-params :target]) 1)))))

;; Test circuit extension functionality
(deftest test-circuit-extension
  (testing "Basic circuit extension"
    (let [c (qc/h-gate (qc/create-circuit 1) 0)
          extended (qc/extend-circuit c 3)]
      (is (= (:num-qubits extended) 3))
      (is (= (get-in (first (:operations extended)) [:operation-params :target]) 0))))

  (testing "Circuit extension with qubit mapping"
    (let [c (qc/cnot-gate (qc/create-circuit 2) 0 1)
          extended (qc/extend-circuit c 4 :qubit-mapping #(+ % 2))]
      (is (= (:num-qubits extended) 4))
      (is (= (get-in (first (:operations extended)) [:operation-params :control]) 2))
      (is (= (get-in (first (:operations extended)) [:operation-params :target]) 3)))))

(comment
  (run-tests)

  ;
  )