(ns org.soulspace.qclojure.domain.circuit-composition-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.spec.alpha :as s]))

;;;
;;; Helper functions for testing
;;;
(defn create-test-circuit-1
  "Create a simple 2-qubit Bell state circuit."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

;;
;; Tests for operation parameter updates
;;
(deftest test-update-operation-params
  (testing "Single-qubit operation parameter update"
    (let [h-op {:operation-type :h :operation-params {:target 0}}
          updated (cc/update-operation-params h-op #(+ % 2))]
      (is (= (:operation-type updated) :h))
      (is (= (get-in updated [:operation-params :target]) 2))))

  (testing "Two-qubit operation parameter update"
    (let [cnot-op {:operation-type :cnot :operation-params {:control 0 :target 1}}
          updated (cc/update-operation-params cnot-op #(+ % 3))]
      (is (= (get-in updated [:operation-params :control]) 3))
      (is (= (get-in updated [:operation-params :target]) 4))))

  (testing "Operation with non-qubit parameters preserved"
    (let [rx-op {:operation-type :rx :operation-params {:target 1 :angle Math/PI}}
          updated (cc/update-operation-params rx-op #(+ % 5))]
      (is (= (get-in updated [:operation-params :target]) 6))
      (is (= (get-in updated [:operation-params :angle]) Math/PI))))

  (testing "Measurement operation with qubit vector"
    (let [measure-op {:operation-type :measure :operation-params {:measurement-qubits [0 1 2]}}
          updated (cc/update-operation-params measure-op #(+ % 10))]
      (is (= (get-in updated [:operation-params :measurement-qubits]) [10 11 12]))))

  (testing "Operation without parameters"
    (let [op {:operation-type :barrier}
          updated (cc/update-operation-params op #(+ % 1))]
      (is (= updated op))))

  (testing "Complex multi-control operation"
    (let [toffoli-op {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
          updated (cc/update-operation-params toffoli-op #(* % 2))]
      (is (= (get-in updated [:operation-params :control1]) 0))
      (is (= (get-in updated [:operation-params :control2]) 2))
      (is (= (get-in updated [:operation-params :target]) 4)))))

;;;
;;; Tests for circuit composition and extension
;;;
(deftest test-extend-circuit
  (testing "Basic circuit extension"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 4)]
      (is (= (:num-qubits extended) 4))
      (is (= (:num-qubits original) 2))
      (is (= (count (:operations extended)) (count (:operations original))))
      (is (s/valid? ::qc/circuit extended))))

  (testing "Extension with qubit mapping"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 4 :qubit-mapping #(+ % 2))]
      (is (= (:num-qubits extended) 4))
      ;; H gate should now be on qubit 2 instead of 0
      (let [h-operation (first (:operations extended))]
        (is (= (get-in h-operation [:operation-params :target]) 2)))
      ;; CNOT should now be from qubit 2 to 3 instead of 0 to 1
      (let [cnot-operation (second (:operations extended))]
        (is (= (get-in cnot-operation [:operation-params :control]) 2))
        (is (= (get-in cnot-operation [:operation-params :target]) 3)))))

  (testing "Extension with same size updates name but keeps operations"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 2)]
      (is (= (:num-qubits original) (:num-qubits extended)))
      (is (= (:operations original) (:operations extended)))
      (is (not= (:name original) (:name extended)))))

  (testing "Extension with identity mapping"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 3 :qubit-mapping identity)]
      (is (= (:operations original) (:operations extended))))))

(deftest test-compose-circuits
  (testing "Basic circuit composition"
    (let [circuit1 (-> (qc/create-circuit 2) (qc/h-gate 0))
          circuit2 (-> (qc/create-circuit 2) (qc/x-gate 1))
          composed (cc/compose-circuits circuit1 circuit2)]
      (is (= (:num-qubits composed) 2))
      (is (= (count (:operations composed)) 2))
      (is (= (get-in composed [:operations 0 :operation-type]) :h))
      (is (= (get-in composed [:operations 1 :operation-type]) :x))))

  (testing "Composition with different qubit counts"
    (let [circuit1 (-> (qc/create-circuit 1) (qc/h-gate 0))
          circuit2 (-> (qc/create-circuit 3) (qc/cnot-gate 0 2))
          composed (cc/compose-circuits circuit1 circuit2)]
      (is (= (:num-qubits composed) 3))
      (is (= (count (:operations composed)) 2))))

  (testing "Composition with offset"
    (let [circuit1 (qc/create-circuit 5)
          circuit2 (-> (qc/create-circuit 2) (qc/h-gate 0) (qc/cnot-gate 0 1))
          composed (cc/compose-circuits circuit1 circuit2 {:offset 3})]
      (is (= (:num-qubits composed) 5))
      ;; H gate should be on qubit 3
      (let [h-op (first (:operations composed))]
        (is (= (get-in h-op [:operation-params :target]) 3)))
      ;; CNOT should be from qubit 3 to 4
      (let [cnot-op (second (:operations composed))]
        (is (= (get-in cnot-op [:operation-params :control]) 3))
        (is (= (get-in cnot-op [:operation-params :target]) 4))))))

(comment
  (run-tests)
  ;
  )
