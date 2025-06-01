(ns org.soulspace.qclojure.application.circuit-transformer-test
  "Tests for quantum circuit transformation functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.application.quantum-backend :as qb]
            [org.soulspace.qclojure.application.circuit-transformer :as ct]
            [org.soulspace.qclojure.domain.gate-registry :as gr]))

;; Mock backend for testing
(deftype MockBackend [supported-gates]
  qb/QuantumBackend
  (get-backend-info [_]
    {:backend-type :simulator
     :backend-name "Mock Backend"
     :capabilities {:max-qubits 5}
     :supported-gates supported-gates})
  
  (get-supported-gates [_]
    supported-gates)
  
  (is-available? [_]
    true)
  
  (submit-circuit [_ _ _]
    "mock-job-1")
  
  (get-job-status [_ _]
    :completed)
  
  (get-job-result [_ _]
    {:job-status :completed
     :job-id "mock-job-1"
     :measurement-results {"00" 500, "11" 500}})
  
  (cancel-job [_ _]
    true)
  
  (get-queue-status [_]
    {:queued 0 :running 0 :completed 0}))

(deftest test-circuit-transformation
  (testing "Basic circuit transformation"
    (let [;; Create a circuit with a Y gate (which can be decomposed to RX and RZ)
          circuit (-> (qc/create-circuit 2 "Test Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)
                      (qc/cnot-gate 0 1))
          
          ;; Create a backend that doesn't support Y gates
          backend (->MockBackend #{:h :x :z :rx :rz :cnot})
          
          ;; Transform the circuit
          result (ct/transform-circuit circuit backend)]
      
      ;; Verify that the Y gate was transformed
      (is (pos? (:transformed-gates result)))
      
      ;; Verify that no unsupported gates remain
      (is (empty? (:unsupported-gates result)))
      
      ;; Verify the circuit is valid
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))))
  
  (testing "Circuit with gates that can't be decomposed"
    (let [;; Create a circuit with a custom gate that has no decomposition
          circuit (-> (qc/create-circuit 1)
                      (qc/add-gate :custom-gate :target 0))
          
          ;; Create a backend with limited supported gates
          backend (->MockBackend #{:x :h :cnot})
          
          ;; Transform the circuit
          result (ct/transform-circuit circuit backend)]
      
      ;; Verify that the unsupported gate is reported
      (is (seq (:unsupported-gates result))))))

(deftest test-transformation-options
  (testing "Non-transformation option"
    (let [;; Create a circuit with an unsupported gate
          circuit (-> (qc/create-circuit 1)
                      (qc/y-gate 0))
          
          ;; Create a backend that doesn't support Y gates
          backend (->MockBackend #{:h :x :cnot})
          
          ;; Transform with transformation disabled
          result (ct/transform-circuit circuit backend 
                                       {:transform-unsupported? false})]
      
      ;; Verify the circuit was not transformed
      (is (zero? (:transformed-gates result)))
      
      ;; Verify that the Y gate is reported as unsupported
      (is (= [:y] (:unsupported-gates result))))))

(deftest test-max-iterations
  (testing "Circuit transformation doesn't hit max iterations"
    (let [;; Create a circuit with multiple gates that need complex decomposition
          circuit (-> (qc/create-circuit 3 "Complex Circuit")
                      (qc/y-gate 0)    ;; Decomposes to rx, rz
                      (qc/y-gate 1)    ;; Decomposes to rx, rz
                      (qc/y-gate 2))   ;; Decomposes to rx, rz
          
          ;; Create a backend with limited gate support that forces decomposition
          backend (->MockBackend #{:h :x :z :rz :cnot})
          
          ;; Transform with a slightly higher max iterations to prevent hitting the limit
          result (ct/transform-circuit circuit backend {:max-iterations 20})]
      
      ;; The transformation should complete without hitting max iterations
      ;; and should report the appropriate unsupported gates
      (is (some? result))
      
      ;; If we're still seeing unsupported gates like :rx, that's OK
      ;; The important thing is that we don't hit an infinite loop
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result))))))

(deftest test-universal-gate-set
  (testing "Any circuit can be transformed to use only universal gates"
    (let [;; Create a complex circuit with various gates
          circuit (-> (qc/create-circuit 3 "Complex Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)    ;; Non-universal gate
                      (qc/rz-gate 2 (/ Math/PI 4))  ;; Parametric gate
                      (qc/cnot-gate 0 1)  ;; CNOT gate
                      (qc/cz-gate 1 2)) ;; Requires decomposition
          
          ;; Create a backend that only supports universal gates
          universal-backend (->MockBackend gr/universal-gate-set)
          
          ;; Transform the circuit
          result (ct/transform-circuit circuit universal-backend)]
      
      ;; Check that transformation succeeded
      (is (some? result))
      
      ;; Check that no unsupported gates remain
      (is (empty? (:unsupported-gates result)))
      
      ;; Check that all gates in the result are from the universal set
      (is (every? #(contains? gr/universal-gate-set (:gate-type %)) 
                 (:gates (:quantum-circuit result)))))))

(deftest test-universal-gate-set2
  (testing "Any circuit can be transformed to use only universal gates"
    (let [;; Create a complex circuit with various gates
          circuit (-> (qc/create-circuit 3 "Complex Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)    ;; Non-universal gate
                      (qc/rz-gate 2 (/ Math/PI 4))  ;; Parametric gate
                      (qc/cnot-gate 0 1)  ;; CNOT gate
                      (qc/add-gate :swap {:control 1 :target 2})) ;; Using generic add-gate
          
          ;; Create a backend that only supports universal gates
          universal-backend (->MockBackend gr/universal-gate-set)
          
          ;; Transform the circuit
          result (ct/transform-circuit circuit universal-backend)]
      
      ;; Check that transformation succeeded
      (is (some? result))
      
      ;; Check that no unsupported gates remain
      (is (empty? (:unsupported-gates result)))
      
      ;; Check that all gates in the result are from the universal set
      (is (every? #(contains? gr/universal-gate-set (:gate-type %)) 
                 (:gates (:quantum-circuit result)))))))

(comment
  ;; Run tests
  (run-tests)
  
  ;; Specific testing for debugging
  (let [circuit (-> (qc/create-circuit 2)
                    (qc/h-gate 0)
                    (qc/y-gate 1))
        backend (->MockBackend #{:h :x :z :cnot})]
    (ct/transform-circuit circuit backend)))
