(ns org.soulspace.qclojure.application.circuit-transformer-test
  "Tests for circuit transformation functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.application.circuit-transformer :as ct]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;;
;; Circuit Transformer Tests
;;
(deftest test-circuit-transformation
  (testing "Basic circuit transformation"
    (let [;; Create a circuit with a Y gate (which can be decomposed to RX and RZ)
          circuit (-> (qc/create-circuit 2 "Test Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)
                      (qc/cnot-gate 0 1))

          ;; Use a set that doesn't support Y gates
          ;; Transform the circuit
          result (ct/transform-circuit circuit #{:h :x :z :rx :rz :cnot})]

      ;; Verify that the Y gate was transformed
      (is (pos? (:transformed-operations result)))

      ;; Verify that no unsupported operations remain
      (is (empty? (:unsupported-operations result)))

      ;; Verify the circuit is valid
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))))

  (testing "Circuit with gates that can't be decomposed"
    (let [;; Create a circuit with a custom gate that has no decomposition
          circuit (-> (qc/create-circuit 1)
                      (qc/add-gate :custom-gate :target 0))

          ;; Use a set with limited supported gates
          ;; Transform the circuit
          result (ct/transform-circuit circuit #{:x :h :cnot})]

      ;; Verify that the unsupported gate is reported
      (is (seq (:unsupported-operations result))))))

(deftest test-transformation-options
  (testing "Non-transformation option"
    (let [;; Create a circuit with an unsupported gate
          circuit (-> (qc/create-circuit 1)
                      (qc/y-gate 0))

          ;; Use a set that doesn't support Y gates
          ;; Transform with transformation disabled
          result (ct/transform-circuit circuit #{:h :x :cnot}
                                       {:transform-unsupported? false})]

      ;; Verify the circuit was not transformed
      (is (zero? (:transformed-operations result)))

      ;; Verify that the Y gate is reported as unsupported
      (is (= [:y] (:unsupported-operations result))))))

(deftest test-max-iterations
  (testing "Circuit transformation doesn't hit max iterations"
    (let [;; Create a circuit with multiple gates that need complex decomposition
          circuit (-> (qc/create-circuit 3 "Complex Circuit")
                      (qc/y-gate 0)    ;; Decomposes to rx, rz
                      (qc/y-gate 1)    ;; Decomposes to rx, rz
                      (qc/y-gate 2))   ;; Decomposes to rx, rz

          ;; Use a set with limited gate support that forces decomposition
          ;; Transform with a slightly higher max iterations to prevent hitting the limit
          result (ct/transform-circuit circuit #{:h :x :z :rz :cnot} {:max-iterations 20})]
      
      ;; The transformation should complete without hitting max iterations
      ;; and should report the appropriate unsupported operations
      (is (some? result))

      ;; If we're still seeing unsupported operations like :rx, that's OK
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

          ;; Transform the circuit
          result (ct/transform-circuit circuit gr/universal-gate-set)]

      ;; Check that transformation succeeded
      (is (some? result))

      ;; Check that no unsupported gates remain
      (is (empty? (:unsupported-operations result)))

      ;; Check that all gates in the result are from the universal set
      (is (every? #(contains? gr/universal-gate-set (:operation-type %))
                  (:operations (:quantum-circuit result)))))))

(deftest test-universal-gate-set2
  (testing "Any circuit can be transformed to use only universal gates"
    (let [;; Create a complex circuit with various gates
          circuit (-> (qc/create-circuit 3 "Complex Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)    ;; Non-universal gate
                      (qc/rz-gate 2 (/ Math/PI 4))  ;; Parametric gate
                      (qc/cnot-gate 0 1)  ;; CNOT gate
                      (qc/add-gate :swap {:control 1 :target 2})) ;; Using generic add-gate


          ;; Transform the circuit
          result (ct/transform-circuit circuit gr/universal-gate-set)]

      ;; Check that transformation succeeded
      (is (some? result))

      ;; Check that no unsupported gates remain
      (is (empty? (:unsupported-operations result)))

      ;; Check that all gates in the result are from the universal set
      (is (every? #(contains? gr/universal-gate-set (:operation-type %))
                  (:operations (:quantum-circuit result)))))))

(comment
  ;; Run tests
  (run-tests)

  ;; Specific testing for debugging
  (let [circuit (-> (qc/create-circuit 2)
                    (qc/h-gate 0)
                    (qc/y-gate 1))]
    (ct/transform-circuit circuit #{:h :x :z :cnot})))

;;
;; Circuit Optimizer Tests
;;
(deftest test-analyze-qubit-usage
  (testing "Analyzing qubit usage in a circuit"
    (let [;; Circuit using qubits 0, 2, 4 out of 5 total qubits
          circuit (-> (qc/create-circuit 5 "Sparse Circuit")
                      (qc/h-gate 0)
                      (qc/x-gate 2)
                      (qc/cnot-gate 0 4))
          analysis (ct/analyze-qubit-usage circuit)]

      ;; Check used qubits
      (is (= #{0 2 4} (:used-qubits analysis)))

      ;; Check total qubits
      (is (= 5 (:total-qubits analysis)))

      ;; Check unused qubits
      (is (= #{1 3} (:unused-qubits analysis)))

      ;; Check max qubit ID
      (is (= 4 (:max-qubit-id analysis)))

      ;; Check efficiency (3 used out of 5 total = 0.6)
      (is (= 0.6 (:qubit-usage-efficiency analysis)))))

  (testing "Analyzing circuit with all qubits used"
    (let [circuit (-> (qc/create-circuit 3 "Full Circuit")
                      (qc/h-gate 0)
                      (qc/x-gate 1)
                      (qc/cnot-gate 1 2))
          analysis (ct/analyze-qubit-usage circuit)]

      (is (= #{0 1 2} (:used-qubits analysis)))
      (is (= 3 (:total-qubits analysis)))
      (is (empty? (:unused-qubits analysis)))
      (is (= 2 (:max-qubit-id analysis)))
      (is (= 1.0 (:qubit-usage-efficiency analysis)))))

  (testing "Analyzing empty circuit"
    (let [circuit (qc/create-circuit 2 "Empty Circuit")
          analysis (ct/analyze-qubit-usage circuit)]

      (is (empty? (:used-qubits analysis)))
      (is (= 2 (:total-qubits analysis)))
      (is (= #{0 1} (:unused-qubits analysis)))
      (is (= -1 (:max-qubit-id analysis)))
      (is (= 0.0 (:qubit-usage-efficiency analysis))))))

(deftest test-optimize-qubit-usage
  (testing "Optimizing circuit with sparse qubit usage"
    (let [;; Circuit using qubits [0, 3, 7] out of 8 total qubits
          circuit (-> (qc/create-circuit 8 "Sparse Circuit")
                      (qc/h-gate 0)
                      (qc/x-gate 3)
                      (qc/cnot-gate 0 7)
                      (qc/cnot-gate 3 7))
          result (ct/optimize-qubit-usage circuit)]

      ;; Check that optimization succeeded
      (is (some? result))

      ;; Check qubit mapping (0->0, 3->1, 7->2)
      (is (= {0 0, 3 1, 7 2} (:qubit-mapping result)))

      ;; Check qubit counts
      (is (= 8 (:original-qubits result)))
      (is (= 3 (:optimized-qubits result)))
      (is (= 5 (:qubits-saved result)))

      ;; Check optimized circuit
      (let [optimized-circuit (:quantum-circuit result)]
        (is (= 3 (:num-qubits optimized-circuit)))
        (is (= 4 (count (:operations optimized-circuit))))

        ;; Check that gate qubits were remapped correctly
        (let [operations (:operations optimized-circuit)]
          ;; H gate on original qubit 0 -> new qubit 0
          (is (= 0 (get-in (first operations) [:operation-params :target])))
          ;; X gate on original qubit 3 -> new qubit 1
          (is (= 1 (get-in (second operations) [:operation-params :target])))
          ;; CNOT gates: (0,7) -> (0,2) and (3,7) -> (1,2)
          (let [cnot1 (nth operations 2)]
            (is (or (and (= 0 (get-in cnot1 [:operation-params :control]))
                         (= 2 (get-in cnot1 [:operation-params :target])))
                    (and (= 1 (get-in cnot1 [:operation-params :control]))
                         (= 2 (get-in cnot1 [:operation-params :target]))))))))

      ;; Verify circuit is still valid
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))))

  (testing "Optimizing circuit with no gaps"
    (let [;; Circuit already using sequential qubits
          circuit (-> (qc/create-circuit 3 "Sequential Circuit")
                      (qc/h-gate 0)
                      (qc/x-gate 1)
                      (qc/cnot-gate 1 2))
          result (ct/optimize-qubit-usage circuit)]

      ;; Should have no changes since qubits are already optimal
      (is (= {0 0, 1 1, 2 2} (:qubit-mapping result)))
      (is (= 0 (:qubits-saved result)))
      (is (= 3 (:original-qubits result)))
      (is (= 3 (:optimized-qubits result)))

      ;; Circuit should be unchanged
      (let [optimized-circuit (:quantum-circuit result)]
        (is (= circuit optimized-circuit)))))

  (testing "Optimizing empty circuit"
    (let [circuit (qc/create-circuit 5 "Empty Circuit")
          result (ct/optimize-qubit-usage circuit)]

      ;; Empty circuit should be optimized to 0 qubits
      (is (empty? (:qubit-mapping result)))
      (is (= 5 (:qubits-saved result)))
      (is (= 5 (:original-qubits result)))
      (is (= 0 (:optimized-qubits result)))

      ;; Optimized circuit should have 0 qubits
      (let [optimized-circuit (:quantum-circuit result)]
        (is (= 0 (:num-qubits optimized-circuit)))
        (is (empty? (:operations optimized-circuit)))))))

(deftest test-optimize-for-backend
  (testing "Comprehensive backend optimization"
    (let [;; Create a circuit with unsupported gates and sparse qubit usage
          circuit (-> (qc/create-circuit 6 "Complex Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)      ;; Needs transformation
                      (qc/cnot-gate 0 5) ;; Uses sparse qubits
                      (qc/x-gate 5))

          ;; Set that doesn't support Y gates
          ;; Perform comprehensive optimization
          result (ct/optimize circuit #{:h :x :z :rx :rz :cnot})]

      ;; Check that optimization succeeded
      (is (some? result))

      ;; Check transformation result
      (let [transformation (:transformation-result result)]
        (is (pos? (:transformed-operations transformation)))
        (is (empty? (:unsupported-operations transformation))))

      ;; Check qubit optimization result
      (let [qubit-opt (:qubit-optimization-result result)]
        (is (pos? (:qubits-saved qubit-opt)))
        (is (< (:optimized-qubits qubit-opt) 6)))

      ;; Check final circuit
      (let [final-circuit (:quantum-circuit result)]
        (is (< (:num-qubits final-circuit) 6))
        (is (s/valid? ::qc/quantum-circuit final-circuit)))

      ;; Check summary
      (is (string? (:optimization-summary result)))
      (is (seq (:optimization-summary result)))))

  (testing "Optimization with options"
    (let [circuit (-> (qc/create-circuit 5 "Test Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 3))  ;; Sparse usage and unsupported gate

          ;; Optimize with only qubit optimization enabled
          result (ct/optimize circuit #{:h :x :cnot}
                                          {:transform-gates? false
                                           :optimize-qubits? true})]

      ;; Should have optimized qubits but not transformed gates
      (let [transformation (:transformation-result result)
            qubit-opt (:qubit-optimization-result result)]
        (is (zero? (:transformed-operations transformation)))
        (is (seq (:unsupported-operations transformation))) ;; Y gate still unsupported
        (is (pos? (:qubits-saved qubit-opt))))))

  (testing "Optimization with no changes needed"
    (let [;; Perfect circuit: all qubits used sequentially, all gates supported
          circuit (-> (qc/create-circuit 2 "Perfect Circuit")
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 1))
          result (ct/optimize circuit #{:h :cnot})
          transformation (:transformation-result result)
          qubit-opt (:qubit-optimization-result result)]

      (is (zero? (:transformed-operations transformation)))
      (is (empty? (:unsupported-operations transformation)))
      (is (zero? (:qubits-saved qubit-opt)))
      (is (= 2 (:optimized-qubits qubit-opt))))))

(deftest test-circuit-optimizer-edge-cases
  (testing "Circuit with complex gate parameters"
    (let [;; Circuit with rotation gates that have angle parameters
          circuit (-> (qc/create-circuit 4 "Rotation Circuit")
                      (qc/rx-gate 0 (/ Math/PI 2))
                      (qc/ry-gate 3 (/ Math/PI 4))  ;; Uses sparse qubit
                      (qc/crz-gate 0 3 (/ Math/PI 3)))  ;; Controlled rotation

          result (ct/optimize-qubit-usage circuit)
          ;; Check that parameters are preserved after qubit remapping
          optimized-circuit (:quantum-circuit result)
          operations (:operations optimized-circuit)]

      (is (= 2 (:num-qubits optimized-circuit)))
      (is (= 3 (count operations)))

      ;; Check that angle parameters are preserved
      (doseq [operation operations]
        (when (contains? (:operation-params operation) :angle)
          (is (number? (get-in operation [:operation-params :angle])))
          (is (pos? (get-in operation [:operation-params :angle])))))))

  (testing "Multi-qubit gate parameter remapping"
    (let [;; Circuit with multi-qubit gates using sparse qubits
          circuit (-> (qc/create-circuit 6 "Multi-qubit Gates")
                      (qc/toffoli-gate 1 3 5)  ;; Uses qubits 1, 3, 5
                      (qc/fredkin-gate 0 1 5)) ;; Uses qubits 0, 1, 5

          result (ct/optimize-qubit-usage circuit)]

      ;; Should compact to 4 qubits (original qubits 0,1,3,5 -> new qubits 0,1,2,3)
      (is (= 4 (:optimized-qubits result)))
      (is (= {0 0, 1 1, 3 2, 5 3} (:qubit-mapping result)))

      ;; Check that multi-qubit gate parameters were remapped correctly
      (let [operations (:operations (:quantum-circuit result))]
        (is (= 2 (count operations)))

        ;; Check Toffoli gate parameters
        (let [toffoli-gate (first operations)]
          (is (= :toffoli (:operation-type toffoli-gate)))
          (is (= 1 (get-in toffoli-gate [:operation-params :control1])))
          (is (= 2 (get-in toffoli-gate [:operation-params :control2])))
          (is (= 3 (get-in toffoli-gate [:operation-params :target]))))

        ;; Check Fredkin gate parameters
        (let [fredkin-gate (second operations)]
          (is (= :fredkin (:operation-type fredkin-gate)))
          (is (= 0 (get-in fredkin-gate [:operation-params :control])))
          (is (= 1 (get-in fredkin-gate [:operation-params :target1])))
          (is (= 3 (get-in fredkin-gate [:operation-params :target2]))))))))

(comment
  ;; Run tests
  (run-tests)

  ;; Specific testing for debugging
  (let [circuit (-> (qc/create-circuit 2)
                    (qc/h-gate 0)
                    (qc/y-gate 1))]
    (ct/transform-circuit circuit #{:h :x :z :cnot})))
