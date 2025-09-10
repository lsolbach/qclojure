(ns org.soulspace.qclojure.domain.gate-optimization-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.gate-optimization :as opt]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;; Test data
(def hadamard-op {:operation-type :h :operation-params {:target 0}})
(def x-op {:operation-type :x :operation-params {:target 0}})
(def y-op {:operation-type :y :operation-params {:target 1}})
(def z-op {:operation-type :z :operation-params {:target 2}})
(def cnot-op {:operation-type :cnot :operation-params {:control 0 :target 1}})
(def rx-op {:operation-type :rx :operation-params {:target 0 :angle 0.5}})

(deftest test-gate-qubits
  (testing "Single qubit gates"
    (is (= #{0} (opt/gate-qubits hadamard-op)))
    (is (= #{0} (opt/gate-qubits x-op)))
    (is (= #{1} (opt/gate-qubits y-op)))
    (is (= #{2} (opt/gate-qubits z-op))))

  (testing "Two qubit gates"
    (is (= #{0 1} (opt/gate-qubits cnot-op))))

  (testing "Parameterized gates"
    (is (= #{0} (opt/gate-qubits rx-op)))))

(deftest test-gates-equivalent?
  (testing "Self-inverse gates should be equivalent when identical"
    (is (opt/gates-equivalent? hadamard-op hadamard-op))
    (is (opt/gates-equivalent? x-op x-op))
    (is (opt/gates-equivalent? y-op y-op))
    (is (opt/gates-equivalent? z-op z-op))
    (is (opt/gates-equivalent? cnot-op cnot-op)))

  (testing "Different gate types should not be equivalent"
    (is (not (opt/gates-equivalent? hadamard-op x-op)))
    (is (not (opt/gates-equivalent? x-op y-op)))
    (is (not (opt/gates-equivalent? y-op z-op))))

  (testing "Same gate type on different qubits should not be equivalent"
    (is (not (opt/gates-equivalent? hadamard-op {:operation-type :h :operation-params {:target 1}})))
    (is (not (opt/gates-equivalent? x-op {:operation-type :x :operation-params {:target 1}}))))

  (testing "Non-self-inverse gates should not be equivalent"
    (is (not (opt/gates-equivalent? rx-op rx-op)))))

(deftest test-find-cancellation-pairs
  (testing "Empty operations list"
    (is (= [] (opt/find-cancellation-pairs []))))

  (testing "Single operation"
    (is (= [] (opt/find-cancellation-pairs [hadamard-op]))))

  (testing "Two consecutive identical self-inverse gates"
    (is (= [[0 1]] (opt/find-cancellation-pairs [hadamard-op hadamard-op])))
    (is (= [[0 1]] (opt/find-cancellation-pairs [x-op x-op])))
    (is (= [[0 1]] (opt/find-cancellation-pairs [cnot-op cnot-op]))))

  (testing "Non-canceling consecutive gates"
    (is (= [] (opt/find-cancellation-pairs [hadamard-op x-op])))
    (is (= [] (opt/find-cancellation-pairs [x-op y-op]))))

  (testing "Multiple cancellation pairs"
    (is (= [[0 1] [2 3]]
           (opt/find-cancellation-pairs [hadamard-op hadamard-op x-op x-op]))))

  (testing "Overlapping pairs (should pick first)"
    (is (= [[0 1]]
           (opt/find-cancellation-pairs [hadamard-op hadamard-op hadamard-op]))))

  (testing "Non-consecutive identical gates"
    (is (= [] (opt/find-cancellation-pairs [hadamard-op x-op hadamard-op])))))

(deftest test-remove-cancellation-pairs
  (testing "Empty pairs"
    (let [ops [hadamard-op x-op]]
      (is (= ops (opt/remove-cancellation-pairs ops [])))))

  (testing "Remove single pair"
    (is (= [] (opt/remove-cancellation-pairs [hadamard-op hadamard-op] [[0 1]])))
    (is (= [y-op] (opt/remove-cancellation-pairs [hadamard-op hadamard-op y-op] [[0 1]]))))

  (testing "Remove multiple pairs"
    (is (= [y-op]
           (opt/remove-cancellation-pairs
            [hadamard-op hadamard-op y-op x-op x-op]
            [[0 1] [3 4]]))))

  (testing "Complex removal pattern"
    (let [ops [hadamard-op hadamard-op y-op x-op x-op z-op z-op]
          pairs [[0 1] [3 4] [5 6]]]
      (is (= [y-op] (opt/remove-cancellation-pairs ops pairs))))))

(deftest test-optimize-gates
  (testing "Empty circuit throws exception"
    (let [empty-circuit (qc/create-circuit 2)
          ctx {:circuit empty-circuit :options {:optimize-gates? true}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx)))))

  (testing "Simple H-H cancellation throws exception"
    (let [circuit (-> (qc/create-circuit 1)
                      (qc/h-gate 0)
                      (qc/h-gate 0))
          ctx {:circuit circuit :options {:optimize-gates? true}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx)))))

  (testing "Multiple gate cancellations throw exception"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/h-gate 0)
                      (qc/x-gate 1)
                      (qc/x-gate 1)
                      (qc/y-gate 2)
                      (qc/y-gate 2))
          ctx {:circuit circuit :options {:optimize-gates? true}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx)))))

  (testing "Partial cancellation"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/h-gate 0)
                      (qc/x-gate 1)
                      (qc/y-gate 1)
                      (qc/y-gate 1))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 1 (count (:operations optimized))))
      (is (= :x (get-in optimized [:operations 0 :operation-type])))))

  (testing "Bell circuit preservation"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 1))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 2 (count (:operations optimized))))
      (is (= :h (get-in optimized [:operations 0 :operation-type])))
      (is (= :cnot (get-in optimized [:operations 1 :operation-type])))))

  (testing "No cancellations possible"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/x-gate 1)
                      (qc/y-gate 2))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 3 (count (:operations optimized))))))

  (testing "Iterative optimization throws exception"
    ;; Test consecutive gates that can be removed in one pass
    (let [circuit (-> (qc/create-circuit 1)
                      (qc/h-gate 0)
                      (qc/h-gate 0)
                      (qc/x-gate 0)
                      (qc/x-gate 0))
          ctx {:circuit circuit :options {:optimize-gates? true}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx))))))

(deftest test-self-inverse-gates-set
  (testing "Known self-inverse gates are in set"
    (is (contains? opt/self-inverse-gates :h))
    (is (contains? opt/self-inverse-gates :x))
    (is (contains? opt/self-inverse-gates :y))
    (is (contains? opt/self-inverse-gates :z))
    (is (contains? opt/self-inverse-gates :cnot)))

  (testing "Non-self-inverse gates are not in set"
    (is (not (contains? opt/self-inverse-gates :rx)))
    (is (not (contains? opt/self-inverse-gates :ry)))
    (is (not (contains? opt/self-inverse-gates :rz)))))

;; Integration test with real circuit structures
(deftest test-integration-with-circuit-domain
  (testing "Optimization preserves circuit structure"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/h-gate 0)
                      (qc/x-gate 1))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 2 (:num-qubits optimized)))
      (is (= 1 (count (:operations optimized))))
      (is (= :x (get-in optimized [:operations 0 :operation-type])))
      (is (s/valid? ::qc/circuit optimized))))

  (testing "Complex optimization scenario resulting in empty circuit"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)  ; These two H gates cancel
                      (qc/h-gate 0)
                      (qc/h-gate 1)  ; These two H gates cancel
                      (qc/h-gate 1)
                      (qc/cnot-gate 0 1)  ; These two CNOT gates cancel
                      (qc/cnot-gate 0 1)
                      (qc/x-gate 0)  ; These two X gates cancel
                      (qc/x-gate 0)
                      (qc/x-gate 1)  ; These two X gates cancel
                      (qc/x-gate 1))
          ctx {:circuit circuit :options {:optimize-gates? true}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx))
          "Complete cancellation should result in empty circuit exception"))))

;; Additional test cases for comprehensive coverage

(deftest test-quantum-mechanical-correctness
  (testing "H gates should NOT cancel across CNOT (fundamental quantum mechanics)"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 1)  ; CNOT shares qubit 0 with H gates
                      (qc/h-gate 0))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 3 (count (:operations optimized)))
          "H gates must not cancel across CNOT because they don't commute")
      (is (= :h (get-in optimized [:operations 0 :operation-type])))
      (is (= :cnot (get-in optimized [:operations 1 :operation-type])))
      (is (= :h (get-in optimized [:operations 2 :operation-type])))))

  (testing "X gates should NOT cancel across controlled gates sharing qubits"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/x-gate 0)
                      (qc/cnot-gate 0 1)  ; Shares qubit 0
                      (qc/x-gate 0))
          ctx {:circuit circuit :options {:optimize-gates? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 3 (count (:operations optimized)))
          "X gates must not cancel across CNOT sharing the same qubit")))

  (testing "Gates should NOT cancel when intervening gate shares ANY qubit"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :cz :operation-params {:control 0 :target 1}}
                                {:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :cz :operation-params {:control 0 :target 1}}]}
          optimized (:circuit (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))]
      (is (= 3 (count (:operations optimized)))
          "CZ gates must not cancel across SWAP that shares both qubits")))

  (testing "Gates on completely different qubits CAN cancel"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/x-gate 1)     ; Different qubit, should not interfere
                      (qc/y-gate 2)     ; Different qubit, should not interfere
                      (qc/h-gate 0))    ; Should cancel with first H
          optimized (:circuit (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))]
      (is (= 2 (count (:operations optimized)))
          "H gates should cancel when intervening gates are on different qubits")
      (is (= :x (get-in optimized [:operations 0 :operation-type])))
      (is (= :y (get-in optimized [:operations 1 :operation-type]))))))

(deftest test-three-qubit-gates
  (testing "Toffoli gates should cancel and throw exception"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Adjacent Toffoli gates should cancel and result in empty circuit exception")))

  (testing "CCX (alias for Toffoli) gates should cancel and throw exception"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :ccx :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :ccx :operation-params {:control1 0 :control2 1 :target 2}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Adjacent CCX gates should cancel and result in empty circuit exception")))

  (testing "Fredkin gates should cancel and throw exception"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :fredkin :operation-params {:control 0 :target1 1 :target2 2}}
                                {:operation-type :fredkin :operation-params {:control 0 :target1 1 :target2 2}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Adjacent Fredkin gates should cancel and result in empty circuit exception")))

  (testing "CSWAP (alias for Fredkin) gates should cancel and throw exception"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :cswap :operation-params {:control 0 :target1 1 :target2 2}}
                                {:operation-type :cswap :operation-params {:control 0 :target1 1 :target2 2}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Adjacent CSWAP gates should cancel and result in empty circuit exception")))

  (testing "Three-qubit gates should NOT cancel across interfering gates"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :x :operation-params {:target 0}}
                                {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}]}
          optimized (:circuit (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))]
      (is (= 3 (count (:operations optimized)))
          "Toffoli gates should NOT cancel when X gate shares a qubit (0)"))))

(deftest test-swap-gate-symmetry
  (testing "SWAP gates with reversed qubit order should cancel and throw exception"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 0}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "SWAP(0,1) and SWAP(1,0) should cancel due to symmetry and result in empty circuit exception")))

  (testing "Multiple SWAP gates with mixed order"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 2}}
                                {:operation-type :swap :operation-params {:qubit1 2 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 0}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Multiple SWAP gates in different orders should cancel out completely"))))

(deftest test-extended-self-inverse-gates
  (testing "Controlled Pauli gates should cancel and throw exception"
    (let [cx-circuit {:num-qubits 2
                      :operations [{:operation-type :cx :operation-params {:control 0 :target 1}}
                                   {:operation-type :cx :operation-params {:control 0 :target 1}}]}
          cy-circuit {:num-qubits 2
                      :operations [{:operation-type :cy :operation-params {:control 0 :target 1}}
                                   {:operation-type :cy :operation-params {:control 0 :target 1}}]}
          cz-circuit {:num-qubits 2
                      :operations [{:operation-type :cz :operation-params {:control 0 :target 1}}
                                   {:operation-type :cz :operation-params {:control 0 :target 1}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit cx-circuit :options {:optimize-gates? true}}))
          "CX gates should cancel and result in empty circuit exception")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit cy-circuit :options {:optimize-gates? true}}))
          "CY gates should cancel and result in empty circuit exception")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit cz-circuit :options {:optimize-gates? true}}))
          "CZ gates should cancel and result in empty circuit exception")))

  (testing "SWAP gates should cancel and throw exception"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}]}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))
          "Adjacent SWAP gates should cancel and result in empty circuit exception")))

  (testing "All self-inverse gates are properly detected"
    (is (contains? opt/self-inverse-gates :toffoli) "Toffoli should be self-inverse")
    (is (contains? opt/self-inverse-gates :fredkin) "Fredkin should be self-inverse")
    (is (contains? opt/self-inverse-gates :ccx) "CCX should be self-inverse")
    (is (contains? opt/self-inverse-gates :cswap) "CSWAP should be self-inverse")
    (is (contains? opt/self-inverse-gates :cx) "CX should be self-inverse")
    (is (contains? opt/self-inverse-gates :cy) "CY should be self-inverse")
    (is (contains? opt/self-inverse-gates :cz) "CZ should be self-inverse")
    (is (contains? opt/self-inverse-gates :swap) "SWAP should be self-inverse"))

  (testing "Non-self-inverse gates are correctly excluded"
    (is (not (contains? opt/self-inverse-gates :iswap)) "iSWAP should not be self-inverse")
    (is (not (contains? opt/self-inverse-gates :s)) "S gate should not be self-inverse")
    (is (not (contains? opt/self-inverse-gates :t)) "T gate should not be self-inverse")))

(deftest test-optimization-interaction-scenarios
  (testing "Gate optimization followed by qubit optimization"
    ;; Test scenario where gate optimization reduces gates but not to empty
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)  ; These will cancel
                      (qc/h-gate 0)
                      (qc/x-gate 2)  ; This will remain, using only qubit 2
                      (qc/y-gate 2))  ; Different gate type, won't cancel
          ;; First gate optimization should leave only X and Y on qubit 2
          gate-optimized (:circuit (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))]
      (is (= 2 (count (:operations gate-optimized))))
      ;; Then qubit optimization should compact from 3 qubits to 1 qubit
      (let [qubit-result (qo/optimize-qubit-usage {:circuit gate-optimized :options {:optimize-qubits? true}})
            optimized-qubits (:num-qubits (:circuit qubit-result))
            original-qubits (:num-qubits gate-optimized)
            qubits-saved (- original-qubits optimized-qubits)]
        (is (= 1 optimized-qubits))
        (is (= 2 qubits-saved)))))

  (testing "Circuit that survives gate optimization but would fail qubit optimization"
    ;; Create a circuit where gates don't all cancel but no qubits are actually used
    ;; This is theoretically possible with measurement-only circuits or special cases
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/x-gate 1))
          ;; Gate optimization should preserve the circuit
          gate-optimized (:circuit (opt/optimize-gates {:circuit circuit :options {:optimize-gates? true}}))]
      (is (= 2 (count (:operations gate-optimized))))
      ;; Qubit optimization should also succeed (qubits are used)
      (let [qubit-result (qo/optimize-qubit-usage {:circuit gate-optimized :options {:optimize-qubits? true}})
            optimized-qubits (:num-qubits (:circuit qubit-result))
            original-qubits (:num-qubits gate-optimized)
            qubits-saved (- original-qubits optimized-qubits)]
        (is (= 2 optimized-qubits))
        (is (= 0 qubits-saved)))))

  (testing "Exception propagation in optimization pipelines"
    ;; Test that exceptions from optimization functions are properly caught
    ;; when used in larger optimization pipelines
    (let [self-cancelling-circuit (-> (qc/create-circuit 2)
                                      (qc/h-gate 0)
                                      (qc/h-gate 0)
                                      (qc/cnot-gate 0 1)
                                      (qc/cnot-gate 0 1))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates {:circuit self-cancelling-circuit :options {:optimize-gates? true}}))
          "Complete self-cancellation should throw exception"))))
(comment
  ;; Run all tests
  (run-tests)

  ;
  )
