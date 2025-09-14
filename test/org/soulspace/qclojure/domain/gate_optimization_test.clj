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

;; Tests for rotation folding functionality
(deftest test-rotation-gates-combinable?
  (testing "Same rotation type on same qubit should be combinable"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle 0.5}}
          rx2 {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
          ry1 {:operation-type :ry :operation-params {:target 1 :angle 0.3}}
          ry2 {:operation-type :ry :operation-params {:target 1 :angle 0.7}}
          rz1 {:operation-type :rz :operation-params {:target 2 :angle 1.2}}
          rz2 {:operation-type :rz :operation-params {:target 2 :angle 0.8}}]
      (is (opt/rotation-gates-combinable? rx1 rx2))
      (is (opt/rotation-gates-combinable? ry1 ry2))
      (is (opt/rotation-gates-combinable? rz1 rz2))))

  (testing "Different rotation types should not be combinable"
    (let [rx {:operation-type :rx :operation-params {:target 0 :angle 0.5}}
          ry {:operation-type :ry :operation-params {:target 0 :angle 1.0}}
          rz {:operation-type :rz :operation-params {:target 0 :angle 0.3}}]
      (is (not (opt/rotation-gates-combinable? rx ry)))
      (is (not (opt/rotation-gates-combinable? ry rz)))
      (is (not (opt/rotation-gates-combinable? rx rz)))))

  (testing "Same rotation type on different qubits should not be combinable"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle 0.5}}
          rx2 {:operation-type :rx :operation-params {:target 1 :angle 1.0}}]
      (is (not (opt/rotation-gates-combinable? rx1 rx2)))))

  (testing "Non-rotation gates should not be combinable"
    (let [rx {:operation-type :rx :operation-params {:target 0 :angle 0.5}}
          h {:operation-type :h :operation-params {:target 0}}]
      (is (not (opt/rotation-gates-combinable? rx h))))))

(deftest test-combine-rotation-gates
  (testing "Basic rotation combination"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle 0.5}}
          rx2 {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
          combined (opt/combine-rotation-gates rx1 rx2)]
      (is (= (:operation-type combined) :rx))
      (is (= (get-in combined [:operation-params :target]) 0))
      (is (= (get-in combined [:operation-params :angle]) 1.5))))

  (testing "Rotation combination resulting in identity (2π) returns nil"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
          rx2 {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
          combined (opt/combine-rotation-gates rx1 rx2)]
      (is (nil? combined) "2π rotation should be eliminated as identity")))

  (testing "Rotation combination with wrap-around"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle (* 1.8 Math/PI)}}
          rx2 {:operation-type :rx :operation-params {:target 0 :angle (* 0.4 Math/PI)}}
          combined (opt/combine-rotation-gates rx1 rx2)
          result-angle (get-in combined [:operation-params :angle])]
      (is (< (Math/abs (- result-angle (* 0.2 Math/PI))) 1e-10)
          "1.8π + 0.4π = 2.2π should normalize to 0.2π")))

  (testing "Near-zero angle elimination"
    (let [rx1 {:operation-type :rx :operation-params {:target 0 :angle 1e-15}}
          rx2 {:operation-type :rx :operation-params {:target 0 :angle 2e-15}}
          combined (opt/combine-rotation-gates rx1 rx2)]
      (is (nil? combined) "Very small angles should be eliminated as effectively zero"))))

(deftest test-angle-equivalent?
  (testing "Identical angles should be equivalent"
    (is (opt/angle-equivalent? 0.5 0.5))
    (is (opt/angle-equivalent? Math/PI Math/PI)))

  (testing "Angles differing by 2π should be equivalent"
    (is (opt/angle-equivalent? 0.5 (+ 0.5 (* 2 Math/PI))))
    (is (opt/angle-equivalent? Math/PI (+ Math/PI (* 4 Math/PI)))))

  (testing "Small differences within tolerance should be equivalent"
    (is (opt/angle-equivalent? 1.0 (+ 1.0 1e-13))))

  (testing "Large differences should not be equivalent"
    (is (not (opt/angle-equivalent? 0.5 1.0)))
    (is (not (opt/angle-equivalent? 0.0 Math/PI)))))

(deftest test-normalize-angle
  (testing "Angles in [0, 2π) should remain unchanged"
    (is (= (opt/normalize-angle 0.5) 0.5))
    (is (= (opt/normalize-angle Math/PI) Math/PI))
    (is (< (Math/abs (- (opt/normalize-angle (* 1.5 Math/PI)) (* 1.5 Math/PI))) 1e-15)))

  (testing "Negative angles should be normalized to positive equivalent"
    (let [normalized (opt/normalize-angle (- Math/PI))]
      (is (> normalized 0))
      (is (< (Math/abs (- normalized Math/PI)) 1e-15))))

  (testing "Angles greater than 2π should be normalized"
    (let [normalized (opt/normalize-angle (* 2.5 Math/PI))]
      (is (< (Math/abs (- normalized (* 0.5 Math/PI))) 1e-15))))

  (testing "Near-zero angles should be set to exactly zero"
    (is (= (opt/normalize-angle 1e-15) 0.0))
    (is (= (opt/normalize-angle -1e-15) 0.0)))

  (testing "Near-2π angles should be set to exactly zero"
    (let [near-2pi (- (* 2 Math/PI) 1e-15)]
      (is (= (opt/normalize-angle near-2pi) 0.0)))))

(deftest test-find-rotation-folding-pairs
  (testing "Empty operations list"
    (is (= [] (opt/find-rotation-folding-pairs []))))

  (testing "Single rotation operation"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}]]
      (is (= [] (opt/find-rotation-folding-pairs ops)))))

  (testing "Two consecutive same-type rotations on same qubit"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
          pairs (opt/find-rotation-folding-pairs ops)]
      (is (= 1 (count pairs)))
      (let [[indices combined-gate] (first pairs)]
        (is (= indices [0 1]))
        (is (= (:operation-type combined-gate) :rx))
        (is (= (get-in combined-gate [:operation-params :angle]) 1.5)))))

  (testing "Two consecutive different-type rotations should not fold"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :ry :operation-params {:target 0 :angle 1.0}}]]
      (is (= [] (opt/find-rotation-folding-pairs ops)))))

  (testing "Two consecutive rotations on different qubits should not fold"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 1 :angle 1.0}}]]
      (is (= [] (opt/find-rotation-folding-pairs ops)))))

  (testing "Rotations separated by intervening gate on same qubit should not fold"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :h :operation-params {:target 0}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]]
      (is (= [] (opt/find-rotation-folding-pairs ops)))))

  (testing "Rotations separated by intervening gate on different qubit should fold"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :h :operation-params {:target 1}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
          pairs (opt/find-rotation-folding-pairs ops)]
      (is (= 1 (count pairs)))
      (let [[indices combined-gate] (first pairs)]
        (is (= indices [0 2]))
        (is (= (get-in combined-gate [:operation-params :angle]) 1.5)))))

  (testing "Multiple rotation folding pairs"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.3}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.7}}]
          pairs (opt/find-rotation-folding-pairs ops)]
      (is (= 2 (count pairs)))
      ;; Should have RX folding and RY folding
      (let [pair-indices (map first pairs)]
        (is (some #(= % [0 1]) pair-indices) "Should fold RX gates")
        (is (some #(= % [2 3]) pair-indices) "Should fold RY gates"))))

  (testing "Rotation folding resulting in identity elimination"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
               {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}]
          pairs (opt/find-rotation-folding-pairs ops)]
      (is (= 1 (count pairs)))
      (let [[indices combined-gate] (first pairs)]
        (is (= indices [0 1]))
        (is (nil? combined-gate) "Identity rotation should be nil for elimination")))))

(deftest test-apply-rotation-folding
  (testing "Empty pairs should return original operations"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}]]
      (is (= ops (opt/apply-rotation-folding ops [])))))

  (testing "Single folding pair"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
          combined-gate {:operation-type :rx :operation-params {:target 0 :angle 1.5}}
          pairs [[[0 1] combined-gate]]
          result (opt/apply-rotation-folding ops pairs)]
      (is (= 1 (count result)))
      (is (= (first result) combined-gate))))

  (testing "Multiple folding pairs"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.3}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.7}}]
          rx-combined {:operation-type :rx :operation-params {:target 0 :angle 1.5}}
          ry-combined {:operation-type :ry :operation-params {:target 1 :angle 1.0}}
          pairs [[[0 1] rx-combined] [[2 3] ry-combined]]
          result (opt/apply-rotation-folding ops pairs)]
      (is (= 2 (count result)))
      (is (some #(= % rx-combined) result))
      (is (some #(= % ry-combined) result))))

  (testing "Identity elimination (nil combined gate)"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
               {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.5}}]
          pairs [[[0 1] nil]]  ; Identity rotation eliminated
          result (opt/apply-rotation-folding ops pairs)]
      (is (= 1 (count result)))
      (is (= (:operation-type (first result)) :ry))))

  (testing "Mixed folding with preservation of non-folded gates"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :h :operation-params {:target 1}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
          combined-gate {:operation-type :rx :operation-params {:target 0 :angle 1.5}}
          pairs [[[0 2] combined-gate]]
          result (opt/apply-rotation-folding ops pairs)]
      (is (= 2 (count result)))
      (is (some #(= (:operation-type %) :h) result) "H gate should be preserved")
      (is (some #(= % combined-gate) result) "Combined RX should be present"))))

(deftest test-optimize-rotations
  (testing "Empty operations"
    (is (= [] (opt/optimize-rotations []))))

  (testing "Single rotation - no optimization possible"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}]]
      (is (= ops (opt/optimize-rotations ops)))))

  (testing "Basic rotation folding"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
          result (opt/optimize-rotations ops)]
      (is (= 1 (count result)))
      (is (= (:operation-type (first result)) :rx))
      (is (= (get-in (first result) [:operation-params :angle]) 1.5))))

  (testing "Identity elimination through rotation optimization"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
               {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}]
          result (opt/optimize-rotations ops)]
      (is (= 0 (count result)) "2π rotation should be completely eliminated")))

  (testing "Mixed rotation types"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.3}}
               {:operation-type :ry :operation-params {:target 1 :angle 0.7}}]
          result (opt/optimize-rotations ops)]
      (is (= 2 (count result)))
      (is (some #(and (= (:operation-type %) :rx) 
                      (= (get-in % [:operation-params :angle]) 1.5)) result))
      (is (some #(and (= (:operation-type %) :ry) 
                      (= (get-in % [:operation-params :angle]) 1.0)) result))))

  (testing "Iterative optimization - multiple passes"
    (let [ops [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
               {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
               {:operation-type :rx :operation-params {:target 0 :angle (* 0.5 Math/PI)}}]
          result (opt/optimize-rotations ops)]
      (is (= 1 (count result)))
      ;; Should combine all three: 0.5 + 1.0 + π/2 
      (let [expected-angle (+ 0.5 1.0 (* 0.5 Math/PI))
            actual-angle (get-in (first result) [:operation-params :angle])]
        (is (< (Math/abs (- actual-angle expected-angle)) 1e-15))))))

(deftest test-rotation-folding-integration
  (testing "Rotation folding integrated with gate optimization"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
                                {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
                                {:operation-type :h :operation-params {:target 1}}
                                {:operation-type :h :operation-params {:target 1}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 1 (count (:operations optimized))) "Should have only combined RX gate")
      (is (= (:operation-type (first (:operations optimized))) :rx))
      (is (= (get-in (first (:operations optimized)) [:operation-params :angle]) 1.5))))

  (testing "Rotation optimization disabled"
    (let [circuit {:num-qubits 1
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
                                {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? false}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 2 (count (:operations optimized))) "Rotation folding should be disabled")
      (is (every? #(= (:operation-type %) :rx) (:operations optimized)))))

  (testing "Complex mixed optimization scenario"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
                                {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
                                {:operation-type :h :operation-params {:target 1}}
                                {:operation-type :x :operation-params {:target 2}}
                                {:operation-type :h :operation-params {:target 1}}
                                {:operation-type :x :operation-params {:target 2}}
                                {:operation-type :ry :operation-params {:target 0 :angle 0.3}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      ;; Should have: RX(1.5) from rotation folding, RY(0.3), H-H and X-X should cancel
      (is (= 2 (count (:operations optimized))))
      (is (some #(and (= (:operation-type %) :rx) 
                      (= (get-in % [:operation-params :angle]) 1.5)) (:operations optimized)))
      (is (some #(and (= (:operation-type %) :ry) 
                      (= (get-in % [:operation-params :angle]) 0.3)) (:operations optimized))))))

;; Tests for inverse gate cancellation functionality
(deftest test-gates-are-inverses?
  (testing "S and S† should be inverses"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}]
      (is (opt/gates-are-inverses? s-gate s-dag-gate))
      (is (opt/gates-are-inverses? s-dag-gate s-gate))))

  (testing "T and T† should be inverses"
    (let [t-gate {:operation-type :t :operation-params {:target 0}}
          t-dag-gate {:operation-type :t-dag :operation-params {:target 0}}]
      (is (opt/gates-are-inverses? t-gate t-dag-gate))
      (is (opt/gates-are-inverses? t-dag-gate t-gate))))

  (testing "Self-inverse gates should be inverses of themselves"
    (is (opt/gates-are-inverses? hadamard-op hadamard-op))
    (is (opt/gates-are-inverses? x-op x-op)))

  (testing "Non-inverse gates should not be inverses"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          t-gate {:operation-type :t :operation-params {:target 0}}]
      (is (not (opt/gates-are-inverses? s-gate t-gate)))
      (is (not (opt/gates-are-inverses? s-gate x-op)))))

  (testing "Same gate type on different qubits should not be inverses"
    (let [s-gate-0 {:operation-type :s :operation-params {:target 0}}
          s-dag-gate-1 {:operation-type :s-dag :operation-params {:target 1}}]
      (is (not (opt/gates-are-inverses? s-gate-0 s-dag-gate-1))))))

(deftest test-find-inverse-cancellation-pairs
  (testing "Empty operations list"
    (is (= [] (opt/find-inverse-cancellation-pairs []))))

  (testing "Single operation"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}]
      (is (= [] (opt/find-inverse-cancellation-pairs [s-gate])))))

  (testing "Two consecutive S and S† gates"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}]
      (is (= [[0 1]] (opt/find-inverse-cancellation-pairs [s-gate s-dag-gate])))
      (is (= [[0 1]] (opt/find-inverse-cancellation-pairs [s-dag-gate s-gate])))))

  (testing "Two consecutive T and T† gates"
    (let [t-gate {:operation-type :t :operation-params {:target 0}}
          t-dag-gate {:operation-type :t-dag :operation-params {:target 0}}]
      (is (= [[0 1]] (opt/find-inverse-cancellation-pairs [t-gate t-dag-gate])))
      (is (= [[0 1]] (opt/find-inverse-cancellation-pairs [t-dag-gate t-gate])))))

  (testing "Non-canceling consecutive gates"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          t-gate {:operation-type :t :operation-params {:target 0}}]
      (is (= [] (opt/find-inverse-cancellation-pairs [s-gate t-gate])))))

  (testing "Multiple inverse pairs"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}
          t-gate {:operation-type :t :operation-params {:target 1}}
          t-dag-gate {:operation-type :t-dag :operation-params {:target 1}}]
      (is (= [[0 1] [2 3]]
             (opt/find-inverse-cancellation-pairs [s-gate s-dag-gate t-gate t-dag-gate])))))

  (testing "Inverse pairs separated by non-interfering gates"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          h-gate {:operation-type :h :operation-params {:target 1}} ; Different qubit
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}]
      (is (= [[0 2]] (opt/find-inverse-cancellation-pairs [s-gate h-gate s-dag-gate])))))

  (testing "Inverse pairs separated by interfering gates should not cancel"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          cnot-gate {:operation-type :cnot :operation-params {:control 0 :target 1}} ; Shares qubit 0
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}]
      (is (= [] (opt/find-inverse-cancellation-pairs [s-gate cnot-gate s-dag-gate]))))))

(deftest test-optimize-inverse-cancellations
  (testing "Empty operations"
    (is (= [] (opt/optimize-inverse-cancellations []))))

  (testing "Basic S and S† cancellation"
    (let [ops [{:operation-type :s :operation-params {:target 0}}
               {:operation-type :s-dag :operation-params {:target 0}}]
          optimized (opt/optimize-inverse-cancellations ops)]
      (is (= [] optimized))))

  (testing "Basic T and T† cancellation"
    (let [ops [{:operation-type :t :operation-params {:target 1}}
               {:operation-type :t-dag :operation-params {:target 1}}]
          optimized (opt/optimize-inverse-cancellations ops)]
      (is (= [] optimized))))

  (testing "Mixed inverse cancellations with preservation"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          s-dag-gate {:operation-type :s-dag :operation-params {:target 0}}
          h-gate {:operation-type :h :operation-params {:target 1}}
          ops [s-gate s-dag-gate h-gate]
          optimized (opt/optimize-inverse-cancellations ops)]
      (is (= [h-gate] optimized))))

  (testing "No cancellations possible"
    (let [s-gate {:operation-type :s :operation-params {:target 0}}
          t-gate {:operation-type :t :operation-params {:target 1}}
          ops [s-gate t-gate]
          optimized (opt/optimize-inverse-cancellations ops)]
      (is (= ops optimized)))))

;; Tests for identity rotation removal functionality
(deftest test-is-identity-rotation?
  (testing "Zero angle rotations should be identity"
    (let [rx-zero {:operation-type :rx :operation-params {:target 0 :angle 0.0}}
          ry-zero {:operation-type :ry :operation-params {:target 1 :angle 0.0}}
          rz-zero {:operation-type :rz :operation-params {:target 2 :angle 0.0}}]
      (is (opt/is-identity-rotation? rx-zero))
      (is (opt/is-identity-rotation? ry-zero))
      (is (opt/is-identity-rotation? rz-zero))))

  (testing "2π angle rotations should be identity"
    (let [rx-2pi {:operation-type :rx :operation-params {:target 0 :angle (* 2 Math/PI)}}
          ry-2pi {:operation-type :ry :operation-params {:target 1 :angle (* 2 Math/PI)}}
          rz-2pi {:operation-type :rz :operation-params {:target 2 :angle (* 2 Math/PI)}}]
      (is (opt/is-identity-rotation? rx-2pi))
      (is (opt/is-identity-rotation? ry-2pi))
      (is (opt/is-identity-rotation? rz-2pi))))

  (testing "Multiple 2π angle rotations should be identity"
    (let [rx-4pi {:operation-type :rx :operation-params {:target 0 :angle (* 4 Math/PI)}}
          ry-6pi {:operation-type :ry :operation-params {:target 1 :angle (* 6 Math/PI)}}]
      (is (opt/is-identity-rotation? rx-4pi))
      (is (opt/is-identity-rotation? ry-6pi))))

  (testing "Near-zero angles should be identity"
    (let [rx-tiny {:operation-type :rx :operation-params {:target 0 :angle 1e-15}}
          ry-tiny {:operation-type :ry :operation-params {:target 1 :angle -1e-15}}]
      (is (opt/is-identity-rotation? rx-tiny))
      (is (opt/is-identity-rotation? ry-tiny))))

  (testing "Non-identity angles should not be identity"
    (let [rx-pi4 {:operation-type :rx :operation-params {:target 0 :angle (/ Math/PI 4)}}
          ry-pi2 {:operation-type :ry :operation-params {:target 1 :angle (/ Math/PI 2)}}
          rz-pi {:operation-type :rz :operation-params {:target 2 :angle Math/PI}}]
      (is (not (opt/is-identity-rotation? rx-pi4)))
      (is (not (opt/is-identity-rotation? ry-pi2)))
      (is (not (opt/is-identity-rotation? rz-pi)))))

  (testing "Non-rotation gates should not be identity rotations"
    (is (not (opt/is-identity-rotation? hadamard-op)))
    (is (not (opt/is-identity-rotation? x-op)))
    (is (not (opt/is-identity-rotation? cnot-op)))))

(deftest test-remove-identity-rotations
  (testing "Empty operations"
    (is (= [] (opt/remove-identity-rotations []))))

  (testing "Remove single identity rotation"
    (let [rx-zero {:operation-type :rx :operation-params {:target 0 :angle 0.0}}
          ops [rx-zero]
          filtered (opt/remove-identity-rotations ops)]
      (is (= [] filtered))))

  (testing "Remove multiple identity rotations"
    (let [rx-zero {:operation-type :rx :operation-params {:target 0 :angle 0.0}}
          ry-2pi {:operation-type :ry :operation-params {:target 1 :angle (* 2 Math/PI)}}
          rz-4pi {:operation-type :rz :operation-params {:target 2 :angle (* 4 Math/PI)}}
          ops [rx-zero ry-2pi rz-4pi]
          filtered (opt/remove-identity-rotations ops)]
      (is (= [] filtered))))

  (testing "Preserve non-identity rotations"
    (let [rx-pi4 {:operation-type :rx :operation-params {:target 0 :angle (/ Math/PI 4)}}
          ry-pi2 {:operation-type :ry :operation-params {:target 1 :angle (/ Math/PI 2)}}
          ops [rx-pi4 ry-pi2]
          filtered (opt/remove-identity-rotations ops)]
      (is (= ops filtered))))

  (testing "Mixed identity and non-identity rotations"
    (let [rx-zero {:operation-type :rx :operation-params {:target 0 :angle 0.0}}
          ry-pi4 {:operation-type :ry :operation-params {:target 1 :angle (/ Math/PI 4)}}
          rz-2pi {:operation-type :rz :operation-params {:target 2 :angle (* 2 Math/PI)}}
          ops [rx-zero ry-pi4 rz-2pi]
          filtered (opt/remove-identity-rotations ops)]
      (is (= [ry-pi4] filtered))))

  (testing "Preserve non-rotation gates"
    (let [rx-zero {:operation-type :rx :operation-params {:target 0 :angle 0.0}}
          h-gate {:operation-type :h :operation-params {:target 1}}
          cnot-gate {:operation-type :cnot :operation-params {:control 0 :target 1}}
          ops [rx-zero h-gate cnot-gate]
          filtered (opt/remove-identity-rotations ops)]
      (is (= [h-gate cnot-gate] filtered)))))

;; Integration tests for all optimizations working together
(deftest test-comprehensive-optimization-integration
  (testing "Rotation folding creates identity that gets removed"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
                                {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}]} ; π + π = 2π (identity)
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}]
      ;; Should throw exception because circuit becomes empty after optimization
      (is (thrown-with-msg? clojure.lang.ExceptionInfo 
                            #"Optimization resulted in an empty circuit"
                            (opt/optimize-gates ctx))
          "Two π rotations should fold to 2π then be removed, resulting in empty circuit")))

  (testing "Rotation folding creates identity with other gates preserved"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
                                {:operation-type :rx :operation-params {:target 0 :angle Math/PI}} ; π + π = 2π (identity)
                                {:operation-type :h :operation-params {:target 1}}]} ; This should remain
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 1 (count (:operations optimized))) "Only H gate should remain")
      (is (= :h (get-in optimized [:operations 0 :operation-type])) "H gate should be preserved")))

  (testing "All optimization types working together"
    (let [circuit {:num-qubits 3
                   :operations [;; Self-inverse cancellation
                                {:operation-type :h :operation-params {:target 0}}
                                {:operation-type :h :operation-params {:target 0}}
                                ;; Rotation folding to identity
                                {:operation-type :rx :operation-params {:target 1 :angle (/ Math/PI 3)}}
                                {:operation-type :rx :operation-params {:target 1 :angle (/ (* 5 Math/PI) 3)}} ; 1/3π + 5/3π = 2π
                                ;; Direct identity rotation
                                {:operation-type :ry :operation-params {:target 2 :angle 0.0}}
                                ;; Inverse pair cancellation
                                {:operation-type :s :operation-params {:target 0}}
                                {:operation-type :s-dag :operation-params {:target 0}}
                                ;; Gates that should remain
                                {:operation-type :rx :operation-params {:target 0 :angle (/ Math/PI 4)}}
                                {:operation-type :cnot :operation-params {:control 0 :target 2}}]}
          ctx {:circuit circuit :options {:optimize-gates? true 
                                          :optimize-rotations? true 
                                          :optimize-inverse-pairs? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 2 (count (:operations optimized))) "Should have only RX(π/4) and CNOT remaining")
      (is (some #(and (= (:operation-type %) :rx) 
                      (< (Math/abs (- (get-in % [:operation-params :angle]) (/ Math/PI 4))) 1e-10))
                (:operations optimized)) "Should preserve RX(π/4)")
      (is (some #(= (:operation-type %) :cnot) (:operations optimized)) "Should preserve CNOT")))

  (testing "Inverse pairs separated by intervening gates should not cancel"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :s :operation-params {:target 0}}
                                {:operation-type :cnot :operation-params {:control 0 :target 1}} ; Interferes with S gate
                                {:operation-type :s-dag :operation-params {:target 0}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-inverse-pairs? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 3 (count (:operations optimized))) "All gates should be preserved - no cancellation")))

  (testing "Optimization options can disable specific optimizations"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.0}}
                                {:operation-type :s :operation-params {:target 1}}
                                {:operation-type :s-dag :operation-params {:target 1}}]}
          ctx-no-rotation {:circuit circuit :options {:optimize-gates? true 
                                                      :optimize-rotations? false 
                                                      :optimize-inverse-pairs? true}}
          result-no-rotation (opt/optimize-gates ctx-no-rotation)
          optimized-no-rotation (:circuit result-no-rotation)]
      ;; With rotation folding disabled but inverse pairs enabled, 
      ;; RX(0) should be preserved but S+S† should cancel via inverse pair logic
      (is (= 1 (count (:operations optimized-no-rotation))) "Only RX(0) should remain")
      (is (some #(and (= (:operation-type %) :rx) 
                      (= (get-in % [:operation-params :angle]) 0.0)) 
                (:operations optimized-no-rotation)) "RX(0) should be preserved")))

  (testing "Phase gates can be optimized through rotation folding"
    (let [circuit {:num-qubits 1
                   :operations [{:operation-type :s :operation-params {:target 0}}
                                {:operation-type :s-dag :operation-params {:target 0}}
                                {:operation-type :h :operation-params {:target 0}}]}
          ctx {:circuit circuit :options {:optimize-gates? true 
                                         :optimize-rotations? true 
                                         :optimize-inverse-pairs? false}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      ;; S + S† should be eliminated through rotation folding (not inverse pairs)
      ;; even when inverse pair optimization is disabled
      (is (= 1 (count (:operations optimized))) "Only H gate should remain")
      (is (= :h (get-in optimized [:operations 0 :operation-type])) "H gate should be preserved"))))

;; Tests for phase gate folding functionality
(deftest test-gate-rotation-angle
  (testing "Parametric rotation gates"
    (let [rx {:operation-type :rx :operation-params {:target 0 :angle 1.5}}
          ry {:operation-type :ry :operation-params {:target 1 :angle 2.0}}
          rz {:operation-type :rz :operation-params {:target 2 :angle 0.5}}
          phase {:operation-type :phase :operation-params {:target 0 :angle 1.0}}]
      (is (= 1.5 (opt/gate-rotation-angle rx)))
      (is (= 2.0 (opt/gate-rotation-angle ry)))
      (is (= 0.5 (opt/gate-rotation-angle rz)))
      (is (= 1.0 (opt/gate-rotation-angle phase)))))

  (testing "Fixed-angle phase gates"
    (let [s {:operation-type :s :operation-params {:target 0}}
          s-dag {:operation-type :s-dag :operation-params {:target 0}}
          t {:operation-type :t :operation-params {:target 0}}
          t-dag {:operation-type :t-dag :operation-params {:target 0}}]
      (is (= (/ Math/PI 2) (opt/gate-rotation-angle s)))
      (is (= (- (/ Math/PI 2)) (opt/gate-rotation-angle s-dag)))
      (is (= (/ Math/PI 4) (opt/gate-rotation-angle t)))
      (is (= (- (/ Math/PI 4)) (opt/gate-rotation-angle t-dag)))))

  (testing "Non-rotation gates"
    (let [h {:operation-type :h :operation-params {:target 0}}
          x {:operation-type :x :operation-params {:target 0}}]
      (is (nil? (opt/gate-rotation-angle h)))
      (is (nil? (opt/gate-rotation-angle x))))))

(deftest test-gate-rotation-axis
  (testing "X-axis rotations"
    (let [rx {:operation-type :rx :operation-params {:target 0 :angle 1.0}}]
      (is (= :x (opt/gate-rotation-axis rx)))))

  (testing "Y-axis rotations"
    (let [ry {:operation-type :ry :operation-params {:target 0 :angle 1.0}}]
      (is (= :y (opt/gate-rotation-axis ry)))))

  (testing "Z-axis rotations"
    (let [rz {:operation-type :rz :operation-params {:target 0 :angle 1.0}}
          phase {:operation-type :phase :operation-params {:target 0 :angle 1.0}}
          s {:operation-type :s :operation-params {:target 0}}
          s-dag {:operation-type :s-dag :operation-params {:target 0}}
          t {:operation-type :t :operation-params {:target 0}}
          t-dag {:operation-type :t-dag :operation-params {:target 0}}]
      (is (= :z (opt/gate-rotation-axis rz)))
      (is (= :z (opt/gate-rotation-axis phase)))
      (is (= :z (opt/gate-rotation-axis s)))
      (is (= :z (opt/gate-rotation-axis s-dag)))
      (is (= :z (opt/gate-rotation-axis t)))
      (is (= :z (opt/gate-rotation-axis t-dag)))))

  (testing "Non-rotation gates"
    (let [h {:operation-type :h :operation-params {:target 0}}
          x {:operation-type :x :operation-params {:target 0}}]
      (is (nil? (opt/gate-rotation-axis h)))
      (is (nil? (opt/gate-rotation-axis x))))))

(deftest test-phase-gate-combinability
  (testing "Phase gates on same qubit should be combinable"
    (let [s {:operation-type :s :operation-params {:target 0}}
          t {:operation-type :t :operation-params {:target 0}}
          s-dag {:operation-type :s-dag :operation-params {:target 0}}
          rz {:operation-type :rz :operation-params {:target 0 :angle 1.0}}]
      (is (opt/rotation-gates-combinable? s t))
      (is (opt/rotation-gates-combinable? s s-dag))
      (is (opt/rotation-gates-combinable? s rz))
      (is (opt/rotation-gates-combinable? t rz))))

  (testing "Phase gates on different qubits should not be combinable"
    (let [s0 {:operation-type :s :operation-params {:target 0}}
          s1 {:operation-type :s :operation-params {:target 1}}]
      (is (not (opt/rotation-gates-combinable? s0 s1)))))

  (testing "Phase gates and non-Z-axis rotations should not be combinable"
    (let [s {:operation-type :s :operation-params {:target 0}}
          rx {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
          ry {:operation-type :ry :operation-params {:target 0 :angle 1.0}}]
      (is (not (opt/rotation-gates-combinable? s rx)))
      (is (not (opt/rotation-gates-combinable? s ry))))))

(deftest test-phase-gate-combination
  (testing "S + T combination"
    (let [s {:operation-type :s :operation-params {:target 0}}
          t {:operation-type :t :operation-params {:target 0}}
          combined (opt/combine-rotation-gates s t)
          expected-angle (+ (/ Math/PI 2) (/ Math/PI 4))]
      (is (= :rz (:operation-type combined)))
      (is (= 0 (get-in combined [:operation-params :target])))
      (is (< (Math/abs (- (get-in combined [:operation-params :angle]) expected-angle)) 1e-10))))

  (testing "S + S† combination (identity)"
    (let [s {:operation-type :s :operation-params {:target 0}}
          s-dag {:operation-type :s-dag :operation-params {:target 0}}
          combined (opt/combine-rotation-gates s s-dag)]
      (is (nil? combined) "S + S† should result in identity (nil)")))

  (testing "T + T† combination (identity)"
    (let [t {:operation-type :t :operation-params {:target 0}}
          t-dag {:operation-type :t-dag :operation-params {:target 0}}
          combined (opt/combine-rotation-gates t t-dag)]
      (is (nil? combined) "T + T† should result in identity (nil)")))

  (testing "Four S gates combination (identity)"
    (let [s {:operation-type :s :operation-params {:target 0}}
          s-plus-s (opt/combine-rotation-gates s s)
          four-s (opt/combine-rotation-gates s-plus-s s-plus-s)]
      (is (nil? four-s) "Four S gates should result in identity (4 × π/2 = 2π)")))

  (testing "Phase gate with RZ combination"
    (let [s {:operation-type :s :operation-params {:target 0}}
          rz {:operation-type :rz :operation-params {:target 0 :angle (/ Math/PI 4)}}
          combined (opt/combine-rotation-gates s rz)
          expected-angle (+ (/ Math/PI 2) (/ Math/PI 4))]
      (is (= :rz (:operation-type combined)))
      (is (< (Math/abs (- (get-in combined [:operation-params :angle]) expected-angle)) 1e-10)))))

(deftest test-phase-gate-optimization-integration
  (testing "Phase gate folding with identity elimination"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :s :operation-params {:target 0}}
                                {:operation-type :s-dag :operation-params {:target 0}}
                                {:operation-type :h :operation-params {:target 1}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 1 (count (:operations optimized))) "S + S† should be eliminated")
      (is (= :h (get-in optimized [:operations 0 :operation-type])) "H gate should remain")))

  (testing "Mixed phase gate and parametric rotation folding"
    (let [circuit {:num-qubits 1
                   :operations [{:operation-type :rz :operation-params {:target 0 :angle (/ Math/PI 4)}}
                                {:operation-type :t :operation-params {:target 0}}
                                {:operation-type :s :operation-params {:target 0}}]}
          ctx {:circuit circuit :options {:optimize-gates? true :optimize-rotations? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)
          final-angle (get-in optimized [:operations 0 :operation-params :angle])
          expected-angle (+ (/ Math/PI 4) (/ Math/PI 4) (/ Math/PI 2))] ; π/4 + π/4 + π/2 = π
      (is (= 1 (count (:operations optimized))) "Three gates should fold into one")
      (is (= :rz (get-in optimized [:operations 0 :operation-type])) "Result should be RZ gate")
      (is (< (Math/abs (- final-angle expected-angle)) 1e-10) "Angle should be π")))

  (testing "Phase gates with different optimization orderings"
    ;; This tests that phase gates work with both rotation folding and inverse cancellation
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :s :operation-params {:target 0}}
                                {:operation-type :t :operation-params {:target 0}}
                                {:operation-type :s :operation-params {:target 1}}
                                {:operation-type :s-dag :operation-params {:target 1}}]}
          ctx {:circuit circuit :options {:optimize-gates? true 
                                         :optimize-rotations? true 
                                         :optimize-inverse-pairs? true}}
          result (opt/optimize-gates ctx)
          optimized (:circuit result)]
      (is (= 1 (count (:operations optimized))) "Should have S+T folded on qubit 0, S+S† eliminated on qubit 1")
      (is (= :rz (get-in optimized [:operations 0 :operation-type])) "Result should be RZ gate")
      (is (= 0 (get-in optimized [:operations 0 :operation-params :target])) "Should be on qubit 0"))))

(comment
  ;; Run all tests
  (run-tests)

  ;
  )
