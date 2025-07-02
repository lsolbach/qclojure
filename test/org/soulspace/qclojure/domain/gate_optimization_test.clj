(ns org.soulspace.qclojure.domain.gate-optimization-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.gate-optimization :as opt]
            [org.soulspace.qclojure.domain.circuit :as circuit]))

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

(deftest test-optimize-gate-cancellations
  (testing "Empty circuit"
    (let [empty-circuit (circuit/create-circuit 2)]
      (is (= empty-circuit (opt/optimize-gate-cancellations empty-circuit)))))
  
  (testing "Simple H-H cancellation"
    (let [circuit (-> (circuit/create-circuit 1)
                     (circuit/h-gate 0)
                     (circuit/h-gate 0))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= [] (:operations optimized)))))
  
  (testing "Multiple gate cancellations"
    (let [circuit (-> (circuit/create-circuit 3)
                     (circuit/h-gate 0)
                     (circuit/h-gate 0)
                     (circuit/x-gate 1)
                     (circuit/x-gate 1)
                     (circuit/y-gate 2)
                     (circuit/y-gate 2))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= [] (:operations optimized)))))
  
  (testing "Partial cancellation"
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/h-gate 0)
                     (circuit/h-gate 0)
                     (circuit/x-gate 1)
                     (circuit/y-gate 1)
                     (circuit/y-gate 1))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 1 (count (:operations optimized))))
      (is (= :x (get-in optimized [:operations 0 :operation-type])))))
  
  (testing "Bell circuit preservation"
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/h-gate 0)
                     (circuit/cnot-gate 0 1))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 2 (count (:operations optimized))))
      (is (= :h (get-in optimized [:operations 0 :operation-type])))
      (is (= :cnot (get-in optimized [:operations 1 :operation-type])))))
  
  (testing "No cancellations possible"
    (let [circuit (-> (circuit/create-circuit 3)
                     (circuit/h-gate 0)
                     (circuit/x-gate 1)
                     (circuit/y-gate 2))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 3 (count (:operations optimized))))))
  
  (testing "Iterative optimization"
    ;; Test consecutive gates that can be removed in one pass
    (let [circuit (-> (circuit/create-circuit 1)
                     (circuit/h-gate 0)
                     (circuit/h-gate 0)
                     (circuit/x-gate 0)
                     (circuit/x-gate 0))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= [] (:operations optimized))))))

(deftest test-circuit-optimization-stats
  (testing "Complete optimization"
    (let [original (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/h-gate 0)
                      (circuit/x-gate 1)
                      (circuit/x-gate 1))
          optimized (circuit/create-circuit 2)
          stats (opt/circuit-optimization-stats original optimized)]
      (is (= 4 (:original-gate-count stats)))
      (is (= 0 (:optimized-gate-count stats)))
      (is (= 4 (:gates-removed stats)))
      (is (= 100.0 (:reduction-percentage stats)))))
  
  (testing "Partial optimization"
    (let [original (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/h-gate 0)
                      (circuit/x-gate 1)
                      (circuit/y-gate 1))
          optimized (-> (circuit/create-circuit 2)
                       (circuit/x-gate 1)
                       (circuit/y-gate 1))
          stats (opt/circuit-optimization-stats original optimized)]
      (is (= 4 (:original-gate-count stats)))
      (is (= 2 (:optimized-gate-count stats)))
      (is (= 2 (:gates-removed stats)))
      (is (= 50.0 (:reduction-percentage stats)))))
  
  (testing "No optimization"
    (let [original (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/x-gate 1)
                      (circuit/y-gate 1))
          optimized (-> (circuit/create-circuit 2)
                       (circuit/h-gate 0)
                       (circuit/x-gate 1)
                       (circuit/y-gate 1))
          stats (opt/circuit-optimization-stats original optimized)]
      (is (= 3 (:original-gate-count stats)))
      (is (= 3 (:optimized-gate-count stats)))
      (is (= 0 (:gates-removed stats)))
      (is (= 0.0 (:reduction-percentage stats)))))
  
  (testing "Empty circuit"
    (let [original (circuit/create-circuit 2)
          optimized (circuit/create-circuit 2)
          stats (opt/circuit-optimization-stats original optimized)]
      (is (= 0 (:original-gate-count stats)))
      (is (= 0 (:optimized-gate-count stats)))
      (is (= 0 (:gates-removed stats)))
      (is (= 0.0 (:reduction-percentage stats))))))

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
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/h-gate 0)
                     (circuit/h-gate 0)
                     (circuit/x-gate 1))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 2 (:num-qubits optimized)))
      (is (= 1 (count (:operations optimized))))
      (is (= :x (get-in optimized [:operations 0 :operation-type])))
      (is (s/valid? ::circuit/quantum-circuit optimized))))
  
  (testing "Complex optimization scenario"
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/h-gate 0)  ; These two H gates cancel
                     (circuit/h-gate 0)
                     (circuit/h-gate 1)  ; These two H gates cancel
                     (circuit/h-gate 1)
                     (circuit/cnot-gate 0 1)  ; These two CNOT gates cancel
                     (circuit/cnot-gate 0 1)
                     (circuit/x-gate 0)  ; These two X gates cancel
                     (circuit/x-gate 0)
                     (circuit/x-gate 1)  ; These two X gates cancel
                     (circuit/x-gate 1))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= [] (:operations optimized)))
      (is (s/valid? ::circuit/quantum-circuit optimized)))))

;; Additional test cases for comprehensive coverage

(deftest test-quantum-mechanical-correctness
  (testing "H gates should NOT cancel across CNOT (fundamental quantum mechanics)"
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/h-gate 0)
                     (circuit/cnot-gate 0 1)  ; CNOT shares qubit 0 with H gates
                     (circuit/h-gate 0))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 3 (count (:operations optimized)))
          "H gates must not cancel across CNOT because they don't commute")
      (is (= :h (get-in optimized [:operations 0 :operation-type])))
      (is (= :cnot (get-in optimized [:operations 1 :operation-type])))
      (is (= :h (get-in optimized [:operations 2 :operation-type])))))
  
  (testing "X gates should NOT cancel across controlled gates sharing qubits"
    (let [circuit (-> (circuit/create-circuit 2)
                     (circuit/x-gate 0)
                     (circuit/cnot-gate 0 1)  ; Shares qubit 0
                     (circuit/x-gate 0))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 3 (count (:operations optimized)))
          "X gates must not cancel across CNOT sharing the same qubit")))
  
  (testing "Gates should NOT cancel when intervening gate shares ANY qubit"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :cz :operation-params {:control 0 :target 1}}
                                {:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :cz :operation-params {:control 0 :target 1}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 3 (count (:operations optimized)))
          "CZ gates must not cancel across SWAP that shares both qubits")))
  
  (testing "Gates on completely different qubits CAN cancel"
    (let [circuit (-> (circuit/create-circuit 3)
                     (circuit/h-gate 0)
                     (circuit/x-gate 1)     ; Different qubit, should not interfere
                     (circuit/y-gate 2)     ; Different qubit, should not interfere
                     (circuit/h-gate 0))    ; Should cancel with first H
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 2 (count (:operations optimized)))
          "H gates should cancel when intervening gates are on different qubits")
      (is (= :x (get-in optimized [:operations 0 :operation-type])))
      (is (= :y (get-in optimized [:operations 1 :operation-type]))))))

(deftest test-three-qubit-gates
  (testing "Toffoli gates should cancel"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "Adjacent Toffoli gates should cancel")))
  
  (testing "CCX (alias for Toffoli) gates should cancel"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :ccx :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :ccx :operation-params {:control1 0 :control2 1 :target 2}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "Adjacent CCX gates should cancel")))
  
  (testing "Fredkin gates should cancel"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :fredkin :operation-params {:control 0 :target1 1 :target2 2}}
                                {:operation-type :fredkin :operation-params {:control 0 :target1 1 :target2 2}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "Adjacent Fredkin gates should cancel")))
  
  (testing "CSWAP (alias for Fredkin) gates should cancel"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :cswap :operation-params {:control 0 :target1 1 :target2 2}}
                                {:operation-type :cswap :operation-params {:control 0 :target1 1 :target2 2}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "Adjacent CSWAP gates should cancel")))
  
  (testing "Three-qubit gates should NOT cancel across interfering gates"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
                                {:operation-type :x :operation-params {:target 1}}  ; Shares qubit 1
                                {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 3 (count (:operations optimized)))
          "Toffoli gates should not cancel when X gate shares a qubit"))))

(deftest test-swap-gate-symmetry
  (testing "SWAP gates with reversed qubit order should cancel"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 0}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "SWAP(0,1) and SWAP(1,0) should cancel due to symmetry")))
  
  (testing "Multiple SWAP gates with mixed order"
    (let [circuit {:num-qubits 3
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 2}}
                                {:operation-type :swap :operation-params {:qubit1 2 :qubit2 1}}  ; Cancels with middle
                                {:operation-type :swap :operation-params {:qubit1 1 :qubit2 0}}]} ; Cancels with first
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "All SWAP gates should cancel with their symmetric counterparts"))))

(deftest test-extended-self-inverse-gates
  (testing "Controlled Pauli gates should cancel"
    (let [cx-circuit {:num-qubits 2
                      :operations [{:operation-type :cx :operation-params {:control 0 :target 1}}
                                   {:operation-type :cx :operation-params {:control 0 :target 1}}]}
          cy-circuit {:num-qubits 2
                      :operations [{:operation-type :cy :operation-params {:control 0 :target 1}}
                                   {:operation-type :cy :operation-params {:control 0 :target 1}}]}
          cz-circuit {:num-qubits 2
                      :operations [{:operation-type :cz :operation-params {:control 0 :target 1}}
                                   {:operation-type :cz :operation-params {:control 0 :target 1}}]}]
      (is (= 0 (count (:operations (opt/optimize-gate-cancellations cx-circuit))))
          "CX gates should cancel")
      (is (= 0 (count (:operations (opt/optimize-gate-cancellations cy-circuit))))
          "CY gates should cancel")
      (is (= 0 (count (:operations (opt/optimize-gate-cancellations cz-circuit))))
          "CZ gates should cancel")))
  
  (testing "SWAP gates should cancel"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}
                                {:operation-type :swap :operation-params {:qubit1 0 :qubit2 1}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 0 (count (:operations optimized)))
          "SWAP gates should cancel")))
  
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

(deftest test-edge-cases-and-error-conditions
  (testing "Empty operations should remain empty"
    (let [circuit (circuit/create-circuit 2)
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= [] (:operations optimized)))
      (is (= 2 (:num-qubits optimized)))))
  
  (testing "Single gate should remain unchanged"
    (let [circuit (-> (circuit/create-circuit 1)
                     (circuit/h-gate 0))
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 1 (count (:operations optimized))))
      (is (= :h (get-in optimized [:operations 0 :operation-type])))))
  
  (testing "Non-self-inverse gates should never cancel"
    (let [circuit {:num-qubits 1
                   :operations [{:operation-type :s :operation-params {:target 0}}
                                {:operation-type :s :operation-params {:target 0}}]}
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 2 (count (:operations optimized)))
          "S gates should not cancel (not self-inverse)")))
  
  (testing "Mixed self-inverse and non-self-inverse gates"
    (let [circuit {:num-qubits 2
                   :operations [{:operation-type :h :operation-params {:target 0}}
                                {:operation-type :s :operation-params {:target 1}}  ; Not self-inverse
                                {:operation-type :h :operation-params {:target 0}}  ; Should cancel with first H
                                {:operation-type :s :operation-params {:target 1}}]} ; Should remain
          optimized (opt/optimize-gate-cancellations circuit)]
      (is (= 2 (count (:operations optimized)))
          "Only S gates should remain")
      (is (every? #(= :s (:operation-type %)) (:operations optimized))))))