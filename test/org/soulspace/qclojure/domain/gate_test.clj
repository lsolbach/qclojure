(ns org.soulspace.qclojure.domain.gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]] 
            [clojure.string :as str]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]))

;;
;; Quantum gate tests
;;
(deftest test-pauli-gates
  (testing "Pauli-X gate flips qubit states"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          x-0 (qg/x-gate |0⟩)
          x-1 (qg/x-gate |1⟩)]
      ;; X|0⟩ = |1⟩
      (is (= (:state-vector x-0) (:state-vector |1⟩)))
      ;; X|1⟩ = |0⟩
      (is (= (:state-vector x-1) (:state-vector |0⟩)))))

  (testing "Pauli-Z gate adds phase to |1⟩"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          z-0 (qg/z-gate |0⟩)
          z-1 (qg/z-gate |1⟩)
          expected-z1 [(fc/complex 0 0) (fc/complex -1 0)]]
      ;; Z|0⟩ = |0⟩
      (is (= (:state-vector z-0) (:state-vector |0⟩)))
      ;; Z|1⟩ = -|1⟩
      (is (= (:state-vector z-1) expected-z1))))

  (testing "HZH = X identity"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          hzh-0 (-> |0⟩ (qg/h-gate) (qg/z-gate) (qg/h-gate))
          hzh-1 (-> |1⟩ (qg/h-gate) (qg/z-gate) (qg/h-gate))
          x-0 (qg/x-gate |0⟩)
          x-1 (qg/x-gate |1⟩)]
      ;; HZH|0⟩ ≈ X|0⟩ = |1⟩ (up to floating point precision)
      (is (< (fc/abs (fc/sub (first (:state-vector hzh-0)) (first (:state-vector x-0)))) 1e-10))
      (is (< (fc/abs (fc/sub (second (:state-vector hzh-0)) (second (:state-vector x-0)))) 1e-10))
      ;; HZH|1⟩ ≈ X|1⟩ = |0⟩ (up to floating point precision)
      (is (< (fc/abs (fc/sub (first (:state-vector hzh-1)) (first (:state-vector x-1)))) 1e-10))
      (is (< (fc/abs (fc/sub (second (:state-vector hzh-1)) (second (:state-vector x-1)))) 1e-10)))))

(deftest test-hadamard-gate
  (testing "Hadamard gate creates superposition"
    (let [|0⟩ (qs/zero-state 1)
          h-0 (qg/h-gate |0⟩)
          amplitudes (:state-vector h-0)
          sqrt2-inv (/ 1.0 (Math/sqrt 2))
          expected-amp (fc/complex sqrt2-inv 0)]
      ;; H|0⟩ = (|0⟩ + |1⟩)/√2
      ;; Both amplitudes should be 1/√2
      (is (< (fc/abs (fc/sub (first amplitudes) expected-amp)) 1e-10))
      (is (< (fc/abs (fc/sub (second amplitudes) expected-amp)) 1e-10))))

  (testing "Hadamard is self-inverse"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          h-h-0 (qg/h-gate (qg/h-gate |0⟩))
          h-h-1 (qg/h-gate (qg/h-gate |1⟩))]
      ;; HH|0⟩ = |0⟩
      ;; HH|1⟩ = |1⟩
      (is (< (fc/abs (fc/sub (first (:state-vector h-h-0)) (fc/complex 1 0))) 1e-10))
      (is (< (fc/abs (second (:state-vector h-h-0))) 1e-10))
      (is (< (fc/abs (first (:state-vector h-h-1))) 1e-10))
      (is (< (fc/abs (fc/sub (second (:state-vector h-h-1)) (fc/complex 1 0))) 1e-10)))))

(deftest test-cnot-gate
  (testing "CNOT gate truth table"
    (let [|00⟩ (qs/zero-state 2)
          |01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          |11⟩ (qs/tensor-product (qs/one-state) (qs/one-state))

          cnot-00 (qg/cnot |00⟩)
          cnot-01 (qg/cnot |01⟩)
          cnot-10 (qg/cnot |10⟩)
          cnot-11 (qg/cnot |11⟩)]

      ;; CNOT|00⟩ = |00⟩
      (is (= (:state-vector cnot-00) (:state-vector |00⟩)))

      ;; CNOT|01⟩ = |01⟩
      (is (= (:state-vector cnot-01) (:state-vector |01⟩)))

      ;; CNOT|10⟩ = |11⟩
      (is (= (:state-vector cnot-10) (:state-vector |11⟩)))

      ;; CNOT|11⟩ = |10⟩
      (is (= (:state-vector cnot-11) (:state-vector |10⟩))))))

(deftest test-quantum-circuits
  (testing "Bell state preparation"
    (let [|00⟩ (qs/zero-state 2)
          ;; Apply H to first qubit then CNOT
          after-h (qg/h-gate |00⟩ 0)
          bell-state (qg/cnot after-h)
          sqrt2-inv (/ 1.0 (Math/sqrt 2))
          expected-amp (fc/complex sqrt2-inv 0)]

      ;; Bell state should be (|00⟩ + |11⟩)/√2
      ;; First amplitude (|00⟩) should be 1/√2
      (is (< (fc/abs (fc/sub (first (:state-vector bell-state)) expected-amp)) 1e-10))
      ;; Second amplitude (|01⟩) should be 0
      (is (< (fc/abs (second (:state-vector bell-state))) 1e-10))
      ;; Third amplitude (|10⟩) should be 0
      (is (< (fc/abs (nth (:state-vector bell-state) 2)) 1e-10))
      ;; Fourth amplitude (|11⟩) should be 1/√2
      (is (< (fc/abs (fc/sub (nth (:state-vector bell-state) 3) expected-amp)) 1e-10))))

  (testing "Quantum teleportation circuit preparation"
    ;; Create entangled pair and apply gates to demonstrate teleportation setup
    (let [|000⟩ (qs/zero-state 3)
          ;; Prepare Bell pair on qubits 1,2: H(qubit1), CNOT(1->2)
          after-h1 (qg/h-gate |000⟩ 1)]
      (is (= (:num-qubits after-h1) 3)))))

(deftest test-rydberg-gates
  (testing "Rydberg CZ gate behaves like standard CZ gate"
    (let [|00⟩ (qs/zero-state 2)
          |01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          |11⟩ (qs/tensor-product (qs/one-state) (qs/one-state))
          
          ;; Apply Rydberg CZ gate
          rydberg-cz-00 (qg/rydberg-cz-gate |00⟩ 0 1)
          rydberg-cz-01 (qg/rydberg-cz-gate |01⟩ 0 1)
          rydberg-cz-10 (qg/rydberg-cz-gate |10⟩ 0 1)
          rydberg-cz-11 (qg/rydberg-cz-gate |11⟩ 0 1)]

      ;; CZ|00⟩ = |00⟩
      (is (= (:state-vector rydberg-cz-00) (:state-vector |00⟩)))
      ;; CZ|01⟩ = |01⟩
      (is (= (:state-vector rydberg-cz-01) (:state-vector |01⟩)))
      ;; CZ|10⟩ = |10⟩
      (is (= (:state-vector rydberg-cz-10) (:state-vector |10⟩)))
      ;; CZ|11⟩ = -|11⟩ (phase flip)
      (is (= (nth (:state-vector rydberg-cz-11) 3) (fc/complex -1 0)))))

  (testing "Rydberg CPhase gate applies controlled phase"
    (let [|00⟩ (qs/zero-state 2)
          |11⟩ (qs/tensor-product (qs/one-state) (qs/one-state))
          
          ;; Test with π/4 phase
          phase-angle (/ Math/PI 4)
          rydberg-cphase-00 (qg/rydberg-cphase-gate |00⟩ 0 1 phase-angle)
          rydberg-cphase-11 (qg/rydberg-cphase-gate |11⟩ 0 1 phase-angle)
          expected-phase (fc/complex (Math/cos phase-angle) (Math/sin phase-angle))]

      ;; CPhase|00⟩ = |00⟩ (no change)
      (is (= (:state-vector rydberg-cphase-00) (:state-vector |00⟩)))
      ;; CPhase|11⟩ = e^(iφ)|11⟩ (phase applied to |11⟩)
      (is (< (fc/abs (fc/sub (nth (:state-vector rydberg-cphase-11) 3) expected-phase)) 1e-10))))

  (testing "Rydberg blockade gate applies global constraint"
    (let [|00⟩ (qs/zero-state 2)
          |01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          
          ;; Apply blockade gate with correct signature: state, qubit-indices, phi
          phase-angle (/ Math/PI 4)
          blockade-00 (qg/rydberg-blockade-gate |00⟩ [0 1] phase-angle)
          blockade-01 (qg/rydberg-blockade-gate |01⟩ [0 1] phase-angle)]

      ;; Blockade should preserve |00⟩ state (no Rydberg excitation)
      (is (= (:state-vector blockade-00) (:state-vector |00⟩)))
      ;; Blockade affects |01⟩ due to blockade mechanism  
      (is (not= (:state-vector blockade-01) (:state-vector |01⟩))))))

(deftest test-global-gates
  (testing "Global X gate flips all qubits"
    (let [|000⟩ (qs/zero-state 3)
          global-x-result (qg/global-x-gate |000⟩)]

      ;; Global X should transform |000⟩ → i|111⟩ (with possible global phase)
      ;; Check that the amplitude is at position 7 (binary 111) and has magnitude close to 1
      (is (< (Math/abs (- (fc/abs (nth (:state-vector global-x-result) 7)) 1.0)) 1e-10))))

  (testing "Global Hadamard creates uniform superposition"
    (let [|00⟩ (qs/zero-state 2)
          global-h-result (qg/global-hadamard-gate |00⟩)
          amplitudes (:state-vector global-h-result)
          expected-magnitude (/ 1.0 2.0)]  ; 1/√4 = 1/2 magnitude

      ;; All four amplitudes should have equal magnitude (uniform superposition)
      (doseq [amp amplitudes]
        (is (< (Math/abs (- (fc/abs amp) expected-magnitude)) 1e-10)))))

  (testing "Global rotation gates apply to all qubits"
    (let [|00⟩ (qs/zero-state 2)
          angle (/ Math/PI 4)
          global-rx-result (qg/global-rx-gate |00⟩ angle)
          global-ry-result (qg/global-ry-gate |00⟩ angle)
          global-rz-result (qg/global-rz-gate |00⟩ angle)]

      ;; Global rotations should create superposition states
      (is (not= (:state-vector global-rx-result) (:state-vector |00⟩)))
      (is (not= (:state-vector global-ry-result) (:state-vector |00⟩)))
      ;; Global RZ should preserve probability magnitude but add phases
      (is (< (Math/abs (- (fc/abs (first (:state-vector global-rz-result))) 1.0)) 1e-10)))))

(deftest test-multi-qubit-gates
  (testing "SWAP gate exchanges qubit states"
    (let [|01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          
          swap-01 (qg/swap-gate |01⟩ 0 1)
          swap-10 (qg/swap-gate |10⟩ 0 1)]

      ;; SWAP|01⟩ = |10⟩
      (is (= (:state-vector swap-01) (:state-vector |10⟩)))
      ;; SWAP|10⟩ = |01⟩
      (is (= (:state-vector swap-10) (:state-vector |01⟩)))))

  (testing "iSWAP gate swaps with imaginary phase"
    (let [|01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          
          iswap-01 (qg/iswap-gate |01⟩ 0 1)
          iswap-10 (qg/iswap-gate |10⟩ 0 1)]

      ;; iSWAP introduces imaginary phases during swap
      (is (= (fc/im (nth (:state-vector iswap-01) 2)) 1.0))  ; |10⟩ coefficient is i
      (is (= (fc/im (nth (:state-vector iswap-10) 1)) 1.0)))) ; |01⟩ coefficient is i

  (testing "Toffoli gate (CCX) flips target when both controls are |1⟩"
    (let [|000⟩ (qs/zero-state 3)
          |110⟩ (qs/tensor-product (qs/one-state) 
                                   (qs/tensor-product (qs/one-state) (qs/zero-state 1)))
          |111⟩ (qs/tensor-product (qs/one-state) 
                                   (qs/tensor-product (qs/one-state) (qs/one-state)))
          
          toffoli-000 (qg/toffoli-gate |000⟩ 0 1 2)
          toffoli-110 (qg/toffoli-gate |110⟩ 0 1 2)
          toffoli-111 (qg/toffoli-gate |111⟩ 0 1 2)]

      ;; Toffoli|000⟩ = |000⟩ (controls not both |1⟩)
      (is (= (:state-vector toffoli-000) (:state-vector |000⟩)))
      ;; Toffoli|110⟩ = |111⟩ (both controls |1⟩, flip target)
      (is (= (:state-vector toffoli-110) (:state-vector |111⟩)))
      ;; Toffoli|111⟩ = |110⟩ (both controls |1⟩, flip target)
      (is (= (:state-vector toffoli-111) (:state-vector |110⟩))))))

(deftest test-fredkin-gate
  (testing "Fredkin (CSWAP) gate swaps targets when control is |1⟩"
    (let [;; Apply Fredkin gate with control=0, target1=1, target2=2
          fredkin-000 (qg/fredkin-gate qs/|000⟩ 0 1 2)
          fredkin-001 (qg/fredkin-gate qs/|001⟩ 0 1 2)
          fredkin-010 (qg/fredkin-gate qs/|010⟩ 0 1 2)
          fredkin-011 (qg/fredkin-gate qs/|011⟩ 0 1 2)
          fredkin-100 (qg/fredkin-gate qs/|100⟩ 0 1 2)
          fredkin-101 (qg/fredkin-gate qs/|101⟩ 0 1 2)
          fredkin-110 (qg/fredkin-gate qs/|110⟩ 0 1 2)
          fredkin-111 (qg/fredkin-gate qs/|111⟩ 0 1 2)]

      ;; When control=0, no swap should occur
      (is (= (:state-vector fredkin-000) (:state-vector qs/|000⟩)) "Fredkin|000⟩ = |000⟩")
      (is (= (:state-vector fredkin-001) (:state-vector qs/|001⟩)) "Fredkin|001⟩ = |001⟩")
      (is (= (:state-vector fredkin-010) (:state-vector qs/|010⟩)) "Fredkin|010⟩ = |010⟩")
      (is (= (:state-vector fredkin-011) (:state-vector qs/|011⟩)) "Fredkin|011⟩ = |011⟩")

      ;; When control=1, swap target1 and target2
      (is (= (:state-vector fredkin-100) (:state-vector qs/|100⟩)) "Fredkin|100⟩ = |100⟩ (00→00)")
      (is (= (:state-vector fredkin-101) (:state-vector qs/|110⟩)) "Fredkin|101⟩ = |110⟩ (01→10)")
      (is (= (:state-vector fredkin-110) (:state-vector qs/|101⟩)) "Fredkin|110⟩ = |101⟩ (10→01)")
      (is (= (:state-vector fredkin-111) (:state-vector qs/|111⟩)) "Fredkin|111⟩ = |111⟩ (11→11)")))

  (testing "Fredkin gate is self-inverse"
    ;; Define states locally within this test
    (doseq [state-vec [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]]]
      (let [state (qs/computational-basis-state 3 state-vec)
            double-fredkin (qg/fredkin-gate (qg/fredkin-gate state 0 1 2) 0 1 2)]
        (is (= (:state-vector state) (:state-vector double-fredkin))
            (str "Fredkin gate is self-inverse for |" (str/join state-vec) "⟩"))))))

(deftest test-pauli-y
  (testing "Pauli-Y gate behavior"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          y-0 (qg/y-gate |0⟩)
          y-1 (qg/y-gate |1⟩)]

      ;; Y|0⟩ = i|1⟩
      (is (< (fc/abs (first (:state-vector y-0))) 1e-10) "Y|0⟩ first component should be 0")
      (is (< (Math/abs (- (fc/im (second (:state-vector y-0))) 1.0)) 1e-10) "Y|0⟩ = i|1⟩")

      ;; Y|1⟩ = -i|0⟩  
      (is (< (Math/abs (- (fc/im (first (:state-vector y-1))) -1.0)) 1e-10) "Y|1⟩ = -i|0⟩")
      (is (< (fc/abs (second (:state-vector y-1))) 1e-10) "Y|1⟩ second component should be 0")))

  (testing "Pauli-Y gate is involution (Y² = I)"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          yy-0 (qg/y-gate (qg/y-gate |0⟩))
          yy-1 (qg/y-gate (qg/y-gate |1⟩))]

      ;; Y² should return to original state
      (is (< (fc/abs (fc/sub (first (:state-vector yy-0)) (fc/complex 1 0))) 1e-10) "YY|0⟩ ≈ |0⟩")
      (is (< (fc/abs (second (:state-vector yy-0))) 1e-10) "YY|0⟩ second component should be 0")

      (is (< (fc/abs (first (:state-vector yy-1))) 1e-10) "YY|1⟩ first component should be 0")
      (is (< (fc/abs (fc/sub (second (:state-vector yy-1)) (fc/complex 1 0))) 1e-10) "YY|1⟩ ≈ |1⟩"))))

(deftest test-controlled-y-and-z-gates
  (testing "Controlled-Y gate applies Y when control is |1⟩"
    (let [cy-00 (qg/apply-controlled-gate qs/|00⟩ 0 1 qg/pauli-y)
          cy-01 (qg/apply-controlled-gate qs/|01⟩ 0 1 qg/pauli-y)
          cy-10 (qg/apply-controlled-gate qs/|10⟩ 0 1 qg/pauli-y)
          cy-11 (qg/apply-controlled-gate qs/|11⟩ 0 1 qg/pauli-y)]

      ;; When control=0, no change
      (is (= (:state-vector cy-00) (:state-vector qs/|00⟩)) "CY|00⟩ = |00⟩")
      (is (= (:state-vector cy-01) (:state-vector qs/|01⟩)) "CY|01⟩ = |01⟩")

      ;; When control=1, apply Y to target - test that states are transformed correctly
      ;; CY|10⟩ transforms |10⟩ → |1⟩⊗Y|0⟩ → some phase of |11⟩
      (is (< (fc/abs (nth (:state-vector cy-10) 2)) 1e-10) "|10⟩ component should be 0")
      (is (> (fc/abs (nth (:state-vector cy-10) 3)) 0.9) "Should have significant |11⟩ component")

      ;; CY|11⟩ transforms |11⟩ → |1⟩⊗Y|1⟩ → some phase of |10⟩  
      (is (> (fc/abs (nth (:state-vector cy-11) 2)) 0.9) "Should have significant |10⟩ component")
      (is (< (fc/abs (nth (:state-vector cy-11) 3)) 1e-10) "|11⟩ component should be 0")))

  (testing "Controlled-Z gate applies Z when control is |1⟩"
    (let [|00⟩ (qs/zero-state 2)
          |01⟩ (qs/tensor-product (qs/zero-state 1) (qs/one-state))
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          |11⟩ (qs/tensor-product (qs/one-state) (qs/one-state))

          cz-00 (qg/apply-controlled-gate |00⟩ 0 1 qg/pauli-z)
          cz-01 (qg/apply-controlled-gate |01⟩ 0 1 qg/pauli-z)
          cz-10 (qg/apply-controlled-gate |10⟩ 0 1 qg/pauli-z)
          cz-11 (qg/apply-controlled-gate |11⟩ 0 1 qg/pauli-z)]

      ;; When control=0, no change
      (is (= (:state-vector cz-00) (:state-vector |00⟩)) "CZ|00⟩ = |00⟩")
      (is (= (:state-vector cz-01) (:state-vector |01⟩)) "CZ|01⟩ = |01⟩")

      ;; When control=1 and target=0, no change  
      (is (= (:state-vector cz-10) (:state-vector |10⟩)) "CZ|10⟩ = |10⟩")

      ;; When control=1 and target=1, apply phase flip
      ;; CZ|11⟩ should be -|11⟩
      (is (< (Math/abs (+ (fc/re (nth (:state-vector cz-11) 3)) 1.0)) 1e-10) "CZ|11⟩ = -|11⟩"))))

(deftest test-phase-gates
  (testing "S gate applies π/2 phase to |1⟩"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          
          s-0 (qg/apply-single-qubit-gate qg/s-gate |0⟩ 0)
          s-1 (qg/apply-single-qubit-gate qg/s-gate |1⟩ 0)]

      ;; S|0⟩ = |0⟩
      (is (= (:state-vector s-0) (:state-vector |0⟩)))
      ;; S|1⟩ = i|1⟩
      (is (= (nth (:state-vector s-1) 1) (fc/complex 0 1)))))

  (testing "T gate applies π/4 phase to |1⟩"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          
          t-0 (qg/apply-single-qubit-gate qg/t-gate |0⟩ 0)
          t-1 (qg/apply-single-qubit-gate qg/t-gate |1⟩ 0)
          expected-t-phase (fc/complex (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2))]

      ;; T|0⟩ = |0⟩
      (is (= (:state-vector t-0) (:state-vector |0⟩)))
      ;; T|1⟩ = e^(iπ/4)|1⟩
      (is (< (fc/abs (fc/sub (nth (:state-vector t-1) 1) expected-t-phase)) 1e-10))))

  (testing "S-dagger and T-dagger are inverses"
    (let [|1⟩ (qs/one-state)
          
          s-applied (qg/apply-single-qubit-gate qg/s-gate |1⟩ 0)
          s-sdag-1 (qg/apply-single-qubit-gate qg/s-dag-gate s-applied 0)
          
          t-applied (qg/apply-single-qubit-gate qg/t-gate |1⟩ 0)
          t-tdag-1 (qg/apply-single-qubit-gate qg/t-dag-gate t-applied 0)]

      ;; S†S|1⟩ = |1⟩
      (is (< (fc/abs (fc/sub (nth (:state-vector s-sdag-1) 1) (fc/complex 1 0))) 1e-10))
      ;; T†T|1⟩ = |1⟩
      (is (< (fc/abs (fc/sub (nth (:state-vector t-tdag-1) 1) (fc/complex 1 0))) 1e-10)))))

(deftest test-rotation-gates
  (testing "RX gate rotates around X-axis"
    (let [|0⟩ (qs/zero-state 1)
          
          ;; π/2 rotation should create equal superposition
          rx-matrix (qg/rx-gate (/ Math/PI 2))
          rx-0 (qg/apply-single-qubit-gate rx-matrix |0⟩ 0)
          sqrt2-inv (/ 1.0 (Math/sqrt 2))]

      ;; RX(π/2)|0⟩ = (|0⟩ - i|1⟩)/√2
      (is (< (fc/abs (fc/sub (first (:state-vector rx-0)) (fc/complex sqrt2-inv 0))) 1e-10))
      (is (< (fc/abs (fc/sub (second (:state-vector rx-0)) (fc/complex 0 (- sqrt2-inv)))) 1e-10))))

  (testing "RY gate rotates around Y-axis"
    (let [|0⟩ (qs/zero-state 1)
          
          ;; π/2 rotation should create real superposition
          ry-matrix (qg/ry-gate (/ Math/PI 2))
          ry-0 (qg/apply-single-qubit-gate ry-matrix |0⟩ 0)
          sqrt2-inv (/ 1.0 (Math/sqrt 2))]

      ;; RY(π/2)|0⟩ = (|0⟩ + |1⟩)/√2
      (is (< (fc/abs (fc/sub (first (:state-vector ry-0)) (fc/complex sqrt2-inv 0))) 1e-10))
      (is (< (fc/abs (fc/sub (second (:state-vector ry-0)) (fc/complex sqrt2-inv 0))) 1e-10))))

  (testing "RZ gate rotates around Z-axis"
    (let [|0⟩ (qs/zero-state 1)
          |1⟩ (qs/one-state)
          
          ;; π/2 rotation adds phase to |1⟩
          rz-matrix (qg/rz-gate (/ Math/PI 2))
          rz-0 (qg/apply-single-qubit-gate rz-matrix |0⟩ 0)
          rz-1 (qg/apply-single-qubit-gate rz-matrix |1⟩ 0)]

      ;; RZ(π/2)|0⟩ = e^(-iπ/4)|0⟩
      (is (< (fc/abs (fc/sub (first (:state-vector rz-0)) 
                             (fc/complex (/ (Math/sqrt 2) 2) (- (/ (Math/sqrt 2) 2))))) 1e-10))
      ;; RZ(π/2)|1⟩ = e^(iπ/4)|1⟩  
      (is (< (fc/abs (fc/sub (second (:state-vector rz-1))
                             (fc/complex (/ (Math/sqrt 2) 2) (/ (Math/sqrt 2) 2)))) 1e-10)))))

(deftest test-controlled-rotation-gates
  (testing "Controlled rotation gates apply rotations conditionally"
    (let [|00⟩ (qs/zero-state 2)
          |10⟩ (qs/tensor-product (qs/one-state) (qs/zero-state 1))
          
          angle (/ Math/PI 2)
          ;; Test controlled rotations with control qubit 0, target qubit 1
          crx-00 (qg/apply-controlled-gate |00⟩ 0 1 (qg/rx-gate angle))
          crx-10 (qg/apply-controlled-gate |10⟩ 0 1 (qg/rx-gate angle))
          
          cry-00 (qg/apply-controlled-gate |00⟩ 0 1 (qg/ry-gate angle))
          cry-10 (qg/apply-controlled-gate |10⟩ 0 1 (qg/ry-gate angle))]

      ;; When control is |0⟩, target should be unchanged
      (is (= (:state-vector crx-00) (:state-vector |00⟩)))
      (is (= (:state-vector cry-00) (:state-vector |00⟩)))
      
      ;; When control is |1⟩, target should be rotated
      (is (not= (:state-vector crx-10) (:state-vector |10⟩)))
      (is (not= (:state-vector cry-10) (:state-vector |10⟩))))))
;;
;; Property-based tests for quantum gates
;; These tests verify fundamental quantum mechanical properties

(defspec gate-preserves-normalization 50
  (prop/for-all [gate-type (gen/elements [:x :y :z :h])]
    (let [initial-state (qs/zero-state 2)
          final-state (case gate-type
                        :x (qg/x-gate initial-state 0)
                        :y (qg/y-gate initial-state 0)
                        :z (qg/z-gate initial-state 0)
                        :h (qg/h-gate initial-state 0))
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec x-gate-is-involution 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 2)
          intermediate-state (qg/x-gate initial-state 0)
          final-state (qg/x-gate intermediate-state 0)
          initial-amplitudes (:state-vector initial-state)
          final-amplitudes (:state-vector final-state)]
      (every? #(< (fc/abs (fc/sub (first %) (second %))) 1e-10)
              (map vector initial-amplitudes final-amplitudes)))))

(defspec test-rydberg-gates-preserve-normalization 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 3)
          final-state (qg/rydberg-cz-gate initial-state 0 1)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec test-global-gates-preserve-normalization 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 2)
          final-state (qg/global-x-gate initial-state)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec test-multi-qubit-gates-preserve-normalization 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 3)
          final-state (qg/swap-gate initial-state 0 1)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec fredkin-gate-preserves-normalization 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 3)
          final-state (qg/fredkin-gate initial-state 0 1 2)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec controlled-gates-preserve-normalization 30
  (prop/for-all [gate-type (gen/elements [:cy :cz])]
    (let [initial-state (qs/zero-state 2)
          gate-matrix (case gate-type :cy qg/pauli-y :cz qg/pauli-z)
          final-state (qg/apply-controlled-gate initial-state 0 1 gate-matrix)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec y-gate-preserves-normalization 20
  (prop/for-all [_dummy (gen/return nil)]
    (let [initial-state (qs/zero-state 2)
          final-state (qg/y-gate initial-state 0)
          amplitudes (:state-vector final-state)
          norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
      (< (Math/abs (- norm 1.0)) 1e-10))))

(comment
  (run-tests)
  ;
  )