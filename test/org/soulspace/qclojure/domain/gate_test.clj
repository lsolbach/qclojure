(ns org.soulspace.qclojure.domain.gate-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]))

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

;; Property-based tests
(defspec gate-preserves-normalization 50
  (prop/for-all [gate-choice (gen/elements [:x :h])]
                (let [|0⟩ (qs/zero-state 1)
                      result (case gate-choice
                               :x (qg/x-gate |0⟩)
                               :h (qg/h-gate |0⟩))
                      amplitudes (:state-vector result)
                      norm (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes))]
                  (< (Math/abs (- norm 1.0)) 1e-10))))

(defspec x-gate-is-involution 20
  (prop/for-all [_dummy (gen/return nil)]
                (let [|0⟩ (qs/zero-state 1)
                      |1⟩ (qs/one-state)
                      x-x-0 (qg/x-gate (qg/x-gate |0⟩))
                      x-x-1 (qg/x-gate (qg/x-gate |1⟩))]
                  (and (= (:state-vector |0⟩) (:state-vector x-x-0))
                       (= (:state-vector |1⟩) (:state-vector x-x-1))))))

(comment
  ;; Run tests in REPL
  (run-tests)

  ;; Run specific test
  (test-pauli-gates)
  (test-hadamard-gate)
  (test-cnot-gate)
  (test-quantum-circuits)

  ;; Test individual quantum circuits
  ;; Bell state preparation
  (let [|00⟩ (qs/zero-state 2)
        after-h (qg/h-gate |00⟩ 0)
        bell-state (qg/cnot after-h)]
    (:state-vector bell-state))

  ;; Quantum NOT with Hadamard sandwich (X = HZH)
  (let [|0⟩ (qs/zero-state 1)
        result (-> |0⟩
                   (qg/h-gate)
                   ;; Apply Z gate (we need to implement this)
                   (qg/h-gate))]
    (:state-vector result)))
