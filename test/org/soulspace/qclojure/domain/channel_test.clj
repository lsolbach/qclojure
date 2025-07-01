(ns org.soulspace.qclojure.domain.channel-test
  "Test suite for quantum channel domain layer.
  
  Comprehensive tests for quantum channel operations, Kraus operators,
  and channel composition functionality."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.util.test :as util]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.channel :as channel]
            [org.soulspace.qclojure.domain.state :as qs]))

;; Helper functions for testing
(defn complex-magnitude-squared 
  "Calculate |c|² for a complex number or real number"
  [c]
  (if (number? c)
    (* c c) ; Real number
    (+ (* (fc/re c) (fc/re c)) (* (fc/im c) (fc/im c))))) ; Complex number

(defn max-coeff-magnitude-squared
  "Find maximum coefficient magnitude squared from a matrix"
  [matrix]
  (apply max (map (fn [row]
                    (apply max (map complex-magnitude-squared row)))
                  matrix)))

(defn state-fidelity
  "Calculate fidelity between two quantum states"
  [state1 state2]
  (let [sv1 (:state-vector state1)
        sv2 (:state-vector state2)
        overlap-terms (map (fn [a1 a2]
                             (let [conj-a1 (fc/complex (fc/re a1) (- (fc/im a1)))
                                   product (fc/mult conj-a1 a2)]
                               product))
                           sv1 sv2)
        total-overlap (reduce fc/add overlap-terms)]
    (fc/abs total-overlap)))

;;
;; Tests for Kraus operator generation
;;

(deftest test-depolarizing-kraus-operators
  (testing "Depolarizing Kraus operators generation"
    (testing "with zero noise"
      (let [kraus-ops (channel/depolarizing-kraus-operators 0.0)
            identity-op (first kraus-ops)]
        (is (= 4 (count kraus-ops)) "Should have 4 Kraus operators")
        (is (util/approx= 1.0 (complex-magnitude-squared (first (first (:matrix identity-op)))))
            "Identity operator should have coefficient 1")
        (is (every? #(util/approx= 0.0 (max-coeff-magnitude-squared (:matrix %)))
                    (rest kraus-ops))
            "Pauli operators should have coefficient 0")))
    
    (testing "with small noise"
      (let [p 0.1
            kraus-ops (channel/depolarizing-kraus-operators p)
            identity-coeff-sq (max-coeff-magnitude-squared (:matrix (first kraus-ops)))
            pauli-coeff-sq (max-coeff-magnitude-squared (:matrix (second kraus-ops)))]
        (is (util/approx= (- 1.0 p) identity-coeff-sq 1e-10) "Identity coefficient should be √(1-p)")
        (is (util/approx= (/ p 3.0) pauli-coeff-sq 1e-10) "Pauli coefficients should be √(p/3)")))
    
    (testing "completeness relation"
      (let [p 0.2
            kraus-ops (channel/depolarizing-kraus-operators p)
            sum-coeffs-sq (reduce + (map #(max-coeff-magnitude-squared (:matrix %)) kraus-ops))]
        (is (util/approx= 1.0 sum-coeffs-sq 1e-10) "Sum of |coefficients|² should equal 1")))))

(deftest test-amplitude-damping-kraus-operators
  (testing "Amplitude damping Kraus operators generation"
    (testing "with zero damping"
      (let [kraus-ops (channel/amplitude-damping-kraus-operators 0.0)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (= 2 (count kraus-ops)) "Should have 2 Kraus operators")
        (is (util/approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (util/approx= 1.0 (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be 1")
        (is (every? #(util/approx= 0.0 (complex-magnitude-squared %))
                    (flatten k1))
            "K₁ should be zero matrix")))
    
    (testing "with full damping"
      (let [kraus-ops (channel/amplitude-damping-kraus-operators 1.0)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (util/approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (util/approx= 0.0 (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be 0")
        (is (util/approx= 1.0 (complex-magnitude-squared (second (first k1))))
            "K₁[0,1] should be 1")))
    
    (testing "with partial damping"
      (let [gamma 0.3
            kraus-ops (channel/amplitude-damping-kraus-operators gamma)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (= 2 (count kraus-ops)) "Should have 2 Kraus operators")
        (is (util/approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (util/approx= (- 1.0 gamma) (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be √(1-γ)")
        (is (util/approx= gamma (complex-magnitude-squared (second (first k1))))
            "K₁[0,1] should be √γ")))))

(deftest test-phase-damping-kraus-operators
  (testing "Phase damping Kraus operators generation"
    (testing "preserve population"
      (let [gamma 0.3
            kraus-ops (channel/phase-damping-kraus-operators gamma)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (= 2 (count kraus-ops)) "Should have 2 Kraus operators")
        (is (util/approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (util/approx= (- 1.0 gamma) (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be √(1-γ)")
        (is (util/approx= gamma (complex-magnitude-squared (second (second k1))))
            "K₁[1,1] should be √γ")))))

(deftest test-coherent-error-kraus-operator
  (testing "Coherent error Kraus operators"
    (testing "X rotation"
      (let [angle (/ m/PI 4) ; 45 degrees
            kraus-op (channel/coherent-error-kraus-operator angle :x)
            matrix (:matrix kraus-op)
            cos-half (m/cos (/ angle 2))]
        (is (util/approx= cos-half (fc/re (first (first matrix))))
            "Matrix[0,0] should be cos(θ/2)")
        (is (util/approx= cos-half (fc/re (second (second matrix))))
            "Matrix[1,1] should be cos(θ/2)")))
    
    (testing "Z rotation"
      (let [angle (/ m/PI 6) ; 30 degrees
            kraus-op (channel/coherent-error-kraus-operator angle :z)
            matrix (:matrix kraus-op)]
        (is (util/approx= (m/cos angle) (fc/re (first (first matrix))))
            "Matrix[0,0] should be cos(θ)")
        (is (util/approx= (m/cos (- angle)) (fc/re (second (second matrix))))
            "Matrix[1,1] should be cos(-θ)")))))

;;
;; Tests for quantum channel application
;;

(deftest test-apply-single-qubit-kraus-operator
  (testing "Single qubit Kraus operator application"
    (testing "identity operation on single qubit"
      (let [initial-state (qs/zero-state 1)
            identity-kraus {:matrix [[(fc/complex 1.0 0) (fc/complex 0 0)]
                                     [(fc/complex 0 0) (fc/complex 1.0 0)]]}
            result-state (channel/apply-single-qubit-kraus-operator initial-state identity-kraus 0)]
        (is (> (state-fidelity initial-state result-state) 0.999)
            "Identity operation should preserve state")))
    
    (testing "Pauli-X operation on single qubit"
      (let [initial-state (qs/zero-state 1)
            pauli-x-kraus {:matrix [[(fc/complex 0 0) (fc/complex 1.0 0)]
                                    [(fc/complex 1.0 0) (fc/complex 0 0)]]}
            result-state (channel/apply-single-qubit-kraus-operator initial-state pauli-x-kraus 0)
            expected-state (qs/one-state)]
        (is (> (state-fidelity expected-state result-state) 0.999)
            "Pauli-X should flip |0⟩ to |1⟩")))
    
    (testing "operation on specific qubit in multi-qubit system"
      (let [initial-state (qs/computational-basis-state 2 [0 0])
            pauli-x-kraus {:matrix [[(fc/complex 0 0) (fc/complex 1.0 0)]
                                    [(fc/complex 1.0 0) (fc/complex 0 0)]]}
            result-state (channel/apply-single-qubit-kraus-operator initial-state pauli-x-kraus 1)
            expected-state (qs/computational-basis-state 2 [0 1])]
        (is (> (state-fidelity expected-state result-state) 0.999)
            "Should apply operation to correct qubit in multi-qubit system")))))

(deftest test-apply-quantum-channel
  (testing "Quantum channel application"
    (testing "single Kraus operator"
      (let [initial-state (qs/zero-state 1)
            kraus-ops [{:matrix [[(fc/complex 0 0) (fc/complex 1.0 0)]
                                 [(fc/complex 1.0 0) (fc/complex 0 0)]]}]
            result-state (channel/apply-quantum-channel initial-state kraus-ops 0)
            expected-state (qs/one-state)]
        (is (> (state-fidelity expected-state result-state) 0.999)
            "Single Kraus operator should be applied directly")))
    
    (testing "multiple Kraus operators probability selection"
      (let [initial-state (qs/zero-state 1)
            kraus-ops (channel/depolarizing-kraus-operators 0.0) ; No noise - should always select identity
            results (repeatedly 100 #(channel/apply-quantum-channel initial-state kraus-ops 0))
            fidelities (map #(state-fidelity initial-state %) results)]
        (is (every? #(> % 0.999) fidelities)
            "With zero noise, should always preserve state")))
    
    (testing "depolarizing channel behavior"
      (let [initial-state (qs/zero-state 1)
            kraus-ops (channel/depolarizing-kraus-operators 0.6) ; 60% noise
            results (repeatedly 1000 #(channel/apply-quantum-channel initial-state kraus-ops 0))
            fidelities (map #(state-fidelity initial-state %) results)
            avg-fidelity (/ (reduce + fidelities) (count fidelities))]
        (is (< avg-fidelity 0.65) ; Allow some margin above theoretical (0.55)
            "High noise should significantly reduce average fidelity")
        (is (> avg-fidelity 0.45) ; Allow some margin below theoretical
            "Fidelity should not drop too far below theoretical expectation")))))

;;
;; Tests for decoherence calculations
;;

(deftest test-calculate-decoherence-params
  (testing "Decoherence parameter calculation"
    (testing "short gate time relative to coherence times"
      (let [t1 1000.0 ; 1ms
            t2 500.0  ; 0.5ms
            gate-time 10.0 ; 10ns
            params (channel/calculate-decoherence-params t1 t2 gate-time)]
        (is (< (:gamma-1 params) 0.001) "γ₁ should be small for short gate times")
        (is (< (:gamma-2 params) 0.001) "γ₂ should be small for short gate times")))
    
    (testing "long gate time relative to coherence times"
      (let [t1 1.0    ; 1μs
            t2 0.5    ; 0.5μs
            gate-time 1000.0 ; 1μs
            params (channel/calculate-decoherence-params t1 t2 gate-time)]
        (is (> (:gamma-1 params) 0.5) "γ₁ should be significant for long gate times")
        (is (> (:gamma-2 params) 0.8) "γ₂ should be significant for long gate times")))))

;;
;; Tests for channel composition and utilities
;;

(deftest test-compose-channels
  (testing "Channel composition"
    (let [depol-channel (channel/depolarizing-kraus-operators 0.1)
          amp-channel (channel/amplitude-damping-kraus-operators 0.1)
          composed (channel/compose-channels depol-channel amp-channel)
          initial-state (qs/zero-state 1)
          result (composed initial-state 0)]
      (is (some? result) "Composed channel should produce a result")
      (is (= 1 (:num-qubits result)) "Result should preserve qubit count"))))

(deftest test-channel-fidelity
  (testing "Channel fidelity calculation"
    (testing "identical states"
      (let [state1 (qs/zero-state 1)
            state2 (qs/zero-state 1)
            fidelity (channel/channel-fidelity state1 state2)]
        (is (util/approx= 1.0 fidelity) "Identical states should have fidelity 1")))
    
    (testing "orthogonal states"
      (let [state1 (qs/zero-state 1)
            state2 (qs/one-state)
            fidelity (channel/channel-fidelity state1 state2)]
        (is (util/approx= 0.0 fidelity) "Orthogonal states should have fidelity 0")))
    
    (testing "superposition states"
      (let [state1 (qs/zero-state 1)
            state2 (qs/plus-state)
            fidelity (channel/channel-fidelity state1 state2)]
        (is (util/approx= 0.5 fidelity) "Should have expected fidelity with superposition")))))

;; Rich comment block for REPL experimentation
(comment
  ;; Test depolarizing channel behavior
  (let [state (qs/zero-state 1)
        channel-ops (channel/depolarizing-kraus-operators 0.3)
        results (repeatedly 100 #(channel/apply-quantum-channel state channel-ops 0))
        fidelities (map #(channel/channel-fidelity state %) results)]
    (println "Average fidelity with 30% depolarizing noise:" 
             (/ (reduce + fidelities) (count fidelities))))
  
  ;; Test amplitude damping on excited state
  (let [excited-state (qs/one-state)
        damping-ops (channel/amplitude-damping-kraus-operators 0.5)
        results (repeatedly 100 #(channel/apply-quantum-channel excited-state damping-ops 0))
        ground-overlaps (map #(channel/channel-fidelity (qs/zero-state 1) %) results)]
    (println "Average ground state overlap after 50% amplitude damping:"
             (/ (reduce + ground-overlaps) (count ground-overlaps))))
  
  ;; Test coherent error accumulation
  (let [state (qs/plus-state)
        rotation-op (channel/coherent-error-kraus-operator 0.1 :z)
        accumulated-state (reduce (fn [s _] 
                                    (channel/apply-single-qubit-kraus-operator s rotation-op 0))
                                  state
                                  (range 10))]
    (println "Fidelity after 10 coherent rotations:"
             (channel/channel-fidelity state accumulated-state))))
