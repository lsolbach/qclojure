(ns org.soulspace.qclojure.adapter.backend.noisy-simulator-test
  "Comprehensive tests for the local noisy simulator backend."
  (:require [clojure.test :refer [deftest is testing use-fixtures run-tests]]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.adapter.backend.noisy-simulator :as noisy]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]))

;; Test fixtures
(defn reset-simulator-state-fixture [f]
  "Reset noisy simulator state before each test."
  (noisy/reset-simulator-state!)
  (f))

(use-fixtures :each reset-simulator-state-fixture)

;; Helper functions for testing
(defn complex-magnitude-squared
  "Calculate |z|² for a complex number represented as [real imag] or fastmath complex."
  [z]
  (cond 
    ;; Handle fastmath complex numbers
    (qs/complex? z)
    (+ (* (fc/re z) (fc/re z)) (* (fc/im z) (fc/im z)))
    
    ;; Handle Clojure vectors [real imag]
    (vector? z)
    (let [[real imag] z]
      (+ (* real real) (* imag imag)))
    
    ;; Handle regular numbers (treat as real)
    (number? z)
    (* z z)
    
    :else
    (throw (IllegalArgumentException. (str "Unsupported complex number format: " (type z))))))

(defn approx=
  "Check if two numbers are approximately equal within tolerance."
  ([a b] (approx= a b 1e-10))
  ([a b tolerance]
   (< (m/abs (- a b)) tolerance)))

(defn state-fidelity
  "Calculate fidelity between two quantum states."
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

;; Helper function to get the maximum coefficient magnitude squared from a matrix
(defn max-coeff-magnitude-squared [matrix]
  (apply max (map (fn [row]
                    (apply max (map (fn [coeff]
                                      (+ (* (fc/re coeff) (fc/re coeff)) 
                                         (* (fc/im coeff) (fc/im coeff))))
                                    row)))
                  matrix)))

;; Tests for Kraus operator generation
(deftest test-depolarizing-kraus-operators
  (testing "Depolarizing Kraus operators"
    (testing "with zero noise"
      (let [kraus-ops (noisy/depolarizing-kraus-operators 0.0)
            identity-op (first kraus-ops)]
        (is (= 4 (count kraus-ops)) "Should have 4 Kraus operators")
        (is (approx= 1.0 (complex-magnitude-squared (first (first (:matrix identity-op)))))
            "Identity operator should have coefficient 1")
        (is (every? #(approx= 0.0 (max-coeff-magnitude-squared (:matrix %)))
                    (rest kraus-ops))
            "Pauli operators should have coefficient 0")))
    
    (testing "with small noise"
      (let [p 0.1
            kraus-ops (noisy/depolarizing-kraus-operators p)
            identity-coeff-sq (max-coeff-magnitude-squared (:matrix (first kraus-ops)))
            pauli-coeff-sq (max-coeff-magnitude-squared (:matrix (second kraus-ops)))]
        (is (approx= (- 1.0 p) identity-coeff-sq 1e-10) "Identity coefficient should be √(1-p)")
        (is (approx= (/ p 3.0) pauli-coeff-sq 1e-10) "Pauli coefficients should be √(p/3)")))
    
    (testing "completeness relation"
      (let [p 0.2
            kraus-ops (noisy/depolarizing-kraus-operators p)
            sum-coeffs-sq (reduce + (map #(max-coeff-magnitude-squared (:matrix %)) kraus-ops))]
        (is (approx= 1.0 sum-coeffs-sq 1e-10) "Sum of |coefficients|² should equal 1")))))

(deftest test-amplitude-damping-kraus-operators
  (testing "Amplitude damping Kraus operators"
    (testing "with zero damping"
      (let [kraus-ops (noisy/amplitude-damping-kraus-operators 0.0)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (= 2 (count kraus-ops)) "Should have 2 Kraus operators")
        (is (approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (approx= 1.0 (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be 1")
        (is (every? #(approx= 0.0 (complex-magnitude-squared %))
                    (flatten k1))
            "K₁ should be zero matrix")))
    
    (testing "with full damping"
      (let [kraus-ops (noisy/amplitude-damping-kraus-operators 1.0)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (approx= 0.0 (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be 0")
        (is (approx= 1.0 (complex-magnitude-squared (second (first k1))))
            "K₁[0,1] should be 1")))))

(deftest test-phase-damping-kraus-operators
  (testing "Phase damping Kraus operators"
    (testing "preserve population"
      (let [gamma 0.3
            kraus-ops (noisy/phase-damping-kraus-operators gamma)
            k0 (:matrix (first kraus-ops))
            k1 (:matrix (second kraus-ops))]
        (is (= 2 (count kraus-ops)) "Should have 2 Kraus operators")
        (is (approx= 1.0 (complex-magnitude-squared (first (first k0))))
            "K₀[0,0] should be 1")
        (is (approx= (- 1.0 gamma) (complex-magnitude-squared (second (second k0))))
            "K₀[1,1] should be √(1-γ)")
        (is (approx= gamma (complex-magnitude-squared (second (second k1))))
            "K₁[1,1] should be √γ")))))

(deftest test-coherent-error-kraus-operator
  (testing "Coherent error Kraus operators"
    (testing "X rotation"
      (let [angle (/ m/PI 4) ; 45 degrees
            kraus-op (noisy/coherent-error-kraus-operator angle :x)
            matrix (:matrix kraus-op)
            cos-half (m/cos (/ angle 2))]
        (is (approx= cos-half (first (first (first matrix))))
            "Matrix[0,0] should be cos(θ/2)")
        (is (approx= cos-half (first (second (second matrix))))
            "Matrix[1,1] should be cos(θ/2)")))
    
    (testing "Z rotation"
      (let [angle (/ m/PI 6) ; 30 degrees
            kraus-op (noisy/coherent-error-kraus-operator angle :z)
            matrix (:matrix kraus-op)]
        (is (approx= (m/cos angle) (first (first (first matrix))))
            "Matrix[0,0] should be cos(θ)")
        (is (approx= (m/cos (- angle)) (first (second (second matrix))))
            "Matrix[1,1] should be cos(-θ)")))))

;; Tests for quantum channel application
(deftest test-apply-single-qubit-kraus-operator
  (testing "Single qubit Kraus operator application"
    (testing "identity operation"
      (let [initial-state (qs/plus-state)
            identity-kraus {:matrix qg/pauli-i}
            result-state (noisy/apply-single-qubit-kraus-operator initial-state identity-kraus 0)]
        (is (> (state-fidelity initial-state result-state) 0.999)
            "Identity operation should preserve state")))
    
    (testing "Pauli-X operation"
      (let [zero-state (qs/zero-state 1)
            one-state (qs/one-state)
            pauli-x-kraus {:matrix qg/pauli-x}
            result-state (noisy/apply-single-qubit-kraus-operator zero-state pauli-x-kraus 0)]
        (is (> (state-fidelity one-state result-state) 0.999)
            "Pauli-X should flip |0⟩ to |1⟩")))
    
    (testing "multi-qubit state"
      (let [initial-state (qs/zero-state 2)
            pauli-x-kraus {:matrix qg/pauli-x}
            result-state (noisy/apply-single-qubit-kraus-operator initial-state pauli-x-kraus 1)
            expected-state (qs/computational-basis-state 2 [0 1])]
        (is (> (state-fidelity expected-state result-state) 0.999)
            "Should apply operation to correct qubit in multi-qubit system")))))

(deftest test-apply-quantum-channel
  (testing "Quantum channel application"
    (testing "single Kraus operator"
      (let [initial-state (qs/zero-state 1)
            kraus-ops [{:matrix qg/pauli-x}]
            result-state (noisy/apply-quantum-channel initial-state kraus-ops 0)
            expected-state (qs/one-state)]
        (is (> (state-fidelity expected-state result-state) 0.999)
            "Single Kraus operator should be applied directly")))
    
    (testing "multiple Kraus operators probability selection"
      (let [initial-state (qs/zero-state 1)
            kraus-ops (noisy/depolarizing-kraus-operators 0.0) ; No noise - should always select identity
            results (repeatedly 100 #(noisy/apply-quantum-channel initial-state kraus-ops 0))
            fidelities (map #(state-fidelity initial-state %) results)]
        (is (every? #(> % 0.999) fidelities)
            "With zero noise, should always preserve state")))
    
    (testing "depolarizing channel behavior"
      (let [initial-state (qs/zero-state 1)
            kraus-ops (noisy/depolarizing-kraus-operators 0.8) ; High noise
            results (repeatedly 1000 #(noisy/apply-quantum-channel initial-state kraus-ops 0))
            fidelities (map #(state-fidelity initial-state %) results)
            avg-fidelity (/ (reduce + fidelities) (count fidelities))]
        (is (< avg-fidelity 0.9) "High depolarizing noise should reduce average fidelity")
        (is (> avg-fidelity 0.1) "Should not completely destroy the state")))))

;; Tests for decoherence calculations
(deftest test-calculate-decoherence-params
  (testing "Decoherence parameter calculation"
    (testing "short gate time"
      (let [t1 100.0 ; μs
            t2 50.0  ; μs
            gate-time 100.0 ; ns = 0.1 μs
            params (noisy/calculate-decoherence-params t1 t2 gate-time)]
        (is (< (:gamma-1 params) 0.01) "Gamma-1 should be small for short gate time")
        (is (< (:gamma-2 params) 0.01) "Gamma-2 should be small for short gate time")))
    
    (testing "long gate time"
      (let [t1 10.0   ; μs
            t2 5.0    ; μs
            gate-time 5000.0 ; ns = 5 μs
            params (noisy/calculate-decoherence-params t1 t2 gate-time)]
        (is (> (:gamma-1 params) 0.3) "Gamma-1 should be significant for long gate time")
        (is (> (:gamma-2 params) 0.6) "Gamma-2 should be significant for long gate time")))))

;; Tests for simulator creation and basic functionality
(deftest test-create-noisy-simulator
  (testing "Simulator creation"
    (testing "with noise model"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))]
        (is (satisfies? qb/QuantumBackend simulator) "Should implement QuantumBackend protocol")
        (is (qb/is-available? simulator) "Should be available")
        (is (contains? (qb/get-supported-gates simulator) :h) "Should support H gate")
        (is (contains? (qb/get-supported-gates simulator) :cnot) "Should support CNOT gate")))
    
    (testing "backend info"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))
            info (qb/get-backend-info simulator)]
        (is (= :advanced-noisy-simulator (:backend-type info)) "Should have correct backend type")
        (is (= (noisy/noise-model-for :ibm-lagos) (:noise-model info)) "Should contain noise model")
        (is (contains? (:capabilities info) :depolarizing-noise) "Should support depolarizing noise")
        (is (contains? (:capabilities info) :readout-errors) "Should support readout errors")))))

;; Tests for circuit execution
(deftest test-circuit-execution
  (testing "Circuit execution with noise"
    (testing "single qubit circuit"
      (let [simulator (noisy/create-noisy-simulator {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.01}}})
            circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            job-id (qb/submit-circuit simulator circuit {:shots 100})]
        (is (string? job-id) "Should return job ID")

        ; Wait for completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should complete")

        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Result should be completed")
          (is (= 100 (:shots-executed result)) "Should execute correct number of shots")
          (is (true? (:noise-applied result)) "Should indicate noise was applied")
          (is (map? (:measurement-results result)) "Should contain measurement results")
          (is (= 100 (reduce + (vals (:measurement-results result)))) "Shot counts should sum to total"))))

    (testing "GHZ circuit with realistic noise"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))
            circuit (qc/ghz-state-circuit 3)
            job-id (qb/submit-circuit simulator circuit {:shots 1000})]

        ; FIXME flaky test, sometimes fails
        ; Wait for completion
        (Thread/sleep 1000)
        (let [result (qb/get-job-result simulator job-id)
              measurements (:measurement-results result)
              ideal-states (+ (get measurements "000" 0) (get measurements "111" 0))
              total-shots (reduce + (vals measurements))
              ideal-percentage (/ ideal-states total-shots)]

          (is (> ideal-percentage 0.85) "Should preserve most entanglement with realistic noise")
          (is (< ideal-percentage 1.0) "Should show some noise effects")
          (is (>= (count measurements) 2) "Should have at least ideal states")
          (is (<= (count measurements) 8) "Should not exceed possible outcomes"))))))

;; Tests for job management
(deftest test-job-management
  (testing "Job management functionality"
    (testing "job status tracking"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 10})]
        
        (is (contains? #{:queued :running :completed} (qb/get-job-status simulator job-id))
            "Job should have valid status")
        
        ; Wait and check completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should complete")))
    
    (testing "queue status"
      (let [simulator (noisy/create-noisy-simulator {})
            queue-status (qb/get-queue-status simulator)]
        (is (map? queue-status) "Should return queue status map")
        (is (contains? queue-status :total-jobs) "Should contain total jobs")
        (is (contains? queue-status :active-jobs) "Should contain active jobs")
        (is (contains? queue-status :completed-jobs) "Should contain completed jobs")))
    
    (testing "nonexistent job"
      (let [simulator (noisy/create-noisy-simulator {})]
        (is (= :not-found (qb/get-job-status simulator "fake-job-id")) "Should return not-found")
        (is (= :not-found (:job-status (qb/get-job-result simulator "fake-job-id")))
            "Should return not-found for result")))))

;; Tests for utility functions
(deftest test-utility-functions
  (testing "Simulator state management"
    (testing "reset state"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            _ (qb/submit-circuit simulator circuit {:shots 10})
            initial-stats (noisy/get-simulator-stats)
            _ (noisy/reset-simulator-state!)
            reset-stats (noisy/get-simulator-stats)]
        
        (is (> (:total-jobs initial-stats) 0) "Should have jobs before reset")
        (is (= 0 (:total-jobs reset-stats)) "Should have no jobs after reset")))
    
    (testing "cleanup completed jobs"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 5})]
        
        ; Wait for completion
        (Thread/sleep 100)
        (is (= :completed (qb/get-job-status simulator job-id)) "Job should be completed")
        
        ; Cleanup with very short max age (should remove the job)
        (let [removed-count (noisy/cleanup-completed-jobs! 1)]
          (is (>= removed-count 0) "Should return number of removed jobs"))))))

;; Tests for circuit fidelity estimation
(deftest test-circuit-fidelity-estimation
  (testing "Circuit fidelity estimation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2) (qc/h-gate 0) (qc/cnot-gate 0 1))
            fidelity-data (noisy/estimate-circuit-fidelity circuit (noisy/noise-model-for :ibm-lagos))]
        
        (is (contains? fidelity-data :estimated-fidelity) "Should contain fidelity estimate")
        (is (contains? fidelity-data :total-estimated-error) "Should contain total error")
        (is (contains? fidelity-data :gate-counts) "Should contain gate counts")
        (is (> (:estimated-fidelity fidelity-data) 0.99) "Should have high fidelity for simple circuit")
        (is (< (:total-estimated-error fidelity-data) 0.01) "Should have low total error")))
    
    (testing "complex circuit"
      (let [circuit (-> (qc/create-circuit 3)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)
                        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2) (qc/cnot-gate 0 2)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2))
            fidelity-data (noisy/estimate-circuit-fidelity circuit (noisy/noise-model-for :ibm-lagos))]
        
        (is (< (:estimated-fidelity fidelity-data) 
               (:estimated-fidelity (noisy/estimate-circuit-fidelity (qc/create-circuit 1) (noisy/noise-model-for :ibm-lagos))))
            "Complex circuit should have lower fidelity than simple circuit")))))

;; Tests for platform comparison
(deftest test-platform-comparison
  (testing "Hardware platform comparison"
    (let [circuit (qc/ghz-state-circuit 3)
          platforms {:ibm-lagos (noisy/noise-model-for :ibm-lagos)
                     :rigetti-aspen (noisy/noise-model-for :rigetti-aspen)}
          comparison (noisy/compare-hardware-platforms circuit platforms)]
      
      (is (map? comparison) "Should return comparison map")
      (is (contains? comparison :ibm-lagos) "Should contain IBM Lagos")
      (is (contains? comparison :rigetti-aspen) "Should contain Rigetti Aspen")
      
      (let [ibm-data (:ibm-lagos comparison)
            rigetti-data (:rigetti-aspen comparison)]
        (is (contains? ibm-data :estimated-fidelity) "Should contain fidelity for IBM")
        (is (contains? rigetti-data :estimated-fidelity) "Should contain fidelity for Rigetti")
        (is (contains? ibm-data :platform-type) "Should identify platform type")
        (is (= :superconducting (:platform-type ibm-data)) "Should identify IBM as superconducting")))))

;; Edge cases and error handling
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "empty circuit"
      (let [simulator (noisy/create-noisy-simulator (noisy/noise-model-for :ibm-lagos))
            circuit (qc/create-circuit 1) ; Empty circuit
            job-id (qb/submit-circuit simulator circuit {:shots 10})]
        
        (Thread/sleep 100)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Empty circuit should execute successfully")
          (is (= 10 (reduce + (vals (:measurement-results result)))) "Should measure all shots"))))
    
    (testing "invalid noise parameters"
      ; Test that very high noise parameters don't break the simulator
      (let [simulator (noisy/create-noisy-simulator 
                        {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.7}}}) ; High but valid
            circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            job-id (qb/submit-circuit simulator circuit {:shots 100})]
        
        (Thread/sleep 100)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Should handle high noise parameters")
          (is (= 100 (reduce + (vals (:measurement-results result)))) "Should count all shots"))))
    
    (testing "zero shots"
      (let [simulator (noisy/create-noisy-simulator {})
            circuit (qc/ghz-state-circuit 2)
            job-id (qb/submit-circuit simulator circuit {:shots 0})]
        
        (Thread/sleep 50)
        (let [result (qb/get-job-result simulator job-id)]
          (is (= :completed (:job-status result)) "Should handle zero shots")
          (is (= 0 (:shots-executed result)) "Should execute zero shots"))))))

(comment
  (run-tests)
  ;
  )