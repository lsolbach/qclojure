(ns org.soulspace.qclojure.application.noise-test
  "Comprehensive tests for quantum noise application functions."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.application.noise :as noise]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.util.test :as util]))

;; Helper functions for testing
(defn close-to?
  "Check if two numbers are approximately equal within tolerance."
  ([a b] (close-to? a b 1e-10))
  ([a b tolerance]
   (< (m/abs (- a b)) tolerance)))

(defn state-norm
  "Calculate the norm of a quantum state."
  [state]
  (let [amplitudes (:state-vector state)]
    (m/sqrt (reduce + (map #(* (fc/abs %) (fc/abs %)) amplitudes)))))

(defn states-approximately-equal?
  "Check if two quantum states are approximately equal."
  ([state1 state2] (states-approximately-equal? state1 state2 1e-10))
  ([state1 state2 tolerance]
   (let [sv1 (:state-vector state1)
         sv2 (:state-vector state2)]
     (and (= (:num-qubits state1) (:num-qubits state2))
          (= (count sv1) (count sv2))
          (every? #(close-to? (fc/abs (first %)) (fc/abs (second %)) tolerance)
                  (map vector sv1 sv2))))))

;; Test fixtures and data
(def test-noise-config
  {:noise-type :depolarizing
   :noise-strength 0.01
   :t1-time 100.0
   :t2-time 50.0
   :gate-time 20.0})

(def test-readout-config
  {:prob-0-to-1 0.05
   :prob-1-to-0 0.03
   :correlated-errors {0 1.2 1 0.8}})

(def test-noise-model
  {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.001
                    :t1-time 125.0 :t2-time 89.0 :gate-time 35.6}
                :x {:noise-type :amplitude-damping :noise-strength 0.0005
                    :t1-time 125.0 :t2-time 89.0 :gate-time 35.6}
                :cnot {:noise-type :depolarizing :noise-strength 0.006
                       :t1-time 125.0 :t2-time 89.0 :gate-time 476.0}}
   :readout-error {:prob-0-to-1 0.013 :prob-1-to-0 0.028}})

;; Tests for gate noise application
(deftest test-apply-gate-noise
  (testing "Gate noise application with different noise types"
    (testing "depolarizing noise"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.1}
            result-state (noise/apply-gate-noise initial-state h-gate noise-config)]
        
        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (vector? (:state-vector result-state)) "Should have valid state vector")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))
    
    (testing "amplitude damping noise"
      (let [initial-state (qs/one-state)
            x-gate {:operation-type :x :operation-params {:target 0}}
            noise-config {:noise-type :amplitude-damping 
                          :noise-strength 0.1 
                          :t1-time 100.0 
                          :gate-time 20.0}
            result-state (noise/apply-gate-noise initial-state x-gate noise-config)]
        
        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))
    
    (testing "phase damping noise"
      (let [initial-state (qs/plus-state)
            z-gate {:operation-type :z :operation-params {:target 0}}
            noise-config {:noise-type :phase-damping 
                          :noise-strength 0.1
                          :t2-time 50.0
                          :gate-time 20.0}
            result-state (noise/apply-gate-noise initial-state z-gate noise-config)]
        
        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))
    
    (testing "coherent error noise"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :coherent
                          :coherent-error {:rotation-angle 0.01 :rotation-axis :z}}
            result-state (noise/apply-gate-noise initial-state h-gate noise-config)]
        
        (is (= (:num-qubits result-state) 1) "Should preserve qubit count")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))
    
    (testing "no noise case"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :unknown :noise-strength 0.1}
            result-state (noise/apply-gate-noise initial-state h-gate noise-config)
            expected-state (qc/apply-operation-to-state initial-state h-gate)]
        
        (is (states-approximately-equal? result-state expected-state 1e-10)
            "Unknown noise type should apply no noise")))))

;; Tests for readout noise application
(deftest test-apply-readout-noise
  (testing "Readout noise application"
    (testing "basic readout errors"
      (let [clean-results {"00" 500 "11" 500}
            readout-config {:prob-0-to-1 0.1 :prob-1-to-0 0.05}
            noisy-results (noise/apply-readout-noise clean-results readout-config)]
        
        (is (map? noisy-results) "Should return a map")
        (is (= (reduce + (vals noisy-results)) 1000) "Should preserve total count")
        (is (every? string? (keys noisy-results)) "Keys should be bitstrings")))
    
    (testing "correlated readout errors"
      (let [clean-results {"000" 1000}
            readout-config {:prob-0-to-1 0.1 
                            :prob-1-to-0 0.05
                            :correlated-errors {0 1.5 1 0.5 2 1.0}}
            noisy-results (noise/apply-readout-noise clean-results readout-config)]
        
        (is (map? noisy-results) "Should return a map")
        (is (= (reduce + (vals noisy-results)) 1000) "Should preserve total count")
        (is (>= (count noisy-results) 1) "Should have at least one result")))
    
    (testing "empty results"
      (let [clean-results {}
            readout-config {:prob-0-to-1 0.1 :prob-1-to-0 0.05}
            noisy-results (noise/apply-readout-noise clean-results readout-config)]
        
        (is (= noisy-results {}) "Empty results should remain empty")))))

;; Tests for circuit fidelity estimation
(deftest test-estimate-circuit-fidelity
  (testing "Circuit fidelity estimation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0)
                        (qc/cnot-gate 0 1))
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]
        
        (is (contains? fidelity-data :estimated-fidelity) "Should contain fidelity estimate")
        (is (contains? fidelity-data :total-estimated-error) "Should contain total error")
        (is (contains? fidelity-data :gate-counts) "Should contain gate counts")
        (is (contains? fidelity-data :dominant-error-sources) "Should contain error sources")
        (is (> (:estimated-fidelity fidelity-data) 0.9) "Should have high fidelity for simple circuit")
        (is (< (:total-estimated-error fidelity-data) 0.1) "Should have low total error")))
    
    (testing "complex circuit"
      (let [circuit (-> (qc/create-circuit 3)
                        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)
                        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2) (qc/cnot-gate 0 2))
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]
        
        (is (< (:estimated-fidelity fidelity-data) 
               (:estimated-fidelity (noise/estimate-circuit-fidelity (qc/create-circuit 1) test-noise-model)))
            "Complex circuit should have lower fidelity than simple circuit")))
    
    (testing "empty circuit"
      (let [circuit (qc/create-circuit 1)
            fidelity-data (noise/estimate-circuit-fidelity circuit test-noise-model)]
        
        (is (= (:estimated-fidelity fidelity-data) 1.0) "Empty circuit should have perfect fidelity")
        (is (= (:total-estimated-error fidelity-data) 0.0) "Empty circuit should have no error")))))

;; Tests for platform comparison
(deftest test-compare-hardware-platforms
  (testing "Hardware platform comparison"
    (let [circuit (qc/ghz-state-circuit 3)
          platforms {:ibm-test {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.005}
                                             :cnot {:noise-type :depolarizing :noise-strength 0.01}}
                                :readout-error {:prob-0-to-1 0.02 :prob-1-to-0 0.03}}
                     :ionq-test {:gate-noise {:h {:noise-type :coherent :coherent-error {:rotation-angle 0.001 :rotation-axis :y}}
                                              :cnot {:noise-type :depolarizing :noise-strength 0.003}}
                                 :readout-error {:prob-0-to-1 0.001 :prob-1-to-0 0.002}}}
          comparison (noise/compare-hardware-platforms circuit platforms)]
      
      (is (map? comparison) "Should return comparison map")
      (is (contains? comparison :ibm-test) "Should contain IBM test platform")
      (is (contains? comparison :ionq-test) "Should contain IonQ test platform")
      
      (let [ibm-data (:ibm-test comparison)
            ionq-data (:ionq-test comparison)]
        (is (contains? ibm-data :estimated-fidelity) "Should contain fidelity for IBM")
        (is (contains? ionq-data :estimated-fidelity) "Should contain fidelity for IonQ")
        (is (contains? ibm-data :platform-type) "Should identify platform type")
        (is (= :superconducting (:platform-type ibm-data)) "Should identify IBM as superconducting")
        (is (= :trapped-ion (:platform-type ionq-data)) "Should identify IonQ as trapped ion")))))

;; Tests for utility functions
(deftest test-noise-aware-circuit-depth
  (testing "Noise-aware circuit depth calculation"
    (testing "simple circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0)
                        (qc/cnot-gate 0 1))
            depth (noise/noise-aware-circuit-depth circuit test-noise-model)]
        
        (is (number? depth) "Should return a number")
        (is (> depth 2) "Should be greater than basic gate count due to noise penalty")))
    
    (testing "empty circuit"
      (let [circuit (qc/create-circuit 1)
            depth (noise/noise-aware-circuit-depth circuit test-noise-model)]
        
        (is (= depth 0) "Empty circuit should have zero depth")))))

(deftest test-recommend-error-mitigation
  (testing "Error mitigation recommendations"
    (testing "high fidelity circuit"
      (let [circuit (-> (qc/create-circuit 1) (qc/h-gate 0))
            low-noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.0001}}}
            recommendations (noise/recommend-error-mitigation circuit low-noise-model)]
        
        (is (vector? recommendations) "Should return vector of recommendations")
        (is (every? map? recommendations) "Each recommendation should be a map")))
    
    (testing "low fidelity circuit"
      (let [circuit (-> (qc/create-circuit 2)
                        (qc/h-gate 0) (qc/cnot-gate 0 1)
                        (qc/h-gate 0) (qc/cnot-gate 0 1))
            high-noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.1}
                                           :cnot {:noise-type :depolarizing :noise-strength 0.2}}}
            recommendations (noise/recommend-error-mitigation circuit high-noise-model)]
        
        (is (vector? recommendations) "Should return vector of recommendations")
        (is (pos? (count recommendations)) "Should have some recommendations for noisy circuit")))
    
    (testing "deep circuit"
      (let [circuit (reduce #(qc/h-gate %1 (mod %2 2)) (qc/create-circuit 2) (range 25))
            recommendations (noise/recommend-error-mitigation circuit test-noise-model)]
        
        (is (some #(= (:strategy %) :circuit-cutting) recommendations)
            "Should recommend circuit cutting for deep circuits")))))

;; Edge cases and error handling
(deftest test-edge-cases
  (testing "Edge cases and error handling"
    (testing "zero noise strength"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.0}
            result-state (noise/apply-gate-noise initial-state h-gate noise-config)
            expected-state (qc/apply-operation-to-state initial-state h-gate)]
        
        (is (states-approximately-equal? result-state expected-state 1e-10)
            "Zero noise should be equivalent to no noise")))
    
    (testing "maximum valid noise strength"
      (let [initial-state (qs/zero-state 1)
            h-gate {:operation-type :h :operation-params {:target 0}}
            noise-config {:noise-type :depolarizing :noise-strength 0.75} ; Max for depolarizing
            result-state (noise/apply-gate-noise initial-state h-gate noise-config)]
        
        (is (= (:num-qubits result-state) 1) "Should handle maximum noise strength")
        (is (util/approx= (state-norm result-state) 1.0 1e-10) "Should maintain normalization")))
    
    (testing "circuit with unsupported gate types"
      (let [circuit {:num-qubits 1 :operations [{:operation-type :custom-gate :operation-params {:target 0}}]}
            noise-model {:gate-noise {:h {:noise-type :depolarizing :noise-strength 0.01}}}
            fidelity-data (noise/estimate-circuit-fidelity circuit noise-model)]
        
        (is (= (:estimated-fidelity fidelity-data) 1.0) "Unsupported gates should contribute no error")
        (is (= (:total-estimated-error fidelity-data) 0.0) "Should handle unsupported gates gracefully")))))

(comment
  (run-tests)
  ;
  )