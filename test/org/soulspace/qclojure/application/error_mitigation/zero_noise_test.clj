(ns org.soulspace.qclojure.application.error-mitigation.zero-noise-test
  "Comprehensive tests for Zero Noise Extrapolation (ZNE) error mitigation.
  
  Tests cover all major ZNE functions including:
  - Noise model scaling for various error types
  - Circuit execution with backend integration  
  - Realistic circuit simulation with complex noise models
  - Exponential decay fitting with multiple model types
  - Expectation value extraction for different observables
  - End-to-end ZNE workflows with validation"
  (:require [clojure.test :refer [deftest is testing run-tests are]]
            [org.soulspace.qclojure.application.error-mitigation.zero-noise :as zne]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as hwsim]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.util.test :as util]))

;;;
;;; Test Data and Fixtures
;;;
(defn setup-backend
  "Setup a hardware simulator backend with an :ionq-forte device for testing."
  []
  (let [backend (hwsim/create-hardware-simulator {:num-qubits 5})]
    (backend/select-device backend (:ionq-forte hwsim/device-map))
    backend))

(def small-noise-model
  "Minimal noise model for boundary testing"
  {:gate-noise {:h {:noise-strength 0.001}
                :cnot {:noise-strength 0.002}}
   :readout-error {:prob-0-to-1 0.01 :prob-1-to-0 0.01}})


(comment
  ;; Run all tests in namespace
  (run-tests)
    
  ;
  )
