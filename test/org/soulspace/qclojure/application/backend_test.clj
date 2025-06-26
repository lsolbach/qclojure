(ns org.soulspace.qclojure.application.backend-test
  "Tests for quantum backend protocol and gate support functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.operation-registry :as gr]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

(deftest test-operation-registry
  (testing "Operation catalog validation"
    (is (contains? gr/operation-catalog :x))
    (is (contains? gr/operation-catalog :cnot))
    (is (contains? gr/operation-catalog :h))
    (is (contains? gr/operation-catalog :rx)))
  
  (testing "Gate set validation"
    (is (gr/validate-gate-set #{:x :y :z}))
    (is (not (gr/validate-gate-set #{:unknown-gate}))))
  
  (testing "Gate information retrieval"
    (let [x-gate-info (gr/get-gate-info :x)]
      (is (= :x (:operation-id x-gate-info)))
      (is (= :single-qubit (:operation-type x-gate-info))))
    
    (is (nil? (gr/get-gate-info :nonexistent-gate))))
  
  (testing "Gate categorization"
    (let [single-qubit-gates (gr/get-gates-by-type :single-qubit)
          two-qubit-gates (gr/get-gates-by-type :two-qubit)]
      (is (contains? single-qubit-gates :x))
      (is (contains? single-qubit-gates :h))
      (is (contains? two-qubit-gates :cnot))
      (is (not (contains? single-qubit-gates :cnot)))))
  
  (testing "Hardware-specific native gate identification"
    (let [ionq-gates (gr/get-native-gates-for-hardware :braket-ionq)
          rigetti-gates (gr/get-native-gates-for-hardware :braket-rigetti)]
      (is (contains? ionq-gates :rx))
      (is (contains? ionq-gates :cnot))
      (is (contains? rigetti-gates :h))
      (is (contains? rigetti-gates :cz))))
  
  (testing "Gate set expansion"
    (let [minimal-set #{:s}
          expanded (gr/expand-gate-set minimal-set)]
      ;; S gate decomposes to RZ, so expanded set should include both
      (is (contains? expanded :s))
      (is (contains? expanded :rz)))))

(deftest test-simulator-backend
  (testing "Simulator creation and gate support"
    (let [simulator (sim/create-simulator)]
      (is (satisfies? qb/QuantumBackend simulator))
      
      (testing "Backend info"
        (let [info (qb/get-backend-info simulator)]
          (is (= :simulator (:backend-type info)))
          (is (string? (:backend-name info)))))
      
      (testing "Gate support"
        (let [supported-gates (qb/get-supported-gates simulator)]
          (is (set? supported-gates))
          (is (contains? supported-gates :x))
          (is (contains? supported-gates :cnot))
          (is (contains? supported-gates :h))
          (is (contains? supported-gates :rx))
          ;; Simulators should support comprehensive gate sets
          (is (>= (count supported-gates) 10)))))))

(deftest test-gate-support-utilities
  (let [simulator (sim/create-simulator)]
    
    (testing "Individual gate support checking"
      (is (qb/supports-gate? simulator :x))
      (is (qb/supports-gate? simulator :cnot)))
    
    (testing "Multiple gate support checking"
      (is (qb/supports-gates? simulator #{:x :y :z}))
      ;; Test with gates that might not be supported
      (let [required-gates #{:x :cnot :h}]
        (is (qb/supports-gates? simulator required-gates))))
    
    (testing "Backend filtering"
      (let [backends [simulator]
            required-gates #{:x :cnot}
            compatible (qb/filter-backends-by-gate-support backends required-gates)]
        (is (= 1 (count compatible))) ; Both should support basic gates
        
        ;; Test with more demanding requirements
        (let [demanding-gates #{:x :y :z :h :s :t :cnot :rx :ry :rz}
              demanding-compatible (qb/filter-backends-by-gate-support backends demanding-gates)]
          ;; Simulator should still be compatible, cloud backend might not
          (is (>= (count demanding-compatible) 1)))))
    
    (testing "Minimal backend selection"
      (let [backends [simulator]
            required-gates #{:x :cnot}
            minimal (qb/find-minimal-backend backends required-gates)]
        (is (not (nil? minimal)))
        (is (satisfies? qb/QuantumBackend minimal))))))

(deftest test-spec-validation
  (testing "Gate set specs"
    (is (s/valid? ::gr/operation-set #{:x :y :z}))
    (is (not (s/valid? ::gr/operation-set [:x :y :z]))) ; should be set, not vector
    (is (not (s/valid? ::gr/operation-set #{"x" "y"}))) ; should be keywords
    )
  
  (testing "Backend info specs"
    (let [valid-info {:backend-type :simulator
                      :backend-name "Test Backend"
                      :supported-gates #{:x :y :z}
                      :capabilities {}}]
      (is (s/valid? ::qb/backend-info valid-info)))
    
    (let [invalid-info {:backend-type :invalid-type
                        :backend-name "Test"}]
      (is (not (s/valid? ::qb/backend-info invalid-info))))))

(deftest test-circuit-transformer-integration
  (testing "Integration with circuit transformer"
    ;; Create a mock backend with limited gate support instead of using simulator
    (let [mock-backend (reify qb/QuantumBackend
                         (get-supported-gates [_] #{:x :h :cnot :rz})
                         (get-backend-info [_]
                           {:backend-type :simulator
                            :backend-name "Mock Backend"
                            :capabilities {:max-qubits 5}
                            :supported-gates #{:x :h :cnot :rz}})
                         (is-available? [_] true)
                         (submit-circuit [_ _ _] "mock-job-id")
                         (get-job-status [_ _] :completed)
                         (get-job-result [_ _] {})
                         (cancel-job [_ _] true)
                         (get-queue-status [_] {}))

          ;; Create a circuit with gates not directly supported
          circuit (-> (qc/create-circuit 2 "Test Circuit")
                      (qc/h-gate 0)
                      (qc/y-gate 1)  ; Y gate decomposition needed
                      (qc/s-gate 0)  ; S gate decomposition needed
                      (qc/cnot-gate 0 1))

          ;; Transform the circuit
          result (qb/transform-circuit-for-backend circuit mock-backend)]

      ;; Verify transformation was successful
      (is (:quantum-circuit result))
      (is (pos? (:transformed-operation-count result)))
      (is (empty? (:unsupported-operations result))))))

(comment
  ;; REPL examples for testing
  (run-tests)

  ;; Create backends and test gate support
  (def sim (sim/create-simulator))
  
  ;; Check what gates they support
  (qb/get-supported-gates sim)
  
  
  ;; Test specific gate support
  (qb/supports-gate? sim :toffoli)
  
  ;; Find which backend supports the most demanding requirements
  (let [demanding-gates #{:x :y :z :h :s :t :cnot :rx :ry :rz :toffoli}]
    (qb/filter-backends-by-gate-support [sim] demanding-gates)))

(comment
  (run-tests)
  ;
  )
