(ns org.soulspace.qclojure.domain.qubit-mapping-test
  "Tests for qubit mapping utilities."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.qubit-mapping :as mapping]))

;;;
;;; Tests for inversion functions
;;;
(deftest test-invert-simple-mapping
  (testing "Invert one-to-one qubit mapping"
    (is (= {0 0, 1 2, 2 4}
           (mapping/invert-simple-mapping {0 0, 2 1, 4 2})))
    (is (= {0 1, 2 0, 1 2}
           (mapping/invert-simple-mapping {1 0, 0 2, 2 1}))))
  
  (testing "Invert empty mapping"
    (is (= {} (mapping/invert-simple-mapping {}))))
  
  (testing "Invert nil mapping"
    (is (nil? (mapping/invert-simple-mapping nil)))))

(deftest test-invert-vector-mapping
  (testing "Invert one-to-many mapping"
    (is (= {0 0, 1 0, 2 0, 3 1, 4 1, 5 1}
           (mapping/invert-vector-mapping {0 [0 1 2], 1 [3 4 5]})))
    (is (= {0 0, 1 0, 2 0, 3 1, 4 1, 5 1, 6 2, 7 2, 8 2}
           (mapping/invert-vector-mapping {0 [0 1 2], 1 [3 4 5], 2 [6 7 8]}))))
  
  (testing "Invert empty mapping"
    (is (= {} (mapping/invert-vector-mapping {}))))
  
  (testing "Invert nil mapping"
    (is (nil? (mapping/invert-vector-mapping nil)))))

;;;
;;; Tests for composition
;;;
(deftest test-compose-mappings
  (testing "Compose two mappings"
    (is (= {0 100, 1 200}
           (mapping/compose-mappings {0 10, 1 20} {10 100, 20 200})))
    (is (= {0 0, 1 3, 2 6}
           (mapping/compose-mappings {0 0, 1 1, 2 2} {0 0, 1 3, 2 6}))))
  
  (testing "Compose with missing intermediate mapping preserves intermediate value"
    (is (= {0 10, 1 20}
           (mapping/compose-mappings {0 10, 1 20} {10 10, 30 30}))))
  
  (testing "Compose with nil mappings"
    (is (nil? (mapping/compose-mappings nil {10 100})))
    (is (nil? (mapping/compose-mappings {0 10} nil)))
    (is (nil? (mapping/compose-mappings nil nil)))))

;;;
;;; Tests for reverse mapping creation
;;;
(deftest test-create-reverse-qubit-mapping
  (testing "Create reverse mapping from qubit optimization"
    (let [ctx {:qubit-mapping {0 0, 2 1, 4 2}}
          result (mapping/create-reverse-qubit-mapping ctx)]
      (is (= {0 0, 1 2, 2 4} (:inverse-qubit-mapping result)))))
  
  (testing "No qubit mapping present"
    (let [ctx {}
          result (mapping/create-reverse-qubit-mapping ctx)]
      (is (nil? (:inverse-qubit-mapping result))))))

(deftest test-create-reverse-ec-mappings
  (testing "Create reverse mappings from error correction (one-to-many)"
    (let [ctx {:logical-to-physical {0 [0 1 2], 1 [3 4 5]}
               :logical-to-ancillas {0 [6 7], 1 [8 9]}}
          result (mapping/create-reverse-ec-mappings ctx)]
      (is (= {0 0, 1 0, 2 0, 3 1, 4 1, 5 1} (:physical-to-logical result)))
      (is (= {6 0, 7 0, 8 1, 9 1} (:ancilla-to-logical result)))))
  
  (testing "Create reverse mappings from topology optimization (one-to-one)"
    (let [ctx {:logical-to-physical {0 0, 1 2, 2 1}}
          result (mapping/create-reverse-ec-mappings ctx)]
      (is (= {0 0, 2 1, 1 2} (:physical-to-logical result)))))
  
  (testing "Mixed scenario: topology mapping with ancillas"
    (let [ctx {:logical-to-physical {0 1, 1 0}
               :logical-to-ancillas {0 [2 3], 1 [4 5]}}
          result (mapping/create-reverse-ec-mappings ctx)]
      (is (= {1 0, 0 1} (:physical-to-logical result)))
      (is (= {2 0, 3 0, 4 1, 5 1} (:ancilla-to-logical result)))))
  
  (testing "Empty mappings"
    (let [ctx {:logical-to-physical {}}
          result (mapping/create-reverse-ec-mappings ctx)]
      (is (= {} (:physical-to-logical result)))))
  
  (testing "No mappings present"
    (let [ctx {}
          result (mapping/create-reverse-ec-mappings ctx)]
      (is (nil? (:physical-to-logical result)))
      (is (nil? (:ancilla-to-logical result))))))

(deftest test-create-all-reverse-mappings
  (testing "Create all reverse mappings"
    (let [ctx {:qubit-mapping {0 0, 2 1, 4 2}
               :logical-to-physical {0 0, 1 2, 2 1}
               :logical-to-ancillas {0 [3 4], 1 [5 6], 2 [7 8]}}
          result (mapping/create-all-reverse-mappings ctx)]
      (is (= {0 0, 1 2, 2 4} (:inverse-qubit-mapping result)))
      (is (= {0 0, 2 1, 1 2} (:physical-to-logical result)))
      (is (= {3 0, 4 0, 5 1, 6 1, 7 2, 8 2} (:ancilla-to-logical result))))))

;;;
;;; Tests for result mapping
;;;
(deftest test-map-qubit-to-original
  (testing "Map through full chain: physical → compacted → original"
    (let [ctx {:physical-to-logical {7 2}
               :inverse-qubit-mapping {2 4}}
          result (mapping/map-qubit-to-original 7 ctx)]
      (is (= 4 result))))
  
  (testing "Map with only qubit optimization requires physical-to-logical"
    (let [ctx {:physical-to-logical {1 1}
               :inverse-qubit-mapping {0 0, 1 2, 2 4}}
          result (mapping/map-qubit-to-original 1 ctx)]
      (is (= 2 result))))
  
  (testing "Map with no reverse mappings"
    (let [ctx {}
          result (mapping/map-qubit-to-original 5 ctx)]
      (is (nil? result)))))

(deftest test-map-measurements-to-original
  (testing "Map all measurements to original qubits"
    (let [ctx {:physical-to-logical {0 0, 1 1, 7 2}
               :inverse-qubit-mapping {0 0, 1 2, 2 4}}
          measurements {0 1, 1 0, 7 1}
          result (mapping/map-measurements-to-original measurements ctx)]
      (is (= {0 1, 2 0, 4 1} result))))
  
  (testing "Map measurements with unmappable qubits"
    (let [ctx {:physical-to-logical {0 0}
               :inverse-qubit-mapping {0 0}}
          measurements {0 1, 5 0, 10 1}
          result (mapping/map-measurements-to-original measurements ctx)]
      (is (= {0 1} result)))))

;;;
;;; Tests for forward mapping queries
;;;
(deftest test-get-physical-qubits-for-logical
  (testing "Get physical qubits with error correction (vector mapping)"
    (let [ctx {:qubit-mapping {4 2}
               :logical-to-physical {2 [6 7 8]}}
          result (mapping/get-physical-qubits-for-logical 4 ctx)]
      (is (= [6 7 8] result))))
  
  (testing "Get physical qubits from topology (scalar mapping returns scalar)"
    (let [ctx {:qubit-mapping {4 2}
               :logical-to-physical {2 5}}
          result (mapping/get-physical-qubits-for-logical 4 ctx)]
      (is (= 5 result))))
  
  (testing "Get physical qubits without qubit optimization"
    (let [ctx {:logical-to-physical {2 [6 7 8]}}
          result (mapping/get-physical-qubits-for-logical 2 ctx)]
      (is (= [6 7 8] result))))
  
  (testing "Get physical qubits with no mappings"
    (let [ctx {}
          result (mapping/get-physical-qubits-for-logical 4 ctx)]
      (is (= [4] result)))))

(deftest test-get-ancilla-qubits-for-logical
  (testing "Get ancilla qubits for logical"
    (let [ctx {:qubit-mapping {4 2}
               :logical-to-ancillas {2 [10 11]}}
          result (mapping/get-ancilla-qubits-for-logical 4 ctx)]
      (is (= [10 11] result))))
  
  (testing "Get ancilla qubits with no error correction"
    (let [ctx {:qubit-mapping {4 2}}
          result (mapping/get-ancilla-qubits-for-logical 4 ctx)]
      (is (= [] result))))
  
  (testing "Get ancilla qubits with no mappings"
    (let [ctx {}
          result (mapping/get-ancilla-qubits-for-logical 4 ctx)]
      (is (= [] result)))))

(comment
  ;; Run all tests in this namespace
  (run-tests)

  ;
  )