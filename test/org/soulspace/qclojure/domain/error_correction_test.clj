(ns org.soulspace.qclojure.domain.error-correction-test
  "Comprehensive tests for quantum error correction codes and stabilizer formalism."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.error-correction :as ec]))

;;;
;;; Tests for Code Registry and Definitions
;;;
(deftest test-available-codes
  (testing "List available error correction codes"
    (let [codes (ec/list-available-codes)]
      (is (vector? codes))
      (is (>= (count codes) 5))
      (is (every? #(contains? % :key) codes))
      (is (every? #(contains? % :name) codes))
      (is (every? #(contains? % :description) codes)))))

(deftest test-get-code
  (testing "Retrieve bit-flip code"
    (let [code (ec/get-code :bit-flip)]
      (is (map? code))
      (is (= "Bit-flip code" (:name code)))
      (is (= 3 (:num-physical-qubits code)))
      (is (= 1 (:num-logical-qubits code)))))
  
  (testing "Retrieve Shor code"
    (let [code (ec/get-code :shor)]
      (is (map? code))
      (is (= "Shor code" (:name code)))
      (is (= 9 (:num-physical-qubits code)))
      (is (= 1 (:num-logical-qubits code)))))
  
  (testing "Retrieve Steane code"
    (let [code (ec/get-code :steane)]
      (is (map? code))
      (is (= "Steane code" (:name code)))
      (is (= 7 (:num-physical-qubits code)))))
  
  (testing "Retrieve five-qubit code"
    (let [code (ec/get-code :five-qubit)]
      (is (map? code))
      (is (= "Five-qubit code" (:name code)))
      (is (= 5 (:num-physical-qubits code)))))
  
  (testing "Invalid code key returns nil"
    (is (nil? (ec/get-code :nonexistent)))))

(deftest test-syndrome-operations
  (testing "Syndrome weight calculation"
    (is (= 0 (ec/syndrome-weight [0 0 0])))
    (is (= 1 (ec/syndrome-weight [1 0 0])))
    (is (= 2 (ec/syndrome-weight [1 1 0])))
    (is (= 3 (ec/syndrome-weight [1 1 1]))))
  
  (testing "Trivial syndrome detection"
    (is (true? (ec/trivial-syndrome? [0 0 0])))
    (is (true? (ec/trivial-syndrome? [0 0 0 0])))
    (is (false? (ec/trivial-syndrome? [1 0 0])))
    (is (false? (ec/trivial-syndrome? [0 1 0])))
    (is (false? (ec/trivial-syndrome? [1 1 1])))))

;;;
;;; Tests for Bit-Flip Code (3-qubit)
;;;
(deftest test-bit-flip-encoding
  (testing "Encode logical qubit with bit-flip code"
    (let [circuit (circuit/create-circuit 3)
          encoded (ec/encode-bit-flip circuit 0)
          ops (:operations encoded)]
      (is (= 3 (:num-qubits encoded)))
      (is (= 2 (count ops)))
      (is (= :cnot (:operation-type (first ops))))
      (is (= 0 (get-in (first ops) [:operation-params :control])))
      (is (= 1 (get-in (first ops) [:operation-params :target])))
      (is (= :cnot (:operation-type (second ops))))
      (is (= 0 (get-in (second ops) [:operation-params :control])))
      (is (= 2 (get-in (second ops) [:operation-params :target]))))))

(deftest test-bit-flip-syndrome-measurement
  (testing "Measure bit-flip syndrome"
    (let [circuit (circuit/create-circuit 5)
          measured (ec/measure-bit-flip-syndrome circuit [0 1 2] [3 4])
          ops (:operations measured)]
      (is (= 5 (:num-qubits measured)))
      ;; 4 CNOT operations + 1 MEASURE operation = 5 total
      (is (= 5 (count ops)))
      ;; Check that CNOT gates target ancilla qubits
      (is (some #(and (= :cnot (:operation-type %))
                      (= 3 (get-in % [:operation-params :target])))
                ops))
      (is (some #(and (= :cnot (:operation-type %))
                      (= 4 (get-in % [:operation-params :target])))
                ops)))))

(deftest test-bit-flip-correction
  (testing "Apply bit-flip correction for syndrome [1 0]"
    (let [circuit (circuit/create-circuit 3)
          corrected (ec/correct-bit-flip-error circuit [0 1 2] [1 0])
          ops (:operations corrected)]
      (is (>= (count ops) 1))
      (is (= :x (:operation-type (last ops))))
      (is (= 0 (get-in (last ops) [:operation-params :target])))))
  
  (testing "Apply bit-flip correction for syndrome [1 1]"
    (let [circuit (circuit/create-circuit 3)
          corrected (ec/correct-bit-flip-error circuit [0 1 2] [1 1])
          ops (:operations corrected)]
      (is (= :x (:operation-type (last ops))))
      (is (= 1 (get-in (last ops) [:operation-params :target])))))
  
  (testing "Apply bit-flip correction for syndrome [0 1]"
    (let [circuit (circuit/create-circuit 3)
          corrected (ec/correct-bit-flip-error circuit [0 1 2] [0 1])
          ops (:operations corrected)]
      (is (= :x (:operation-type (last ops))))
      (is (= 2 (get-in (last ops) [:operation-params :target])))))
  
  (testing "No correction for trivial syndrome [0 0]"
    (let [circuit (circuit/create-circuit 3)
          corrected (ec/correct-bit-flip-error circuit [0 1 2] [0 0])]
      (is (= 0 (count (:operations corrected)))))))

(deftest test-bit-flip-decoding
  (testing "Decode bit-flip code"
    (let [circuit (circuit/create-circuit 3)
          decoded (ec/decode-bit-flip circuit [0 1 2])
          ops (:operations decoded)]
      (is (= 2 (count ops)))
      (is (every? #(= :cnot (:operation-type %)) ops)))))

;;;
;;; Tests for Five-Qubit Code
;;;
(deftest test-five-qubit-encoding
  (testing "Encode logical qubit with five-qubit code"
    (let [circuit (circuit/create-circuit 5)
          encoded (ec/encode-five-qubit circuit 0)
          ops (:operations encoded)]
      (is (= 5 (:num-qubits encoded)))
      (is (>= (count ops) 8)) ; Five-qubit encoding requires multiple gates
      ;; Check for expected gate types
      (is (some #(= :cnot (:operation-type %)) ops))
      (is (some #(= :h (:operation-type %)) ops)))))

(deftest test-five-qubit-syndrome-measurement
  (testing "Measure five-qubit syndrome"
    (let [circuit (circuit/create-circuit 9) ; 5 data + 4 ancilla
          measured (ec/measure-five-qubit-syndrome circuit [0 1 2 3 4] [5 6 7 8])
          ops (:operations measured)]
      (is (= 9 (:num-qubits measured)))
      (is (>= (count ops) 8)) ; Multiple syndrome measurement operations
      ;; Check that ancilla qubits are targeted
      (is (some #(and (= :cnot (:operation-type %))
                      (>= (get-in % [:operation-params :target]) 5))
                ops)))))

(deftest test-five-qubit-correction
  (testing "No correction for trivial syndrome"
    (let [circuit (circuit/create-circuit 5)
          corrected (ec/correct-five-qubit-error circuit [0 1 2 3 4] [0 0 0 0])]
      (is (= 0 (count (:operations corrected))))))
  
  (testing "Unknown syndrome throws exception"
    (let [circuit (circuit/create-circuit 5)]
      ;; Syndrome [1 0 0 0] is not in the lookup table
      (is (thrown? clojure.lang.ExceptionInfo
                   (ec/correct-five-qubit-error circuit [0 1 2 3 4] [1 0 0 0]))))))

(deftest test-five-qubit-decoding
  (testing "Decode five-qubit code"
    (let [circuit (circuit/create-circuit 5)
          decoded (ec/decode-five-qubit circuit [0 1 2 3 4])
          ops (:operations decoded)]
      (is (>= (count ops) 8)) ; Inverse of encoding
      (is (every? #(contains? #{:cnot :h} (:operation-type %)) ops)))))

;;;
;;; Tests for Steane Code (7-qubit)
;;;
(deftest test-steane-encoding
  (testing "Encode logical qubit with Steane code"
    (let [circuit (circuit/create-circuit 7)
          encoded (ec/encode-steane circuit 0)
          ops (:operations encoded)]
      (is (= 7 (:num-qubits encoded)))
      (is (>= (count ops) 10)) ; Steane encoding requires multiple gates
      ;; Steane encoding uses only CNOT gates (CSS code construction)
      (is (every? #(= :cnot (:operation-type %)) ops)))))

(deftest test-steane-syndrome-measurement
  (testing "Measure Steane syndrome"
    (let [circuit (circuit/create-circuit 13) ; 7 data + 6 ancilla
          measured (ec/measure-steane-syndrome circuit [0 1 2 3 4 5 6] [7 8 9 10 11 12])
          ops (:operations measured)]
      (is (= 13 (:num-qubits measured)))
      (is (>= (count ops) 12)) ; Multiple syndrome measurements
      ;; Check ancilla qubits are used
      (is (some #(and (= :cnot (:operation-type %))
                      (>= (get-in % [:operation-params :target]) 7))
                ops)))))

(deftest test-steane-correction
  (testing "No correction for trivial syndrome"
    (let [circuit (circuit/create-circuit 7)
          corrected (ec/correct-steane-error circuit [0 1 2 3 4 5 6] [0 0 0 0 0 0])]
      (is (= 0 (count (:operations corrected))))))
  
  (testing "Correction applied for non-trivial syndrome"
    (let [circuit (circuit/create-circuit 7)
          corrected (ec/correct-steane-error circuit [0 1 2 3 4 5 6] [1 0 0 0 0 0])]
      (is (s/valid? ::circuit/circuit corrected)))))

(deftest test-steane-decoding
  (testing "Decode Steane code"
    (let [circuit (circuit/create-circuit 7)
          decoded (ec/decode-steane circuit [0 1 2 3 4 5 6])
          ops (:operations decoded)]
      (is (>= (count ops) 10)) ; Inverse of encoding
      (is (every? #(contains? #{:cnot :h} (:operation-type %)) ops)))))

;;;
;;; Tests for Shor Code (9-qubit)
;;;
(deftest test-shor-encoding
  (testing "Encode logical qubit with Shor code"
    (let [circuit (circuit/create-circuit 9)
          encoded (ec/encode-shor circuit 0)
          ops (:operations encoded)]
      (is (= 9 (:num-qubits encoded)))
      (is (>= (count ops) 11)) ; Shor encoding: 1 H + 2 CNOTs + 3 Hs + 6 CNOTs
      (is (some #(= :h (:operation-type %)) ops))
      (is (some #(= :cnot (:operation-type %)) ops)))))

(deftest test-shor-syndrome-measurement
  (testing "Measure Shor syndrome"
    (let [circuit (circuit/create-circuit 17) ; 9 data + 6 bit-flip + 2 phase-flip ancilla
          measured (ec/measure-shor-syndrome circuit 
                                            [0 1 2 3 4 5 6 7 8]
                                            [9 10 11 12 13 14]
                                            [15 16])
          ops (:operations measured)]
      (is (= 17 (:num-qubits measured)))
      (is (>= (count ops) 16)) ; Multiple syndrome measurements
      ;; Check ancilla qubits are targeted
      (is (some #(and (= :cnot (:operation-type %))
                      (>= (get-in % [:operation-params :target]) 9))
                ops)))))

(deftest test-shor-correction
  (testing "No correction for trivial syndromes"
    (let [circuit (circuit/create-circuit 9)
          corrected (ec/correct-shor-error circuit 
                                          [0 1 2 3 4 5 6 7 8]
                                          [0 0 0 0 0 0]
                                          [0 0])]
      (is (= 0 (count (:operations corrected))))))
  
  (testing "Bit-flip correction applied"
    (let [circuit (circuit/create-circuit 9)
          ;; Syndrome [1 0 0 0 0 0] indicates error on qubit 0
          corrected (ec/correct-shor-error circuit 
                                          [0 1 2 3 4 5 6 7 8]
                                          [1 0 0 0 0 0]
                                          [0 0])]
      (is (>= (count (:operations corrected)) 1))
      (is (s/valid? ::circuit/circuit corrected)))))

(deftest test-shor-decoding
  (testing "Decode Shor code"
    (let [circuit (circuit/create-circuit 9)
          decoded (ec/decode-shor circuit [0 1 2 3 4 5 6 7 8])
          ops (:operations decoded)]
      (is (>= (count ops) 11)) ; Inverse of encoding
      (is (every? #(contains? #{:cnot :h} (:operation-type %)) ops)))))

;;;
;;; Tests for Pipeline Integration
;;;
(deftest test-apply-error-correction-disabled
  (testing "Error correction disabled by default"
    (let [circuit (circuit/create-circuit 2)
          ctx {:circuit circuit
               :options {:apply-error-correction? false}}
          result (ec/apply-error-correction ctx)]
      (is (= circuit (:circuit result)))
      (is (nil? (:error-correction-applied? result)))
      (is (nil? (:logical-to-physical result))))))

(deftest test-apply-error-correction-bit-flip
  (testing "Apply bit-flip error correction to circuit"
    (let [circuit (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/cnot-gate 0 1))
          ctx {:circuit circuit
               :options {:apply-error-correction? true}
                        :error-correction-code :bit-flip}
          result (ec/apply-error-correction ctx)]
      (is (true? (:error-correction-applied? result)))
      (is (= "Bit-flip code" (:name (:error-correction-code result))))
      (is (map? (:logical-to-physical result)))
      (is (= 2 (count (:logical-to-physical result))))
      ;; Each logical qubit maps to 3 physical qubits
      (is (= 3 (count (get (:logical-to-physical result) 0))))
      (is (= 3 (count (get (:logical-to-physical result) 1))))
      ;; Circuit should have physical qubits + ancilla qubits: (2*3) + (2*2) = 10
      (is (= 10 (get-in result [:circuit :num-qubits])))))
  
  (testing "Operations are translated to physical qubits"
    (let [circuit (-> (circuit/create-circuit 1)
                      (circuit/h-gate 0))
          ctx {:circuit circuit
               :options {:apply-error-correction? true}
                        :error-correction-code :bit-flip}
          result (ec/apply-error-correction ctx)
          ops (get-in result [:circuit :operations])]
      ;; Should have encoding CNOTs + translated H gates
      (is (>= (count ops) 3)) ; At least 2 encoding CNOTs + 1 H gate
      (is (some #(= :h (:operation-type %)) ops)))))

(deftest test-apply-error-correction-shor
  (testing "Apply Shor code to circuit"
    (let [circuit (circuit/create-circuit 1)
          ctx {:circuit circuit
               :options {:apply-error-correction? true
                        :error-correction-code :shor}}
          result (ec/apply-error-correction ctx)]
      (is (true? (:error-correction-applied? result)))
      (is (= "Shor code" (:name (:error-correction-code result))))
      (is (map? (:logical-to-physical result)))
      ;; 1 logical qubit maps to 9 physical qubits for Shor code
      (is (= 9 (count (get (:logical-to-physical result) 0))))
      ;; Total: 9 physical + 8 ancilla (6 for bit-flip + 2 for phase-flip) = 17 qubits
      (is (= 17 (get-in result [:circuit :num-qubits]))))))

(deftest test-apply-error-correction-five-qubit
  (testing "Apply five-qubit code to circuit"
    (let [circuit (circuit/create-circuit 1)
          ctx {:circuit circuit
               :options {:apply-error-correction? true
                        :error-correction-code :five-qubit}}
          result (ec/apply-error-correction ctx)]
      (is (true? (:error-correction-applied? result)))
      (is (= "Five-qubit code" (:name (:error-correction-code result))))
      ;; 1 logical qubit maps to 5 physical qubits for five-qubit code
      (is (= 5 (count (get (:logical-to-physical result) 0))))
      ;; Total: 5 physical + 4 ancilla = 9 qubits
      (is (= 9 (get-in result [:circuit :num-qubits]))))))

(deftest test-apply-error-correction-steane
  (testing "Apply Steane code to circuit"
    (let [circuit (circuit/create-circuit 1)
          ctx {:circuit circuit
               :options {:apply-error-correction? true
                        :error-correction-code :steane}}
          result (ec/apply-error-correction ctx)]
      (is (true? (:error-correction-applied? result)))
      (is (= "Steane code" (:name (:error-correction-code result))))
      ;; 1 logical qubit maps to 7 physical qubits for Steane code
      (is (= 7 (count (get (:logical-to-physical result) 0))))
      ;; Total: 7 physical + 6 ancilla = 13 qubits
      (is (= 13 (get-in result [:circuit :num-qubits]))))))

(deftest test-error-correction-with-multi-qubit-circuit
  (testing "Error correction with 3-qubit circuit"
    (let [circuit (-> (circuit/create-circuit 3)
                      (circuit/h-gate 0)
                      (circuit/cnot-gate 0 1)
                      (circuit/cnot-gate 1 2))
          ctx {:circuit circuit
               :options {:apply-error-correction? true
                        :error-correction-code :bit-flip}}
          result (ec/apply-error-correction ctx)]
      (is (true? (:error-correction-applied? result)))
      ;; 3 logical qubits * 3 physical each + 3 logical * 2 ancilla = 15 physical qubits
      (is (= 15 (get-in result [:circuit :num-qubits])))
      ;; Check all logical qubits are mapped
      (is (= 3 (count (:logical-to-physical result))))
      (is (every? #(= 3 (count %)) (vals (:logical-to-physical result)))))))

(deftest test-error-correction-preserves-measurement
  (testing "Measurement operations are preserved and translated"
    (let [circuit (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/measure-all-operation))
          ctx {:circuit circuit
               :options {:apply-error-correction? true}
                        :error-correction-code :bit-flip}
          result (ec/apply-error-correction ctx)
          ops (get-in result [:circuit :operations])
          measure-ops (filter #(= :measure (:operation-type %)) ops)]
      ;; Should have measurement on all physical qubits
      (is (pos? (count measure-ops)))
      (is (s/valid? ::circuit/circuit (:circuit result))))))

;;;
;;; Integration Tests with Full Pipeline
;;;
(deftest test-error-correction-in-optimization-pipeline
  (testing "Error correction integrates with optimization pipeline"
    (let [circuit (-> (circuit/create-circuit 2)
                      (circuit/h-gate 0)
                      (circuit/cnot-gate 0 1))
          ctx {:circuit circuit
               :device {:supported-operations #{:h :x :z :cnot}}
               :options {:apply-error-correction? true}
                        :error-correction-code :bit-flip
                        :optimize-gates? false
                        :optimize-qubits? false
                        :optimize-topology? false
                        :transform-operations? false}
          result (ec/apply-error-correction ctx)]
      ;; Verify context structure
      (is (map? result))
      (is (contains? result :circuit))
      (is (contains? result :logical-to-physical))
      (is (contains? result :error-correction-applied?))
      ;; Verify circuit is valid
      (is (s/valid? ::circuit/circuit (:circuit result))))))

(comment
  (run-tests))
  ;
