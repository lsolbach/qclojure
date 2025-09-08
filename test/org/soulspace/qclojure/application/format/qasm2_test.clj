(ns org.soulspace.qclojure.application.format.qasm2-test
  "Tests for OpenQASM 2.0 conversion functionality.
   
   These tests verify that the OpenQASM 2.0 adapter correctly converts
   quantum circuits to QASM 2.0 format and back, including result specs parsing."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [org.soulspace.qclojure.application.format.qasm2 :as qasm2]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.application.algorithms :as qa]))

;;;
;;; Helper functions for tests
;;;
(defn- contains-line?
  "Check if the text contains the given line."
  [text line]
  (some #(= % line) (str/split-lines text)))

(defn- round-trip-test
  "Test that a circuit can be converted to QASM2 and back without loss."
  [circuit]
  (let [qasm2-text (qasm2/circuit-to-qasm circuit)
        circuit-result (qasm2/qasm-to-circuit qasm2-text)]
    {:original-ops (count (:operations circuit))
     :round-trip-ops (count (:operations circuit-result))
     :qasm2-text qasm2-text}))

;;;
;;; Test circuits
;;;
(defn- bell-state-circuit
  "Create a Bell state circuit for testing."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

(defn- rotation-gates-circuit
  "Create a circuit with rotation gates for testing."
  []
  (-> (qc/create-circuit 2 "Rotation Gates")
      (qc/rx-gate 0 (/ Math/PI 2))
      (qc/ry-gate 1 (/ Math/PI 4))
      (qc/rz-gate 0 (/ Math/PI 3))))

(defn- swap-gates-circuit
  "Create a circuit with swap gates for testing."
  []
  (-> (qc/create-circuit 3 "Swap Gates")
      (qc/h-gate 0)
      (qc/swap-gate 0 2)
      (qc/iswap-gate 1 2)))

(defn- complex-circuit
  "Create a complex circuit with various gate types for testing."
  []
  (-> (qc/create-circuit 4 "Complex Circuit")
      (qc/h-gate 0)
      (qc/x-gate 1)
      (qc/y-gate 2)
      (qc/z-gate 3)
      (qc/s-gate 0)
      (qc/t-gate 1)
      (qc/s-dag-gate 2)
      (qc/t-dag-gate 3)
      (qc/cnot-gate 0 1)
      (qc/cz-gate 1 2)
      (qc/swap-gate 0 3)
      (qc/toffoli-gate 0 1 2)))

;;;
;;; Basic parsing tests
;;;
(deftest test-basic-qasm2-parsing
  (testing "Basic QASM2 circuit parsing"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[2];
creg c[2];
h q[0];
cx q[0], q[1];"
          circuit (qasm2/qasm-to-circuit qasm2-text)]
      (is (= 2 (:num-qubits circuit)))
      (is (= 2 (count (:operations circuit))))
      (is (= :h (:operation-type (first (:operations circuit)))))
      (is (= :cnot (:operation-type (second (:operations circuit))))))))

(deftest test-parametric-gates-parsing
  (testing "Parametric gates parsing"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[2];
ry(0.785) q[0];
rz(1.571) q[1];"
          circuit (qasm2/qasm-to-circuit qasm2-text)]
      (is (= 2 (count (:operations circuit))))
      (let [ry-op (first (:operations circuit))
            rz-op (second (:operations circuit))]
        (is (= :ry (:operation-type ry-op)))
        (is (= 0.785 (:angle (:operation-params ry-op))))
        (is (= :rz (:operation-type rz-op)))
        (is (= 1.571 (:angle (:operation-params rz-op))))))))

(deftest test-swap-gates-parsing
  (testing "Swap gates parsing with correct indices"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[3];
swap q[0], q[2];
iswap q[1], q[2];"
          circuit (qasm2/qasm-to-circuit qasm2-text)]
      (is (= 2 (count (:operations circuit))))
      (let [swap-op (first (:operations circuit))
            iswap-op (second (:operations circuit))]
        (is (= :swap (:operation-type swap-op)))
        (is (= 0 (:qubit1 (:operation-params swap-op))))
        (is (= 2 (:qubit2 (:operation-params swap-op))))
        (is (= :iswap (:operation-type iswap-op)))))))

;;;
;;; Result specs tests
;;;
#_(deftest test-result-specs-parsing
  (testing "Result specifications parsing from QASM2 comments"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
// #pragma qclojure result measurement shots=1000 qubits=0,1
// #pragma qclojure result expectation observable=pauli-z target=0
qreg q[2];
h q[0];
cx q[0], q[1];"
          [circuit result-specs] (qasm2/qasm-to-circuit qasm2-text)]
      (is (= 2 (count result-specs)))
      (is (contains? result-specs :measurement))
      (is (contains? result-specs :expectation))
      (is (= 1000 (get-in result-specs [:measurement :shots])))
      (is (= [0 1] (get-in result-specs [:measurement :qubits]))))))

;;;
;;; Emission tests
;;;
(deftest test-basic-circuit-emission
  (testing "Basic circuit emission to QASM2"
    (let [circuit (bell-state-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)]
      (is (str/includes? qasm2-text "OPENQASM 2.0;"))
      (is (str/includes? qasm2-text "include \"qelib1.inc\";"))
      (is (str/includes? qasm2-text "qreg q[2];"))
      (is (str/includes? qasm2-text "h q[0];"))
      (is (str/includes? qasm2-text "cx q[0],q[1];")))))

(deftest test-swap-gate-emission
  (testing "Swap gates emission with correct indices"
    (let [circuit (swap-gates-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)]
      (is (str/includes? qasm2-text "swap q[0],q[2];"))
      (is (str/includes? qasm2-text "iswap q[1],q[2];")))))

(deftest test-parametric-gates-emission
  (testing "Parametric gates emission"
    (let [circuit (rotation-gates-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)]
      (is (str/includes? qasm2-text "rx("))
      (is (str/includes? qasm2-text "ry("))
      (is (str/includes? qasm2-text "rz(")))))

#_(deftest test-result-specs-emission
  (testing "Result specifications emission as QASM2 comments"
    (let [circuit (bell-state-circuit)
          result-specs {:measurement {:shots 1000 :qubits [0 1]}
                       :expectation {:observable "pauli-z" :target 0}}
          qasm2-text (qasm2/circuit-to-qasm circuit result-specs)]
      (is (str/includes? qasm2-text "// #pragma qclojure result measurement"))
      (is (str/includes? qasm2-text "// #pragma qclojure result expectation"))
      (is (str/includes? qasm2-text "shots=1000"))
      (is (str/includes? qasm2-text "qubits=0,1")))))

;;;
;;; Round-trip tests
;;;
(deftest test-bell-state-round-trip
  (testing "Bell state circuit round-trip"
    (let [circuit (bell-state-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)
          circuit-result (qasm2/qasm-to-circuit qasm2-text)]
      (is (= (:qubit-count circuit) (:qubit-count circuit-result)))
      (is (= (count (:operations circuit)) (count (:operations circuit-result)))))))

(deftest test-swap-gates-round-trip
  (testing "Swap gates round-trip with correct indices"
    (let [circuit (swap-gates-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)
          circuit (qasm2/qasm-to-circuit qasm2-text)
          re-emitted (qasm2/circuit-to-qasm circuit)]
      (is (str/includes? qasm2-text "swap q[0],q[2];"))
      (is (str/includes? re-emitted "swap q[0],q[2];"))
      (is (str/includes? qasm2-text "iswap q[1],q[2];"))
      (is (str/includes? re-emitted "iswap q[1],q[2];")))))

(deftest test-complex-circuit-round-trip
  (testing "Complex circuit round-trip"
    (let [circuit (complex-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit)
          circuit-result (qasm2/qasm-to-circuit qasm2-text)]
      (is (= (:qubit-count circuit) (:qubit-count circuit-result)))
      (is (= (count (:operations circuit)) (count (:operations circuit-result))))
      ;; Verify swap gate is correctly preserved
      (is (str/includes? qasm2-text "swap q[0],q[3];")))))

#_(deftest test-result-specs-round-trip
  (testing "Result specs round-trip"
    (let [original-specs {:measurement {:shots 2000 :qubits [0 1 2]}
                          :expectation {:observable "pauli-x" :target 1}}
          circuit (complex-circuit)
          qasm2-text (qasm2/circuit-to-qasm circuit original-specs)
          result-specs (qasm2/qasm-to-circuit qasm2-text)]
      (is (= original-specs result-specs)))))

;;;
;;; Error handling tests
;;;
(deftest test-invalid-qasm2
  (testing "Invalid QASM2 parsing"
    (let [invalid-qasm "INVALID QASM CONTENT"]
      (is (thrown? Exception (qasm2/qasm-to-circuit invalid-qasm))))))

(deftest test-empty-circuit
  (testing "Empty circuit handling"
    (let [empty-circuit (qc/create-circuit 1 "Empty")
          qasm2-text (qasm2/circuit-to-qasm empty-circuit)]
      (is (str/includes? qasm2-text "OPENQASM 2.0;"))
      (is (str/includes? qasm2-text "qreg q[1];")))))

;;;
;;; Performance and integration tests
;;;
(deftest test-large-circuit
  (testing "Large circuit handling"
    (let [large-circuit (reduce (fn [c i] (qc/h-gate c i))
                                (qc/create-circuit 10 "Large")
                                (range 10))
          qasm2-text (qasm2/circuit-to-qasm large-circuit)
          circuit (qasm2/qasm-to-circuit qasm2-text)]
      (is (= 10 (:num-qubits circuit)))
      (is (= 10 (count (:operations circuit)))))))

(deftest test-all-single-qubit-gates
  (testing "All single-qubit gates"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[8];
h q[0];
x q[1];
y q[2];
z q[3];
s q[4];
t q[5];
sdg q[6];
tdg q[7];"
          circuit (qasm2/qasm-to-circuit qasm2-text)
          ops (map :operation-type (:operations circuit))]
      (is (= 8 (count (:operations circuit))))
      (is (= [:h :x :y :z :s :t :s-dag :t-dag] ops)))))

(deftest test-all-two-qubit-gates
  (testing "All two-qubit gates"
    (let [qasm2-text "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[4];
cx q[0], q[1];
cz q[0], q[2];
swap q[1], q[3];
cy q[2], q[3];"
          circuit (qasm2/qasm-to-circuit qasm2-text)
          ops (map :operation-type (:operations circuit))]
      (is (= 4 (count (:operations circuit)))) 
      (is (= [:cnot :cz :swap :cy] ops)))))

(comment
  ;; Run all tests
  (run-tests)
  ;
  )
