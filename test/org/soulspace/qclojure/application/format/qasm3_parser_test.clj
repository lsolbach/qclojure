(ns org.soulspace.qclojure.application.format.qasm3-parser-test
  "Comprehensive tests for the QASM3 parser implementation.
   
   Tests every quantum gate type for both parsing and emission,
   including result specs functionality and round-trip verification."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [org.soulspace.qclojure.application.format.qasm3-parser :as qasm3]))

;;;
;;; Helper functions for tests
;;;
(defn- parse-and-emit-test
  "Test parsing QASM3 code and emitting it back, checking for specific content."
  [qasm-code expected-operation-count expected-content]
  (let [circuit (qasm3/qasm-to-circuit qasm-code)
        result-specs (:result-specs circuit)
        emitted (qasm3/circuit-to-qasm circuit result-specs)]
    {:operations-count (count (:operations circuit))
     :expected-count expected-operation-count
     :contains-expected? (str/includes? emitted expected-content)
     :emitted-qasm emitted}))

(defn- verify-operation-params
  "Verify that an operation has the expected parameters."
  [operation expected-type expected-params]
  (and (= expected-type (:operation-type operation))
       (= expected-params (:operation-params operation))))

;;;
;;; Single-qubit gate tests
;;;
(deftest test-hadamard-gate
  (testing "Hadamard gate parsing and emission"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[2] q;
h q[0];
h q[1];"
          result (parse-and-emit-test qasm 2 "h q[0];")]
      (is (= 2 (:operations-count result)))
      (is (:contains-expected? result)))))

(deftest test-pauli-gates
  (testing "Pauli X, Y, Z gates"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[3] q;
x q[0];
y q[1];
z q[2];"
          circuit (qasm3/qasm-to-circuit qasm)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 3 (count (:operations circuit))))
      (is (str/includes? emitted "x q[0];"))
      (is (str/includes? emitted "y q[1];"))
      (is (str/includes? emitted "z q[2];")))))

(deftest test-phase-gates
  (testing "S and T gates (and their daggers)"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[4] q;
s q[0];
t q[1];
sdg q[2];
tdg q[3];"
          circuit (qasm3/qasm-to-circuit qasm)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 4 (count (:operations circuit))))
      (is (str/includes? emitted "s q[0];"))
      (is (str/includes? emitted "t q[1];"))
      (is (str/includes? emitted "sdg q[2];"))
      (is (str/includes? emitted "tdg q[3];")))))

;;;
;;; Parametric gate tests
;;;
(deftest test-rotation-gates
  (testing "Parametric rotation gates RX, RY, RZ"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[3] q;
rx(1.5708) q[0];
ry(0.7854) q[1];
rz(3.1416) q[2];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 3 (count operations)))
      ;; Check operation types and parameters
      (is (verify-operation-params (nth operations 0) :rx {:target 0 :angle 1.5708}))
      (is (verify-operation-params (nth operations 1) :ry {:target 1 :angle 0.7854}))
      (is (verify-operation-params (nth operations 2) :rz {:target 2 :angle 3.1416}))
      ;; Check emission
      (is (str/includes? emitted "rx(1.5708) q[0];"))
      (is (str/includes? emitted "ry(0.7854) q[1];"))
      (is (str/includes? emitted "rz(3.1416) q[2];")))))

(deftest test-phase-gate-parametric
  (testing "Parametric phase gate"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[1] q;
p(1.2345) q[0];"
          circuit (qasm3/qasm-to-circuit qasm)
          op (first (:operations circuit))
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 1 (count (:operations circuit))))
      (is (verify-operation-params op :phase {:target 0 :angle 1.2345}))
      (is (str/includes? emitted "p(1.2345) q[0];")))))

;;;
;;; Two-qubit gate tests
;;;
(deftest test-cnot-gate
  (testing "CNOT/CX gate"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[3] q;
cx q[0], q[1];
cx q[1], q[2];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 2 (count operations)))
      (is (verify-operation-params (first operations) :cnot {:control 0 :target 1}))
      (is (verify-operation-params (second operations) :cnot {:control 1 :target 2}))
      (is (str/includes? emitted "cx q[0], q[1];"))
      (is (str/includes? emitted "cx q[1], q[2];")))))

(deftest test-controlled-gates
  (testing "Controlled Y and Z gates"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[4] q;
cy q[0], q[1];
cz q[2], q[3];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 2 (count operations)))
      (is (verify-operation-params (first operations) :cy {:control 0 :target 1}))
      (is (verify-operation-params (second operations) :cz {:control 2 :target 3}))
      (is (str/includes? emitted "cy q[0], q[1];"))
      (is (str/includes? emitted "cz q[2], q[3];")))))

(deftest test-swap-gates
  (testing "SWAP and iSWAP gates with correct qubit indices"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[4] q;
swap q[0], q[3];
iswap q[1], q[2];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 2 (count operations)))
      ;; Verify swap gates use qubit1/qubit2 parameters
      (is (verify-operation-params (first operations) :swap {:qubit1 0 :qubit2 3}))
      (is (verify-operation-params (second operations) :iswap {:qubit1 1 :qubit2 2}))
      ;; Verify emission preserves correct indices
      (is (str/includes? emitted "swap q[0], q[3];"))
      (is (str/includes? emitted "iswap q[1], q[2];")))))

;;;
;;; Multi-qubit gate tests
;;;
(deftest test-toffoli-gate
  (testing "Toffoli (CCX) gate"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[4] q;
ccx q[0], q[1], q[2];
ccx q[1], q[2], q[3];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 2 (count operations)))
      (is (verify-operation-params (first operations) :toffoli {:control1 0 :control2 1 :target 2}))
      (is (verify-operation-params (second operations) :toffoli {:control1 1 :control2 2 :target 3}))
      (is (str/includes? emitted "ccx q[0], q[1], q[2];"))
      (is (str/includes? emitted "ccx q[1], q[2], q[3];")))))

(deftest test-fredkin-gate
  (testing "Fredkin (CSWAP) gate"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[4] q;
cswap q[0], q[1], q[2];
cswap q[1], q[2], q[3];"
          circuit (qasm3/qasm-to-circuit qasm)
          operations (:operations circuit)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 2 (count operations)))
      (is (verify-operation-params (first operations) :fredkin {:control 0 :target1 1 :target2 2}))
      (is (verify-operation-params (second operations) :fredkin {:control 1 :target1 2 :target2 3}))
      (is (str/includes? emitted "cswap q[0], q[1], q[2];"))
      (is (str/includes? emitted "cswap q[1], q[2], q[3];")))))

;;;
;;; Round-trip tests
;;;
(deftest test-comprehensive-round-trip
  (testing "Comprehensive circuit with all gate types round-trip"
    (let [original-qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[5] q;
h q[0];
x q[1];
y q[2];
z q[3];
s q[4];
t q[0];
sdg q[1];
tdg q[2];
rx(pi/2) q[3];
ry(0.7854) q[4];
rz(pi) q[0];
p(0.5236) q[1];
cx q[0], q[1];
cy q[1], q[2];
cz q[2], q[3];
swap q[0], q[4];
iswap q[1], q[3];
ccx q[0], q[1], q[2];
cswap q[2], q[3], q[4];"
          circuit (qasm3/qasm-to-circuit original-qasm)
          emitted (qasm3/circuit-to-qasm circuit)
          circuit-2 (qasm3/qasm-to-circuit emitted)]
      ;; Verify parsing
      (is (= 19 (count (:operations circuit))))
      ;; Verify round-trip preservation
      (is (= (count (:operations circuit)) (count (:operations circuit-2))))
      ;; Verify specific gates are preserved
      (is (str/includes? emitted "swap q[0], q[4];"))
      (is (str/includes? emitted "iswap q[1], q[3];"))
      (is (str/includes? emitted "ccx q[0], q[1], q[2];"))
      (is (str/includes? emitted "cswap q[2], q[3], q[4];")))))

(deftest test-empty-circuit
  (testing "Empty circuit handling"
    (let [qasm "OPENQASM 3.0;
include \"stdgates.inc\";
qubit[1] q;"
          circuit (qasm3/qasm-to-circuit qasm)
          emitted (qasm3/circuit-to-qasm circuit)]
      (is (= 0 (count (:operations circuit))))
      (is (str/includes? emitted "OPENQASM 3.0;"))
      (is (str/includes? emitted "qubit[1] q;")))))

;;;
;;; Error handling tests
;;;
(deftest test-invalid-qasm
  (testing "Invalid QASM3 syntax handling"
    (is (thrown? Exception (qasm3/qasm-to-circuit "INVALID QASM CONTENT")))))

;;;
;;; Performance tests
;;;
(deftest test-large-circuit-performance
  (testing "Large circuit parsing and emission performance"
    (let [large-qasm (str "OPENQASM 3.0;\ninclude \"stdgates.inc\";\nqubit[20] q;\n"
                          (str/join "\n" (for [i (range 50)]
                                           (str "h q[" (mod i 20) "];"))))
          start-time (System/nanoTime)
          circuit (qasm3/qasm-to-circuit large-qasm)
          parse-time (- (System/nanoTime) start-time)
          emit-start (System/nanoTime)
          emitted (qasm3/circuit-to-qasm circuit)
          emit-time (- (System/nanoTime) emit-start)]
      (is (= 50 (count (:operations circuit))))
      (is (< parse-time 750000000)) ;; Less than 750ms
      (is (< emit-time 100000000))  ;; Less than 100ms
      (is (str/includes? emitted "h q[0];")))))

(comment
  ;; Run all tests
  (run-tests)
  ;
  )
