(ns org.soulspace.qclojure.application.format.qasm3-test
  "Tests for OpenQASM 3.0 conversion functionality.
   
   These tests verify that the OpenQASM 3.0 adapter correctly converts
   quantum circuits to QASM 3.0 format and back without file I/O operations."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [org.soulspace.qclojure.application.format.qasm3 :as qasm3]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;; Helper functions for tests
(defn- contains-line?
  "Check if the text contains the given line."
  [text line]
  (some #(= % line) (str/split-lines text)))

;; Test circuits
(defn- bell-state-circuit
  "Create a Bell state circuit for testing."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/add-gate :h :target 0)
      (qc/add-gate :cnot :control 0 :target 1)
      (qc/measure-operation [0 1])))

(defn- rotation-gates-circuit
  "Create a circuit with rotation gates for testing."
  []
  (-> (qc/create-circuit 2 "Rotation Gates")
      (qc/add-gate :rx :target 0 :angle (/ Math/PI 2))
      (qc/add-gate :ry :target 1 :angle (/ Math/PI 4))
      (qc/add-gate :rz :target 0 :angle (/ Math/PI 3))))

(defn- complex-circuit
  "Create a complex circuit with various gate types for testing."
  []
  (-> (qc/create-circuit 3 "Complex Circuit")
      (qc/add-gate :h :target 0)
      (qc/add-gate :x :target 1)
      (qc/add-gate :y :target 2)
      (qc/add-gate :z :target 0)
      (qc/add-gate :s :target 1)
      (qc/add-gate :t :target 2)
      (qc/add-gate :s-dag :target 0)
      (qc/add-gate :t-dag :target 1)
      (qc/add-gate :cnot :control 0 :target 1)
      (qc/add-gate :cz :control 1 :target 2)
      (qc/add-gate :cy :control 0 :target 2)
      (qc/add-gate :swap :qubit1 0 :qubit2 2)
      (qc/add-gate :phase :target 1 :angle (/ Math/PI 6))))

(defn- advanced-gates-circuit
  "Create a circuit with advanced gates for testing."
  []
  (-> (qc/create-circuit 4 "Advanced Gates")
      (qc/add-gate :iswap :qubit1 0 :qubit2 1)
      (qc/add-gate :toffoli :control1 0 :control2 1 :target 2)
      (qc/add-gate :fredkin :control 3 :target1 0 :target2 2)
      (qc/add-gate :crx :control 1 :target 3 :angle (/ Math/PI 4))
      (qc/add-gate :cry :control 2 :target 0 :angle (/ Math/PI 3))
      (qc/add-gate :crz :control 0 :target 3 :angle (/ Math/PI 8))))

;; Test cases

(deftest test-circuit-to-qasm-header
  (testing "QASM 3.0 header generation"
    (let [circuit (qc/create-circuit 3 "Test Circuit")
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (str/starts-with? qasm-code "OPENQASM 3.0;"))
      (is (contains-line? qasm-code "qubit[3] q;"))
      (is (contains-line? qasm-code "bit[3] c;")))))

(deftest test-circuit-to-qasm-basic-gates
  (testing "Basic gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 2 "Basic Gates")
                      (qc/add-gate :x :target 0)
                      (qc/add-gate :y :target 1)
                      (qc/add-gate :z :target 0)
                      (qc/add-gate :h :target 1))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "x q[0];"))
      (is (contains-line? qasm-code "y q[1];"))
      (is (contains-line? qasm-code "z q[0];"))
      (is (contains-line? qasm-code "h q[1];")))))

(deftest test-circuit-to-qasm-pauli-gates
  (testing "Pauli and phase gates conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 2 "Pauli Gates")
                      (qc/add-gate :s :target 0)
                      (qc/add-gate :t :target 1)
                      (qc/add-gate :s-dag :target 0)
                      (qc/add-gate :t-dag :target 1))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "s q[0];"))
      (is (contains-line? qasm-code "t q[1];"))
      (is (contains-line? qasm-code "sdg q[0];"))
      (is (contains-line? qasm-code "tdg q[1];")))))

(deftest test-circuit-to-qasm-controlled-gates
  (testing "Controlled gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 3 "Controlled Gates")
                      (qc/add-gate :cnot :control 0 :target 1)
                      (qc/add-gate :cz :control 1 :target 2)
                      (qc/add-gate :cy :control 0 :target 2))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "cx q[0], q[1];"))
      (is (contains-line? qasm-code "cz q[1], q[2];"))
      (is (contains-line? qasm-code "cy q[0], q[2];")))))

(deftest test-circuit-to-qasm-rotation-gates
  (testing "Rotation gate conversion to QASM 3.0"
    (let [circuit (rotation-gates-circuit)
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code (str "rx(" (/ Math/PI 2) ") q[0];")))
      (is (contains-line? qasm-code (str "ry(" (/ Math/PI 4) ") q[1];")))
      (is (contains-line? qasm-code (str "rz(" (/ Math/PI 3) ") q[0];"))))))

(deftest test-circuit-to-qasm-phase-gate
  (testing "Phase gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 1 "Phase Gate")
                      (qc/add-gate :phase :target 0 :angle (/ Math/PI 4)))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code (str "p(" (/ Math/PI 4) ") q[0];"))))))

(deftest test-circuit-to-qasm-swap-gates
  (testing "SWAP gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 3 "SWAP Gates")
                      (qc/add-gate :swap :qubit1 0 :qubit2 1)
                      (qc/add-gate :iswap :qubit1 1 :qubit2 2))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "swap q[0], q[1];"))
      (is (contains-line? qasm-code "iswap q[1], q[2];")))))

(deftest test-circuit-to-qasm-three-qubit-gates
  (testing "Three-qubit gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 4 "Three-qubit Gates")
                      (qc/add-gate :toffoli :control1 0 :control2 1 :target 2)
                      (qc/add-gate :fredkin :control 3 :target1 0 :target2 2))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "ccx q[0], q[1], q[2];"))
      (is (contains-line? qasm-code "cswap q[3], q[0], q[2];")))))

(deftest test-circuit-to-qasm-controlled-rotations
  (testing "Controlled rotation gate conversion to QASM 3.0"
    (let [circuit (-> (qc/create-circuit 2 "Controlled Rotations")
                      (qc/add-gate :crx :control 0 :target 1 :angle (/ Math/PI 4))
                      (qc/add-gate :cry :control 1 :target 0 :angle (/ Math/PI 3))
                      (qc/add-gate :crz :control 0 :target 1 :angle (/ Math/PI 6)))
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code (str "crx(" (/ Math/PI 4) ") q[0], q[1];")))
      (is (contains-line? qasm-code (str "cry(" (/ Math/PI 3) ") q[1], q[0];")))
      (is (contains-line? qasm-code (str "crz(" (/ Math/PI 6) ") q[0], q[1];"))))))

(deftest test-circuit-to-qasm-measurements
  (testing "Measurement conversion to QASM 3.0"
    (let [circuit (bell-state-circuit)
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "c[0] = measure q[0];"))
      (is (contains-line? qasm-code "c[1] = measure q[1];")))))

(deftest test-qasm-to-circuit-basic
  (testing "Basic QASM 3.0 to circuit conversion"
    (let [qasm-code "OPENQASM 3.0;

qubit[2] q;
bit[2] c;

h q[0];
cx q[0], q[1];
c[0] = measure q[0];
c[1] = measure q[1];"
          circuit (qasm3/qasm-to-circuit qasm-code)]
      (is (= 2 (:num-qubits circuit)))
      (is (= 4 (count (:operations circuit))))

      ;; Check operations
      (let [ops (:operations circuit)
            op1 (first ops)
            op2 (second ops)
            op3 (nth ops 2)
            op4 (nth ops 3)]
        (is (= :h (:operation-type op1)))
        (is (= 0 (get-in op1 [:operation-params :target])))
        (is (= :cnot (:operation-type op2)))
        (is (= 0 (get-in op2 [:operation-params :control])))
        (is (= 1 (get-in op2 [:operation-params :target])))
        (is (= :measure (:operation-type op3)))
        (is (= :measure (:operation-type op4)))))))

(deftest test-qasm-to-circuit-single-qubit-gates
  (testing "Single-qubit gates QASM 3.0 to circuit conversion"
    (let [qasm-code "OPENQASM 3.0;

qubit[3] q;
bit[3] c;

x q[0];
y q[1];
z q[2];
h q[0];
s q[1];
t q[2];
sdg q[0];
tdg q[1];"
          circuit (qasm3/qasm-to-circuit qasm-code)]
      (is (= 3 (:num-qubits circuit)))
      (is (= 8 (count (:operations circuit))))
      
      ;; Check gate types
      (let [gate-types (mapv :operation-type (:operations circuit))]
        (is (= [:x :y :z :h :s :t :s-dag :t-dag] gate-types))))))

(deftest test-qasm-to-circuit-parametric-gates
  (testing "Parametric gates QASM 3.0 to circuit conversion"
    (let [qasm-code "OPENQASM 3.0;

qubit[2] q;
bit[2] c;

rx(1.5708) q[0];
ry(0.7854) q[1];
rz(1.0472) q[0];
p(0.5236) q[1];"
          circuit (qasm3/qasm-to-circuit qasm-code)]
      (is (= 2 (:num-qubits circuit)))
      (is (= 4 (count (:operations circuit))))
      
      ;; Check angles
      (let [ops (:operations circuit)]
        (is (= :rx (:operation-type (first ops))))
        (is (< (Math/abs (- 1.5708 (get-in (first ops) [:operation-params :angle]))) 1e-6))
        (is (= :ry (:operation-type (second ops))))
        (is (< (Math/abs (- 0.7854 (get-in (second ops) [:operation-params :angle]))) 1e-6))))))

(deftest test-qasm-to-circuit-advanced-gates
  (testing "Advanced gates QASM 3.0 to circuit conversion"
    (let [qasm-code "OPENQASM 3.0;

qubit[4] q;
bit[4] c;

swap q[0], q[1];
iswap q[1], q[2];
ccx q[0], q[1], q[2];
cswap q[3], q[0], q[2];"
          circuit (qasm3/qasm-to-circuit qasm-code)]
      (is (= 4 (:num-qubits circuit)))
      (is (= 4 (count (:operations circuit))))
      
      ;; Check gate types
      (let [gate-types (mapv :operation-type (:operations circuit))]
        (is (= [:swap :iswap :toffoli :fredkin] gate-types))))))

(deftest test-round-trip-conversion-bell-state
  (testing "Round-trip conversion from Bell state circuit to QASM 3.0 and back"
    (let [original-circuit (bell-state-circuit)
          qasm-code (qasm3/circuit-to-qasm original-circuit)
          converted-circuit (qasm3/qasm-to-circuit qasm-code)]
      
      ;; Check basic structure
      (is (= (:num-qubits original-circuit) (:num-qubits converted-circuit)))
      
      ;; Check that all gates are preserved (measurements may be split)
      (let [original-gates (remove #(= :measure (:operation-type %)) (:operations original-circuit))
            converted-gates (remove #(= :measure (:operation-type %)) (:operations converted-circuit))]
        (is (= (count original-gates) (count converted-gates)))
        (is (= (mapv :operation-type original-gates) 
               (mapv :operation-type converted-gates)))))))

(deftest test-round-trip-conversion-complex-circuit
  (testing "Round-trip conversion of complex circuit"
    (let [original-circuit (complex-circuit)
          qasm-code (qasm3/circuit-to-qasm original-circuit)
          converted-circuit (qasm3/qasm-to-circuit qasm-code)]
      
      ;; Check basic structure
      (is (= (:num-qubits original-circuit) (:num-qubits converted-circuit)))
      (is (= (count (:operations original-circuit)) (count (:operations converted-circuit))))
      
      ;; Check that all gate types are preserved
      (let [original-gate-types (frequencies (mapv :operation-type (:operations original-circuit)))
            converted-gate-types (frequencies (mapv :operation-type (:operations converted-circuit)))]
        (is (= original-gate-types converted-gate-types))))))

(deftest test-round-trip-conversion-advanced-gates
  (testing "Round-trip conversion of advanced gates circuit"
    (let [original-circuit (advanced-gates-circuit)
          qasm-code (qasm3/circuit-to-qasm original-circuit)
          converted-circuit (qasm3/qasm-to-circuit qasm-code)]
      
      ;; Check basic structure
      (is (= (:num-qubits original-circuit) (:num-qubits converted-circuit)))
      (is (= (count (:operations original-circuit)) (count (:operations converted-circuit))))
      
      ;; Check that angles are preserved for parametric gates
      (let [original-angles (mapv #(get-in % [:operation-params :angle]) 
                                  (filter #(contains? #{:crx :cry :crz} (:operation-type %)) 
                                          (:operations original-circuit)))
            converted-angles (mapv #(get-in % [:operation-params :angle]) 
                                   (filter #(contains? #{:crx :cry :crz} (:operation-type %)) 
                                           (:operations converted-circuit)))]
        (doseq [i (range (count original-angles))]
          (is (< (Math/abs (- (nth original-angles i) (nth converted-angles i))) 1e-6)
              "Parametric gate angles should be preserved"))))))

(deftest test_qasm3_vs_qasm2_headers
  (testing "QASM 3.0 uses different header format than QASM 2.0"
    (let [circuit (qc/create-circuit 2 "Test")
          qasm3-code (qasm3/circuit-to-qasm circuit)]
      ;; Should use QASM 3.0 syntax
      (is (str/includes? qasm3-code "OPENQASM 3.0;"))
      (is (str/includes? qasm3-code "qubit[2] q;"))
      (is (str/includes? qasm3-code "bit[2] c;"))
      ;; Should NOT use QASM 2.0 syntax
      (is (not (str/includes? qasm3-code "OPENQASM 2.0;")))
      (is (not (str/includes? qasm3-code "qreg q[2];")))
      (is (not (str/includes? qasm3-code "creg c[2];"))))))

(deftest test_qasm3_measurement_syntax
  (testing "QASM 3.0 uses different measurement syntax"
    (let [circuit (-> (qc/create-circuit 1 "Test")
                      (qc/add-gate :h :target 0)
                      (qc/measure-operation [0]))
          qasm3-code (qasm3/circuit-to-qasm circuit)]
      ;; Should use QASM 3.0 measurement syntax
      (is (str/includes? qasm3-code "c[0] = measure q[0];"))
      ;; Should NOT use QASM 2.0 measurement syntax
      (is (not (str/includes? qasm3-code "measure q -> c;"))))))

(deftest test-edge-cases
  (testing "Edge cases in QASM 3.0 conversion"
    ;; Empty circuit
    (let [empty-circuit (qc/create-circuit 1 "Empty")
          qasm-code (qasm3/circuit-to-qasm empty-circuit)]
      (is (str/includes? qasm-code "OPENQASM 3.0;"))
      (is (str/includes? qasm-code "qubit[1] q;")))
    
    ;; Large circuit
    (let [large-circuit (qc/create-circuit 10 "Large")
          qasm-code (qasm3/circuit-to-qasm large-circuit)]
      (is (str/includes? qasm-code "qubit[10] q;"))
      (is (str/includes? qasm-code "bit[10] c;")))))

(deftest test-unknown-gate-handling
  (testing "Handling of unknown gates in circuit to QASM 3.0"
    (let [circuit {:operations [{:operation-type :unknown-gate
                                :operation-params {:target 0}}]
                   :num-qubits 1}
          qasm-code (qasm3/circuit-to-qasm circuit)]
      (is (str/includes? qasm-code "// Unknown gate: unknown-gate")))))

(deftest test-parameter-precision
  (testing "Parameter precision preservation in QASM 3.0"
    (let [precise-angle 1.2345678901234567
          circuit (-> (qc/create-circuit 1 "Precision Test")
                      (qc/add-gate :rx :target 0 :angle precise-angle))
          qasm-code (qasm3/circuit-to-qasm circuit)
          converted-circuit (qasm3/qasm-to-circuit qasm-code)
          converted-angle (get-in (first (:operations converted-circuit)) 
                                  [:operation-params :angle])]
      (is (< (Math/abs (- precise-angle converted-angle)) 1e-10)
          "High precision angles should be preserved"))))

(comment
  ;; Run all tests
  (run-tests)
  
  ;; Run specific test
  (test-round-trip-conversion-bell-state)
  
  ;; Test REPL examples
  (def bell (bell-state-circuit))
  (def qasm-str (qasm3/circuit-to-qasm bell))
  (println qasm-str)
  (def converted (qasm3/qasm-to-circuit qasm-str))
  )
