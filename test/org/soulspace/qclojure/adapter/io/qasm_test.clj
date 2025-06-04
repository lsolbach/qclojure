(ns org.soulspace.qclojure.adapter.io.qasm-test
  "Tests for OpenQASM conversion functionality.
   
   These tests verify that the OpenQASM adapter correctly converts
   quantum circuits to QASM format and back without file I/O operations."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.application.format.qasm2 :as qasm2]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;; Helper functions for tests
(defn- contains-line?
  "Check if the text contains the given line."
  [text line]
  (some #(= % line) (str/split-lines text)))

;; Test circuits
(defn- single-gate-circuit
  "Create a single-gate circuit for testing."
  []
  (-> (qc/create-circuit 1 "Single Gate")
      (qc/x-gate 0)))

(defn- multi-gate-circuit
  "Create a circuit with multiple gates for testing."
  []
  (-> (qc/create-circuit 2 "Multi Gate")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)
      (qc/z-gate 1)))

(defn- rotation-gates-circuit
  "Create a circuit with rotation gates for testing."
  []
  (-> (qc/create-circuit 1 "Rotation Gates")
      (qc/rx-gate 0 (/ Math/PI 2))
      (qc/ry-gate 0 (/ Math/PI 4))
      (qc/rz-gate 0 (/ Math/PI 3))))

(defn- extended-gates-circuit
  "Create a circuit with extended gate types for testing (s-dag, t-dag, phase, etc.)."
  []
  (-> (qc/create-circuit 2 "Extended Gates")
      (qc/s-dag-gate 0)
      (qc/t-dag-gate 1)
      (qc/phase-gate 0 (/ Math/PI 3))
      (qc/cz-gate 0 1)
      (qc/cy-gate 1 0)))

(defn- complex-test-circuit
  "Create a more complex circuit with various gate types for thorough testing."
  []
  (-> (qc/create-circuit 3 "Complex Test Circuit")
      (qc/h-gate 0)
      (qc/x-gate 1)
      (qc/y-gate 2)
      (qc/z-gate 0)
      (qc/cnot-gate 0 1)
      (qc/cnot-gate 1 2)
      (qc/rx-gate 2 (/ Math/PI 3))
      (qc/ry-gate 0 (/ Math/PI 4))
      (qc/rz-gate 1 (/ Math/PI 5))))

(defn- advanced-gates-circuit
  "Create a circuit with advanced gate types for testing (swap, toffoli, controlled rotations)."
  []
  (-> (qc/create-circuit 3 "Advanced Gates")
      (qc/swap-gate 0 1)
      (qc/toffoli-gate 0 1 2)
      (qc/crx-gate 0 2 (/ Math/PI 4))
      (qc/cry-gate 1 2 (/ Math/PI 3))
      (qc/crz-gate 0 1 (/ Math/PI 6))))

;; Test cases

(deftest test-circuit-to-qasm-header
  (testing "QASM header generation"
    (let [circuit (qc/create-circuit 3 "Test Circuit")
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (str/starts-with? qasm-code "OPENQASM 2.0;"))
      (is (contains-line? qasm-code "include \"qelib1.inc\";"))
      (is (contains-line? qasm-code "qreg q[3];"))
      (is (contains-line? qasm-code "creg c[3];")))))

(deftest test-circuit-to-qasm-basic-gates
  (testing "Basic gate conversion to QASM"
    (let [circuit (-> (qc/create-circuit 2 "Basic Gates")
                      (qc/x-gate 0)
                      (qc/y-gate 1)
                      (qc/z-gate 0)
                      (qc/h-gate 1))
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "x q[0];"))
      (is (contains-line? qasm-code "y q[1];"))
      (is (contains-line? qasm-code "z q[0];"))
      (is (contains-line? qasm-code "h q[1];")))))

(deftest test-circuit-to-qasm-controlled-gates
  (testing "Controlled gate conversion to QASM"
    (let [circuit (-> (qc/create-circuit 2 "CNOT Circuit")
                      (qc/cnot-gate 0 1))
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "cx q[0],q[1];")))))

(deftest test-circuit-to-qasm-rotation-gates
  (testing "Rotation gate conversion to QASM"
    (let [circuit (rotation-gates-circuit)
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code (str "rx(" (/ Math/PI 2) ") q[0];")))
      (is (contains-line? qasm-code (str "ry(" (/ Math/PI 4) ") q[0];")))
      (is (contains-line? qasm-code (str "rz(" (/ Math/PI 3) ") q[0];"))))))

(deftest test-circuit-to-qasm-bell-state
  (testing "Bell state circuit conversion to QASM"
    (let [bell-circuit (qc/bell-state-circuit)
          qasm-code (qasm2/circuit-to-qasm bell-circuit)]
      (is (contains-line? qasm-code "h q[0];"))
      (is (contains-line? qasm-code "cx q[0],q[1];")))))

(deftest test-circuit-to-qasm-extended-gates
  (testing "Extended gate conversion to QASM"
    (let [circuit (extended-gates-circuit)
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "sdg q[0];") "S-dagger gate conversion")
      (is (contains-line? qasm-code "tdg q[1];") "T-dagger gate conversion")
      (is (contains-line? qasm-code (str "p(" (/ Math/PI 3) ") q[0];")) "Phase gate conversion")
      (is (contains-line? qasm-code "cz q[0],q[1];") "CZ gate conversion")
      (is (contains-line? qasm-code "cy q[1],q[0];") "CY gate conversion"))))

(deftest test-circuit-to-qasm-advanced-gates
  (testing "Advanced gate conversion to QASM"
    (let [circuit (-> (qc/create-circuit 3 "Advanced Gates")
                      (qc/swap-gate 0 1)
                      (qc/toffoli-gate 0 1 2)
                      (qc/crx-gate 0 2 (/ Math/PI 4))
                      (qc/cry-gate 1 2 (/ Math/PI 3))
                      (qc/crz-gate 0 1 (/ Math/PI 6)))
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "swap q[0],q[1];") "SWAP gate conversion")
      (is (contains-line? qasm-code "ccx q[0],q[1],q[2];") "Toffoli gate conversion")
      (is (contains-line? qasm-code (str "crx(" (/ Math/PI 4) ") q[0],q[2];")) "CRX gate conversion")
      (is (contains-line? qasm-code (str "cry(" (/ Math/PI 3) ") q[1],q[2];")) "CRY gate conversion")
      (is (contains-line? qasm-code (str "crz(" (/ Math/PI 6) ") q[0],q[1];")) "CRZ gate conversion"))))

(deftest test-circuit-to-qasm-new-gates
  (testing "New gate support: iSWAP and Fredkin gates"
    (let [circuit (-> (qc/create-circuit 3 "New Gates")
                      (qc/iswap-gate 0 1)
                      (qc/fredkin-gate 0 1 2))
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (contains-line? qasm-code "iswap q[0],q[1];") "iSWAP gate conversion")
      (is (contains-line? qasm-code "cswap q[0],q[1],q[2];") "Fredkin gate conversion"))))

(deftest test-qasm-to-circuit-new-gates
  (testing "QASM to circuit conversion for new gates"
    (let [qasm-code "OPENQASM 2.0;\ninclude \"qelib1.inc\";\nqreg q[3];\ncreg c[3];\n\niswap q[0],q[1];\ncswap q[0],q[1],q[2];\nmeasure q -> c;"
          circuit (qasm2/qasm-to-circuit qasm-code)]
      (is (= 3 (:num-qubits circuit)))
      (is (= 2 (count (:gates circuit))))
      
      ;; Check gates
      (let [gate1 (first (:gates circuit))
            gate2 (second (:gates circuit))]
        (is (= :iswap (:gate-type gate1)))
        (is (= 0 (get-in gate1 [:gate-params :qubit1])))
        (is (= 1 (get-in gate1 [:gate-params :qubit2])))
        (is (= :fredkin (:gate-type gate2)))
        (is (= 0 (get-in gate2 [:gate-params :control])))
        (is (= 1 (get-in gate2 [:gate-params :target1])))
        (is (= 2 (get-in gate2 [:gate-params :target2])))))))

(deftest test-new-gates-round-trip
  (testing "Round-trip conversion for new gates (iSWAP and Fredkin)"
    (let [original-circuit (-> (qc/create-circuit 3 "New Gates Test")
                               (qc/h-gate 0)
                               (qc/iswap-gate 0 1)
                               (qc/fredkin-gate 2 0 1)
                               (qc/x-gate 2))
          qasm-code (qasm2/circuit-to-qasm original-circuit)
          converted-circuit (qasm2/qasm-to-circuit qasm-code)]
      
      ;; Verify circuit structure
      (is (= (:num-qubits original-circuit) (:num-qubits converted-circuit)))
      (is (= (count (:gates original-circuit)) (count (:gates converted-circuit))))
      
      ;; Check that all gate types are preserved
      (let [original-gate-types (mapv :gate-type (:gates original-circuit))
            converted-gate-types (mapv :gate-type (:gates converted-circuit))]
        (is (= (frequencies original-gate-types) (frequencies converted-gate-types))))
      
      ;; Specifically test the new gates
      (let [original-iswap (first (filter #(= :iswap (:gate-type %)) (:gates original-circuit)))
            converted-iswap (first (filter #(= :iswap (:gate-type %)) (:gates converted-circuit)))
            original-fredkin (first (filter #(= :fredkin (:gate-type %)) (:gates original-circuit)))
            converted-fredkin (first (filter #(= :fredkin (:gate-type %)) (:gates converted-circuit)))]
        
        ;; Check iSWAP gate parameters
        (is (= (get-in original-iswap [:gate-params :qubit1])
               (get-in converted-iswap [:gate-params :qubit1])))
        (is (= (get-in original-iswap [:gate-params :qubit2])
               (get-in converted-iswap [:gate-params :qubit2])))
        
        ;; Check Fredkin gate parameters
        (is (= (get-in original-fredkin [:gate-params :control])
               (get-in converted-fredkin [:gate-params :control])))
        (is (= (get-in original-fredkin [:gate-params :target1])
               (get-in converted-fredkin [:gate-params :target1])))
        (is (= (get-in original-fredkin [:gate-params :target2])
               (get-in converted-fredkin [:gate-params :target2])))))))

(deftest test-qasm-to-circuit-basic
  (testing "Basic QASM to circuit conversion"
    (let [qasm-code "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[2];
creg c[2];

h q[0];
cx q[0],q[1];
measure q -> c;"
          circuit (qasm2/qasm-to-circuit qasm-code)]
      (is (= 2 (:num-qubits circuit)))
      (is (= 2 (count (:gates circuit))))
      
      ;; Check gates
      (let [gate1 (first (:gates circuit))
            gate2 (second (:gates circuit))]
        (is (= :h (:gate-type gate1)))
        (is (= 0 (get-in gate1 [:gate-params :target])))
        (is (contains? #{:cnot :cx} (:gate-type gate2)))))))

(deftest test-qasm-to-circuit-multiple-gates
  (testing "QASM with multiple gates to circuit conversion"
    (let [qasm-code "OPENQASM 2.0;
include \"qelib1.inc\";
qreg q[3];
creg c[3];

x q[0];
y q[1];
z q[2];
h q[0];
measure q -> c;"
          circuit (qasm2/qasm-to-circuit qasm-code)]
      (is (= 3 (:num-qubits circuit)))
      (is (= 4 (count (:gates circuit))))
      
      ;; Check the gate types
      (let [gate-types (mapv :gate-type (:gates circuit))]
        (is (= [:x :y :z :h] gate-types)))
      
      ;; Check target qubits
      (let [targets (mapv #(get-in % [:gate-params :target]) (:gates circuit))]
        (is (= [0 1 2 0] targets))))))

(deftest test-invalid-qasm-handling
  (testing "Handling of invalid QASM code"
    (let [invalid-qasm "This is not valid QASM code"]
      (is (thrown? Exception (qasm2/qasm-to-circuit invalid-qasm))))))

(deftest test-unknown-gate-handling
  (testing "Handling of unknown gates in circuit to QASM"
    (let [circuit {:gates [{:gate-type :unknown-gate 
                            :gate-params {:target 0}}]
                  :num-qubits 1}
          qasm-code (qasm2/circuit-to-qasm circuit)]
      (is (str/includes? qasm-code "// Unknown gate: unknown-gate")))))

(deftest test-round-trip-conversion
  (testing "Round-trip conversion from circuit to QASM and back"
    (let [original-circuit (complex-test-circuit)
          ;; Convert to QASM
          qasm-code (qasm2/circuit-to-qasm original-circuit)
          ;; Convert back to circuit
          converted-circuit (qasm2/qasm-to-circuit qasm-code)]
      
      ;; Verify circuit structure
      (is (= (:num-qubits original-circuit) (:num-qubits converted-circuit)))
      
      ;; Check that all original gates are preserved (may be in different order)
      (let [original-gates (:gates original-circuit)
            converted-gates (:gates converted-circuit)
            original-gate-types (frequencies (map :gate-type original-gates))
            converted-gate-types (frequencies (map :gate-type converted-gates))]
        
        ;; Check gate counts by type match
        (is (= original-gate-types converted-gate-types))
        
        ;; Check that gate targets were preserved
        (doseq [gate-type (keys original-gate-types)]
          (let [orig-targets (->> original-gates 
                                  (filter #(= gate-type (:gate-type %)))
                                  (mapv #(get-in % [:gate-params :target]))
                                  sort)
                conv-targets (->> converted-gates
                                 (filter #(= gate-type (:gate-type %)))
                                 (mapv #(get-in % [:gate-params :target]))
                                 sort)]
            (is (= orig-targets conv-targets) (str "Target preservation for gate type: " gate-type)))))
      
      ;; Check rotation angles are preserved
      (let [original-rx-gates (filter #(= :rx (:gate-type %)) (:gates original-circuit))
            converted-rx-gates (filter #(= :rx (:gate-type %)) (:gates converted-circuit))]
        (is (= (count original-rx-gates) (count converted-rx-gates)))
        (when (seq original-rx-gates)
          (let [orig-angles (set (map #(get-in % [:gate-params :angle]) original-rx-gates))
                conv-angles (set (map #(get-in % [:gate-params :angle]) converted-rx-gates))]
            (is (= orig-angles conv-angles) "RX gate angles preserved"))))
      
      ;; Test that the code includes all expected elements
      (is (str/includes? qasm-code "OPENQASM 2.0;"))
      (is (str/includes? qasm-code "h q[0];"))
      (is (str/includes? qasm-code "x q[1];"))
      (is (str/includes? qasm-code "rx("))
      (is (str/includes? qasm-code "ry("))
      (is (str/includes? qasm-code "rz(")))))

(deftest test-controlled-gate-roundtrip
  (testing "Round-trip conversion preserves controlled gates"
    (let [control-circuit (-> (qc/create-circuit 3 "Control Test")
                              (qc/cnot-gate 0 1)
                              (qc/cnot-gate 0 2)
                              (qc/h-gate 0)
                              (qc/cnot-gate 1 2))
          qasm-code (qasm2/circuit-to-qasm control-circuit)
          converted (qasm2/qasm-to-circuit qasm-code)

          ;; Count the CNOT gates in both circuits
          original-cnots (filter #(contains? #{:cnot :cx} (:gate-type %))
                                 (:gates control-circuit))
          converted-cnots (filter #(contains? #{:cnot :cx} (:gate-type %))
                                  (:gates converted))
          original-controls (mapv #(get-in % [:gate-params :control]) original-cnots)
          original-targets (mapv #(get-in % [:gate-params :target]) original-cnots)]

        ;; Check that the counts match
        (is (= (count original-cnots) (count converted-cnots)))

        ;; Check that all control/target pairs exist in the converted circuit
        (doseq [i (range (count original-cnots))]
          (let [orig-control (nth original-controls i)
                orig-target (nth original-targets i)
                pair-exists? (some #(and (= (get-in % [:gate-params :control]) orig-control)
                                         (= (get-in % [:gate-params :target]) orig-target))
                                   converted-cnots)]
            (is pair-exists? (str "Control-Target pair preserved: " orig-control "->" orig-target)))))))

(deftest test-parametrized-gate-roundtrip
  (testing "Round-trip conversion preserves parametrized gates and their values"
    (let [param-circuit (-> (qc/create-circuit 1 "Parametrized Gates")
                            (qc/rx-gate 0 (/ Math/PI 2))
                            (qc/ry-gate 0 (/ Math/PI 3))
                            (qc/rz-gate 0 (/ Math/PI 4)))
          qasm-code (qasm2/circuit-to-qasm param-circuit)
          converted (qasm2/qasm-to-circuit qasm-code)

          ;; Extract all rotation gates and their angles
          original-rotations (filter #(contains? #{:rx :ry :rz} (:gate-type %))
                                     (:gates param-circuit))
          converted-rotations (filter #(contains? #{:rx :ry :rz} (:gate-type %))
                                      (:gates converted))

          ;; Create maps of gate-type -> [angles] for comparison
          original-angles (reduce (fn [m gate]
                                    (update m (:gate-type gate)
                                            conj (get-in gate [:gate-params :angle])))
                                  {:rx [] :ry [] :rz []}
                                  original-rotations)
          converted-angles (reduce (fn [m gate]
                                     (update m (:gate-type gate)
                                             conj (get-in gate [:gate-params :angle])))
                                   {:rx [] :ry [] :rz []}
                                   converted-rotations)]

        ;; Check each rotation gate type
        (doseq [gate-type [:rx :ry :rz]]
          (is (= (count (get original-angles gate-type))
                 (count (get converted-angles gate-type)))
              (str "Count of " gate-type " gates preserved"))

          ;; Check that the angles match (allowing for minor floating-point differences)
          (let [orig-angles (sort (get original-angles gate-type))
                conv-angles (sort (get converted-angles gate-type))]
            (doseq [i (range (count orig-angles))]
              (let [orig (nth orig-angles i)
                    conv (nth conv-angles i)]
                (is (< (Math/abs (- orig conv)) 1.0e-6)
                    (str gate-type " angle preserved: " orig " vs " conv)))))))))

(comment
  (run-tests)

  ;
  )