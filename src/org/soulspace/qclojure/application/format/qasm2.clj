(ns org.soulspace.qclojure.application.format.qasm2
  "QASM 2.0 (Quantum Assembly Language) format conversion for quantum circuits.
   
   This namespace provides functions to convert quantum circuits to and from
   the OpenQASM 2.0 format, which is widely used in quantum computing platforms
   such as IBM Qiskit and others."
  (:require [clojure.string :as str]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]))

(defn gate-to-qasm-fn
  "Ret "
  [num-qubits]
  (fn [gate]
  (let [gate-type (:operation-type gate)
        params (:operation-params gate)]
    (case gate-type
      ;; Identity gate (QASM 2.0 compatible)
      :i (str "id q[" (:target params) "];")
      :x (str "x q[" (:target params) "];")
      :y (str "y q[" (:target params) "];")
      :z (str "z q[" (:target params) "];")
      :h (str "h q[" (:target params) "];")
      :s (str "s q[" (:target params) "];")
      :t (str "t q[" (:target params) "];")
      :s-dag (str "sdg q[" (:target params) "];")
      :t-dag (str "tdg q[" (:target params) "];")
      :phase (str "p(" (:angle params) ") q[" (:target params) "];")
      :cnot (str "cx q[" (:control params) "],q[" (:target params) "];")
      :cx (str "cx q[" (:control params) "],q[" (:target params) "];") ;; Added cx as alias for cnot
      :cz (str "cz q[" (:control params) "],q[" (:target params) "];")
      :cy (str "cy q[" (:control params) "],q[" (:target params) "];")
      :swap (str "swap q[" (:qubit1 params) "],q[" (:qubit2 params) "];")
      :iswap (str "iswap q[" (:qubit1 params) "],q[" (:qubit2 params) "];")
      :toffoli (str "ccx q[" (:control1 params) "],q[" (:control2 params) "],q[" (:target params) "];")
      :fredkin (str "cswap q[" (:control params) "],q[" (:target1 params) "],q[" (:target2 params) "];")
      :crx (str "crx(" (:angle params) ") q[" (:control params) "],q[" (:target params) "];")
      :cry (str "cry(" (:angle params) ") q[" (:control params) "],q[" (:target params) "];")
      :crz (str "crz(" (:angle params) ") q[" (:control params) "],q[" (:target params) "];")
      :rx (str "rx(" (:angle params) ") q[" (:target params) "];")
      :ry (str "ry(" (:angle params) ") q[" (:target params) "];")
      :rz (str "rz(" (:angle params) ") q[" (:target params) "];")

      ;; Global gates - decompose to individual gate applications in QASM 2.0
      :global-x (str "// Global X gate - decomposed to individual X gates\n"
                     (str/join "\n" (map #(str "x q[" % "];") (range num-qubits))))
      :global-y (str "// Global Y gate - decomposed to individual Y gates\n"
                     (str/join "\n" (map #(str "y q[" % "];") (range num-qubits))))
      :global-z (str "// Global Z gate - decomposed to individual Z gates\n"
                     (str/join "\n" (map #(str "z q[" % "];") (range num-qubits))))
      :global-h (str "// Global Hadamard gate - decomposed to individual H gates\n"
                     (str/join "\n" (map #(str "h q[" % "];") (range num-qubits))))
      :global-rx (str "// Global RX(" (:angle params) ") gate - decomposed to individual RX gates\n"
                      (str/join "\n" (map #(str "rx(" (:angle params) ") q[" % "];") (range num-qubits))))
      :global-ry (str "// Global RY(" (:angle params) ") gate - decomposed to individual RY gates\n"
                      (str/join "\n" (map #(str "ry(" (:angle params) ") q[" % "];") (range num-qubits))))
      :global-rz (str "// Global RZ(" (:angle params) ") gate - decomposed to individual RZ gates\n"
                      (str/join "\n" (map #(str "rz(" (:angle params) ") q[" % "];") (range num-qubits))))

      ;; Rydberg gates - decompose to standard gates in QASM 2.0
      :rydberg-cz (str "// Rydberg CZ gate - decomposed to standard CZ\n"
                       "cz q[" (:control params) "],q[" (:target params) "];")
      :rydberg-cphase (str "// Rydberg controlled phase gate - decomposed to CRZ\n"
                           "crz(" (:angle params) ") q[" (:control params) "],q[" (:target params) "];")
      :rydberg-blockade (str "// Rydberg blockade gate - cannot be expressed in QASM 2.0\n"
                             "// Requires hardware-specific backend support")

      ;; Measurement handling
      :measure "// Measurement will be handled by final measure statement"

      ;; Default case - try to get gate info and suggest decomposition
      (let [gate-info (opreg/get-gate-info gate-type)]
        (if gate-info
          (str "// QClojure gate: " (:description gate-info) "\n"
               "// Gate type: " (name gate-type) " - requires decomposition for QASM 2.0")
          (str "// Unknown gate: " (name gate-type))))))))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM format.
  
  OpenQASM is a standard quantum assembly language used by many
  quantum computing platforms including IBM Qiskit and others.
  
  Parameters:
  - circuit: Quantum circuit to convert
  
  Returns:
  String containing QASM code"
  ([circuit]
   (circuit-to-qasm circuit {}))
  ([circuit result-specs]
   (let [header (str "OPENQASM 2.0;\n"
                     "include \"qelib1.inc\";\n"
                     "qreg q[" (:num-qubits circuit) "];\n"
                     "creg c[" (:num-qubits circuit) "];\n\n")

         gate-to-qasm (gate-to-qasm-fn (:num-qubits circuit))
         gates-qasm (str/join "\n" (map gate-to-qasm (:operations circuit)))

         footer "\nmeasure q -> c;"]

     (str header gates-qasm footer))))

;; QASM 2.0 pragma parsing utilities
(defn parse-result-pragma
  "Parse a QClojure result pragma from QASM 2.0 comment.
   
   Example pragmas:
   // #pragma qclojure result measurement shots=1000 qubits=0,1
   // #pragma qclojure result expectation observable=pauli-z target=0
   // #pragma qclojure result variance observable=pauli-x target=1"
  [pragma-line]
  (when (str/starts-with? pragma-line "#pragma qclojure result")
    (let [parts (str/split pragma-line #"\s+")
          result-type (keyword (nth parts 3 nil))]
      (when result-type
        (let [param-pairs (drop 4 parts)
              params (into {} (map (fn [pair]
                                     (let [[k v] (str/split pair #"=" 2)]
                                       [(keyword k)
                                        (cond
                                          (re-matches #"\d+" v) (Long/parseLong v)
                                          (str/includes? v ",") (mapv str/trim (str/split v #","))
                                          :else v)]))
                                   param-pairs))]
          [result-type params])))))

(defn collect-result-specs-from-qasm
  "Collect all result specifications from QASM pragma comments."
  [qasm-lines]
  (let [pragmas (filter #(str/starts-with? (str/trim %) "// #pragma qclojure result") qasm-lines)
        parsed-pragmas (keep parse-result-pragma pragmas)]
    (reduce (fn [specs [result-type params]]
              (update specs result-type
                      (fn [existing]
                        (if existing
                          (cond
                            ;; Merge observables and targets for expectation/variance/sample
                            (#{:expectation :variance :sample} result-type)
                            (-> existing
                                (update :observables (fnil conj []) (:observable params))
                                (update :targets (fnil conj []) (:target params)))

                            ;; Replace for other types
                            :else params)
                          params))))
            {} parsed-pragmas)))

(defn qasm-to-gate
  "Add the gate for the line of QASM to the circuit."
  [circuit line]
    (cond
      ;; Parse single-qubit gates (x, y, z, h, s, t, sdg, tdg, id)
      (re-find #"^(x|y|z|h|s|t|sdg|tdg|id)\s+q\[(\d+)\]" line)
      (let [[_ gate-type target] (re-find #"^(x|y|z|h|s|t|sdg|tdg|id)\s+q\[(\d+)\]" line)
            target-idx (Integer/parseInt target)
            gate-keyword (keyword gate-type)]
        (circuit/add-gate circuit gate-keyword :target target-idx))

      ;; Parse 2-qubit controlled gates (cx, cz, cy)
      (re-find #"^c([xyz])\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ gate-suffix control target] (re-find #"^c([xyz])\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
            control-idx (Integer/parseInt control)
            target-idx (Integer/parseInt target)
            gate-fn (case gate-suffix
                      "x" circuit/cnot-gate
                      "z" circuit/cz-gate
                      "y" circuit/cy-gate)]
        (gate-fn circuit control-idx target-idx))

      ;; Parse SWAP gate
      (re-find #"^swap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ qubit1 qubit2] (re-find #"^swap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
            qubit1-idx (Integer/parseInt qubit1)
            qubit2-idx (Integer/parseInt qubit2)]
        (circuit/swap-gate circuit qubit1-idx qubit2-idx))

      ;; Parse iSWAP gate
      (re-find #"^iswap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ qubit1 qubit2] (re-find #"^iswap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
            qubit1-idx (Integer/parseInt qubit1)
            qubit2-idx (Integer/parseInt qubit2)]
        (circuit/iswap-gate circuit qubit1-idx qubit2-idx))

      ;; Parse Toffoli gate (ccx)
      (re-find #"^ccx\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ control1 control2 target] (re-find #"^ccx\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
            control1-idx (Integer/parseInt control1)
            control2-idx (Integer/parseInt control2)
            target-idx (Integer/parseInt target)]
        (circuit/toffoli-gate circuit control1-idx control2-idx target-idx))

      ;; Parse Fredkin gate (cswap)
      (re-find #"^cswap\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ control target1 target2] (re-find #"^cswap\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
            control-idx (Integer/parseInt control)
            target1-idx (Integer/parseInt target1)
            target2-idx (Integer/parseInt target2)]
        (circuit/fredkin-gate circuit control-idx target1-idx target2-idx))

      ;; Parse controlled rotation gates (crx, cry, crz)
      (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
      (let [[_ axis angle-str control target] (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
            angle (Double/parseDouble angle-str)
            control-idx (Integer/parseInt control)
            target-idx (Integer/parseInt target)
            gate-fn (case axis
                      "x" circuit/crx-gate
                      "y" circuit/cry-gate
                      "z" circuit/crz-gate)]
        (gate-fn circuit control-idx target-idx angle))

      ;; Parse phase gate (p)
      (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
      (let [[_ angle-str target] (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
            angle (Double/parseDouble angle-str)
            target-idx (Integer/parseInt target)]
        (circuit/phase-gate circuit target-idx angle))

      ;; Parse rotation gates (rx, ry, rz)
      (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
      (let [[_ axis angle-str target] (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
            angle (Double/parseDouble angle-str)
            target-idx (Integer/parseInt target)
            gate-fn (case axis
                      "x" circuit/rx-gate
                      "y" circuit/ry-gate
                      "z" circuit/rz-gate)]
        (gate-fn circuit target-idx angle))

      ;; Skip other lines (includes qreg, creg, measure, etc.)
      :else circuit))

(defn qasm-to-circuit
  "Convert OpenQASM code to a quantum circuit.
  
  This function parses OpenQASM code and constructs a quantum circuit
  object. It supports basic gates and measurements.
  
  Parameters:
  - qasm: String containing OpenQASM code
  
  Returns:
  Quantum circuit object"
  [qasm]
  (let [lines (str/split-lines qasm)
        ;; Parse the number of qubits from qreg declaration
        num-qubits (->> (filter #(str/starts-with? % "qreg") lines)
                        first
                        (re-find #"\d+")
                        Integer/parseInt)
        ;; Create empty circuit
        circuit (circuit/create-circuit num-qubits "Converted Circuit")

        ;; Process each line to add gates
        processed-circuit (reduce qasm-to-gate circuit lines)]
    processed-circuit))

(comment
  ;; Example usage:
  (def example-circuit
    (-> (circuit/create-circuit 3 "Example Circuit")
        (circuit/h-gate 0)
        (circuit/cnot-gate 0 1)
        (circuit/ry-gate 2 (/ Math/PI 2))
        (circuit/cz-gate 1 2)
        ;(qc/measure-operation [0 1 2])
        ;
        ))

  (def qasm-code (circuit-to-qasm example-circuit))
  (println "Generated QASM:\n" qasm-code)
  (def parsed-circuit (qasm-to-circuit qasm-code))
  (println "Parsed Circuit:" parsed-circuit)

  (def empty-circuit (circuit/create-circuit 3))
  (qasm-to-gate empty-circuit "h q[0];")
  (qasm-to-gate empty-circuit "x q[1];")
  (qasm-to-gate empty-circuit "y q[2];")
  (qasm-to-gate empty-circuit "z q[3];")
  (qasm-to-gate empty-circuit "s q[4];")
  (qasm-to-gate empty-circuit "t q[5];")
  (qasm-to-gate empty-circuit "sdg q[6];")
  (qasm-to-gate empty-circuit "tdg q[7];")

  (qasm-to-gate empty-circuit "cx q[0], q[1];")
  (qasm-to-gate empty-circuit "cy q[0], q[1];")
  (qasm-to-gate empty-circuit "cz q[0], q[1];")

  (qasm-to-gate empty-circuit "swap q[1], q[3];")
  (qasm-to-gate empty-circuit "iswap q[1], q[3];")
  
  (qasm-to-gate empty-circuit "rx(0.785) q[0];")
  (qasm-to-gate empty-circuit "ry(0.785) q[0];")
  (qasm-to-gate empty-circuit "rz(0.785) q[0];")

  ;
  )