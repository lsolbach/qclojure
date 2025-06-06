(ns org.soulspace.qclojure.application.format.qasm3
  "OpenQASM 3.0 conversion functions for quantum circuits.
  
  This namespace provides conversion between quantum circuit data structures
  and OpenQASM 3.0 format strings. QASM 3.0 is the latest version of the
  OpenQASM quantum assembly language with improved syntax and features."
  (:require
   [org.soulspace.qclojure.domain.circuit :as qc]
   [clojure.string :as str]))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM 3.0 format.
  
  OpenQASM 3.0 is the latest version of the quantum assembly language standard
  with improved syntax, built-in gates, and better type system support.
  
  Parameters:
  - circuit: Quantum circuit to convert
  
  Returns:
  String containing QASM 3.0 code"
  [circuit]
  (let [num-qubits (:num-qubits circuit)
        header (str "OPENQASM 3.0;\n\n"
                    "qubit[" num-qubits "] q;\n"
                    "bit[" num-qubits "] c;\n\n")

        gate-to-qasm (fn [gate]
                       (let [gate-type (:operation-type gate)
                             params (:operation-params gate)]
                         (case gate-type
                           ;; Single-qubit Pauli gates
                           :x (str "x q[" (:target params) "];")
                           :y (str "y q[" (:target params) "];")
                           :z (str "z q[" (:target params) "];")
                           
                           ;; Single-qubit gates
                           :h (str "h q[" (:target params) "];")
                           :s (str "s q[" (:target params) "];")
                           :t (str "t q[" (:target params) "];")
                           :s-dag (str "sdg q[" (:target params) "];")
                           :t-dag (str "tdg q[" (:target params) "];")
                           
                           ;; Rotation gates
                           :rx (str "rx(" (:angle params) ") q[" (:target params) "];")
                           :ry (str "ry(" (:angle params) ") q[" (:target params) "];")
                           :rz (str "rz(" (:angle params) ") q[" (:target params) "];")
                           :phase (str "p(" (:angle params) ") q[" (:target params) "];")
                           
                           ;; Two-qubit gates
                           :cnot (str "cx q[" (:control params) "], q[" (:target params) "];")
                           :cx (str "cx q[" (:control params) "], q[" (:target params) "];")
                           :cz (str "cz q[" (:control params) "], q[" (:target params) "];")
                           :cy (str "cy q[" (:control params) "], q[" (:target params) "];")
                           
                           ;; Controlled rotation gates
                           :crx (str "crx(" (:angle params) ") q[" (:control params) "], q[" (:target params) "];")
                           :cry (str "cry(" (:angle params) ") q[" (:control params) "], q[" (:target params) "];")
                           :crz (str "crz(" (:angle params) ") q[" (:control params) "], q[" (:target params) "];")
                           
                           ;; SWAP gates
                           :swap (str "swap q[" (:qubit1 params) "], q[" (:qubit2 params) "];")
                           :iswap (str "iswap q[" (:qubit1 params) "], q[" (:qubit2 params) "];")
                           
                           ;; Three-qubit gates
                           :toffoli (str "ccx q[" (:control1 params) "], q[" (:control2 params) "], q[" (:target params) "];")
                           :fredkin (str "cswap q[" (:control params) "], q[" (:target1 params) "], q[" (:target2 params) "];")
                           
                           ;; Measurement
                           :measure (let [qubits (:measurement-qubits params)]
                                      (if (= 1 (count qubits))
                                        (str "c[" (first qubits) "] = measure q[" (first qubits) "];")
                                        (str/join "\n" 
                                                  (map #(str "c[" % "] = measure q[" % "];") qubits))))
                           
                           ;; Default case for unknown gates
                           (str "// Unknown gate: " (name gate-type)))))

        gates-qasm (str/join "\n" (map gate-to-qasm (:operations circuit)))]

    (str header gates-qasm)))

(defn qasm-to-circuit
  "Convert OpenQASM 3.0 code to a quantum circuit.
  
  This function parses OpenQASM 3.0 code and constructs a quantum circuit
  object. It supports the basic gates and measurements defined in QASM 3.0.
  
  Parameters:
  - qasm: String containing OpenQASM 3.0 code
  
  Returns:
  Quantum circuit object"
  [qasm]
  (let [lines (str/split-lines qasm)
        ;; Parse the number of qubits from qubit declaration
        num-qubits (->> (filter #(str/starts-with? (str/trim %) "qubit[") lines)
                        first
                        (re-find #"\d+")
                        Integer/parseInt)
        ;; Create empty circuit
        circuit (qc/create-circuit num-qubits "Converted Circuit")

        ;; Process each line to add gates
        processed-circuit (reduce
                           (fn [c line]
                             (let [line (str/trim line)]
                               (cond
                                 ;; Parse single-qubit gates (x, y, z, h, s, t, sdg, tdg)
                                 (re-find #"^(x|y|z|h|s|t|sdg|tdg)\s+q\[(\d+)\]" line)
                                 (let [[_ gate-type target] (re-find #"^(x|y|z|h|s|t|sdg|tdg)\s+q\[(\d+)\]" line)
                                       target-idx (Integer/parseInt target)
                                       ;; Map QASM gate names to QClojure gate keywords
                                       gate-keyword (case gate-type
                                                      "sdg" :s-dag
                                                      "tdg" :t-dag
                                                      (keyword gate-type))]
                                   (qc/add-gate c gate-keyword :target target-idx))

                                 ;; Parse 2-qubit controlled gates (cx, cz, cy)
                                 (re-find #"^(cx|cz|cy)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ gate-type control target] (re-find #"^(cx|cz|cy)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       control-idx (Integer/parseInt control)
                                       target-idx (Integer/parseInt target)
                                       gate-keyword (case gate-type
                                                      "cx" :cnot
                                                      "cz" :cz
                                                      "cy" :cy)]
                                   (qc/add-gate c gate-keyword :control control-idx :target target-idx))

                                 ;; Parse SWAP gate
                                 (re-find #"^swap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ qubit1 qubit2] (re-find #"^swap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       qubit1-idx (Integer/parseInt qubit1)
                                       qubit2-idx (Integer/parseInt qubit2)]
                                   (qc/add-gate c :swap :qubit1 qubit1-idx :qubit2 qubit2-idx))

                                 ;; Parse iSWAP gate
                                 (re-find #"^iswap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ qubit1 qubit2] (re-find #"^iswap\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       qubit1-idx (Integer/parseInt qubit1)
                                       qubit2-idx (Integer/parseInt qubit2)]
                                   (qc/add-gate c :iswap :qubit1 qubit1-idx :qubit2 qubit2-idx))

                                 ;; Parse Toffoli gate (ccx)
                                 (re-find #"^ccx\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ control1 control2 target] (re-find #"^ccx\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       control1-idx (Integer/parseInt control1)
                                       control2-idx (Integer/parseInt control2)
                                       target-idx (Integer/parseInt target)]
                                   (qc/add-gate c :toffoli :control1 control1-idx :control2 control2-idx :target target-idx))

                                 ;; Parse Fredkin gate (cswap)
                                 (re-find #"^cswap\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ control target1 target2] (re-find #"^cswap\s+q\[(\d+)\],\s*q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       control-idx (Integer/parseInt control)
                                       target1-idx (Integer/parseInt target1)
                                       target2-idx (Integer/parseInt target2)]
                                   (qc/add-gate c :fredkin :control control-idx :target1 target1-idx :target2 target2-idx))

                                 ;; Parse controlled rotation gates (crx, cry, crz)
                                 (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                 (let [[_ axis angle-str control target] (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],\s*q\[(\d+)\]" line)
                                       angle (Double/parseDouble angle-str)
                                       control-idx (Integer/parseInt control)
                                       target-idx (Integer/parseInt target)
                                       gate-keyword (case axis
                                                      "x" :crx
                                                      "y" :cry
                                                      "z" :crz)]
                                   (qc/add-gate c gate-keyword :control control-idx :target target-idx :angle angle))

                                 ;; Parse phase gate (p)
                                 (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
                                 (let [[_ angle-str target] (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
                                       angle (Double/parseDouble angle-str)
                                       target-idx (Integer/parseInt target)]
                                   (qc/add-gate c :phase :target target-idx :angle angle))

                                 ;; Parse rotation gates (rx, ry, rz)
                                 (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                                 (let [[_ axis angle-str target] (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                                       angle (Double/parseDouble angle-str)
                                       target-idx (Integer/parseInt target)
                                       gate-keyword (case axis
                                                      "x" :rx
                                                      "y" :ry
                                                      "z" :rz)]
                                   (qc/add-gate c gate-keyword :target target-idx :angle angle))

                                 ;; Parse QASM 3.0 measurement syntax: c[i] = measure q[i];
                                 (re-find #"^c\[(\d+)\]\s*=\s*measure\s+q\[(\d+)\]" line)
                                 (let [[_ _cbit-idx qubit-idx] (re-find #"^c\[(\d+)\]\s*=\s*measure\s+q\[(\d+)\]" line)
                                       qubit-idx-int (Integer/parseInt qubit-idx)]
                                   (qc/measure-operation c [qubit-idx-int]))

                                 ;; Skip other lines (comments, declarations, etc.)
                                 :else c)))
                           circuit
                           lines)]
    processed-circuit))