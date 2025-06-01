(ns org.soulspace.qclojure.adapter.io.qasm
  "OpenQASM I/O adapter for quantum circuits.
   
   This module provides methods to export quantum circuits to OpenQASM
   format, which is a standard quantum assembly language used by many
   quantum computing platforms."
  (:require [clojure.string :as str]
            [org.soulspace.qclojure.adapter.io :as io]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.gate-registry :as gr]))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM format.
  
  OpenQASM is a standard quantum assembly language used by many
  quantum computing platforms including IBM Qiskit and others.
  
  Parameters:
  - circuit: Quantum circuit to convert
  
  Returns:
  String containing QASM code"
  [circuit]
  (let [header (str "OPENQASM 2.0;\n"
                    "include \"qelib1.inc\";\n"
                    "qreg q[" (:num-qubits circuit) "];\n"
                    "creg c[" (:num-qubits circuit) "];\n\n")

        gate-to-qasm (fn [gate]
                       (let [gate-type (:gate-type gate)
                             params (:gate-params gate)]
                         (case gate-type
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
                           (str "// Unknown gate: " (name gate-type)))))

        gates-qasm (str/join "\n" (map gate-to-qasm (:gates circuit)))

        footer "\nmeasure q -> c;"]

    (str header gates-qasm footer)))

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
        circuit (qc/create-circuit num-qubits "Converted Circuit")
        
        ;; Process each line to add gates
        processed-circuit (reduce 
                           (fn [c line]
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
                               (re-find #"^c([xyz])\s+q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ gate-suffix control target] (re-find #"^c([xyz])\s+q\[(\d+)\],q\[(\d+)\]" line)
                                     control-idx (Integer/parseInt control)
                                     target-idx (Integer/parseInt target)
                                     gate-fn (case gate-suffix
                                               "x" qc/cnot-gate
                                               "z" qc/cz-gate
                                               "y" qc/cy-gate)]
                                 (gate-fn c control-idx target-idx))
                               
                               ;; Parse SWAP gate
                               (re-find #"^swap\s+q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ qubit1 qubit2] (re-find #"^swap\s+q\[(\d+)\],q\[(\d+)\]" line)
                                     qubit1-idx (Integer/parseInt qubit1)
                                     qubit2-idx (Integer/parseInt qubit2)]
                                 (qc/swap-gate c qubit1-idx qubit2-idx))
                               
                               ;; Parse iSWAP gate
                               (re-find #"^iswap\s+q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ qubit1 qubit2] (re-find #"^iswap\s+q\[(\d+)\],q\[(\d+)\]" line)
                                     qubit1-idx (Integer/parseInt qubit1)
                                     qubit2-idx (Integer/parseInt qubit2)]
                                 (qc/iswap-gate c qubit1-idx qubit2-idx))
                               
                               ;; Parse Toffoli gate (ccx)
                               (re-find #"^ccx\s+q\[(\d+)\],q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ control1 control2 target] (re-find #"^ccx\s+q\[(\d+)\],q\[(\d+)\],q\[(\d+)\]" line)
                                     control1-idx (Integer/parseInt control1)
                                     control2-idx (Integer/parseInt control2)
                                     target-idx (Integer/parseInt target)]
                                 (qc/toffoli-gate c control1-idx control2-idx target-idx))
                               
                               ;; Parse Fredkin gate (cswap)
                               (re-find #"^cswap\s+q\[(\d+)\],q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ control target1 target2] (re-find #"^cswap\s+q\[(\d+)\],q\[(\d+)\],q\[(\d+)\]" line)
                                     control-idx (Integer/parseInt control)
                                     target1-idx (Integer/parseInt target1)
                                     target2-idx (Integer/parseInt target2)]
                                 (qc/fredkin-gate c control-idx target1-idx target2-idx))
                               
                               ;; Parse controlled rotation gates (crx, cry, crz)
                               (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],q\[(\d+)\]" line)
                               (let [[_ axis angle-str control target] (re-find #"^cr([xyz])\((.+?)\)\s+q\[(\d+)\],q\[(\d+)\]" line)
                                     angle (Double/parseDouble angle-str)
                                     control-idx (Integer/parseInt control)
                                     target-idx (Integer/parseInt target)
                                     gate-fn (case axis
                                               "x" qc/crx-gate
                                               "y" qc/cry-gate
                                               "z" qc/crz-gate)]
                                 (gate-fn c control-idx target-idx angle))
                               
                               ;; Parse phase gate (p)
                               (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
                               (let [[_ angle-str target] (re-find #"^p\((.+?)\)\s+q\[(\d+)\]" line)
                                     angle (Double/parseDouble angle-str)
                                     target-idx (Integer/parseInt target)]
                                 (qc/phase-gate c target-idx angle))
                               
                               ;; Parse rotation gates (rx, ry, rz)
                               (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                               (let [[_ axis angle-str target] (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                                     angle (Double/parseDouble angle-str)
                                     target-idx (Integer/parseInt target)
                                     gate-fn (case axis
                                               "x" qc/rx-gate
                                               "y" qc/ry-gate
                                               "z" qc/rz-gate)]
                                 (gate-fn c target-idx angle))
                               
                               ;; Skip other lines (includes qreg, creg, measure, etc.)
                               :else c))
                           circuit
                           lines)]
    processed-circuit))

(defmethod io/export-quantum-circuit :qasm
  [_format circuit filename]
  (spit filename (circuit-to-qasm circuit)))

(defmethod io/import-quantum-circuit :qasm
  [_format filename]
  (qasm-to-circuit (slurp filename)))