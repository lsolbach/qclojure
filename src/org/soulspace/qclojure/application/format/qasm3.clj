(ns org.soulspace.qclojure.application.format.qasm3
  "OpenQASM 3.0 conversion functions for quantum circuits with enhanced result types.
  
  This namespace provides conversion between quantum circuit data structures
  and OpenQASM 3.0 format strings. QASM 3.0 is the latest version of the
  OpenQASM quantum assembly language with improved syntax, features, and
  comprehensive support for result extraction including measurements,
  expectation values, variance, probability, and observables.
  
  Supported QASM 3.0 result extensions:
  - Basic measurements (c[i] = measure q[i])
  - Observable expectation values (#pragma result expectation)
  - Observable variance (#pragma result variance)
  - Probability distributions (#pragma result probability)
  - Amplitude extraction for specific states (#pragma result amplitude)
  - Sample-based measurements with shot specification (#pragma result sample)
  
  Note: State vector and density matrix extraction are simulation-only
  and not included in hardware-compatible QASM output."
  (:require
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.domain.operation-registry :as gr]
   [clojure.string :as str]))

;; Observable and result type conversion utilities
(defn observable-to-qasm-comment
  "Convert an observable to a QASM comment for documentation.
   QASM 3.0 doesn't have native observable syntax yet, so we use comments."
  [observable]
  (cond
    (= observable :pauli-x) "// Observable: Pauli-X"
    (= observable :pauli-y) "// Observable: Pauli-Y"  
    (= observable :pauli-z) "// Observable: Pauli-Z"
    (map? observable) (str "// Observable: " (pr-str observable))
    :else (str "// Observable: " (str observable))))

(defn result-specs-to-qasm-pragmas
  "Convert result specifications to QASM 3.0 pragma comments.
   These provide metadata about what results should be extracted."
  [result-specs]
  (when (seq result-specs)
    (let [pragma-lines
          (mapcat (fn [[result-type spec]]
                    (case result-type
                      :measurements
                      [(str "#pragma qclojure result measurement shots=" (get spec :shots 1000)
                            (when-let [qubits (:qubits spec)]
                              (str " qubits=" (str/join "," qubits))))]
                      
                      :expectation
                      (let [observables (:observables spec [])
                            targets (:targets spec [])]
                        (map-indexed 
                         (fn [idx obs]
                           (str "#pragma qclojure result expectation observable=" 
                                (name obs) 
                                (when (< idx (count targets))
                                  (str " target=" (nth targets idx)))))
                         observables))
                      
                      :variance  
                      (let [observables (:observables spec [])
                            targets (:targets spec [])]
                        (map-indexed 
                         (fn [idx obs]
                           (str "#pragma qclojure result variance observable=" 
                                (name obs)
                                (when (< idx (count targets))
                                  (str " target=" (nth targets idx)))))
                         observables))
                      
                      :probability
                      [(str "#pragma qclojure result probability"
                            (when-let [targets (:targets spec)]
                              (str " targets=" (str/join "," targets)))
                            (when-let [states (:states spec)]
                              (str " states=" (str/join "," states))))]
                      
                      :amplitude
                      [(str "#pragma qclojure result amplitude states=" 
                            (str/join "," (:states spec [])))]
                      
                      :sample
                      (let [observables (:observables spec [])
                            shots (:shots spec 1000)
                            targets (:targets spec [])]
                        (map-indexed 
                         (fn [idx obs]
                           (str "#pragma qclojure result sample observable=" 
                                (name obs) " shots=" shots
                                (when (< idx (count targets))
                                  (str " target=" (nth targets idx)))))
                         observables))
                      
                      ;; Skip simulation-only results in hardware QASM
                      (:state-vector :density-matrix :fidelity)
                      [(str "// Simulation-only result: " (name result-type))]
                      
                      ;; Unknown result type
                      [(str "// Unknown result type: " (name result-type))]))
                  result-specs)]
      (when (seq pragma-lines)
        (str "\n// Result extraction specifications\n"
             (str/join "\n" pragma-lines) "\n")))))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM 3.0 format with result type support.
  
  OpenQASM 3.0 is the latest version of the quantum assembly language standard
  with improved syntax, built-in gates, and better type system support.
  Enhanced with QClojure result extraction pragmas for comprehensive
  quantum result types including measurements, expectation values, variance,
  probability distributions, and observables.
  
  Parameters:
  - circuit: Quantum circuit to convert
  - result-specs: (optional) Map specifying result extraction requirements
  
  Returns:
  String containing QASM 3.0 code with result pragmas"
  ([circuit]
   (circuit-to-qasm circuit nil))
  ([circuit result-specs]
   (let [num-qubits (:num-qubits circuit)
         header (str "OPENQASM 3.0;\n"
                     "include \"stdgates.inc\";\n\n"
                     "qubit[" num-qubits "] q;\n"
                     "bit[" num-qubits "] c;\n")

         ;; Add result specifications as pragmas
         result-pragmas (result-specs-to-qasm-pragmas result-specs)

         gate-to-qasm (fn [gate]
                        (let [gate-type (:operation-type gate)
                              params (:operation-params gate)]
                          (case gate-type
                            ;; Identity gate
                            :i (str "id q[" (:target params) "];")

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

                            ;; Controlled gates with single control
                            :controlled (str "ctrl @ " (:gate params) " q[" (:control params) "], q[" (:target params) "];")

                            ;; SWAP gates
                            :swap (str "swap q[" (:qubit1 params) "], q[" (:qubit2 params) "];")
                            :iswap (str "iswap q[" (:qubit1 params) "], q[" (:qubit2 params) "];")

                            ;; Three-qubit gates
                            :toffoli (str "ccx q[" (:control1 params) "], q[" (:control2 params) "], q[" (:target params) "];")
                            :fredkin (str "cswap q[" (:control params) "], q[" (:target1 params) "], q[" (:target2 params) "];")

                            ;; Global gates (neutral atom specific) - QASM 3.0 extension
                            :global-x (str "// Global X gate - apply X to all qubits\n"
                                           (str/join "\n" (map #(str "x q[" % "];") (range num-qubits))))
                            :global-y (str "// Global Y gate - apply Y to all qubits\n"
                                           (str/join "\n" (map #(str "y q[" % "];") (range num-qubits))))
                            :global-z (str "// Global Z gate - apply Z to all qubits\n"
                                           (str/join "\n" (map #(str "z q[" % "];") (range num-qubits))))
                            :global-h (str "// Global Hadamard gate - apply H to all qubits\n"
                                           (str/join "\n" (map #(str "h q[" % "];") (range num-qubits))))
                            :global-rx (str "// Global RX(" (:angle params) ") gate - apply RX to all qubits\n"
                                            (str/join "\n" (map #(str "rx(" (:angle params) ") q[" % "];") (range num-qubits))))
                            :global-ry (str "// Global RY(" (:angle params) ") gate - apply RY to all qubits\n"
                                            (str/join "\n" (map #(str "ry(" (:angle params) ") q[" % "];") (range num-qubits))))
                            :global-rz (str "// Global RZ(" (:angle params) ") gate - apply RZ to all qubits\n"
                                            (str/join "\n" (map #(str "rz(" (:angle params) ") q[" % "];") (range num-qubits))))

                            ;; Rydberg gates (neutral atom specific) - QASM 3.0 extension
                            ;; These are hardware-specific and decompose to standard gates in QASM
                            :rydberg-cz (str "// Rydberg CZ gate - decomposed to standard CZ\n"
                                             "cz q[" (:control params) "], q[" (:target params) "];")
                            :rydberg-cphase (str "// Rydberg controlled phase gate - decomposed to CRZ\n"
                                                 "crz(" (:angle params) ") q[" (:control params) "], q[" (:target params) "];")
                            :rydberg-blockade (str "// Rydberg blockade gate - hardware specific\n"
                                                   "// Cannot be directly expressed in standard QASM\n"
                                                   "// Requires hardware-specific backend support")

                            ;; Measurement
                            :measure (let [qubits (:measurement-qubits params)]
                                       (if (= 1 (count qubits))
                                         (str "c[" (first qubits) "] = measure q[" (first qubits) "];")
                                         (str/join "\n"
                                                   (map #(str "c[" % "] = measure q[" % "];") qubits))))

                            ;; Default case for unknown gates - try to get decomposition info
                            (let [gate-info (gr/get-gate-info gate-type)]
                              (if gate-info
                                (str "// QClojure gate: " (:description gate-info) "\n"
                                     "// Gate type: " (name gate-type) " - requires decomposition")
                                (str "// Unknown gate: " (name gate-type)))))))

         gates-qasm (str/join "\n" (map gate-to-qasm (:operations circuit)))]

    (str header 
         result-pragmas
         "\n"
         gates-qasm))))

;; QASM 3.0 pragma parsing utilities
(defn parse-result-pragma
  "Parse a QClojure result pragma from QASM 3.0 comment.
   
   Example pragmas:
   #pragma qclojure result measurement shots=1000 qubits=0,1
   #pragma qclojure result expectation observable=pauli-z target=0
   #pragma qclojure result variance observable=pauli-x target=1"
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

(defn parse-qasm-expression
  "Parse a QASM mathematical expression and return its numeric value.
   Supports expressions like 'pi/3', '2*pi', 'pi/2', etc."
  [expr-str]
  (let [expr (str/trim expr-str)]
    (cond
      ;; Handle pure numbers
      (re-matches #"^-?\d+(\.\d+)?$" expr)
      (Double/parseDouble expr)
      
      ;; Handle pi expressions
      (= expr "pi") Math/PI
      (= expr "-pi") (- Math/PI)
      
      ;; Handle pi/n expressions
      (re-matches #"^-?pi/\d+(\.\d+)?$" expr)
      (let [[_ sign divisor] (re-find #"^(-?)pi/(.+)$" expr)
            div-val (Double/parseDouble divisor)]
        (if (= sign "-")
          (/ (- Math/PI) div-val)
          (/ Math/PI div-val)))
      
      ;; Handle n*pi expressions  
      (re-matches #"^-?\d+(\.\d+)?\*pi$" expr)
      (let [[_ multiplier] (re-find #"^(.+)\*pi$" expr)
            mult-val (Double/parseDouble multiplier)]
        (* mult-val Math/PI))
      
      ;; Handle pi*n expressions
      (re-matches #"^-?pi\*\d+(\.\d+)?$" expr)
      (let [[_ multiplier] (re-find #"^pi\*(.+)$" expr)
            mult-val (Double/parseDouble multiplier)]
        (* Math/PI mult-val))
      
      ;; Handle fractional expressions without pi
      (re-matches #"^-?\d+(\.\d+)?/\d+(\.\d+)?$" expr)
      (let [[numerator denominator] (str/split expr #"/")]
        (/ (Double/parseDouble numerator) (Double/parseDouble denominator)))
      
      ;; Default - try to parse as double, throw informative error if it fails
      :else
      (try
        (Double/parseDouble expr)
        (catch NumberFormatException _
          (throw (ex-info (str "Unsupported QASM expression: " expr
                               ". Supported: numbers, pi, pi/n, n*pi, pi*n, fractions")
                          {:expression expr :type :unsupported-qasm-expression})))))))

(defn collect-result-specs-from-qasm
  "Collect all result specifications from QASM pragma comments."
  [qasm-lines]
  (let [pragmas (filter #(str/starts-with? (str/trim %) "#pragma qclojure result") qasm-lines)
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

(defn qasm-to-circuit
  "Convert OpenQASM 3.0 code to a quantum circuit with result specifications.
  
  This function parses OpenQASM 3.0 code and constructs a quantum circuit
  object. It supports the basic gates and measurements defined in QASM 3.0,
  plus QClojure result extraction pragmas.
  
  Parameters:
  - qasm: String containing OpenQASM 3.0 code
  
  Returns:
  Map with :circuit (quantum circuit object) and :result-specs (parsed result specifications)"
  [qasm]
  (let [lines (str/split-lines qasm)
        ;; Parse the number of qubits from qubit declaration
        num-qubits (->> (filter #(str/starts-with? (str/trim %) "qubit[") lines)
                        first
                        (re-find #"\d+")
                        Integer/parseInt)
        ;; Create empty circuit
        circuit (qc/create-circuit num-qubits "Converted Circuit")
        
        ;; Collect result specifications from pragmas
        result-specs (collect-result-specs-from-qasm lines)

        ;; Process each line to add gates
        processed-circuit (reduce
                           (fn [c line]
                             (let [line (str/trim line)]
                               (cond
                                 ;; Parse single-qubit gates (x, y, z, h, s, t, sdg, tdg, id)
                                 (re-find #"^(x|y|z|h|s|t|sdg|tdg|id)\s+q\[(\d+)\]" line)
                                 (let [[_ gate-type target] (re-find #"^(x|y|z|h|s|t|sdg|tdg|id)\s+q\[(\d+)\]" line)
                                       target-idx (Integer/parseInt target)
                                       ;; Map QASM gate names to QClojure gate keywords
                                       gate-keyword (case gate-type
                                                      "sdg" :s-dag
                                                      "tdg" :t-dag
                                                      "id" :i
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
                                       angle (parse-qasm-expression angle-str)
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
                                       angle (parse-qasm-expression angle-str)
                                       target-idx (Integer/parseInt target)]
                                   (qc/add-gate c :phase :target target-idx :angle angle))

                                 ;; Parse rotation gates (rx, ry, rz)
                                 (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                                 (let [[_ axis angle-str target] (re-find #"^r([xyz])\((.+?)\)\s+q\[(\d+)\]" line)
                                       angle (parse-qasm-expression angle-str)
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
    {:circuit processed-circuit
     :result-specs result-specs}))

(comment
  ;; Example usage:
  (def example-circuit
    (-> (qc/create-circuit 3 "Example Circuit")
        (qc/h-gate 0)
        (qc/cnot-gate 0 1)
        (qc/ry-gate 2 (/ Math/PI 2))
        (qc/cz-gate 1 2)
        (qc/measure-operation [0 1 2])
        ;
        ))

  (def result-specs
    {:measurements {:shots 1000 :qubits [0 1]}
     :expectation {:observables [:pauli-z] :targets [0]}
     :variance {:observables [:pauli-x] :targets [1]}
     :probability {:targets [0 1] :states ["00" "01" "10" "11"]}
     :amplitude {:states ["00" "11"]}})

  (def qasm-output (circuit-to-qasm example-circuit result-specs))
  (println qasm-output)

  (def parsed (qasm-to-circuit qasm-output))
  (println (:circuit parsed))
  (println (:result-specs parsed))

  (def qasm-sample "OPENQASM 3;
include \"stdgates.inc\";

qubit[4] q;
bit[4] c;

rz(pi/3) q[0];
rz(pi/5) q[0];   // can be folded to rz(pi/3 + pi/5)
rx(pi/2) q[1];
rx(-pi/2) q[1];  // cancels
h q[2];
h q[2];          // cancels
cx q[0], q[1];
cx q[0], q[1];   // cancels")

  (def parsed-sample (qasm-to-circuit qasm-sample))
  (println (:circuit parsed-sample))
  (println (:result-specs parsed-sample))
  ;
  )