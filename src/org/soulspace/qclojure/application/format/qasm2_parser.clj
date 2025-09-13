(ns org.soulspace.qclojure.application.format.qasm2-parser
  "QASM2 parser and emitter using Instaparse.
  
  This namespace provides functions to parse OpenQASM 2.0 quantum circuits
  into the qclojure domain model and emit qclojure circuits back to QASM2 format."
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]))

;;;
;;; Comment removal functions
;;;
(defn remove-line-comments
  "Remove C++-style line comments (// ...) from QASM code.
   Preserves line structure to maintain line-based pragmas."
  [qasm-code]
  (str/replace qasm-code #"//.*?(?=\r?\n|$)" ""))

(defn remove-block-comments
  "Remove C-style block comments (/* ... */) from QASM code."
  [qasm-code]
  (str/replace qasm-code #"(?s)/\*.*?\*/" ""))

(defn preprocess-qasm
  "Remove comments from QASM code while preserving structure.
   Removes both line comments (//) and block comments (/* */)."
  [qasm-code]
  (-> qasm-code
      remove-block-comments
      remove-line-comments))

;;;
;;; Result specs parsing utilities (adapted from qasm3_parser.clj)
;;;
(defn parse-result-pragma
  "Parse a QClojure result pragma from QASM 2.0 comment.
   
   Example pragmas:
   // #pragma qclojure result measurement shots=1000 qubits=0,1
   // #pragma qclojure result expectation observable=pauli-z target=0
   // #pragma qclojure result variance observable=pauli-x target=1"
  [pragma-line]
  (when (or (str/starts-with? pragma-line "// #pragma qclojure result")
            (str/starts-with? pragma-line "#pragma qclojure result"))
    (let [clean-line (str/replace pragma-line #"^// " "")  ; Remove leading // if present
          parts (str/split clean-line #"\s+")
          result-type (keyword (nth parts 3 nil))]
      (when result-type
        (let [param-pairs (drop 4 parts)
              params (into {} (map (fn [pair]
                                     (let [[k v] (str/split pair #"=" 2)]
                                       [(keyword k) 
                                        (cond
                                          (re-matches #"\d+" v) (Long/parseLong v)
                                          (str/includes? v ",") (mapv #(if (re-matches #"\d+" %)
                                                                         (Long/parseLong %)
                                                                         (str/trim %))
                                                                      (str/split v #","))
                                          :else v)]))
                                   param-pairs))]
          [result-type params])))))

(defn collect-result-specs-from-qasm
  "Collect all result specifications from QASM pragma comments."
  [qasm-lines]
  (let [pragmas (filter #(or (str/starts-with? (str/trim %) "// #pragma qclojure result")
                             (str/starts-with? (str/trim %) "#pragma qclojure result")) qasm-lines)
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
                            
                            ;; Merge qubits for measurement
                            (= result-type :measurement)
                            (update existing :qubits (fnil into []) (:qubits params))
                            
                            ;; Merge states for probability/amplitude
                            (#{:probability :amplitude} result-type)
                            (update existing :states (fnil into []) (:states params))
                            
                            :else params)
                          params))))
            {}
            parsed-pragmas)))

(defn result-specs-to-qasm-pragmas
  "Convert result specifications to QASM pragma comments for QASM2 format.
   Uses // comments to be compatible with QASM2 parsers."
  [result-specs]
  (when result-specs
    (let [pragma-lines 
          (mapcat 
           (fn [[result-type spec]]
             (case result-type
               :measurement 
               [(str "// #pragma qclojure result measurement shots=" 
                     (:shots spec 1000) " qubits=" (str/join "," (:qubits spec [])))]
               
               :expectation  
               (let [observables (if (:observables spec)
                                  (:observables spec)
                                  [(:observable spec)])  ; Handle singular form
                     targets (if (:targets spec)
                              (:targets spec)
                              [(:target spec)])]       ; Handle singular form
                 (map-indexed 
                  (fn [idx obs]
                    (str "// #pragma qclojure result expectation observable=" 
                         (name obs)
                         (when (< idx (count targets))
                           (str " target=" (nth targets idx)))))
                  observables))
               
               :variance  
               (let [observables (if (:observables spec)
                                  (:observables spec)
                                  [(:observable spec)])  ; Handle singular form
                     targets (if (:targets spec)
                              (:targets spec)
                              [(:target spec)])]       ; Handle singular form
                 (map-indexed 
                  (fn [idx obs]
                    (str "// #pragma qclojure result variance observable=" 
                         (name obs)
                         (when (< idx (count targets))
                           (str " target=" (nth targets idx)))))
                  observables))
               
               :probability
               [(str "// #pragma qclojure result probability"
                     (when-let [targets (:targets spec)]
                       (str " targets=" (str/join "," targets)))
                     (when-let [states (:states spec)]
                       (str " states=" (str/join "," states))))]
               
               :amplitude
               [(str "// #pragma qclojure result amplitude states=" 
                     (str/join "," (:states spec [])))]
               
               :sample
               (let [observables (if (:observables spec)
                                  (:observables spec)
                                  [(:observable spec)])  ; Handle singular form
                     shots (:shots spec 1000)
                     targets (if (:targets spec)
                              (:targets spec)
                              [(:target spec)])]       ; Handle singular form
                 (map-indexed 
                  (fn [idx obs]
                    (str "// #pragma qclojure result sample observable=" 
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
        (str "\n" (str/join "\n" pragma-lines) "\n")))))

;;;
;;; Parser using the EBNF grammar
;;;
(def qasm2-parser
  (insta/parser (io/resource "openqasm/qasm2.ebnf")
                :auto-whitespace :standard))

;;;
;;; Helper functions for extracting data from parse trees
;;;
(defn extract-number-from-expression
  "Extract a number from an expression parse tree."
  [expr]
  (when expr
    ;; Navigate through the expression tree to find the number
    (let [number-path (insta/transform
                        {:Number (fn [num] num)
                         :IntegerLiteral (fn [val] (Integer/parseInt val))
                         :FloatLiteral (fn [val] (Double/parseDouble val))}
                        expr)]
      ;; The number should be somewhere in this transformed structure
      (first (filter number? (tree-seq coll? seq number-path))))))

(defn extract-array-size
  "Extract the array size from a designator parse node."
  [designator]
  (when designator
    (let [expr (get-in designator [2])] ; [:designator [:LBRACKET "["] [:expression ...] [:RBRACKET "]"]]
      (when expr
        (extract-number-from-expression expr)))))

(defn extract-qreg-size
  "Extract the qubit count from a qreg id_list structure."
  [id-list]
  (when (and (vector? id-list) (= :id_list (first id-list)))
    (let [id-node (some #(when (and (vector? %) (= :id (first %))) %) (rest id-list))]
      (when id-node
        (let [designator (some #(when (and (vector? %) (= :designator (first %))) %) (rest id-node))]
          (extract-array-size designator))))))

(defn extract-qubit-index
  "Extract qubit index from an indexed identifier."
  [operand]
  ;; Structure: [:id [:Identifier "q"] [:designator [:LBRACKET "["] [:expression ...] [:RBRACKET "]"]]]
  (when operand
    (if (and (vector? operand) (= :id (first operand)))
      ;; Handle indexed identifiers with designator
      (when-let [designator (some #(when (and (vector? %) (= :designator (first %))) %) (rest operand))]
        (extract-array-size designator))
      ;; Try direct numeric extraction for simple cases
      (extract-number-from-expression operand))))

(defn extract-gate-operands
  "Extract qubit operands from gate call arguments."
  [args]
  ;; Args structure: [[:id_list [:id [:Identifier "q"] [:designator ...]] ...] [:SEMICOLON ";"]]
  (when-let [id-list (first args)]
    (when (and (vector? id-list) (= :id_list (first id-list)))
      (->> (rest id-list)
           (filter #(and (vector? %) (= :id (first %))))
           (map extract-qubit-index)
           (filter some?)))))

(defn extract-expression-params
  "Extract parameters from expression list (for parametric gates)."
  [expr-list]
  (when expr-list
    (if (and (vector? expr-list) (= :expression_list (first expr-list)))
      (->> (rest expr-list)
           (filter #(and (vector? %) (= :expression (first %))))
           (map extract-number-from-expression)
           (filter some?))
      [])))

(defn parse-gate-call
  "Parse QASM2 gate call and convert to circuit operation using circuit namespace functions."
  [gate-name-node args-list]
  (let [gate-name (if (and (vector? gate-name-node) 
                           (= :Identifier (first gate-name-node)))
                    (second gate-name-node)  ; Extract name from [:Identifier "h"]
                    gate-name-node)
        
        ;; Check if there are expression parameters (for parametric gates)
        ;; Structure for parametric gates: [:Identifier "ry"] [:LPAREN "("] [:expression_list ...] [:RPAREN ")"] [:id_list ...] [:SEMICOLON ";"]
        has-params? (and (seq args-list) 
                         (>= (count args-list) 4)  ; Need at least [:LPAREN] [:expression_list] [:RPAREN] [:id_list]
                         (vector? (first args-list))
                         (= :LPAREN (first (first args-list))))
        
        ;; Extract parameters if present, otherwise extract operands
        [params operands] (if has-params?
                            ;; For parametric gates: [:LPAREN] [:expression_list] [:RPAREN] [:id_list] [:SEMICOLON]
                            [(extract-expression-params (second args-list))  ; The expression_list is second
                             (extract-gate-operands [(nth args-list 3)])]    ; The id_list is fourth (index 3)
                            [[] (extract-gate-operands args-list)])
        
        ;; Resolve any gate aliases to canonical names
        canonical-gate (when (and gate-name (not= gate-name ""))
                         (opreg/resolve-gate-alias (keyword gate-name)))
        gate-info (when canonical-gate 
                    (opreg/get-gate-info-with-alias canonical-gate))]
    
    (if (and gate-info canonical-gate)
      ;; Use circuit namespace functions to create a temporary circuit, then extract the operation
      (let [max-qubit (if (seq operands) (apply max operands) 0)
            temp-circuit (circuit/create-circuit (inc max-qubit) "temp")]  ; +1 because qubits are 0-indexed
        (try
          (let [updated-circuit 
                (case canonical-gate
                  ;; Single-qubit gates
                  :h (circuit/h-gate temp-circuit (first operands))
                  :x (circuit/x-gate temp-circuit (first operands))
                  :y (circuit/y-gate temp-circuit (first operands))
                  :z (circuit/z-gate temp-circuit (first operands))
                  :s (circuit/s-gate temp-circuit (first operands))
                  :s-dag (circuit/s-dag-gate temp-circuit (first operands))
                  :sdg (circuit/s-dag-gate temp-circuit (first operands))  ; QASM2 alias
                  :t (circuit/t-gate temp-circuit (first operands))
                  :t-dag (circuit/t-dag-gate temp-circuit (first operands))
                  :tdg (circuit/t-dag-gate temp-circuit (first operands))  ; QASM2 alias
                  ;; Identity gate
                  :i (circuit/i-gate temp-circuit (first operands))
                  :id (circuit/i-gate temp-circuit (first operands))  ; QASM2 alias for identity
                  
                  ;; Parametric single-qubit gates
                  :rx (circuit/rx-gate temp-circuit (first operands) (or (first params) 0.0))
                  :ry (circuit/ry-gate temp-circuit (first operands) (or (first params) 0.0))
                  :rz (circuit/rz-gate temp-circuit (first operands) (or (first params) 0.0))
                  :phase (circuit/phase-gate temp-circuit (first operands) (or (first params) 0.0))
                  :p (circuit/phase-gate temp-circuit (first operands) (or (first params) 0.0))  ; QASM2 alias
                  
                  ;; Two-qubit gates
                  :cnot (circuit/cnot-gate temp-circuit (first operands) (second operands))
                  :cx (circuit/cnot-gate temp-circuit (first operands) (second operands))  ; cx is alias for cnot
                  :cz (circuit/cz-gate temp-circuit (first operands) (second operands))
                  :cy (circuit/cy-gate temp-circuit (first operands) (second operands))
                  :swap (circuit/swap-gate temp-circuit (first operands) (second operands))
                  :iswap (circuit/iswap-gate temp-circuit (first operands) (second operands))
                  
                  ;; Parametric two-qubit gates
                  :crx (circuit/crx-gate temp-circuit (first operands) (second operands) (or (first params) 0.0))
                  :cry (circuit/cry-gate temp-circuit (first operands) (second operands) (or (first params) 0.0))
                  :crz (circuit/crz-gate temp-circuit (first operands) (second operands) (or (first params) 0.0))
                  
                  ;; Multi-qubit gates
                  :toffoli (circuit/toffoli-gate temp-circuit (first operands) (second operands) (nth operands 2))
                  :ccx (circuit/toffoli-gate temp-circuit (first operands) (second operands) (nth operands 2))  ; ccx is alias
                  :fredkin (circuit/fredkin-gate temp-circuit (first operands) (second operands) (nth operands 2))
                  :cswap (circuit/fredkin-gate temp-circuit (first operands) (second operands) (nth operands 2))  ; cswap is alias
                  
                  ;; Fallback: use generic add-gate for any gates not explicitly handled
                  (do
                    (println (str "Using generic add-gate for: " canonical-gate))
                    (circuit/add-gate temp-circuit canonical-gate 
                                      ;; Build params map based on operand count and parameters
                                      (merge
                                        (case (count operands)
                                          1 {:target (first operands)}
                                          2 {:control (first operands) :target (second operands)}
                                          3 {:control1 (first operands) :control2 (second operands) :target (nth operands 2)}
                                          {})
                                        (when (seq params) {:angle (first params)})))))]
            ;; Extract the last operation that was added to the circuit
            (last (:operations updated-circuit)))
          (catch Exception e
            (println (str "Error creating gate " canonical-gate ": " (.getMessage e)))
            nil)))
      
      ;; Gate not found in operation registry
      (do 
        (println (str "Warning: Unknown gate '" gate-name "' not found in operation registry"))
        nil)))) ; Unknown gate

(defn transform-parse-tree
  "Transform the Instaparse parse tree into a circuit structure using purely functional approach.
  
  This function uses insta/transform to process the parse tree in a functional way,
  collecting quantum declarations and gate operations into data structures,
  then building the circuit from this collected data without any mutable state."
  [parse-tree qasm-code]
  (let [;; Extract result specs from the original QASM code (since comments aren't in parse tree)
        qasm-lines (str/split-lines qasm-code)
        result-specs (collect-result-specs-from-qasm qasm-lines)
        
        ;; Collect circuit data using insta/transform with accumulator
        collected-data 
        (insta/transform
          {:qreg_declaration 
           (fn [_ _identifier designator _]
             (when-let [size (extract-array-size designator)]
               {:type :quantum-declaration :num-qubits size}))
           
           :builtin_gate_statement
           (fn [gate-name & args]
             (when-let [operation (parse-gate-call gate-name args)]
               {:type :gate-operation :operation operation}))
           
           :gate_call_statement
           (fn [gate-name & args]
             ;; Handle special case where qreg/creg are parsed as gate_call_statement
             ;; due to grammar precedence issues
             (let [gate-name-str (if (and (vector? gate-name) 
                                         (= :Identifier (first gate-name)))
                                  (second gate-name)
                                  gate-name)]
               (cond
                 ;; qreg declaration parsed as gate call
                 (= gate-name-str "qreg")
                 (when-let [size (extract-qreg-size (first args))]  ; Extract from id_list
                   {:type :quantum-declaration :num-qubits size})
                 
                 ;; creg declaration parsed as gate call - ignore for now
                 (= gate-name-str "creg")
                 nil
                 
                 ;; Regular gate call
                 :else
                 (when-let [operation (parse-gate-call gate-name args)]
                   {:type :gate-operation :operation operation}))))
           
           :statement
           (fn [content]
             ;; Pass through the actual content if it's a map with our data
             (if (and (map? content) (:type content))
               content
               nil))  ; Filter out other statement types
           
           :program 
           (fn [& statements] 
             ;; Filter out nil values and collect the actual data
             {:type :program :data (filter some? statements)})}
          parse-tree)
        
        ;; Extract the data from the transformed tree
        program-data (when (and (map? collected-data) 
                               (= :program (:type collected-data)))
                      (:data collected-data))
        
        ;; Separate quantum declarations and gate operations functionally
        quantum-decl (first (filter #(and (map? %) (= :quantum-declaration (:type %))) program-data))
        gate-ops (filter #(and (map? %) (= :gate-operation (:type %))) program-data)
        
        ;; Get number of qubits
        num-qubits (or (:num-qubits quantum-decl) 0)
        
        ;; Extract operations (filter out nil operations from identity gates)
        operations (filter some? (map :operation gate-ops))
        
        ;; Build circuit functionally using reduce
        circuit (reduce 
                  (fn [circ operation]
                    (circuit/add-gate circ 
                                      (:operation-type operation) 
                                      (:operation-params operation)))
                  (circuit/create-circuit num-qubits "Parsed QASM2 Circuit")
                  operations)]
    
    ;; Return result with parsed result specs
    {:circuit circuit
     :result-specs result-specs}))

;;;
;;; QASM2 Emitter
;;;
(defn gate-to-qasm2
  "Convert a single gate operation to QASM2 syntax using operation registry information."
  [gate]
  (let [gate-type (:operation-type gate)
        params (:operation-params gate)
        gate-info (opreg/get-gate-info-with-alias gate-type)]

    (if gate-info
      (let [operation-name (:operation-name gate-info)
            qasm-name (str/lower-case operation-name)]
        (case (:operation-type gate-info)
          :single-qubit
          (let [target (:target params)]
            (case gate-type
              ;; Handle special QASM2 gate names
              :s-dag (str "sdg q[" target "];")
              :t-dag (str "tdg q[" target "];")
              :i (str "id q[" target "];")  ; Identity gate uses "id" in QASM2
              (str qasm-name " q[" target "];")))

          :parametric
          (let [target (:target params)
                angle (:angle params)]
            (case gate-type
              :phase (str "p(" angle ") q[" target "];")  ; phase gate uses "p" in QASM2
              (str qasm-name "(" angle ") q[" target "];")))

          :two-qubit
          (let [control (:control params)
                target (:target params)
                qubit1 (:qubit1 params)
                qubit2 (:qubit2 params)]
            ;; Handle special cases for QASM2 naming conventions
            (case gate-type
              :cnot (str "cx q[" control "], q[" target "];")
              :swap (str "swap q[" qubit1 "], q[" qubit2 "];")  ; swap uses qubit1/qubit2
              :iswap (str "iswap q[" qubit1 "], q[" qubit2 "];")  ; iswap uses qubit1/qubit2
              (str qasm-name " q[" control "], q[" target "];")))  ; default uses control/target

          :multi-qubit
          (case gate-type
            :toffoli (let [control1 (:control1 params)
                           control2 (:control2 params)
                           target (:target params)]
                       (str "ccx q[" control1 "], q[" control2 "], q[" target "];"))
            :fredkin (let [control (:control params)
                           target1 (:target1 params)
                           target2 (:target2 params)]
                       (str "cswap q[" control "], q[" target1 "], q[" target2 "];"))
            (str "// Multi-qubit gate: " (name gate-type)))

          ;; Default case
          (str "// Unsupported gate type: " (:operation-type gate-info))))

      ;; Gate not found in registry
      (str "// Unknown gate: " (name gate-type)))))

;;;
;;; Public API
;;;
(defn qasm-to-circuit
  "Parse OpenQASM 2.0 code into a quantum circuit.
  
  Returns a map with:
  - :circuit - the quantum circuit object
  - :result-specs - any measurement or result specifications"
  [qasm-code]
  (let [;; Preprocess to remove comments for parsing
        preprocessed-code (preprocess-qasm qasm-code)
        parse-result (qasm2-parser preprocessed-code)]
    (if (insta/failure? parse-result)
      (throw (ex-info "Parse error" {:error parse-result}))
      (transform-parse-tree parse-result qasm-code))))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM 2.0 code with result type support.
  
  Parameters:
  - circuit: Quantum circuit to convert
  - result-specs: (optional) Map specifying result extraction requirements
  
  Returns:
  String containing QASM 2.0 code with result pragmas as comments"
  ([circuit]
   (circuit-to-qasm circuit nil))
  ([circuit result-specs]
   (let [header (str "OPENQASM 2.0;\n"
                     "include \"qelib1.inc\";\n"
                     "\nqreg q[" (:num-qubits circuit) "];\n"
                     "creg c[" (:num-qubits circuit) "];\n\n")
         
         gates-qasm (str/join "\n" (map gate-to-qasm2 (:operations circuit)))
         
         ;; Add result specifications as pragmas if provided
         pragmas-qasm (when result-specs
                        (result-specs-to-qasm-pragmas result-specs))
         
         footer "\nmeasure q -> c;"]
     
     (str header gates-qasm pragmas-qasm footer))))

