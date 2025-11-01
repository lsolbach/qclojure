(ns org.soulspace.qclojure.application.format.qasm3-parser
  "QASM3 parser and emitter using Instaparse.
  
  This namespace provides functions to parse OpenQASM 3.0 quantum circuits
  into the qclojure domain model and emit qclojure circuits back to QASM3 format."
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math :as math]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]))

(def pragma-target
  "Maps target keywords to QASM target strings."
  {:qclojure "qclojure"
   :braket "braket"})

;; Result specs parsing utilities (adapted from qasm3.clj)
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

(defn emit-qasm-pragmas
  "Convert result specifications to QASM 3.0 pragma comments."
  [options]
  (let [result-specs (:result-specs options)
        target (:target options :qclojure)]
    (when (seq result-specs)
      (let [pragma-lines
            (mapcat (fn [[result-type spec]]
                      (case result-type
                        :measurement
                        [(str "#pragma " (pragma-target target) " result measurement shots=" (get spec :shots 1000)
                              (when-let [qubits (:qubits spec)]
                                (str " qubits=" (str/join "," qubits))))]

                        :expectation
                        (let [observables (if (:observables spec)
                                            (:observables spec)
                                            [(:observable spec)])  ; Handle singular form
                              targets (if (:targets spec)
                                        (:targets spec)
                                        [(:target spec)])]       ; Handle singular form
                          (map-indexed
                           (fn [idx obs]
                             (str "#pragma " (pragma-target target) " result expectation observable="
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
                             (str "#pragma " (pragma-target target) " result variance observable="
                                  (name obs)
                                  (when (< idx (count targets))
                                    (str " target=" (nth targets idx)))))
                           observables))

                        :probability
                        [(str "#pragma " (pragma-target target) " result probability"
                              (when-let [targets (:targets spec)]
                                (str " targets=" (str/join "," targets)))
                              (when-let [states (:states spec)]
                                (str " states=" (str/join "," states))))]

                        :amplitude
                        [(str "#pragma " (pragma-target target) " result amplitude states="
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
                             (str "#pragma " (pragma-target target) " result sample observable="
                                  (name obs) " shots=" shots
                                  (when (< idx (count targets))
                                    (str " target=" (nth targets idx)))))
                           observables))

                        ;; Simulation-only results
                        :state-vector
                        [(str "#pragma " (pragma-target target) " result state-vector // Simulation-only result")]
                        
                        :density-matrix
                        [(str "#pragma " (pragma-target target) " result density-matrix // Simulation-only result")]
                        
                        :fidelity
                        [(str "#pragma " (pragma-target target) " result fidelity // Simulation-only result")]

                        ;; Unknown result type
                        [(str "// Unknown result type: " (name result-type))]))
                    result-specs)]
        (when (seq pragma-lines)
          (str "\n" (str/join "\n" pragma-lines) "\n"))))))

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
;;; Parser using the EBNF grammar
;;;
(def ^:private qasm3-parser
  (insta/parser (io/resource "openqasm/qasm3.ebnf")
                :auto-whitespace :standard))

;;;
;;; Helper functions
;;;
(defn parse-simple-integer
  "Parse a simple integer from parse tree tokens.
   Only handles DecimalIntegerLiteral, not complex expressions."
  [tokens]
  (let [flat-tokens (flatten tokens)]
    (when-let [idx (some #(when (= (nth flat-tokens %) :DecimalIntegerLiteral) %)
                         (range (count flat-tokens)))]
      (when-let [value (nth flat-tokens (inc idx) nil)]
        (try
          (Integer/parseInt (str value))
          (catch Exception _
            nil))))))

(def constants
  "Constants according to OpenQASM 3 (used in expression evaluation)"
  {"pi" math/PI
   "π" math/PI
   "tau" (* 2.0 math/PI)
   "τ" (* 2.0 math/PI)
   "euler" math/E
   "ℇ" math/E})

(defn to-bool
  "Convert a value to boolean."
  [v]
  (cond
    (instance? Boolean v) v
    (number? v) (not (zero? (double v)))
    :else (throw (ex-info "Cannot cast to bool" {:value v}))))

(defn to-long
  "Convert a value to long integer."
  [v]
  (cond
    (nil? v) 0
    (instance? Boolean v) (if v 1 0)
    (integer? v) (long v)
    (number? v) (long (Math/floor (double v)))
    :else (throw (ex-info "Cannot cast to integer" {:value v}))))

(defn to-double
  "Convert a value to double."
  [v]
  (cond
    (nil? v) 0.0
    (instance? Boolean v) (if v 1.0 0.0)
    (number? v) (double v)
    :else (throw (ex-info "Cannot cast to double" {:value v}))))

(defn numeric-promote
  "Promote two numeric values to a common type (long or double).
   If either is floating-point, promote both to double.
   Otherwise, promote both to long."
  [a b]
  (if (or (float? a) (double? a) (float? b) (double? b)
          (and (number? a) (not (integer? a)))
          (and (number? b) (not (integer? b))))
    [:double (to-double a) (to-double b)]
    [:long (to-long a) (to-long b)]))

(defn bit-rotl
  "Rotate bits left (64-bit semantics)."
  [x dist]
  (let [v (to-long x)
        n (mod (to-long dist) 64)]
    (bit-or (bit-shift-left v n)
            (unsigned-bit-shift-right v (- 64 n)))))

(defn bit-rotr
  "Rotate bits right (64-bit semantics)."
  [x dist]
  (let [v (to-long x)
        n (mod (to-long dist) 64)]
    (bit-or (unsigned-bit-shift-right v n)
            (bit-shift-left v (- 64 n)))))

(defn double-mod
  "Floating-point modulus on doubles."
  [a b]
  (let [ad (to-double a) bd (to-double b)]
    (- ad (* (math/floor (/ ad bd)) bd))))

(defn pow
  "Power function for numeric types."
  [a b]
  (math/pow (to-double a) (to-double b)))

(defn op-str
  "Convert an operation identifier to string."
  [op]
  (cond
    (string? op) op
    (keyword? op) (name op)
    (vector? op) (let [s (second op)]
                   (if (string? s) s (name (first op))))
    :else (str op)))

(defn fold-infix
  "Helper to fold left-associative operator lists, where 'more' can be
   - alternating op rhs children, or
   - a single sequential group of alternating op rhs children."
  [x more apply-op]
  (let [m (cond
            (and (= 1 (count more)) (sequential? (first more))) (first more)
            :else more)]
    (loop [acc x, rest m]
      (if (seq rest)
        (let [op (first rest)
              rhs (second rest)]
          (recur (apply-op acc op rhs) (nnext rest)))
        acc))))

(defn apply-function
  "Apply a built-in QASM3 function to arguments.
   Supports common math functions and integer/bitwise operations.
   Arguments are already evaluated to Clojure values.
   Returns the computed value or throws if unsupported."
  [fname args]
  (let [f (str/lower-case fname)]
    (case f
      ;; trig and inverse trig
      "sin" (math/sin (to-double (first args)))
      "cos" (math/cos (to-double (first args)))
      "tan" (math/tan (to-double (first args)))
      "arcsin" (math/asin (to-double (first args)))
      "arccos" (math/acos (to-double (first args)))
      "arctan" (math/atan (to-double (first args)))

      ;; exp/log/sqrt
      "exp" (math/exp (to-double (first args)))
      "sqrt" (math/sqrt (to-double (first args)))
      "floor" (math/floor (to-double (first args)))
      "ceiling" (math/ceil (to-double (first args)))
      "ceil" (math/ceil (to-double (first args)))
      "log" (case (count args)
              1 (math/log (to-double (first args)))
              2 (/ (math/log (to-double (first args)))
                   (math/log (to-double (second args))))
              (throw (ex-info "log expects 1 or 2 args" {:args args})))

      ;; integer/bit ops helpers
      "pow" (pow (first args) (second args))
      "mod" (let [[a b] args]
              (if (and (integer? a) (integer? b))
                (long (mod (to-long a) (to-long b)))
                (double-mod a b)))
      "popcount" (long (Long/bitCount (to-long (first args))))
      "rotl" (let [[v d] args] (bit-rotl v d))
      "rotr" (let [[v d] args] (bit-rotr v d))

      ;; real/imag stubs (complex not yet supported)
      "real" (to-double (first args))
      "imag" 0.0

      (throw (ex-info "Unsupported or unknown function" {:fn f :args args})))))

(defn evaluate-expression
  "Evaluate a QASM3 mathematical expression to a numeric value.
   Input is an Instaparse subtree rooted at :expression (or any expression non-terminal).
   Supports OpenQASM 3 operators (precedence/associativity), numeric/boolean literals,
   built-in constants (pi, τ/tau, euler/ℇ) and common built-in functions on const values.

   Returns Clojure numbers/booleans. Bitwise ops use 64-bit semantics.
   Unknown identifiers are not resolved and will throw."
  [expr-tokens]
  (let [; constants according to OpenQASM 3
        consts {"pi" Math/PI, "π" Math/PI
                "tau" (* 2.0 Math/PI), "τ" (* 2.0 Math/PI)
                "euler" Math/E, "ℇ" Math/E}

        ;; The actual transform map for insta/transform
        tmap {;; leaf tokens
              :FloatLiteral (fn [s] (Double/parseDouble s))
              :DecimalIntegerLiteral (fn [s] (Long/parseLong s))
              :BooleanLiteral (fn [s] (= s "true"))
              :Number (fn [x] x)
              :Identifier (fn [s] s)
              :HardwareQubit (fn [s]
                               (throw (ex-info "Hardware qubit not valid in const expression"
                                               {:identifier s})))
              ;; delimiters and operators to plain strings
              :LPAREN (fn [_] "(")
              :RPAREN (fn [_] ")")
              :COMMA (fn [_] ",")
              :PLUS (fn [_] "+")
              :MINUS (fn [_] "-")
              :ASTERISK (fn [_] "*")
              :DOUBLE_ASTERISK (fn [_] "**")
              :SLASH (fn [_] "/")
              :PERCENT (fn [_] "%")
              :AMPERSAND (fn [_] "&")
              :CARET (fn [_] "^")
              :PIPE (fn [_] "|")
              :DOUBLE_AMPERSAND (fn [_] "&&")
              :DOUBLE_PIPE (fn [_] "||")
              :TILDE (fn [_] "~")
              :EXCLAMATION_POINT (fn [_] "!")
              :EqualityOperator (fn [s] s)
              :ComparisonOperator (fn [s] s)

              ;; expression lists -> vector of values
              :expression_list (fn [& xs]
                                 (->> xs (remove string?) vec))

              ;; literal wrapper (e.g. [:literal [:FloatLiteral "1.2345"]])
              :literal (fn [& xs]
                         (first xs))

              ;; call expressions
              :call_expression (fn [& xs]
                                 ;; xs typically: name, "(", expr-list?, ")". Identify name and optional vector args
                                 (let [name (first xs)
                                       args (or (some #(when (vector? %) %) xs) [])]
                                   (apply-function (str name) args)))

              ;; parentheses primary
              :primary (fn [& xs]
                         ;; return inner expression value; if a bare identifier, treat as constant
                         (let [non-delims (remove #(or (= % "(") (= % ")")) xs)
                               v (first non-delims)]
                           (cond
                             (nil? v) (throw (ex-info "Empty primary" {:children xs}))
                             (string? v) (if-let [c (get consts v)]
                                           c
                                           (throw (ex-info "Unknown identifier in constant expression" {:identifier v})))
                             :else v)))

              ;; unary
              :unary (fn [& xs]
                       (cond
                         (= 1 (count xs)) (first xs)
                         (= 2 (count xs)) (let [[op v] xs]
                                            (case op
                                              "-" (- (to-double v))
                                              "+" (to-double v)
                                              "!" (not (to-bool v))
                                              "~" (bit-not (to-long v))
                                              (throw (ex-info "Unknown unary operator" {:op op}))))
                         :else (throw (ex-info "Invalid unary form" {:children xs}))))

              ;; power: right-associative a ** b
              :power (fn [& xs]
                       (case (count xs)
                         1 (first xs)
                         3 (let [[a op b] xs]
                             (if (= op "**")
                               (pow a b)
                               (throw (ex-info "Unexpected operator in power" {:op op}))))
                         (throw (ex-info "Invalid power form" {:children xs}))))

              ;; multiplicative: left-assoc *, /, %
              :multiplicative (fn [x & more]
                                (fold-infix x more (fn [a op b]
                                                     (case (op-str op)
                                                       "*" (let [[_ x y] (numeric-promote a b)]
                                                             (if (= _ :double) (* x y) (long (* x y))))
                                                       "/" (/ (to-double a) (to-double b))
                                                       "%" (if (and (integer? a) (integer? b))
                                                             (long (mod (to-long a) (to-long b)))
                                                             (double-mod a b))
                                                       (throw (ex-info "Unknown multiplicative op" {:op op}))))))

              ;; additive: left-assoc +, -
              :additive (fn [x & more]
                          (fold-infix x more (fn [a op b]
                                               (case (op-str op)
                                                 "+" (let [[k x y] (numeric-promote a b)]
                                                       (if (= k :double) (+ x y) (long (+ x y))))
                                                 "-" (let [[k x y] (numeric-promote a b)]
                                                       (if (= k :double) (- x y) (long (- x y))))
                                                 (throw (ex-info "Unknown additive op" {:op op}))))))

              ;; comparison: <, >, <=, >=
              :comparison (fn [x & more]
                            (fold-infix x more (fn [a op b]
                                                 (case (op-str op)
                                                   "<" (< (to-double a) (to-double b))
                                                   ">" (> (to-double a) (to-double b))
                                                   "<=" (<= (to-double a) (to-double b))
                                                   ">=" (>= (to-double a) (to-double b))
                                                   (throw (ex-info "Unknown comparison op" {:op op}))))))

              ;; equality: ==, !=
              :equality (fn [x & more]
                          (fold-infix x more (fn [a op b]
                                               (case (op-str op)
                                                 "==" (= (to-double a) (to-double b))
                                                 "!=" (not= (to-double a) (to-double b))
                                                 (throw (ex-info "Unknown equality op" {:op op}))))))

              ;; bitwise and/xor/or
              :bitwise_and (fn [x & more]
                             (fold-infix x more (fn [a op b]
                                                  (if (= (op-str op) "&")
                                                    (bit-and (to-long a) (to-long b))
                                                    (throw (ex-info "Unknown bitwise and op" {:op op}))))))
              :bitwise_xor (fn [x & more]
                             (fold-infix x more (fn [a op b]
                                                  (if (= (op-str op) "^")
                                                    (bit-xor (to-long a) (to-long b))
                                                    (throw (ex-info "Unknown bitwise xor op" {:op op}))))))
              :bitwise_or (fn [x & more]
                            (fold-infix x more (fn [a op b]
                                                 (if (= (op-str op) "|")
                                                   (bit-or (to-long a) (to-long b))
                                                   (throw (ex-info "Unknown bitwise or op" {:op op}))))))

              ;; logical and/or
              :logical_and (fn [x & more]
                             (fold-infix x more (fn [a op b]
                                                  (if (= (op-str op) "&&")
                                                    (and (to-bool a) (to-bool b))
                                                    (throw (ex-info "Unknown logical and op" {:op op}))))))
              :logical_or (fn [x & more]
                            (fold-infix x more (fn [a op b]
                                                 (if (= (op-str op) "||")
                                                   (or (to-bool a) (to-bool b))
                                                   (throw (ex-info "Unknown logical or op" {:op op}))))))

              ;; top level
              :expression (fn [x] x)}]

    (try
      (insta/transform tmap expr-tokens)
      (catch Exception e
        (throw (ex-info "Failed to evaluate OpenQASM expression"
                        {:expr expr-tokens}
                        e))))))

(defn extract-array-size
  "Extract the array size from a designator parse node."
  [designator]
  (when designator
    (let [expr (get-in designator [2])] ; [:designator [:LBRACKET "["] [:expression ...] [:RBRACKET "]"]]
      (when expr
        ;; For array sizes, we expect simple integers, not complex expressions
        (or (parse-simple-integer [expr])
            (evaluate-expression [expr]))))))

(defn extract-qubit-index
  "Extract qubit index from an indexed identifier."
  [operand]
  ;; Structure: [:gate_operand [:indexed_identifier [:Identifier "q"] [:index_operator ...]]]
  (when-let [indexed-id (second operand)]
    (when (and (vector? indexed-id) (= :indexed_identifier (first indexed-id)))
      (let [index-ops (filter #(and (vector? %) (= :index_operator (first %))) indexed-id)]
        (when-let [index-op (first index-ops)]
          ;; For qubit indices, we expect simple integers, not complex expressions
          (let [index-expr (get-in index-op [2])]  ; [:index_operator [:LBRACKET "["] [:expression ...] [:RBRACKET "]"]]
            (parse-simple-integer [index-expr])))))))

(defn extract-gate-parameters
  "Extract parameter values from an Instaparse expression_list node.
   Expects a parse node of the form [:expression_list expr1 expr2 ...].
   Returns a vector of evaluated parameter values."
  [expression-list-node]
  (when (and expression-list-node (vector? expression-list-node))
    (let [expr-nodes (filter vector? (rest expression-list-node))]
      (when (seq expr-nodes)
        (vec (map evaluate-expression expr-nodes))))))

(defn extract-gate-operands
  "Extract qubit operand indices from a gate_operand_list parse node.
   Expects a node like [:gate_operand_list [:gate_operand ...] ...]."
  [gate-operand-list-node]
  (when (and gate-operand-list-node (vector? gate-operand-list-node))
    (let [operand-nodes (filter #(and (vector? %) (= :gate_operand (first %)))
                                (rest gate-operand-list-node))]
      (vec (keep extract-qubit-index operand-nodes)))))

(defn extract-gate-name
  "Extract gate name from gate call node."
  [gate-call-node]
  ;; Gate call structure: [:gate_call_statement [:Identifier "gate_name"] [:gate_operand_list ...] ...]
  (when (and (vector? gate-call-node) (= :gate_call_statement (first gate-call-node)))
    (let [gate-name-node (second gate-call-node)]
      (when (and (vector? gate-name-node) (= :Identifier (first gate-name-node)))
        (second gate-name-node)))))

(defn parse-gate-call
  "Parse QASM gate call and convert to circuit operation using circuit namespace functions."
  [gate-name-node & args]
  (let [gate-name (cond
                    ;; Sometimes the full gate_call node is passed
                    (and (vector? gate-name-node) (= :gate_call_statement (first gate-name-node)))
                    (extract-gate-name gate-name-node)
                    ;; Or an identifier node
                    (and (vector? gate-name-node) (= :Identifier (first gate-name-node)))
                    (second gate-name-node)
                    :else gate-name-node)
        all-args (vec args)

        ;; unwrap nested structure produced by insta/transform
        all-elements (if (and (seq all-args) (sequential? (first all-args))) (first all-args) all-args)

        ;; Use helper extractors for parameters and operands
        expression-list-node (first (filter #(and (vector? %) (= :expression_list (first %))) all-elements))
        gate-operand-list-node (first (filter #(and (vector? %) (= :gate_operand_list (first %))) all-elements))

        parameters (when expression-list-node
                     (extract-gate-parameters expression-list-node))

        operands (when gate-operand-list-node
                   (extract-gate-operands gate-operand-list-node))

        ;; Clean up any nil values
        clean-parameters (vec (remove nil? parameters))
        clean-operands (vec (remove nil? operands))

        ;; Resolve any gate aliases to canonical names
        canonical-gate (when (and gate-name (not= gate-name ""))
                         (opreg/resolve-gate-alias (keyword gate-name)))
        gate-info (when canonical-gate
                    (opreg/get-gate-info-with-alias canonical-gate))]

    (if (and gate-info canonical-gate)
      ;; Use circuit namespace functions to create a temporary circuit, then extract the operation
      (let [max-qubit (if (seq clean-operands) (apply max clean-operands) 0)
            temp-circuit (circuit/create-circuit (inc max-qubit) "temp")]  ; +1 because qubits are 0-indexed
        (try
          (let [updated-circuit
                (case canonical-gate
                  ;; Single-qubit gates
                  :h (circuit/h-gate temp-circuit (first clean-operands))
                  :x (circuit/x-gate temp-circuit (first clean-operands))
                  :y (circuit/y-gate temp-circuit (first clean-operands))
                  :z (circuit/z-gate temp-circuit (first clean-operands))
                  :s (circuit/s-gate temp-circuit (first clean-operands))
                  :s-dag (circuit/s-dag-gate temp-circuit (first clean-operands))
                  :t (circuit/t-gate temp-circuit (first clean-operands))
                  :t-dag (circuit/t-dag-gate temp-circuit (first clean-operands))

                  ;; Parametric single-qubit gates
                  :rx (circuit/rx-gate temp-circuit (first clean-operands) (or (first clean-parameters) 0.0))
                  :ry (circuit/ry-gate temp-circuit (first clean-operands) (or (first clean-parameters) 0.0))
                  :rz (circuit/rz-gate temp-circuit (first clean-operands) (or (first clean-parameters) 0.0))
                  :phase (circuit/phase-gate temp-circuit (first clean-operands) (or (first clean-parameters) 0.0))

                  ;; Two-qubit gates
                  :cnot (circuit/cnot-gate temp-circuit (first clean-operands) (second clean-operands))
                  :cx (circuit/cnot-gate temp-circuit (first clean-operands) (second clean-operands))  ; cx is alias for cnot
                  :cz (circuit/cz-gate temp-circuit (first clean-operands) (second clean-operands))
                  :cy (circuit/cy-gate temp-circuit (first clean-operands) (second clean-operands))
                  :swap (circuit/swap-gate temp-circuit (first clean-operands) (second clean-operands))
                  :iswap (circuit/iswap-gate temp-circuit (first clean-operands) (second clean-operands))

                  ;; Parametric two-qubit gates
                  :crx (circuit/crx-gate temp-circuit (first clean-operands) (second clean-operands) (or (first clean-parameters) 0.0))
                  :cry (circuit/cry-gate temp-circuit (first clean-operands) (second clean-operands) (or (first clean-parameters) 0.0))
                  :crz (circuit/crz-gate temp-circuit (first clean-operands) (second clean-operands) (or (first clean-parameters) 0.0))

                  ;; Multi-qubit gates
                  :toffoli (circuit/toffoli-gate temp-circuit (first clean-operands) (second clean-operands) (nth clean-operands 2))
                  :ccx (circuit/toffoli-gate temp-circuit (first clean-operands) (second clean-operands) (nth clean-operands 2))  ; ccx is alias
                  :fredkin (circuit/fredkin-gate temp-circuit (first clean-operands) (second clean-operands) (nth clean-operands 2))
                  :cswap (circuit/fredkin-gate temp-circuit (first clean-operands) (second clean-operands) (nth clean-operands 2))  ; cswap is alias

                  ;; Rydberg gates (if available)
                  :rydberg-cz (circuit/rydberg-cz-gate temp-circuit (first clean-operands) (second clean-operands))
                  :rydberg-cphase (circuit/rydberg-cphase-gate temp-circuit (first clean-operands) (second clean-operands) (or (first clean-parameters) 0.0))

                  ;; Global gates (if available) 
                  :global-h (circuit/global-hadamard-gate temp-circuit)
                  :global-x (circuit/global-x-gate temp-circuit)
                  :global-y (circuit/global-y-gate temp-circuit)
                  :global-z (circuit/global-z-gate temp-circuit)
                  :global-rx (circuit/global-rx-gate temp-circuit (or (first clean-parameters) 0.0))
                  :global-ry (circuit/global-ry-gate temp-circuit (or (first clean-parameters) 0.0))
                  :global-rz (circuit/global-rz-gate temp-circuit (or (first clean-parameters) 0.0))

                  ;; Fallback: use generic add-gate for any gates not explicitly handled
                  (do
                    (println (str "Using generic add-gate for: " canonical-gate))
                    (circuit/add-gate temp-circuit canonical-gate
                                      ;; Build params map based on operand count
                                      (case (count clean-operands)
                                        1 {:target (first clean-operands)}
                                        2 {:control (first clean-operands) :target (second clean-operands)}
                                        3 {:control1 (first clean-operands) :control2 (second clean-operands) :target (nth clean-operands 2)}
                                        {}))))]
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
         {:quantum_declaration
          (fn [_ designator _identifier _]
            (when-let [size (extract-array-size designator)]
              {:type :quantum-declaration :num-qubits size}))

          :gate_call_statement
          (fn [gate-name & args]
            (when-let [operation (parse-gate-call gate-name args)]
              {:type :gate-operation :operation operation}))

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

        ;; Extract operations
        operations (map :operation gate-ops)

        ;; Build circuit functionally using reduce
        circuit (reduce
                 (fn [circ operation]
                   (circuit/add-gate circ
                                     (:operation-type operation)
                                     (:operation-params operation)))
                 (circuit/create-circuit num-qubits "Parsed QASM3 Circuit")
                 operations)]

    ;; Return result with parsed result specs
    (assoc circuit :result-specs result-specs)))

;; Public API
(defn qasm-to-circuit
  "Parse OpenQASM 3.0 code into a quantum circuit.
  
  Returns a map with:
  - :circuit - the quantum circuit object
  - :result-specs - any measurement or result specifications"
  [qasm-code]
  (let [;; Extract result specs from original code (before comment removal)
        qasm-lines (str/split-lines qasm-code)
        result-specs (collect-result-specs-from-qasm qasm-lines)
        ;; Preprocess to remove comments for parsing
        preprocessed-code (preprocess-qasm qasm-code)
        parse-result (qasm3-parser preprocessed-code)]
    (if (insta/failure? parse-result)
      (throw (ex-info "Parse error" {:error parse-result}))
      (let [circuit (transform-parse-tree parse-result preprocessed-code)]
        ;; Merge result specs from original code
        (assoc circuit :result-specs (merge (:result-specs circuit) result-specs))))))

(defn gate-to-qasm3
  "Convert a single gate operation to QASM3 syntax using operation registry information."
  [gate options]
  (let [target (:target options :qclojure)
        gate-type (:operation-type gate)
        params (:operation-params gate)
        gate-info (opreg/get-gate-info-with-alias gate-type)]

    (if gate-info
      (let [operation-name (:operation-name gate-info)
            qasm-name (case gate-type
                        :phase "p"      ; QASM3 uses "p" for phase gate
                        :s-dag "sdg"    ; QASM3 uses "sdg" for S-dag 
                        :t-dag "tdg"    ; QASM3 uses "tdg" for T-dag
                        (str/lower-case operation-name))]
        (case (:operation-type gate-info)
          :single-qubit
          (let [target (:target params)]
            (str qasm-name " q[" target "];"))

          :parametric
          (let [target (:target params)
                angle (:angle params)]
            (str qasm-name "(" angle ") q[" target "];"))

          :two-qubit
          (let [control (:control params)
                target (:target params)
                qubit1 (:qubit1 params)
                qubit2 (:qubit2 params)]
            ;; Handle special cases for QASM naming conventions
            (case gate-type
              :cnot (if (= target :braket)
                      (str "cnot q[" control "], q[" target "];")
                      (str "cx q[" control "], q[" target "];"))
              :swap (str "swap q[" qubit1 "], q[" qubit2 "];")  ; swap uses qubit1/qubit2
              :iswap (str "iswap q[" qubit1 "], q[" qubit2 "];")  ; iswap uses qubit1/qubit2
              (str qasm-name " q[" control "], q[" target "];")))  ; default uses control/target

          :multi-qubit
          (case gate-type
            :toffoli (let [control1 (:control1 params)
                           control2 (:control2 params)
                           target (:target params)]
                       (if (= target :braket)
                         (str "ccnot q[" control1 "], q[" control2 "], q[" target "];")
                         (str "ccx q[" control1 "], q[" control2 "], q[" target "];")))
            :fredkin (let [control (:control params)
                           target1 (:target1 params)
                           target2 (:target2 params)]
                       (str "cswap q[" control "], q[" target1 "], q[" target2 "];"))
            (str "// Multi-qubit gate: " (name gate-type)))

          :global
          (str "// Global gate: " (name gate-type))

          :global-parametric
          (let [angle (:angle params)]
            (str "// Global parametric gate: " (name gate-type) "(" angle ")"))

          ;; Default case
          (str "// Unsupported gate type: " (:operation-type gate-info))))

      ;; Gate not found in registry
      (str "// Unknown gate: " (name gate-type)))))

(defn circuit-to-qasm
  "Convert a quantum circuit to OpenQASM 3.0 code with result type support.
  
  Parameters:
  - circuit: Quantum circuit to convert
  - result-specs: (optional) Map specifying result extraction requirements
  
  Returns:
  String containing QASM 3.0 code with result pragmas"
  ([circuit]
   (circuit-to-qasm circuit {}))
  ([circuit options]
   (let [target (:target options :qclojure)
         result-specs (:result-specs options)
         header (str "OPENQASM 3.0;\n"
                     (when-not (= target :braket)
                       "include \"stdgates.inc\";\n\n")
                     "qubit[" (:num-qubits circuit) "] q;\n"
                     "bit[" (:num-qubits circuit) "] c;\n\n")

         gates-qasm (str/join "\n" (map #(gate-to-qasm3 % options) (:operations circuit)))

         ;; Add result specifications as pragmas if provided
         pragmas-qasm (when result-specs
                        (emit-qasm-pragmas result-specs))]

     (str header gates-qasm pragmas-qasm))))

(comment
  ;; Example usage
  (def example-qasm "
    // Example OpenQASM 3.0 code
    OPENQASM 3.0;
    include \"stdgates.inc\";

    qubit[3] q;
    bit[3] c;

    h q[0];
    cx q[0], q[1];
    rz(1.5708) q[1];
    measure q[0] -> c[0];
    measure q[1] -> c[1];
    measure q[2] -> c[2];

    // Result specifications
    #pragma result count c
    #pragma result prob c
    #pragma result statevector c")

  (def parsed-circuit (qasm-to-circuit example-qasm))
  (println parsed-circuit)
  )
