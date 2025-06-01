(ns org.soulspace.qclojure.adapter.io
  "Input/Output adapters for quantum computing library"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-gate :as qg]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]))

;; Data format specifications
(s/def ::file-format #{:edn :json :csv :qasm})
(s/def ::quantum-data (s/or :state ::qs/quantum-state
                            :circuit ::qc/quantum-circuit
                            :result map?))

;; State serialization and deserialization
(defn complex-to-map
  "Convert a fastmath complex number to a serializable map.
  
  Parameters:
  - z: fastmath complex number (Vec2)
  
  Returns:
  Map with :real and :imag keys"
  [z]
  {:real (fc/re z)
   :imag (fc/im z)})

(defn map-to-complex
  "Convert a map back to fastmath complex number.
  
  Parameters:
  - m: Map with :real and :imag keys
  
  Returns:
  fastmath complex number"
  [m]
  (fc/complex (:real m) (:imag m)))

(defn serialize-quantum-state
  "Serialize a quantum state to a portable format.
  
  Converts the quantum state to a format that can be saved to files
  or transmitted over networks. Complex numbers are converted to
  maps with real and imaginary parts.
  
  Parameters:
  - state: Quantum state to serialize
  
  Returns:
  Map with serializable data"
  [state]
  {:state-vector (mapv complex-to-map (:state-vector state))
   :num-qubits (:num-qubits state)
   :metadata (or (:metadata state) {})
   :format-version "1.0"})

(defn deserialize-quantum-state
  "Deserialize a quantum state from portable format.
  
  Parameters:
  - data: Serialized quantum state data
  
  Returns:
  Quantum state with fastmath complex numbers"
  [data]
  {:state-vector (mapv map-to-complex (:state-vector data))
   :num-qubits (:num-qubits data)
   :metadata (:metadata data)})

(defn serialize-quantum-circuit
  "Serialize a quantum circuit to portable format.
  
  Parameters:
  - circuit: Quantum circuit to serialize
  
  Returns:
  Serializable circuit data"
  [circuit]
  {:gates (:gates circuit)
   :num-qubits (:num-qubits circuit)
   :name (:name circuit)
   :description (:description circuit)
   :metadata (or (:metadata circuit) {})
   :format-version "1.0"})

(defn deserialize-quantum-circuit
  "Deserialize a quantum circuit from portable format.
  
  Parameters:
  - data: Serialized circuit data
  
  Returns:
  Quantum circuit"
  [data]
  (select-keys data [:gates :num-qubits :name :description :metadata]))

;; File I/O operations
(defn write-quantum-data
  "Write quantum data to a file in specified format.
  
  Parameters:
  - filename: Output file path
  - data: Quantum state, circuit, or algorithm result
  - format: File format (:edn, :json, :csv)
  
  Returns:
  Boolean indicating success"
  [filename data format]
  {:pre [(s/valid? ::file-format format)]}
  
  (try
    (case format
      :edn (spit filename (pr-str data))
      :json (spit filename (json/write-str data :indent true))
      :csv (throw (ex-info "CSV format not yet implemented for quantum data" 
                          {:format format})))
    true
    (catch Exception e
      (println "Error writing file:" (.getMessage e))
      false)))

(defn read-quantum-data
  "Read quantum data from a file.
  
  Parameters:
  - filename: Input file path
  - format: Expected file format
  
  Returns:
  Deserialized quantum data"
  [filename format]
  {:pre [(s/valid? ::file-format format)]}
  
  (try
    (case format
      :edn (edn/read-string (slurp filename))
      :json (json/read-str (slurp filename) :key-fn keyword)
      :csv (throw (ex-info "CSV format not yet implemented" {:format format})))
    (catch Exception e
      (println "Error reading file:" (.getMessage e))
      nil)))

;; Export to quantum programming languages
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
                           :cnot (str "cx q[" (:control params) "],q[" (:target params) "];")
                           :rx (str "rx(" (:angle params) ") q[" (:target params) "];")
                           :ry (str "ry(" (:angle params) ") q[" (:target params) "];")
                           :rz (str "rz(" (:angle params) ") q[" (:target params) "];")
                           (str "// Unknown gate: " gate-type))))
        
        gates-qasm (str/join "\n" (map gate-to-qasm (:gates circuit)))
        
        footer "\nmeasure q -> c;"]
    
    (str header gates-qasm footer)))

;; Result formatting and display
(defn format-quantum-state
  "Format quantum state for human-readable display.
  
  Parameters:
  - state: Quantum state
  - options: Display options map
    :precision - Number of decimal places (default 3)
    :threshold - Minimum amplitude to display (default 0.001)
    :format - :cartesian or :polar (default :cartesian)
  
  Returns:
  Formatted string representation"
  [state & {:keys [precision threshold format]
            :or {precision 3 threshold 0.001 format :cartesian}}]
  
  (let [amplitudes (:state-vector state)
        n-qubits (:num-qubits state)
        
        format-complex (fn [z]
                         (case format
                           :cartesian (let [                                       r (fc/re z)
                                       i (fc/im z)]
                                       (cond
                                         (and (< (abs r) threshold) (< (abs i) threshold)) "0"
                                         (< (abs i) threshold) (str (qmath/round-precision r precision))
                                         (< (abs r) threshold) (str (qmath/round-precision i precision) "i")
                                         :else (str (qmath/round-precision r precision) 
                                                   (if (>= i 0) "+" "")
                                                   (qmath/round-precision i precision) "i")))
                           :polar (let [mag (fc/abs z)
                                       phase (fc/arg z)]
                                   (if (< mag threshold)
                                     "0"
                                     (str (qmath/round-precision mag precision) 
                                         "∠" (qmath/round-precision (m/degrees phase) 1) "°")))))
        
        basis-labels (for [i (range (bit-shift-left 1 n-qubits))]
                       (str "|" (format (str "%0" n-qubits "d") 
                                       (Long/parseLong (Integer/toBinaryString i) 2)) "⟩"))
        
        non-zero-terms (filter (fn [[amp label]] 
                                (> (fc/abs amp) threshold))
                              (map vector amplitudes basis-labels))]
    
    (if (empty? non-zero-terms)
      "|0⟩"
      (str/join " + " 
                (map (fn [[amp label]]
                       (let [formatted-amp (format-complex amp)]
                         (cond
                           (= formatted-amp "1") label
                           (= formatted-amp "-1") (str "-" label)
                           :else (str formatted-amp label))))
                     non-zero-terms)))))

(defn format-measurement-result
  "Format quantum measurement results for display.
  
  Parameters:
  - measurements: Collection of measurement outcomes
  - options: Display options
  
  Returns:
  Formatted string with statistics"
  [measurements & {:keys [show-histogram show-probabilities]
                   :or {show-histogram true show-probabilities true}}]
  
  (let [freq-map (frequencies measurements)
        total-measurements (count measurements)
        sorted-outcomes (sort (keys freq-map))
        
        format-outcome (fn [outcome]
                        (if (number? outcome)
                          (str "|" outcome "⟩")
                          (str outcome)))
        
        histogram-lines (when show-histogram
                          (map (fn [outcome]
                                 (let [count (freq-map outcome)
                                       percentage (/ count total-measurements)
                                       bar-length (int (* percentage 50))
                                       bar (str/join (repeat bar-length "█"))]
                                   (str (format-outcome outcome) ": " 
                                       count " (" 
                                       (qmath/round-precision (* percentage 100) 1) "%) " 
                                       bar)))
                               sorted-outcomes))
        
        probability-lines (when show-probabilities
                            [(str "Total measurements: " total-measurements)
                             (str "Distinct outcomes: " (count freq-map))])]
    
    (str/join "\n" (concat probability-lines [""] histogram-lines))))

(defn export-algorithm-results
  "Export quantum algorithm results to various formats.
  
  Parameters:
  - results: Algorithm results map
  - filename-base: Base filename without extension  
  - formats: Collection of formats to export (:edn :json :txt)
  
  Returns:
  Map of format -> success boolean"
  [results filename-base formats]
  
  (let [export-format (fn [format]
                        (let [filename (str filename-base "." (name format))
                              success (case format
                                        :edn (write-quantum-data filename results :edn)
                                        :json (write-quantum-data filename results :json)
                                        :txt (do
                                               (spit filename (pr-str results))
                                               true))]
                          [format success]))]
    
    (into {} (map export-format formats))))

;; Logging and monitoring adapters
(defn create-quantum-logger
  "Create a logger for quantum computation events.
  
  Parameters:
  - log-file: Optional log file path
  - log-level: :debug :info :warn :error
  
  Returns:
  Logger function"
  [& {:keys [log-file log-level] :or {log-level :info}}]
  
  (let [levels {:debug 0 :info 1 :warn 2 :error 3}
        current-level (levels log-level)
        
        write-log (if log-file
                    (fn [message] 
                      (spit log-file (str message "\n") :append true))
                    println)]
    
    (fn [level message & args]
      (when (>= (levels level) current-level)
        (let [timestamp (.format (java.time.LocalDateTime/now)
                                (java.time.format.DateTimeFormatter/ofPattern 
                                 "yyyy-MM-dd HH:mm:ss"))
              formatted-message (apply format message args)
              log-entry (str "[" timestamp "] " 
                           (str/upper-case (name level)) ": " 
                           formatted-message)]
          (write-log log-entry))))))

(comment
  ;; REPL examples for I/O operations
  
  ;; Create and serialize a quantum state
  (def bell-state (-> (qs/zero-state 2)
                      (qg/h-gate 0)
                      (qg/cnot)))
  
  (def serialized-state (serialize-quantum-state bell-state))
  (def recovered-state (deserialize-quantum-state serialized-state))
  
  ;; Create and export a circuit
  (def bell-circuit (qc/bell-state-circuit))
  (def qasm-code (circuit-to-qasm bell-circuit))
  (println qasm-code)
  
  ;; Write and read quantum data
  (write-quantum-data "bell-state.edn" serialized-state :edn)
  (write-quantum-data "bell-circuit.json" (serialize-quantum-circuit bell-circuit) :json)
  
  (def loaded-state (read-quantum-data "bell-state.edn" :edn))
  (def loaded-circuit (read-quantum-data "bell-circuit.json" :json))
  
  ;; Format quantum states for display
  (println (format-quantum-state bell-state))
  (println (format-quantum-state bell-state :format :polar))
  
  ;; Create a logger
  (def qlogger (create-quantum-logger :log-file "quantum.log" :log-level :debug))
  (qlogger :info "Starting quantum computation")
  (qlogger :debug "Applied Hadamard gate to qubit 0")
  )
