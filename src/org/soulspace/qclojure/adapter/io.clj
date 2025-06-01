(ns org.soulspace.qclojure.adapter.io
  "Input/Output adapters for quantum computing library"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]))

;; Data format specifications
(s/def ::file-format #{:edn :json :qasm})
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

(defn serialize-quantum-data
  "Serialize quantum data to a portable format.
  
  Parameters:
  - data: Quantum state, circuit, or algorithm result
  
  Returns:
  Map with serializable data"
  [data]
  (cond
    (s/valid? ::qs/quantum-state data) (serialize-quantum-state data)
    (s/valid? ::qc/quantum-circuit data) (serialize-quantum-circuit data)
    :else (throw (ex-info "Unsupported quantum data type" {:data data}))))

(defn deserialize-quantum-data
  "Deserialize quantum data from portable format.
  
  Parameters:
  - data: Serialized quantum data
  
  Returns:
  Quantum state, circuit, or algorithm result"
  [data]
  (cond
    (contains? data :state-vector) (deserialize-quantum-state data)
    (contains? data :gates) (deserialize-quantum-circuit data)
    :else (throw (ex-info "Unsupported quantum data format" {:data data}))))

(defn file-format
  "Returns the file format to dispatch on."
  [format & _rest] format)

(defmulti export-quantum-circuit
  "Write a quantum circuit to a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format to write the circuit in
  - circuit: Quantum circuit data structure to write
  - filename: Output file path
  
  Returns:
  String containing the formatted quantum circuit"
  file-format)

(defmulti import-quantum-circuit
  "Read a quantum circuit from a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format of the input circuit
  - filename: Input file path

  Returns:
  Parsed quantum circuit data structure"
  file-format)

(defmulti export-quantum-state
  "Write quantum state to a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format to write the state in
  - state: Quantum state data structure to write
  - filename: Output file path
  
  Returns:
  Boolean indicating success"
  file-format)
(defmulti import-quantum-state
  "Read quantum state from a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format of the input state
  - filename: Input file path
  
  Returns:
  Deserialized quantum state data structure"
  file-format)

  (defmulti export-quantum-data
    "Write quantum data to a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format to write the data in
  - data: Quantum state, circuit, or algorithm result
  - filename: Output file path
  
  Returns:
  Boolean indicating success"
    file-format)

  (defmulti import-quantum-data
    "Read quantum data from a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format of the input data
  - filename: Input file path
  
  Returns:
  Deserialized quantum data"
    file-format)

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
                             :cartesian (let [r (fc/re z)
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

