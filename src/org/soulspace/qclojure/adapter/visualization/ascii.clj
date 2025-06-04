(ns org.soulspace.qclojure.adapter.visualization.ascii
  "ASCII-based visualization for quantum states and circuits.
  
  This namespace provides text-based visualizations that can be displayed
  in terminals, REPLs, and simple text outputs. All functions return strings
  containing formatted ASCII art representations."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.adapter.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coord]
            [org.soulspace.qclojure.adapter.visualization.common :as common]
            [clojure.spec.alpha :as s]))

(s/def ::quantum-data (s/or :state ::qs/quantum-state
                            :circuit ::qc/quantum-circuit
                            :result map?))

;;
;; Result formatting and display
;;
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

(comment
  (format-quantum-state qs/|0⟩)
  (format-quantum-state qs/|1⟩)
  (format-quantum-state qs/|00⟩)
  (format-quantum-state qs/|01⟩)
  (format-quantum-state qs/|10⟩)
  (format-quantum-state qs/|11⟩)

  (format-measurement-result [0 1 0 1 1 0 0 1]
                             :show-histogram false :show-probabilities false)
  (format-measurement-result [0 1 0 1 1 0 0 1]
                             :show-histogram false :show-probabilities true)
  (format-measurement-result [0 1 0 1 1 0 0 1]
                             :show-histogram true :show-probabilities false)
  (format-measurement-result [0 1 0 1 1 0 0 1]
                             :show-histogram true :show-probabilities true)
  ;
  )

;;; 
;;; ASCII Format Implementations
;;; 
(defmethod viz/visualize-quantum-state :ascii
  [_format state & {:keys [show-amplitudes show-phases threshold max-bars]
            :or {show-amplitudes false show-phases false
                 threshold 0.001 max-bars 16}}]

  (let [;; Use refactored bar chart method for main visualization
        main-chart (viz/visualize-bar-chart :ascii state 
                                            :threshold threshold 
                                            :max-bars max-bars)

        ;; Additional information when requested
        chart-data (common/prepare-bar-chart-data state
                                                  :threshold threshold
                                                  :max-bars max-bars)
        indices (:indices chart-data)
        filtered-labels (:labels chart-data)
        summary (:summary chart-data)

        amplitude-info (when show-amplitudes
                         (let [amp-details (common/extract-amplitude-info state indices filtered-labels)]
                           (str "\nAmplitudes:\n"
                                (str/join "\n"
                                          (map (fn [detail]
                                                 (str (:label detail) ": " (:amplitude detail)))
                                               amp-details)))))

        phase-info (when show-phases
                     (let [phase-details (common/calculate-phase-info state indices filtered-labels)]
                       (str "\nPhases:\n"
                            (str/join "\n"
                                      (map (fn [detail]
                                             (str (:label detail) ": " (:phase-degrees detail) "°"))
                                               phase-details)))))

        summary-text (str "\nState Summary:\n"
                          "Total qubits: " (:num-qubits summary) "\n"
                          "Total probability shown: " (qmath/round-precision (:percentage-shown summary) 1) "%\n"
                          (when (> (:hidden-states summary) 0)
                            (str "Hidden states (below threshold): " (:hidden-states summary) "\n")))]

    (str "Quantum State Probability Distribution:\n"
         main-chart
         amplitude-info
         phase-info
         summary-text)))

(defmethod viz/visualize-bloch-sphere :ascii
  [_format state & _options]
  {:pre [(= (:num-qubits state) 1)]}

  (let [coords (coord/quantum-state-to-bloch-coordinates state)
        {x :x z :z} (:cartesian coords)

        ;; Sphere rendering parameters
        radius 8.0

        ;; Generate ASCII sphere representation using character positioning
        sphere-chars (for [row (range -8 9)]
                       (for [col (range -16 17)]
                         (let [x-sphere (/ col 2.0)  ; Account for character aspect ratio
                               y-sphere (- row)
                               dist-from-center (m/sqrt (+ (* x-sphere x-sphere) (* y-sphere y-sphere)))]
                           (cond
                             ;; Draw the state point
                             (and (< (m/abs (- x-sphere (* x radius))) 1)
                                  (< (m/abs (- y-sphere (* z radius))) 1))
                             "●"

                             ;; Draw sphere outline
                             (and (< (m/abs (- dist-from-center radius)) 0.8)
                                  (>= dist-from-center (- radius 0.8)))
                             "·"

                             ;; Draw coordinate axes
                             (and (< (m/abs x-sphere) 0.5) (< (m/abs y-sphere) 0.5))
                             "+"

                             ;; Empty space
                             :else " "))))

        sphere-string (str/join "\n" (map #(str/join "" %) sphere-chars))

        ;; Use common utilities for display data
        display-data (common/prepare-bloch-display-data state coords
                                                        :show-coordinates true
                                                        :show-distances true
                                                        :precision 3
                                                        :format :ascii)
        
        distance-text (str "Distances: " (common/format-reference-distances (:distances display-data)))]

    (str "Bloch Sphere Visualization:\n\n"
         sphere-string "\n\n"
         "State: " (:state-expression display-data) "\n"
         "Coordinates: " (get-in display-data [:coordinates :spherical-text]) "\n"
         "Bloch vector: " (get-in display-data [:coordinates :cartesian-text]) "\n\n"
         (:legend display-data) "\n"
         distance-text)))

(def gate-symbols
  "Map of gate types to their ASCII symbols.
  
  Used for circuit diagram visualization."
  {:x "X" :y "Y" :z "Z" :h "H" :s "S" :t "T"
   :s-dag "S†" :t-dag "T†"
   :cnot "●" :cx "●" :cy "●" :cz "●"
   :target "⊕"
   :rx "RX" :ry "RY" :rz "RZ" :phase "P"
   :crx "●" :cry "●" :crz "●"
   :swap "×" :iswap "i×"
   :toffoli "●●" :fredkin "●×"})

(defn add-gate-to-lines
  "Helper function to add a gate to the ASCII circuit lines.
  
  Parameters:
  - lines: Current circuit lines
  - gate: Gate definition map
  - gate-pos: Position of the gate in the circuit
  - gate-symbols: Sybmbols of the gates
  
  Returns:
  Updated circuit lines with the gate added"
[lines gate gate-pos gate-symbols]
(let [gate-type (:gate-type gate)
      params (:gate-params gate)
      target (:target params)
      control (:control params)
      ;; Add spacing between gates
      ;gate-spacing (if (> gate-pos 0) "─" "")
      gate-spacing "─"
      ]
  (case gate-type
    ;; Standard CNOT gate
    :cnot (let [updated-lines (-> lines
                                  (update control #(str % gate-spacing "●─"))
                                  (update target #(str % gate-spacing "⊕─")))
                min-qubit (min control target)
                max-qubit (max control target)]
            ;; Add vertical connection line
            (mapv (fn [i line]
                    (if (and (> i min-qubit) (< i max-qubit))
                      (str line gate-spacing "│─")
                      line))
                  (range (count updated-lines))
                  updated-lines))

    ;; Other controlled gates (CX, CY, CZ)
    (:cx :cy :cz) (let [updated-lines (-> lines
                                          (update control #(str % gate-spacing "●─"))
                                          (update target #(str % gate-spacing "["
                                                               (subs (name gate-type) 1)
                                                               "]─")))
                        min-qubit (min control target)
                        max-qubit (max control target)]
                    ;; Add vertical connection line
                    (mapv (fn [i line]
                            (if (and (> i min-qubit) (< i max-qubit))
                              (str line gate-spacing "│─")
                              line))
                          (range (count updated-lines))
                          updated-lines))

    ;; Controlled rotation gates (CRX, CRY, CRZ)
    (:crx :cry :crz) (let [rotation-type (subs (name gate-type) 1)
                           updated-lines (-> lines
                                             (update control #(str % gate-spacing "●─"))
                                             (update target #(str % gate-spacing "["
                                                                  rotation-type
                                                                  "]─")))
                           min-qubit (min control target)
                           max-qubit (max control target)]
                       ;; Add vertical connection line
                       (mapv (fn [i line]
                               (if (and (> i min-qubit) (< i max-qubit))
                                 (str line gate-spacing "│─")
                                 line))
                             (range (count updated-lines))
                             updated-lines))

    ;; SWAP gate
    :swap (let [qubit1 (:qubit1 params)
                qubit2 (:qubit2 params)
                updated-lines (-> lines
                                  (update qubit1 #(str % gate-spacing "×─"))
                                  (update qubit2 #(str % gate-spacing "×─")))
                min-qubit (min qubit1 qubit2)
                max-qubit (max qubit1 qubit2)]
            ;; Add vertical connection line
            (mapv (fn [i line]
                    (if (and (> i min-qubit) (< i max-qubit))
                      (str line gate-spacing "│─")
                      line))
                  (range (count updated-lines))
                  updated-lines))

    ;; iSWAP gate
    :iswap (let [qubit1 (:qubit1 params)
                 qubit2 (:qubit2 params)
                 updated-lines (-> lines
                                   (update qubit1 #(str % gate-spacing "i×─"))
                                   (update qubit2 #(str % gate-spacing "i×─")))
                 min-qubit (min qubit1 qubit2)
                 max-qubit (max qubit1 qubit2)]
             ;; Add vertical connection line
             (mapv (fn [i line]
                     (if (and (> i min-qubit) (< i max-qubit))
                       (str line gate-spacing "│─")
                       line))
                   (range (count updated-lines))
                   updated-lines))

    ;; Toffoli gate (CCX)
    :toffoli (let [control1 (:control1 params)
                   control2 (:control2 params)
                   target (:target params)
                   updated-lines (-> lines
                                     (update control1 #(str % gate-spacing "●─"))
                                     (update control2 #(str % gate-spacing "●─"))
                                     (update target #(str % gate-spacing "⊕─")))
                   all-qubits [control1 control2 target]
                   min-qubit (apply min all-qubits)
                   max-qubit (apply max all-qubits)]
               ;; Add vertical connection lines
               (mapv (fn [i line]
                       (if (and (>= i min-qubit) (<= i max-qubit)
                                (not (contains? (set all-qubits) i)))
                         (str line gate-spacing "│─")
                         line))
                     (range (count updated-lines))
                     updated-lines))

    ;; Fredkin gate (CSWAP)
    :fredkin (let [control (:control params)
                   target1 (:target1 params)
                   target2 (:target2 params)
                   updated-lines (-> lines
                                     (update control #(str % gate-spacing "●─"))
                                     (update target1 #(str % gate-spacing "×─"))
                                     (update target2 #(str % gate-spacing "×─")))
                   all-qubits [control target1 target2]
                   min-qubit (apply min all-qubits)
                   max-qubit (apply max all-qubits)]
               ;; Add vertical connection lines
               (mapv (fn [i line]
                       (if (and (>= i min-qubit) (<= i max-qubit)
                                (not (contains? (set all-qubits) i)))
                         (str line gate-spacing "│─")
                         line))
                     (range (count updated-lines))
                     updated-lines))
    
    ;; Single-qubit gates
    (update lines target
            #(str % gate-spacing "["
                  (get gate-symbols gate-type (name gate-type))
                  "]─")))))

(defmethod viz/visualize-circuit :ascii
  [_format circuit & {:keys [show-measurements]
                      :or {show-measurements true}}]

  (let [n-qubits (:num-qubits circuit)
        gates (:gates circuit)

        ;; Gate symbols
        gate-symbols {:x "X" :y "Y" :z "Z" :h "H" :s "S" :t "T"
                      :s-dag "S†" :t-dag "T†" 
                      :cnot "●" :cx "●" :cy "●" :cz "●" 
                      :target "⊕" 
                      :rx "RX" :ry "RY" :rz "RZ" :phase "P"
                      :crx "●" :cry "●" :crz "●" 
                      :swap "×" :iswap "i×" 
                      :toffoli "●●" :fredkin "●×"}

        ;; Create qubit lines with initial spacing
        qubit-lines (mapv (fn [i]
                            (str "q" i " |0⟩─"))
                          (range n-qubits))


        ;; Process all gates
        final-lines (loop [lines qubit-lines
                           remaining-gates gates
                           gate-pos 0]
                      (if (empty? remaining-gates)
                        lines
                        (recur (add-gate-to-lines lines (first remaining-gates) gate-pos gate-symbols)
                               (rest remaining-gates)
                               (inc gate-pos))))

        ;; Add measurements if requested
        measurement-lines (if show-measurements
                            (mapv #(str % "╫═") final-lines)
                            final-lines)

        ;; Add header and footer
        header (str "Circuit: " (or (:name circuit) "Unnamed") "\n"
                    (when (:description circuit)
                      (str (:description circuit) "\n")))

        diagram (str/join "\n" measurement-lines)

        footer (str "\nGates: " (count gates)
                    ", Depth: " (qc/circuit-depth circuit))]

    (str header "\n" diagram footer)))

(defmethod viz/visualize-bar-chart :ascii
  [_format state & {:keys [width threshold max-bars] 
                    :or {width 40 threshold 0.001 max-bars 16}}]

  (let [;; Use common utilities for data extraction and filtering
        chart-data (common/prepare-bar-chart-data state
                                                  :threshold threshold
                                                  :max-bars max-bars)
        probabilities (:probabilities chart-data)
        labels (:labels chart-data)
        max-prob (:max-probability chart-data)
        
        ;; Create normalized probabilities for bar length calculation
        normalize-prob #(if (zero? max-prob) 0 (/ % max-prob))

        create-bar (fn [prob label]
                     (let [bar-length (int (* (normalize-prob prob) width))
                           bar (str/join (repeat bar-length "█"))
                           spaces (str/join (repeat (- width bar-length) " "))
                           percentage (qmath/round-precision (* prob 100) 1)]
                       (str label ": " bar spaces " " percentage "%")))]

    (str/join "\n" (map create-bar probabilities labels))))

(defmethod viz/visualize-state-evolution :ascii
  [_format circuit initial-state & {:keys [show-probabilities]
                                    :or {show-probabilities true}}]

  (let [gates (:gates circuit)

        apply-gate-with-info (fn [acc gate]
                               (let [current-state (:state acc)
                                     new-state (qc/apply-gate-to-state current-state gate)
                                     frame {:step (inc (:step acc))
                                            :gate gate
                                            :state new-state
                                            :probabilities (when show-probabilities
                                                             (mapv #(* (fc/abs %) (fc/abs %))
                                                                   (:state-vector new-state)))}]
                                 (-> acc
                                     (assoc :state new-state)
                                     (update :frames conj frame))))

        initial-frame {:step 0
                       :gate nil
                       :state initial-state
                       :probabilities (when show-probabilities
                                        (mapv #(* (fc/abs %) (fc/abs %))
                                              (:state-vector initial-state)))}
        evolution (reduce apply-gate-with-info
                          {:step 0 :state initial-state :frames [initial-frame]}
                          gates)]

    (:frames evolution)))

(defmethod viz/visualize-algorithm-summary :ascii
  [_format algorithm-result & {:keys [show-circuit show-complexity]
                               :or {show-circuit true show-complexity true}}]

  (let [algorithm-name (or (:algorithm algorithm-result)
                           (get-in algorithm-result [:circuit :name])
                           "Unknown Algorithm")

        result-section (when (:result algorithm-result)
                         (str "Result: " (:result algorithm-result) "\n"))

        measurement-section (when (:measurements algorithm-result)
                              (str "Measurements: " (:measurements algorithm-result) "\n"))

        circuit-section (when (and show-circuit (:circuit algorithm-result))
                          (let [circuit-info (:circuit algorithm-result)]
                            (str "Circuit Information:\n"
                                 "  Qubits: " (:qubits circuit-info) "\n"
                                 "  Operations: " (:operations circuit-info) "\n")))

        complexity-section (when (and show-complexity (:complexity algorithm-result))
                             (let [comp (:complexity algorithm-result)]
                               (str "Complexity Analysis:\n"
                                    "  Classical: " (:classical comp) "\n"
                                    "  Quantum: " (:quantum comp) "\n"
                                    "  Speedup: " (:speedup comp) "\n")))]

    (str "═══ " algorithm-name " Summary ═══\n"
         result-section
         measurement-section
         circuit-section
         complexity-section)))

(comment
  ;; REPL examples for ASCII visualization

  ;; Visualize quantum states
  (require '[qclojure.domain.quantum-gate :as qg])

  ;; Test basic bar chart
  (def probs [0.5 0.3 0.2])
  (def labels ["|00⟩" "|01⟩" "|10⟩"])
  (println (viz/visualize-bar-chart :ascii probs labels))

  ;; Visualize Bell state
  (def bell-state (-> (qs/zero-state 2)
                      (qg/h-gate 0)
                      (qg/cnot)))

  (println (viz/visualize-quantum-state :ascii bell-state))
  (println (viz/visualize-quantum-state :ascii bell-state :show-amplitudes true))

  ;; Visualize single qubit states
  (println (viz/visualize-bloch-sphere :ascii qs/|0⟩))
  (println (viz/visualize-bloch-sphere :ascii qs/|+⟩))
  (println (viz/visualize-bloch-sphere :ascii (qg/y-gate qs/|0⟩)))

  ;; Circuit diagram
  (def sample-circuit (qc/bell-state-circuit))
  (def all-gates-circuit (qc/all-gates-circuit))
  (println (viz/visualize-circuit :ascii sample-circuit))
  (println (viz/visualize-circuit :ascii all-gates-circuit))

  ;; State evolution
  (def evolution (viz/visualize-state-evolution :ascii sample-circuit (qs/zero-state 2)))
  (doseq [frame evolution]
    (println "Step" (:step frame)
             (when (:gate frame) (str "- Applied " (:gate-type (:gate frame)))))
    (println (viz/visualize-quantum-state (:state frame)))
    (println "---"))

  ;
  )
