(ns org.soulspace.qclojure.adapter.visualization.ascii
  "ASCII-based visualization for quantum states and circuits.
  
  This namespace provides text-based visualizations that can be displayed
  in terminals, REPLs, and simple text outputs. All functions return strings
  containing formatted ASCII art representations."
  (:require [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.adapter.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coord]
            [org.soulspace.qclojure.adapter.visualization.common :as common]))

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

(defmethod viz/visualize-circuit :ascii
  [_format circuit & {:keys [show-measurements]
                      :or {show-measurements true}}]

  (let [n-qubits (:num-qubits circuit)
        gates (:gates circuit)

        ;; Gate symbols
        gate-symbols {:x "X" :y "Y" :z "Z" :h "H" :s "S" :t "T"
                      :cnot "●" :target "⊕" :rx "RX" :ry "RY" :rz "RZ"}

        ;; Create qubit lines with initial spacing
        qubit-lines (mapv (fn [i]
                            (str "q" i " |0⟩─"))
                          (range n-qubits))

        ;; Add gates to diagram
        add-gate-to-lines (fn [lines gate gate-pos]
                            (let [gate-type (:gate-type gate)
                                  params (:gate-params gate)
                                  target (:target params)
                                  control (:control params)
                                  ;; Add spacing between gates
                                  gate-spacing (if (> gate-pos 0) "─" "")]
                              (case gate-type
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

                                ;; Single-qubit gates
                                (update lines target
                                        #(str % gate-spacing "["
                                              (get gate-symbols gate-type (name gate-type))
                                              "]─")))))

        ;; Process all gates
        final-lines (loop [lines qubit-lines
                           remaining-gates gates
                           gate-pos 0]
                      (if (empty? remaining-gates)
                        lines
                        (recur (add-gate-to-lines lines (first remaining-gates) gate-pos)
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
  (println (viz/visualize-circuit :ascii sample-circuit))

  ;; State evolution
  (def evolution (viz/visualize-state-evolution :ascii sample-circuit (qs/zero-state 2)))
  (doseq [frame evolution]
    (println "Step" (:step frame)
             (when (:gate frame) (str "- Applied " (:gate-type (:gate frame)))))
    (println (viz/visualize-quantum-state (:state frame)))
    (println "---"))
  
  ;
  )
