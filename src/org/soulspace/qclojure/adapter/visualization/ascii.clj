(ns org.soulspace.qclojure.adapter.visualization.ascii
  "ASCII-based visualization for quantum states and circuits.
  
  This namespace provides text-based visualizations that can be displayed
  in terminals, REPLs, and simple text outputs. All functions return strings
  containing formatted ASCII art representations."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as vcoord]
            [org.soulspace.qclojure.adapter.visualization.common :as common]))

(s/def ::quantum-data (s/or :state ::qs/state
                            :circuit ::qc/circuit
                            :result map?))

;;
;; Result formatting and display
;;
(defn format-complex-number
  "Format a complex number for human-readable display.
   
   Parameters:
   - z: Complex number to format
   - format: :cartesian or :polar (default :cartesian)
   - precision: Number of decimal places (default 3)
   - threshold: Minimum amplitude to display (default 0.001)
   Returns:
   Formatted string representation of the complex number"
  ([z]
   (format-complex-number z :cartesian 3 0.001))
  ([z format precision]
   (format-complex-number z format precision 0.001))
  ([z format precision threshold]
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
                     "∠" (qmath/round-precision (fm/degrees phase) 1) "°"))))))

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

        basis-labels (qs/basis-labels n-qubits)

        non-zero-terms (filter (fn [[amp label]]
                                 (> (fc/abs amp) threshold))
                               (map vector amplitudes basis-labels))]

    (if (empty? non-zero-terms)
      "|0⟩"
      (str/join " + "
                (map (fn [[amp label]]
                       (let [formatted-amp (format-complex-number amp format precision threshold)]
                         (cond
                           (= formatted-amp "1") label
                           (= formatted-amp "-1") (str "-" label)
                           :else (str formatted-amp label))))
                     non-zero-terms)))))

(defn format-outcome
  "Format a measurement outcome for display.
  
  Parameters:
  - outcome: Measurement outcome (e.g., |0⟩, |1⟩, etc.)
  
  Returns:
  Formatted string representation of the outcome"
  [outcome]
  (if (number? outcome)
    (str "|" outcome "⟩")
    (str outcome)))

(defn format-history-line-fn
  [freq-map total-measurements]
  (fn [outcome]
    (let [count (freq-map outcome)
          percentage (/ count total-measurements)
          bar-length (int (* percentage 50))
          bar (str/join (repeat bar-length "█"))]
      (str (format-outcome outcome) ": "
           count " ("
           (qmath/round-precision (* percentage 100) 1) "%) "
           bar))))

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

        histogram-lines (when show-histogram
                          (map (format-history-line-fn freq-map total-measurements)
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

(defn- format-phase-info
  "Format phase information for display.
   
   Parameters:
   - state: Quantum state
   - chart-data: Map containing indices and labels for phases

   Returns:
   Formatted string with phase details"
  [state [indices labels]]
  (let [phase-details (common/calculate-phase-info state indices labels)]
    (str "\nPhases:\n"
         (str/join "\n"
                   (map (fn [detail]
                          (str (:label detail) ": " (:phase-degrees detail) "°"))
                        phase-details)))))

(defn- format-amplitude-info
  "Format amplitude information for display.
   
   Parameters:
   - state: Quantum state
   - chart-data: Map containing indices and labels for amplitudes

   Returns:
   Formatted string with amplitude details"
  [state [indices labels]]
  (let [amp-details (common/extract-amplitude-info state indices labels)]
    (str "\nAmplitudes:\n"
         (str/join "\n"
                   (map (fn [detail]
                          (str (:label detail) ": " (:amplitude detail)))
                        amp-details)))))

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
        summary (:summary chart-data)

        amplitude-info (when show-amplitudes
                         (format-amplitude-info state chart-data))

        phase-info (when show-phases
                     (format-phase-info state chart-data))

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

  (let [coords (vcoord/quantum-state-to-bloch-coordinates state)
        {x :x z :z} (:cartesian coords)

        ;; Sphere rendering parameters
        radius 8.0

        ;; Generate ASCII sphere representation using character positioning
        sphere-chars (for [row (range -8 9)]
                       (for [col (range -16 17)]
                         (let [x-sphere (/ col 2.0)  ; Account for character aspect ratio
                               y-sphere (- row)
                               dist-from-center (fm/sqrt (+ (* x-sphere x-sphere) (* y-sphere y-sphere)))]
                           (cond
                             ;; Draw the state point
                             (and (< (fm/abs (- x-sphere (* x radius))) 1)
                                  (< (fm/abs (- y-sphere (* z radius))) 1))
                             "●"

                             ;; Draw sphere outline
                             (and (< (fm/abs (- dist-from-center radius)) 0.8)
                                  (>= dist-from-center (- radius 0.8)))
                             "·"

                             ;; Draw coordinate axes
                             (and (< (fm/abs x-sphere) 0.5) (< (fm/abs y-sphere) 0.5))
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

(defn gate-symbols
  "Map of gate types to their ASCII symbols.
  Used for circuit diagram visualization."
  []
  {:x "X" :y "Y" :z "Z" :h "H" :s "S" :t "T"
   :s-dag "S†" :t-dag "T†"
   :cnot "●" :cx "●" :cy "●" :cz "●"
   :target "⊕"
   :rx "RX" :ry "RY" :rz "RZ" :phase "P"
   :crx "●" :cry "●" :crz "●"
   :swap "×" :iswap "i×"
   :toffoli "●●" :fredkin "●×"
   ;; Rydberg gates
   :rydberg-cz "R●" :rydberg-cphase "R●" :rydberg-blockade "RB"
   ;; Global gates
   :global-x "GX" :global-y "GY" :global-z "GZ" :global-h "GH"
   :global-rx "GRX" :global-ry "GRY" :global-rz "GRZ"})

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
(let [gate-type (:operation-type gate)
      params (:operation-params gate)
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

(defn- center-text
  "Center text within a fixed width, padding with specified character.
  
  Parameters:
  - text: Text to center
  - width: Target width 
  - pad-char: Character to use for padding (default space)
  
  Returns:
  Centered text of exactly 'width' characters"
  ([text width] (center-text text width " "))
  ([text width pad-char]
   (let [text-len (count text)
         padding-needed (max 0 (- width text-len))
         left-padding (quot padding-needed 2)
         right-padding (- padding-needed left-padding)]
     (str (str/join (repeat left-padding pad-char))
          text
          (str/join (repeat right-padding pad-char))))))

(defn- format-gate-symbol
  "Format a gate symbol to consistent width with proper centering.
  
  Parameters:
  - symbol: Gate symbol string
  - column-width: Target column width
  
  Returns:
  Formatted symbol of consistent width"
  [symbol column-width]
  (center-text symbol column-width "─"))

(defmethod viz/visualize-circuit :ascii
  [_format circuit & {:keys [show-measurements column-width]
                      :or {show-measurements true column-width 5}}]

  (let [n-qubits (:num-qubits circuit)
        
        ;; Use common layer assignment functions
        gates (common/extract-circuit-gates circuit)
        gate-layer-assignments (common/assign-gates-to-layers gates n-qubits)
        
        ;; Group gates by layer for parallel processing
        gates-by-layer (group-by :layer gate-layer-assignments)
        max-layer (if (empty? gates-by-layer) 0 (apply max (keys gates-by-layer)))

        ;; Gate symbols
        gate-symbols (gate-symbols)

        ;; Build circuit as a grid (qubit x layer)
        ;; Each cell contains the gate symbol or connection info
        init-grid (vec (repeat n-qubits (vec (repeat (inc max-layer) (format-gate-symbol "─" column-width)))))
        
        ;; Place gates in the grid
        final-grid (reduce 
                    (fn [grid assignment]
                      (let [gate (:gate assignment)
                            layer (:layer assignment)
                            gate-type (:operation-type gate)
                            params (:operation-params gate)]
                        (case gate-type
                          ;; Single-qubit gates
                          (:x :y :z :h :s :s-dag :t :t-dag :rx :ry :rz :phase)
                          (let [target (:target params)
                                symbol (str "[" (get gate-symbols gate-type (name gate-type)) "]")
                                formatted-symbol (format-gate-symbol symbol column-width)]
                            (assoc-in grid [target layer] formatted-symbol))
                          
                          ;; CNOT and other controlled gates
                          (:cnot :cx :cz :cy :crx :cry :crz)
                          (let [control (:control params)
                                target (:target params)
                                min-q (min control target)
                                max-q (max control target)
                                control-symbol (format-gate-symbol "●" column-width)
                                target-symbol (if (= gate-type :cnot) 
                                                (format-gate-symbol "⊕" column-width)
                                                (format-gate-symbol (str "[" (subs (name gate-type) 1) "]") column-width))
                                connector-symbol (format-gate-symbol "│" column-width)]
                            (-> grid
                                (assoc-in [control layer] control-symbol)
                                (assoc-in [target layer] target-symbol)
                                ;; Add vertical connections
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (> q min-q) (< q max-q))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          ;; SWAP gates 
                          (:swap :iswap)
                          (let [q1 (:qubit1 params)
                                q2 (:qubit2 params)
                                symbol (get gate-symbols gate-type "×")
                                formatted-symbol (format-gate-symbol symbol column-width)
                                connector-symbol (format-gate-symbol "│" column-width)
                                min-q (min q1 q2)
                                max-q (max q1 q2)]
                            (-> grid
                                (assoc-in [q1 layer] formatted-symbol)
                                (assoc-in [q2 layer] formatted-symbol)
                                ;; Add vertical connections
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (> q min-q) (< q max-q))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          ;; Multi-qubit gates (simplified representation)
                          :toffoli
                          (let [c1 (:control1 params)
                                c2 (:control2 params)
                                target (:target params)
                                all-qubits [c1 c2 target]
                                min-q (apply min all-qubits)
                                max-q (apply max all-qubits)
                                control-symbol (format-gate-symbol "●" column-width)
                                target-symbol (format-gate-symbol "⊕" column-width)
                                connector-symbol (format-gate-symbol "│" column-width)]
                            (-> grid
                                (assoc-in [c1 layer] control-symbol)
                                (assoc-in [c2 layer] control-symbol)
                                (assoc-in [target layer] target-symbol)
                                ;; Add vertical connections
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (>= q min-q) (<= q max-q)
                                                      (not (contains? (set all-qubits) q)))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          :fredkin
                          (let [control (:control params)
                                t1 (:target1 params)
                                t2 (:target2 params)
                                all-qubits [control t1 t2]
                                min-q (apply min all-qubits)
                                max-q (apply max all-qubits)
                                control-symbol (format-gate-symbol "●" column-width)
                                swap-symbol (format-gate-symbol "×" column-width)
                                connector-symbol (format-gate-symbol "│" column-width)]
                            (-> grid
                                (assoc-in [control layer] control-symbol)
                                (assoc-in [t1 layer] swap-symbol)
                                (assoc-in [t2 layer] swap-symbol)
                                ;; Add vertical connections
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (>= q min-q) (<= q max-q)
                                                      (not (contains? (set all-qubits) q)))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          ;; Rydberg controlled gates
                          (:rydberg-cz :rydberg-cphase)
                          (let [control (:control params)
                                target (:target params)
                                min-q (min control target)
                                max-q (max control target)
                                control-symbol (format-gate-symbol "R●" column-width)
                                target-symbol (if (= gate-type :rydberg-cz) 
                                                (format-gate-symbol "[RZ]" column-width)
                                                (format-gate-symbol "[RP]" column-width))
                                connector-symbol (format-gate-symbol "│" column-width)]
                            (-> grid
                                (assoc-in [control layer] control-symbol)
                                (assoc-in [target layer] target-symbol)
                                ;; Add vertical connections
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (> q min-q) (< q max-q))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          ;; Rydberg blockade gate - multi-qubit
                          :rydberg-blockade
                          (let [qubit-indices (:qubit-indices params)
                                min-q (apply min qubit-indices)
                                max-q (apply max qubit-indices)
                                blockade-symbol (format-gate-symbol "[RB]" column-width)
                                connector-symbol (format-gate-symbol "│" column-width)]
                            (-> grid
                                ;; Place blockade symbols on all target qubits
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (assoc-in grid-acc [q layer] blockade-symbol))
                                           g
                                           qubit-indices)))
                                ;; Add vertical connections between qubits
                                ((fn [g]
                                   (reduce (fn [grid-acc q]
                                             (if (and (>= q min-q) (<= q max-q)
                                                      (not (contains? (set qubit-indices) q)))
                                               (assoc-in grid-acc [q layer] connector-symbol)
                                               grid-acc))
                                           g
                                           (range n-qubits))))))
                          
                          ;; Global gates - affect all qubits
                          (:global-x :global-y :global-z :global-h :global-rx :global-ry :global-rz)
                          (let [symbol (str "[" (get gate-symbols gate-type (name gate-type)) "]")
                                formatted-symbol (format-gate-symbol symbol column-width)]
                            ;; Apply gate symbol to all qubits
                            (reduce (fn [g q]
                                      (assoc-in g [q layer] formatted-symbol))
                                    grid
                                    (range n-qubits)))
                          
                          ;; Default case
                          grid)))
                    init-grid
                    gate-layer-assignments)

        ;; Convert grid to strings with consistent spacing
        circuit-lines (mapv (fn [qubit-idx]
                              (let [qubit-row (nth final-grid qubit-idx)
                                    qubit-label (str "q" qubit-idx " |0⟩")
                                    line-parts (cons qubit-label qubit-row)]
                                (str/join "" line-parts)))
                            (range n-qubits))

        ;; Add measurements if requested
        measurement-lines (if show-measurements
                            (mapv #(str % (format-gate-symbol "╫" column-width) "═") circuit-lines)
                            circuit-lines)

        ;; Add header and footer
        header (str "Circuit: " (or (:name circuit) "Unnamed") "\n"
                    (when (:description circuit)
                      (str (:description circuit) "\n")))

        diagram (str/join "\n" measurement-lines)

        footer (str "\nGates: " (count gates)
                    ", Depth: " max-layer
                    ", Column width: " column-width)]

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

  (let [gates (:operations circuit)

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

(defmethod viz/visualize-measurement-histogram :ascii
  [_format measurements & {:keys [width threshold max-bars show-percentages]
                          :or {width 40 threshold 1 max-bars 16 show-percentages true}}]
  (let [;; Use common utilities for data preparation
        chart-data (common/prepare-measurement-histogram-data measurements
                                                              :threshold threshold
                                                              :max-bars max-bars
                                                              :normalize true) ; Always normalize for ASCII
        counts (:counts chart-data)
        labels (:labels chart-data)
        max-count (:max-count chart-data)
        total-shots (:total-shots chart-data)
        
        ;; Create normalized counts for bar length calculation
        normalize-count #(if (zero? max-count) 0 (double (/ % max-count)))
        
        ;; Bar creation function
        create-bar (fn [count label]
                     (let [bar-length (int (* (normalize-count count) width))
                           bar (str/join (repeat bar-length "█"))
                           spaces (str/join (repeat (- width bar-length) " "))
                           percentage (qmath/round-precision (* (double (/ count total-shots)) 100) 1)
                           percentage-text (if show-percentages
                                             (str " " percentage "% (" count " shots)")
                                             (str " " count " shots"))]
                       (str label ": " bar spaces percentage-text)))
        
        ;; Chart header
        header (str "Measurement Histogram (" total-shots " total shots)\n"
                   (str/join (repeat (+ width 25) "=")) "\n")
        
        ;; Generate bars
        bars (str/join "\n" (map create-bar counts labels))
        
        ;; Summary footer
        summary (:summary chart-data)
        footer (str "\n" (str/join (repeat (+ width 25) "=")) "\n"
                   "Outcomes shown: " (:num-shown summary) "/" (:num-outcomes summary)
                   " (" (qmath/round-precision (:percentage-shown summary) 1) "%)")]
    
    (str header bars footer)))

;;
;; Hardware Topology Visualization Functions
;;
(defn visualize-topology-grid
  "Create a grid-based ASCII representation of a hardware topology.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  - grid-width: Width of the grid (auto-calculated if nil)
  - show-connections: Whether to show connection lines (default true)
  
  Returns:
  ASCII string representation of the topology"
  [topology & {:keys [grid-width show-connections] 
               :or {show-connections true}}]
  (let [num-qubits (count topology)
        ;; Calculate grid dimensions
        calculated-width (or grid-width (int (Math/ceil (Math/sqrt num-qubits))))
        grid-height (int (Math/ceil (/ num-qubits calculated-width)))
        
        ;; Create empty grid
        char-grid (vec (repeat (* grid-height 3) ; 3 chars height per row
                              (vec (repeat (* calculated-width 5) \space)))) ; 5 chars width per column
        
        ;; Function to get grid position for a qubit
        qubit-to-pos (fn [qubit-id]
                       (let [row (quot qubit-id calculated-width)
                             col (mod qubit-id calculated-width)]
                         [(* row 3) (* col 5)]))
        
        ;; Place qubits on grid
        grid-with-qubits (reduce (fn [grid qubit-id]
                                   (let [[row col] (qubit-to-pos qubit-id)
                                         qubit-str (if (< qubit-id 10) 
                                                     (str " " qubit-id " ")
                                                     (str qubit-id " "))]
                                     (loop [g grid
                                            chars (seq qubit-str)
                                            c col]
                                       (if (empty? chars)
                                         g
                                         (recur (assoc-in g [row c] (first chars))
                                                (rest chars)
                                                (inc c))))))
                                 char-grid
                                 (range num-qubits))
        
        ;; Add connections if requested
        final-grid (if show-connections
                     (reduce (fn [grid qubit-id]
                               (let [neighbors (get topology qubit-id)
                                     [q-row q-col] (qubit-to-pos qubit-id)]
                                 (reduce (fn [g neighbor]
                                           (when (and neighbor (< neighbor num-qubits)) ; Valid neighbor
                                             (let [[n-row n-col] (qubit-to-pos neighbor)]
                                               ;; Draw connection line
                                               (cond
                                                 ;; Horizontal connection
                                                 (= q-row n-row)
                                                 (let [start-col (min (+ q-col 3) (+ n-col 3))
                                                       end-col (max (+ q-col 3) (+ n-col 3))]
                                                   (loop [g2 g c start-col]
                                                     (if (< c end-col)
                                                       (recur (assoc-in g2 [q-row c] \─) (inc c))
                                                       g2)))
                                                 
                                                 ;; Vertical connection  
                                                 (= q-col n-col)
                                                 (let [start-row (min (inc q-row) (inc n-row))
                                                       end-row (max (inc q-row) (inc n-row))]
                                                   (loop [g2 g r start-row]
                                                     (if (< r end-row)
                                                       (recur (assoc-in g2 [r (+ q-col 1)] \│) (inc r))
                                                       g2)))
                                                 
                                                 ;; Default: just return grid unchanged for diagonal connections
                                                 :else g)))
                                           g)
                                         grid
                                         neighbors)))
                             grid-with-qubits
                             (range num-qubits))
                     grid-with-qubits)]
    
    ;; Convert grid to string
    (str/join "\n" (map #(str/join "" %) final-grid))))

(defn visualize-topology-linear
  "Create a linear ASCII representation of a hardware topology.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  - horizontal: Whether to layout horizontally (default true)
  
  Returns:
  ASCII string representation of the linear topology"
  [topology & {:keys [horizontal] :or {horizontal true}}]
  (let [num-qubits (count topology)]
    (if horizontal
      ;; Horizontal layout: 0─1─2─3─4
      (str/join "─" (map str (range num-qubits)))
      ;; Vertical layout
      (str/join "\n│\n" (map str (range num-qubits))))))

(defmethod viz/visualize-topology :ascii
  [_format topology & {:keys [layout show-info] 
                       :or {layout :auto show-info true}}]
  (let [topology-type (common/detect-topology-type topology)
        
        ;; Choose layout based on topology type and user preference
        chosen-layout (common/choose-layout-for-format topology :ascii layout)
        
        ;; Generate the main visualization
        main-viz (case chosen-layout
                   :linear (visualize-topology-linear topology)
                   :grid (visualize-topology-grid topology)
                   (visualize-topology-grid topology))
        
        ;; Add topology information if requested
        info-text (when show-info
                     (let [summary (common/calculate-topology-summary topology topology-type)]
                       (str "\n" (common/format-topology-info summary :show-connectivity true))))]
    
    (str "Hardware Topology (" (name topology-type) "):\n"
         main-viz
         info-text)))
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
             (when (:gate frame) (str "- Applied " (:operation-type (:gate frame)))))
    (println (viz/visualize-quantum-state (:state frame)))
    (println "---"))

  ;
  )
