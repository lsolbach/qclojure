(ns org.soulspace.qclojure.adapter.visualization.common
  "Common utilities for quantum visualization - shared calculations and data extraction.
  
  This namespace contains shared functions used across different visualization formats.
  It focuses on pure data transformation and calculation, keeping format-specific
  rendering logic in the individual format namespaces."
  (:require [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coord]))
;;
;; Data Extraction and Calculation
;;
(defn filter-significant-probabilities
  "Filter probabilities above threshold and limit count.
  
  Parameters:
  - probabilities: Vector of probability values
  - threshold: Minimum probability to include (default 0.001)
  - max-count: Maximum number of states to include (default 16)
  
  Returns:
  Map with :indices, :probabilities, :labels, and :summary info"
  [probabilities labels & {:keys [threshold max-count] 
                           :or {threshold 0.001 max-count 16}}]
  (let [significant-indices (->> (range (count probabilities))
                                 (filter #(> (nth probabilities %) threshold))
                                 (sort-by #(nth probabilities %) >)
                                 (take max-count))
        filtered-probs (mapv #(nth probabilities %) significant-indices)
        filtered-labels (mapv #(nth labels %) significant-indices)
        total-shown (reduce + filtered-probs)
        n-hidden (- (count probabilities) (count filtered-probs))]
    
    {:indices significant-indices
     :probabilities filtered-probs
     :labels filtered-labels
     :total-shown total-shown
     :n-hidden n-hidden}))

(defn format-amplitude-display
  "Format complex amplitude for human-readable display.
  
  Parameters:
  - amplitude: Complex amplitude (fastmath Vec2)
  - precision: Decimal places for rounding (default 3)
  
  Returns:
  String representation like '0.707+0.0i' or '0.5-0.3i'"
  [amplitude & {:keys [precision] :or {precision 3}}]
  (let [real-part (qmath/round-precision (fc/re amplitude) precision)
        imag-part (qmath/round-precision (fc/im amplitude) precision)
        sign (if (>= imag-part 0) "+" "")]
    (str real-part sign imag-part "i")))

(defn format-state-expression
  "Format quantum state as amplitude expression.
  
  Parameters:
  - state: Quantum state
  - indices: Indices of significant amplitudes to show
  - labels: Corresponding basis state labels
  - precision: Decimal places (default 3)
  
  Returns:
  String like '0.707|0⟩ + 0.707|1⟩'"
  [state indices labels & {:keys [precision] :or {precision 3}}]
  (let [amplitudes (:state-vector state)]
    (->> (map (fn [idx label]
                (let [amp (nth amplitudes idx)]
                  (str (format-amplitude-display amp :precision precision) 
                       " " label)))
              indices labels)
         (str/join " + "))))

(defn calculate-state-summary
  "Calculate summary information about quantum state.
  
  Parameters:
  - state: Quantum state
  - significant-probabilities: Result from filter-significant-probabilities
  
  Returns:
  Map with summary statistics"
  [state significant-probabilities]
  (let [{:keys [total-shown n-hidden]} significant-probabilities
        n-qubits (:num-qubits state)]
    {:num-qubits n-qubits
     :total-probability-shown total-shown
     :percentage-shown (* total-shown 100)
     :hidden-states n-hidden
     :total-dimension (bit-shift-left 1 n-qubits)}))

;;
;; Amplitude and Phase Analysis
;;
(defn extract-amplitude-info
  "Extract detailed amplitude information for display.
  
  Parameters:
  - state: Quantum state
  - indices: Indices of amplitudes to extract
  - labels: Corresponding labels
  - precision: Decimal places (default 3)
  
  Returns:
  Vector of maps with :label, :amplitude, :magnitude, :phase, :probability"
  [state indices labels & {:keys [precision] :or {precision 3}}]
  (let [amplitudes (:state-vector state)]
    (mapv (fn [idx label]
            (let [amp (nth amplitudes idx)
                  magnitude (fc/abs amp)
                  phase (m/degrees (fc/arg amp))
                  probability (* magnitude magnitude)]
              {:label label
               :amplitude (format-amplitude-display amp :precision precision)
               :magnitude (qmath/round-precision magnitude precision)
               :phase (qmath/round-precision phase 1)
               :probability (qmath/round-precision probability precision)}))
          indices labels)))

(defn calculate-phase-info
  "Calculate phase information for quantum state amplitudes.
  
  Parameters:
  - state: Quantum state
  - indices: Indices to analyze
  - labels: Corresponding labels
  
  Returns:
  Vector of maps with :label and :phase-degrees"
  [state indices labels]
  (let [amplitudes (:state-vector state)]
    (mapv (fn [idx label]
            (let [amp (nth amplitudes idx)
                  phase (m/degrees (fc/arg amp))]
              {:label label
               :phase-degrees (qmath/round-precision phase 1)}))
          indices labels)))

;;
;; Probability Bar Chart Data
;;
(defn prepare-bar-chart-data
  "Prepare data for probability bar charts across different formats.
  
  This function extracts and normalizes probability data in a format-agnostic way.
  
  Parameters:
  - state: Quantum state
  - options: Chart options
    :threshold - Minimum probability threshold (default 0.001)
    :max-bars - Maximum number of bars (default 16)
    :normalize - Whether to normalize to max probability (default true)
  
  Returns:
  Map with chart data:
  - :probabilities - Filtered probability values
  - :labels - Corresponding state labels  
  - :normalized - Normalized probabilities (0-1 scale)
  - :max-probability - Maximum probability value
  - :indices - Indices of significant probabilities
  - :summary - Summary information"
  [state & {:keys [threshold max-bars normalize]
            :or {threshold 0.001 max-bars 16 normalize true}}]
  (let [num-qubits (:num-qubits state)
        ;; Guard against invalid states
        _ (when (or (nil? num-qubits) (not (pos? num-qubits)))
            (throw (ex-info "Invalid quantum state: missing or invalid num-qubits" 
                           {:state state :num-qubits num-qubits})))
        all-probabilities (qs/measurement-probabilities state)
        all-labels (qs/basis-labels num-qubits)
        filtered (filter-significant-probabilities all-probabilities all-labels
                                                   :threshold threshold
                                                   :max-count max-bars)
        max-prob (if (empty? (:probabilities filtered)) 
                   1 
                   (apply max (:probabilities filtered)))
        normalized (when normalize
                     (mapv #(if (zero? max-prob) 0 (/ % max-prob)) 
                           (:probabilities filtered)))
        summary (calculate-state-summary state filtered)]
    
    {:probabilities (:probabilities filtered)
     :labels (:labels filtered)
     :normalized normalized
     :max-probability max-prob
     :indices (:indices filtered)
     :summary summary}))

;;
;; Measurement Histogram Data
;;
(defn prepare-measurement-histogram-data
  "Prepare measurement counts data for histogram bar charts across different formats.
  
  This function takes measurement results (counts) and converts them to chart-ready data.
  
  Parameters:
  - measurement-results: Map of measurement outcomes to counts (e.g., {\"000\" 145, \"111\" 155})
  - options: Chart options
    :threshold - Minimum count threshold (default 1)
    :max-bars - Maximum number of bars (default 16)
    :normalize - Whether to normalize to max count (default true)
  
  Returns:
  Map with chart data:
  - :counts - Filtered count values
  - :labels - Corresponding state labels  
  - :normalized - Normalized counts (0-1 scale)
  - :max-count - Maximum count value
  - :total-shots - Total number of measurements
  - :summary - Summary information"
  [measurement-results & {:keys [threshold max-bars normalize]
                          :or {threshold 1 max-bars 16 normalize true}}]
  (let [;; Guard against invalid input
        _ (when (or (nil? measurement-results) (not (map? measurement-results)))
            (throw (ex-info "Invalid measurement results: must be a map" 
                           {:measurement-results measurement-results})))
        
        ;; Extract counts and labels from measurement results
        sorted-outcomes (sort-by val > measurement-results) ; Sort by count descending
        filtered-outcomes (->> sorted-outcomes
                               (filter #(>= (val %) threshold))
                               (take max-bars))
        
        counts (mapv val filtered-outcomes)
        labels (mapv key filtered-outcomes)
        
        ;; Calculate statistics
        max-count (if (empty? counts) 1 (apply max counts))
        total-shots (reduce + (vals measurement-results))
        
        ;; Normalize if requested
        normalized (when normalize
                     (mapv #(if (zero? max-count) 0 (/ % max-count)) counts))
        
        ;; Generate basis labels with ket notation
        ket-labels (mapv #(str "|" % "⟩") labels)
        
        ;; Summary information
        summary {:total-shots total-shots
                 :num-outcomes (count measurement-results)
                 :num-shown (count counts)
                 :num-hidden (- (count measurement-results) (count counts))
                 :percentage-shown (* 100.0 (/ (reduce + counts) total-shots))}]
    
    {:counts counts
     :labels ket-labels
     :normalized normalized
     :max-count max-count
     :total-shots total-shots
     :summary summary}))

;;
;; Color Palette Generation
;;
(defn generate-color-palette
  "Generate color palette for quantum visualizations.
  
  Parameters:
  - n-colors: Number of colors needed
  - scheme: Color scheme (:quantum, :rainbow, :monochrome)
  
  Returns:
  Vector of color codes (format depends on target: hex for web, ANSI for terminal)"
  [n-colors & {:keys [scheme] :or {scheme :quantum}}]
  (let [base-colors (case scheme
                      :quantum ["#7c3aed" "#3b82f6" "#10b981" "#f59e0b" "#ef4444" "#8b5cf6" "#06b6d4" "#84cc16"]
                      :rainbow ["#ff0000" "#ff7f00" "#ffff00" "#00ff00" "#0000ff" "#4b0082" "#9400d3"]
                      :monochrome ["#6b7280"])]
    (vec (take n-colors (cycle base-colors)))))

;;
;; Bloch Sphere Common Utilities
;;
(defn format-single-qubit-state
  "Format single-qubit quantum state as amplitude expression.
  
  Parameters:
  - state: Single-qubit quantum state
  - precision: Decimal places (default 3)
  
  Returns:
  String like '0.707|0⟩ + 0.707i|1⟩'"
  [state & {:keys [precision] :or {precision 3}}]
  {:pre [(= (:num-qubits state) 1)]}
  (let [amplitudes (:state-vector state)
        α (first amplitudes)   ; amplitude for |0⟩  
        β (second amplitudes)] ; amplitude for |1⟩
    (str (format-amplitude-display α :precision precision) " |0⟩ + "
         (format-amplitude-display β :precision precision) " |1⟩")))

(defn format-bloch-coordinates
  "Format Bloch sphere coordinates for display.
  
  Parameters:
  - coords: Result from quantum-state-to-bloch-coordinates
  - precision: Decimal places (default 3 for cartesian, 1 for angles)
  
  Returns:
  Map with formatted coordinate strings"
  [coords & {:keys [precision] :or {precision 3}}]
  (let [{θ :theta φ :phi} (:spherical coords)
        {x :x y :y z :z} (:cartesian coords)]
    {:spherical-text (str "θ=" (qmath/round-precision (m/degrees θ) 1) "°, "
                          "φ=" (qmath/round-precision (m/degrees φ) 1) "°")
     :cartesian-text (str "(" (qmath/round-precision x precision) ", "
                              (qmath/round-precision y precision) ", " 
                              (qmath/round-precision z precision) ")")
     :angles {:theta-deg (qmath/round-precision (m/degrees θ) 1)
              :phi-deg (qmath/round-precision (m/degrees φ) 1)}
     :vector {:x (qmath/round-precision x precision)
              :y (qmath/round-precision y precision) 
              :z (qmath/round-precision z precision)}}))

(defn calculate-reference-distances
  "Calculate distances from current state to reference states.
  
  Parameters:
  - coords: Current state coordinates from quantum-state-to-bloch-coordinates
  - precision: Decimal places for distance values (default 2)
  
  Returns:
  Vector of maps with :label and :distance for each reference state"
  [coords & {:keys [precision] :or {precision 2}}]
  (mapv (fn [[label ref-coords]]
          (let [dist (coord/bloch-distance coords ref-coords)]
            {:label label
             :distance (qmath/round-precision dist precision)}))
        coord/reference-state-coordinates))

(defn format-reference-distances
  "Format reference state distances as display text.
  
  Parameters:
  - distance-data: Result from calculate-reference-distances
  - format: Output format (:inline or :list)
  
  Returns:
  Formatted string"
  [distance-data & {:keys [format] :or {format :inline}}]
  (case format
    :inline (str/join ", " (map #(str (:label %) ":" (:distance %)) distance-data))
    :list (str/join "\n" (map #(str "  " (:label %) ": " (:distance %)) distance-data))))

(defn generate-bloch-legend
  "Generate legend text for Bloch sphere visualizations.
  
  Parameters:
  - format: Target format (:ascii or :svg)
  
  Returns:
  String with appropriate legend information"
  [format]
  (case format
    :ascii "Legend: ● = current state, · = sphere outline, + = axes"
    :svg "Interactive Bloch Sphere: Hover for details, scroll to zoom"
    "Bloch Sphere Visualization"))

(defn prepare-bloch-display-data
  "Prepare all display data for Bloch sphere visualization.
  
  This aggregates all the text and coordinate information needed by both
  ASCII and SVG formats, reducing code duplication.
  
  Parameters:
  - state: Single-qubit quantum state
  - coords: Coordinates from quantum-state-to-bloch-coordinates
  - options: Display options
    :show-coordinates - Include coordinate text (default true)
    :show-distances - Include reference distances (default true)
    :precision - Decimal places (default 3)
  
  Returns:
  Map with formatted display strings and data"
  [state coords & {:keys [show-coordinates show-distances precision format]
                   :or {show-coordinates true show-distances true precision 3 format :ascii}}]
  (let [state-text (format-single-qubit-state state :precision precision)
        coord-info (when show-coordinates
                     (format-bloch-coordinates coords :precision precision))
        distance-info (when show-distances
                        (calculate-reference-distances coords))
        legend (generate-bloch-legend format)]
    
    {:state-expression state-text
     :coordinates coord-info
     :distances distance-info
     :legend legend
     :summary (str "Single-qubit state on Bloch sphere\n"
                   "State: " state-text "\n"
                   (when show-coordinates
                     (str "Coordinates: " (:spherical-text coord-info) "\n"
                          "Bloch vector: " (:cartesian-text coord-info) "\n"))
                   (when show-distances
                     (str "Distances: " (format-reference-distances distance-info) "\n")))}))

;;
;; Circuit Layer Assignment - shared between visualizations
;;
(defn assign-gates-to-layers
  "Assign gates to non-overlapping layers for circuit visualization.
  
  This function implements span-aware layering to prevent visual overlaps between
  multi-qubit gates and ensures proper sequencing.
  
  Algorithm:
  1. Track the last layer used by each qubit
  2. Track spans created by multi-qubit gates
  3. For each gate, find the earliest layer where:
     - No affected qubits are still busy
     - No spans conflict with existing spans
     - Single-qubit gates don't conflict with existing spans
  
  Parameters:
  - gates: Vector of gate operations (non-measurement operations)
  - n-qubits: Total number of qubits in the circuit
  
  Returns:
  Vector of assignment maps with :gate, :layer, and :qubit-layers"
  [gates n-qubits]
  (let [qubit-last-layer (vec (repeat n-qubits 0))]
    (loop [remaining-gates gates
           qubit-layers qubit-last-layer
           span-layers {}
           assignments []
           current-max-layer 0]
      (if (empty? remaining-gates)
        assignments
        (let [gate (first remaining-gates)
              op-info (qc/operation-qubits-with-spans gate)
              op-qubits (remove nil? (:qubits op-info))
              op-spans (:spans op-info)

              ;; Find the maximum layer from affected qubits
              max-qubit-layer (qc/safe-max 0 (map #(nth qubit-layers %) op-qubits))

              ;; For controlled gates, check if any existing spans conflict
              conflicting-span-layers (for [[span layer] span-layers
                                           op-span op-spans
                                           :when (qc/spans-conflict? span op-span)]
                                       layer)
              max-span-layer (qc/safe-max 0 conflicting-span-layers)

              ;; For single-qubit gates, check if they conflict with existing spans
              conflicting-qubit-layers (if (empty? op-spans)
                                        (for [[span layer] span-layers
                                              qubit op-qubits
                                              :let [[start end] span]
                                              :when (<= start qubit end)]
                                          layer)
                                        [])
              max-conflict-layer (qc/safe-max 0 conflicting-qubit-layers)

              ;; For multi-qubit gates, check if any qubits in the proposed layer would conflict
              ;; This is the reverse check: does this new gate's spans conflict with existing single-qubit gates?
              proposed-layer (inc (max max-qubit-layer max-span-layer max-conflict-layer))
              reverse-conflicts (if (seq op-spans)
                                  ;; Check if any qubits already in the proposed layer would be within our spans
                                  (for [q (range n-qubits)
                                        :when (= (nth qubit-layers q) proposed-layer)
                                        span op-spans
                                        :let [[start end] span]
                                        :when (<= start q end)]
                                    proposed-layer)
                                  [])

              ;; The new layer is one more than the maximum constraint (including reverse conflicts)
              new-layer (if (seq reverse-conflicts)
                          (inc proposed-layer)  ; If there are reverse conflicts, bump to next layer
                          proposed-layer)       ; Otherwise use the proposed layer

              ;; Update qubit layers
              updated-qubit-layers (reduce #(assoc %1 %2 new-layer)
                                          qubit-layers
                                          op-qubits)

              ;; Update span layers
              updated-span-layers (reduce #(assoc %1 %2 new-layer)
                                         span-layers
                                         op-spans)

              ;; Create assignment
              assignment {:gate gate 
                         :layer new-layer 
                         :qubit-layers updated-qubit-layers}]

          (recur (rest remaining-gates)
                 updated-qubit-layers
                 updated-span-layers
                 (conj assignments assignment)
                 (max current-max-layer new-layer)))))))

(defn extract-circuit-gates
  "Extract non-measurement gates from circuit for layer assignment.
  
  Parameters:
  - circuit: Quantum circuit
  
  Returns:
  Vector of gate operations (excluding measurements)"
  [circuit]
  (filter #(not= (:operation-type %) :measure) (:operations circuit)))

(comment
  ;; REPL testing of common utilities
  
  ;; Test state preparation
  (def test-state (qs/zero-state 2))
  (def bell-like {:state-vector [(fc/complex 0.7 0) (fc/complex 0 0) 
                                 (fc/complex 0 0) (fc/complex 0.7 0)] 
                  :num-qubits 2})
  
  ;; Test bar chart data preparation
  (prepare-bar-chart-data bell-like)
  
  ;; Test amplitude formatting
  (format-amplitude-display (fc/complex 0.707 0.0))
  (format-amplitude-display (fc/complex 0.5 -0.3))
  
  ;; Test summary calculations
  (let [probs [0.5 0.3 0.2 0.0]
        labels ["|00⟩" "|01⟩" "|10⟩" "|11⟩"]
        filtered (filter-significant-probabilities probs labels)]
    filtered)
  ;; Test Bloch sphere utilities
  (def single-qubit-state {:state-vector [(fc/complex 0.707 0) (fc/complex 0.707 0)] 
                            :num-qubits 1})
  (def bloch-coords {:cartesian {:x 0.5 :y 0.5 :z 0.707}
                      :spherical {:theta (m/acos 0.707) :phi (m/atan2 0.5 0.5)}})
  
  (format-single-qubit-state single-qubit-state)
  (format-bloch-coordinates bloch-coords)
  (calculate-reference-distances bloch-coords)
  (format-reference-distances (calculate-reference-distances bloch-coords))
  (generate-bloch-legend :ascii)
  (prepare-bloch-display-data single-qubit-state bloch-coords)
  )

