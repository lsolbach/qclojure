(ns org.soulspace.qclojure.adapter.visualization.svg
  "SVG-based visualization for quantum states and circuits.
  
  This namespace provides scalable vector graphics (SVG) generation using
  Hiccup for high-quality quantum visualization that can be embedded in
  web pages or saved as standalone files."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coord]
            [org.soulspace.qclojure.adapter.visualization.common :as common]
            [org.soulspace.qclojure.application.hardware-optimization :as hw]))

;;;
;;; Parameter Formatting for Tooltips
;;;
(defn format-gate-parameters
  "Format gate parameters for tooltip display with appropriate symbols and formatting.
  
  Parameters:
  - gate-type: Keyword identifying the gate type
  - params: Operation parameters map containing :angle
  
  Returns:
  Formatted string for tooltip display, or nil if no parameters"
  [gate-type params]
  (when-let [angle (:angle params)]
    (case gate-type
      ;; Single-qubit rotation gates
      :rx (str "θ=" (common/format-angle-value angle))
      :ry (str "θ=" (common/format-angle-value angle))
      :rz (str "φ=" (common/format-angle-value angle))
      :phase (str "φ=" (common/format-angle-value angle))

      ;; Controlled rotation gates  
      :crx (str "θ=" (common/format-angle-value angle))
      :cry (str "θ=" (common/format-angle-value angle))
      :crz (str "φ=" (common/format-angle-value angle))

      ;; Rydberg gates
      :rydberg-cphase (str "φ=" (common/format-angle-value angle))

      ;; Global rotation gates
      :global-rx (str "θ=" (common/format-angle-value angle))
      :global-ry (str "θ=" (common/format-angle-value angle))
      :global-rz (str "φ=" (common/format-angle-value angle))

      ;; Default case for any other parametric gates
      (str "param=" (common/format-angle-value angle)))))

;;;
;;; Bar Chart Common SVG Components  
;;;
(defn generate-bar-chart-layout
  "Generate layout parameters for bar charts.
  
  Parameters:
  - data-count: Number of data points (bars)
  - width: Chart width
  - height: Chart height
  - options: Layout options
  
  Returns:
  Map with layout parameters including margins, bar dimensions, rotation settings"
  [data-count width height & {:keys [force-rotation] :or {force-rotation false}}]
  (let [;; Determine if labels need rotation based on bar count
        needs-rotation (or force-rotation (> data-count 8))
        bottom-margin (if needs-rotation 120 100)
        margin {:top 60 :right 40 :bottom bottom-margin :left 80}
        chart-width (- width (:left margin) (:right margin))
        chart-height (- height (:top margin) (:bottom margin))
        bar-width (/ chart-width (max 1 data-count))]

    {:margin margin
     :chart-width chart-width
     :chart-height chart-height
     :bar-width bar-width
     :needs-rotation needs-rotation}))

(defn generate-svg-bars
  "Generate SVG bar elements for charts.
  
  Parameters:
  - values: Vector of bar values (probabilities or counts)
  - labels: Vector of bar labels
  - layout: Layout parameters from generate-bar-chart-layout
  - options: Bar generation options
    - :max-value - Maximum value for scaling
    - :value-formatter - Function to format values for display
    - :tooltip-formatter - Function to format tooltips
    - :unit-suffix - Unit suffix for values (e.g., '%', 'shots')
  
  Returns:
  Vector of SVG bar group elements"
  [values labels layout & {:keys [max-value value-formatter tooltip-formatter unit-suffix]
                           :or {value-formatter identity
                                tooltip-formatter str
                                unit-suffix ""}}]
  (let [{:keys [margin chart-height bar-width needs-rotation]} layout
        bar-colors (common/generate-color-palette (count values) :scheme :quantum)]

    (map-indexed
     (fn [i value]
       (let [bar-height (* (/ value max-value) chart-height)
             x (+ (:left margin) (* i bar-width))
             y (+ (:top margin) (- chart-height bar-height))
             color (nth bar-colors i)
             label (nth labels i)
             tooltip-text (tooltip-formatter label value)
             value-text (str (value-formatter value) unit-suffix)]
         [:g {:class "bar-group"}
          [:rect {:x x :y y
                  :width (- bar-width 2) :height bar-height
                  :fill color :opacity 0.8
                  :stroke "#ffffff" :stroke-width 1}
           [:title tooltip-text]]
          [:text {:x (+ x (/ bar-width 2)) :y (+ (:top margin) chart-height 20)
                  :text-anchor (if needs-rotation "end" "middle")
                  :font-size "12" :fill "#374151"
                  :transform (when needs-rotation
                               (str "rotate(-45 " (+ x (/ bar-width 2)) " " (+ (:top margin) chart-height 20) ")"))}
           label]
          [:text {:x (+ x (/ bar-width 2)) :y (- y 5)
                  :text-anchor "middle" :font-size "10" :fill "#6b7280"}
           value-text]]))
     values)))

(defn generate-svg-y-axis
  "Generate SVG Y-axis with ticks and labels.
  
  Parameters:
  - layout: Layout parameters
  - max-value: Maximum value for scaling
  - unit-label: Unit label for axis (e.g., 'Probability (%)', 'Count')
  - tick-formatter: Function to format tick labels
  
  Returns:
  SVG Y-axis group element"
  [layout max-value unit-label & {:keys [tick-formatter] :or {tick-formatter str}}]
  (let [{:keys [margin chart-height]} layout
        max-display (if (= unit-label "Probability (%)")
                      (int (Math/ceil (* max-value 100)))
                      (int (Math/ceil max-value)))
        tick-step (max (if (= unit-label "Probability (%)")
                         10
                         (max 1 (int (/ max-display 5))))
                       1)
        y-ticks (range 0 (inc max-display) tick-step)]

    [:g
     [:line {:x1 (:left margin) :y1 (:top margin)
             :x2 (:left margin) :y2 (+ (:top margin) chart-height)
             :stroke "#9ca3af" :stroke-width 1}]
     (map (fn [tick]
            (let [normalized-tick (if (= unit-label "Probability (%)")
                                    (/ tick 100.0)
                                    tick)
                  y (+ (:top margin) (- chart-height (* (/ normalized-tick max-value) chart-height)))]
              [:g
               [:line {:x1 (- (:left margin) 5) :y1 y :x2 (:left margin) :y2 y
                       :stroke "#9ca3af" :stroke-width 1}]
               [:text {:x (- (:left margin) 10) :y (+ y 3)
                       :text-anchor "end" :font-size "10" :fill "#6b7280"}
                (tick-formatter tick)]]))
          y-ticks)]))

(defn generate-svg-chart-labels
  "Generate SVG chart title and axis labels.
  
  Parameters:
  - width: Chart width
  - height: Chart height
  - title: Chart title
  - y-label: Y-axis label
  - x-label: X-axis label
  
  Returns:
  Vector of SVG text elements [title y-label x-label]"
  [width height title y-label x-label]
  [[:text {:x (/ width 2) :y 25
           :text-anchor "middle" :font-size "16" :font-weight "bold" :fill "#111827"}
    title]
   [:text {:x 20 :y (/ height 2)
           :text-anchor "middle" :font-size "12" :fill "#374151"
           :transform (str "rotate(-90 20 " (/ height 2) ")")}
    y-label]
   [:text {:x (/ width 2) :y (- height 10)
           :text-anchor "middle" :font-size "12" :fill "#374151"}
    x-label]])

(defn generate-svg-x-axis
  "Generate SVG X-axis line.
  
  Parameters:
  - layout: Layout parameters
  
  Returns:
  SVG line element"
  [layout]
  (let [{:keys [margin chart-width chart-height]} layout]
    [:line {:x1 (:left margin) :y1 (+ (:top margin) chart-height)
            :x2 (+ (:left margin) chart-width) :y2 (+ (:top margin) chart-height)
            :stroke "#9ca3af" :stroke-width 1}]))

;;;
;;; Hardware Topology Visualization
;;;
(defn calculate-topology-layout
  "Calculate optimal layout positions for topology visualization.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  - layout-type: Layout algorithm (:force, :grid, :circular, :hierarchical)
  - canvas-size: [width height] of the canvas
  
  Returns:
  Vector of [x y] positions for each qubit"
  [topology layout-type canvas-size]
  (let [num-qubits (count topology)
        [width height] canvas-size
        center-x (/ width 2)
        center-y (/ height 2)
        padding 50
        usable-width (- width (* 2 padding))
        usable-height (- height (* 2 padding))]

    (case layout-type
      :grid
      (let [cols (int (Math/ceil (Math/sqrt num-qubits)))
            rows (int (Math/ceil (/ num-qubits cols)))
            x-step (/ usable-width (max 1 (dec cols)))
            y-step (/ usable-height (max 1 (dec rows)))]
        (mapv (fn [i]
                (let [row (quot i cols)
                      col (mod i cols)]
                  [(+ padding (* col x-step))
                   (+ padding (* row y-step))]))
              (range num-qubits)))

      :hexagonal
      ;; Specialized layout for heavy-hex topologies using proper 2D lattice coordinates
      ;; Based on IBM Quantum heavy-hex lattice structure from real hardware
      (letfn [(generate-heavy-hex-coordinates [n-qubits]
                "Generate 2D lattice coordinates for IBM heavy-hex topologies"
                (cond
                  ;; 7-qubit basic heavy-hex (single hexagon)
                  (= n-qubits 7)
                  [[1 2]   ; qubit 0 - center
                   [0 1]   ; qubit 1
                   [0 2]   ; qubit 2  
                   [0 3]   ; qubit 3
                   [1 3]   ; qubit 4
                   [2 2]   ; qubit 5
                   [2 1]]  ; qubit 6

                  ;; 27-qubit Falcon-style (IBM 27-qubit devices)
                  (= n-qubits 27)
                  [[1 0]   ; qubit 0
                   [1 1]   ; qubit 1
                   [2 1]   ; qubit 2
                   [3 1]   ; qubit 3
                   [1 2]   ; qubit 4
                   [3 2]   ; qubit 5
                   [0 3]   ; qubit 6
                   [1 3]   ; qubit 7
                   [3 3]   ; qubit 8
                   [4 3]   ; qubit 9
                   [1 4]   ; qubit 10
                   [3 4]   ; qubit 11
                   [1 5]   ; qubit 12
                   [2 5]   ; qubit 13
                   [3 5]   ; qubit 14
                   [0 6]   ; qubit 15
                   [1 6]   ; qubit 16
                   [3 6]   ; qubit 17
                   [4 6]   ; qubit 18
                   [1 7]   ; qubit 19
                   [3 7]   ; qubit 20
                   [2 8]   ; qubit 21
                   [1 9]   ; qubit 22
                   [2 9]   ; qubit 23
                   [3 9]   ; qubit 24
                   [1 10]  ; qubit 25
                   [3 10]] ; qubit 26

                  ;; 65-qubit Hummingbird-style (larger IBM devices)
                  (= n-qubits 65)
                  (let [;; Generate coordinates based on heavy-hex lattice pattern
                        coords (atom [])]
                    ;; Start with center region
                    (doseq [row (range 0 9)
                            col (range 0 9)
                            :let [coord [row col]]
                            :when (and
                                   ;; Apply heavy-hex constraints
                                   (or (even? (+ row col))
                                       (and (odd? row) (odd? col))
                                       (and (even? row) (even? col)))
                                   ;; Limit to reasonable bounds
                                   (< (count @coords) 65))]
                      (swap! coords conj coord))
                    ;; Pad with additional coordinates if needed
                    (while (< (count @coords) 65)
                      (let [last-coord (last @coords)
                            next-coord [(inc (first last-coord)) (second last-coord)]]
                        (swap! coords conj next-coord)))
                    @coords)

                  ;; For other sizes, generate approximate heavy-hex pattern
                  :else
                  (let [;; Estimate grid size based on qubit count
                        grid-size (int (Math/ceil (Math/sqrt n-qubits)))
                        coords (atom [])]
                    ;; Generate coordinates in heavy-hex pattern
                    (doseq [row (range grid-size)
                            col (range grid-size)
                            :when (< (count @coords) n-qubits)]
                      ;; Use heavy-hex connectivity pattern
                      (when (or (and (even? row) (even? col))
                                (and (odd? row) (odd? col))
                                (= (mod (+ row col) 3) 0))
                        (swap! coords conj [row col])))
                    ;; Fill remaining with grid pattern if needed
                    (doseq [row (range grid-size)
                            col (range grid-size)
                            :when (< (count @coords) n-qubits)]
                      (when-not (some #(= % [row col]) @coords)
                        (swap! coords conj [row col])))
                    @coords)))

              (transform-to-visual-coordinates [lattice-coords usable-width usable-height padding]
                "Transform lattice coordinates to visual SVG coordinates"
                (when (seq lattice-coords)
                  (let [;; Find bounds of lattice coordinates
                        all-rows (map first lattice-coords)
                        all-cols (map second lattice-coords)
                        min-row (apply min all-rows)
                        max-row (apply max all-rows)
                        min-col (apply min all-cols)
                        max-col (apply max all-cols)

                        ;; Calculate scaling factors
                        row-range (max 1 (- max-row min-row))
                        col-range (max 1 (- max-col min-col))
                        scale-y (/ usable-height row-range)
                        scale-x (/ usable-width col-range)]

                    ;; Transform each coordinate
                    (mapv (fn [[row col]]
                            (let [;; Normalize to 0-based coordinates
                                  norm-row (- row min-row)
                                  norm-col (- col min-col)
                                  ;; Scale to fit available space
                                  x (+ padding (* norm-col scale-x))
                                  y (+ padding (* norm-row scale-y))]
                              [x y]))
                          lattice-coords))))]

        ;; Generate and transform coordinates
        (let [lattice-coords (generate-heavy-hex-coordinates num-qubits)]
          (transform-to-visual-coordinates lattice-coords usable-width usable-height padding)))

      :circular
      (let [radius (min (/ usable-width 2) (/ usable-height 2))
            angle-step (/ (* 2 Math/PI) num-qubits)]
        (mapv (fn [i]
                (let [angle (* i angle-step)]
                  [(+ center-x (* radius (Math/cos angle)))
                   (+ center-y (* radius (Math/sin angle)))]))
              (range num-qubits)))

      :force
      ;; Improved force-directed layout with topology-aware parameters
      (let [positions (atom (mapv (fn [_]
                                    [(+ padding (rand usable-width))
                                     (+ padding (rand usable-height))])
                                  (range num-qubits)))
            ;; Scale parameters based on topology size
            iterations (min 200 (max 50 (* num-qubits 2)))
            spring-length (max 50 (min 150 (/ (Math/sqrt (+ (* usable-width usable-width)
                                                            (* usable-height usable-height)))
                                              (Math/sqrt num-qubits))))
            spring-strength (/ 0.15 (Math/log (max 2 num-qubits)))
            repulsion-strength (* 2000 (Math/sqrt num-qubits))
            damping 0.8]

        (dotimes [_ iterations]
          (let [forces (atom (vec (repeat num-qubits [0 0])))]
            ;; Calculate spring forces (attractions)
            (doseq [i (range num-qubits)
                    j (get topology i)]
              (when (< j num-qubits)
                (let [[xi yi] (nth @positions i)
                      [xj yj] (nth @positions j)
                      dx (- xj xi)
                      dy (- yj yi)
                      distance (Math/sqrt (+ (* dx dx) (* dy dy)))
                      force-magnitude (* spring-strength (- distance spring-length))]
                  (when (> distance 0.1)
                    (let [fx (* force-magnitude (/ dx distance))
                          fy (* force-magnitude (/ dy distance))]
                      (swap! forces update i (fn [[fx-old fy-old]] [(+ fx-old fx) (+ fy-old fy)]))
                      (swap! forces update j (fn [[fx-old fy-old]] [(- fx-old fx) (- fy-old fy)])))))))

            ;; Calculate repulsion forces
            (doseq [i (range num-qubits)
                    j (range (inc i) num-qubits)]
              (let [[xi yi] (nth @positions i)
                    [xj yj] (nth @positions j)
                    dx (- xj xi)
                    dy (- yj yi)
                    distance-sq (+ (* dx dx) (* dy dy))
                    distance (Math/sqrt distance-sq)]
                (when (> distance 0.1)
                  (let [force-magnitude (/ repulsion-strength distance-sq)
                        fx (* force-magnitude (/ dx distance))
                        fy (* force-magnitude (/ dy distance))]
                    (swap! forces update i (fn [[fx-old fy-old]] [(- fx-old fx) (- fy-old fy)]))
                    (swap! forces update j (fn [[fx-old fy-old]] [(+ fx-old fx) (+ fy-old fy)]))))))

            ;; Apply forces with adaptive damping
            (swap! positions
                   (fn [pos]
                     (mapv (fn [i [x y]]
                             (let [[fx fy] (nth @forces i)
                                   ;; Use adaptive damping for better convergence
                                   step-size (* damping 0.1)
                                   new-x (max padding (min (- width padding) (+ x (* fx step-size))))
                                   new-y (max padding (min (- height padding) (+ y (* fy step-size))))]
                               [new-x new-y]))
                           (range num-qubits)
                           pos)))))
        @positions)

      :hierarchical
      ;; For star topologies or trees - put high-degree nodes at center
      (let [degrees (mapv count topology)
            max-degree (if (empty? degrees) 0 (apply max degrees))
            central-nodes (keep-indexed (fn [i degree]
                                          (when (= degree max-degree) i))
                                        degrees)]
        (if (= (count central-nodes) 1)
          ;; Star layout with one center
          (let [center (first central-nodes)
                others (remove #(= % center) (range num-qubits))
                angle-step (/ (* 2 Math/PI) (count others))
                radius (min (/ usable-width 2) (/ usable-height 2))]
            (mapv (fn [i]
                    (if (= i center)
                      [center-x center-y]
                      (let [angle (* (.indexOf others i) angle-step)]
                        [(+ center-x (* radius (Math/cos angle)))
                         (+ center-y (* radius (Math/sin angle)))])))
                  (range num-qubits)))
          ;; Fallback to grid layout
          (calculate-topology-layout topology :grid canvas-size)))

      ;; Default: grid layout
      (calculate-topology-layout topology :grid canvas-size))))

;;;
;;; Rendering Functions (hiccup output)
;;;
(defn render-bar-chart
  [state & {:keys [width height threshold max-bars show-amplitudes]
            :or {width 600 height 400 threshold 0.001 max-bars 16 show-amplitudes false}}]

  (let [;; Use common utilities for data extraction and filtering
        chart-data (common/prepare-bar-chart-data state
                                                  :threshold threshold
                                                  :max-bars max-bars)
        probabilities (:probabilities chart-data)
        labels (:labels chart-data)
        indices (:indices chart-data)
        max-prob (:max-probability chart-data)
        amplitudes (:state-vector state)
        n-qubits (:num-qubits state)

        ;; Chart layout - adaptive bottom margin for label overlap handling
        total-basis-states (bit-shift-left 1 n-qubits) ; 2^n_qubits
        needs-rotation (> total-basis-states 8)
        bottom-margin (if needs-rotation 120 100) ; Extra space for rotated labels
        margin {:top 60 :right 40 :bottom bottom-margin :left 80}
        chart-width (- width (:left margin) (:right margin))
        chart-height (- height (:top margin) (:bottom margin))

        bar-width (/ chart-width (max 1 (count probabilities)))

        ;; Color scheme - quantum-inspired blues and purples
        bar-colors (common/generate-color-palette (count probabilities) :scheme :quantum)

        ;; Generate bars
        bars (map-indexed
              (fn [i prob]
                (let [bar-height (* (/ prob max-prob) chart-height)
                      x (+ (:left margin) (* i bar-width))
                      y (+ (:top margin) (- chart-height bar-height))
                      color (nth bar-colors i)
                      amplitude (nth amplitudes (nth indices i))
                      tooltip-text (if show-amplitudes
                                     (str (nth labels i) ": "
                                          (common/format-amplitude-display amplitude))
                                     (str (nth labels i) ": "
                                          (qmath/round-precision (* prob 100) 1) "%"))]
                  [:g {:class "bar-group"}
                   [:rect {:x x :y y
                           :width (- bar-width 2) :height bar-height
                           :fill color :opacity 0.8
                           :stroke "#ffffff" :stroke-width 1}
                    [:title tooltip-text]]
                   [:text {:x (+ x (/ bar-width 2)) :y (+ (:top margin) chart-height 20)
                           :text-anchor (if needs-rotation "end" "middle")
                           :font-size "12" :fill "#374151"
                           :transform (when needs-rotation
                                        (str "rotate(-45 " (+ x (/ bar-width 2)) " " (+ (:top margin) chart-height 20) ")"))}
                    (nth labels i)]
                   [:text {:x (+ x (/ bar-width 2)) :y (- y 5)
                           :text-anchor "middle" :font-size "10" :fill "#6b7280"}
                    (str (qmath/round-precision (* prob 100) 1) "%")]]))
              probabilities)

        ;; Y-axis
        max-percent (int (Math/ceil (* max-prob 100)))
        tick-step (max 10 (int (/ max-percent 5)))
        y-ticks (range 0 (inc max-percent) tick-step)
        y-axis [:g
                [:line {:x1 (:left margin) :y1 (:top margin)
                        :x2 (:left margin) :y2 (+ (:top margin) chart-height)
                        :stroke "#9ca3af" :stroke-width 1}]
                (map (fn [tick]
                       (let [y (+ (:top margin) (- chart-height (* (/ tick 100.0) (/ chart-height max-prob))))]
                         [:g
                          [:line {:x1 (- (:left margin) 5) :y1 y :x2 (:left margin) :y2 y
                                  :stroke "#9ca3af" :stroke-width 1}]
                          [:text {:x (- (:left margin) 10) :y (+ y 3)
                                  :text-anchor "end" :font-size "10" :fill "#6b7280"}
                           (str tick "%")]]))
                     y-ticks)]

        ;; X-axis
        x-axis [:line {:x1 (:left margin) :y1 (+ (:top margin) chart-height)
                       :x2 (+ (:left margin) chart-width) :y2 (+ (:top margin) chart-height)
                       :stroke "#9ca3af" :stroke-width 1}]

        ;; Chart title and labels
        title [:text {:x (/ width 2) :y 25
                      :text-anchor "middle" :font-size "16" :font-weight "bold" :fill "#111827"}
               (str "Quantum State Probability Distribution (" n-qubits " qubits)")]

        y-label [:text {:x 20 :y (/ height 2)
                        :text-anchor "middle" :font-size "12" :fill "#374151"
                        :transform (str "rotate(-90 20 " (/ height 2) ")")}
                 "Probability (%)"]

        x-label [:text {:x (/ width 2) :y (- height 10)
                        :text-anchor "middle" :font-size "12" :fill "#374151"}
                 "Basis States"]]

    [:svg {:width width :height height
           :xmlns "http://www.w3.org/2000/svg"
           :style "background-color: #f9fafb; border: 1px solid #e5e7eb; border-radius: 8px;"}
     [:defs
      [:style "text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }
                   rect:hover { opacity: 1; stroke-width: 2; }"]]
     title
     y-label
     x-label
     y-axis
     x-axis
     bars]))

(defn render-quantum-state
  [state & options]
  (render-bar-chart state options))

(defn render-bloch-sphere
  [state & {:keys [size show-coordinates show-references animate]
            :or {size 420 show-coordinates true show-references true animate false}}]
  {:pre [(= (:num-qubits state) 1)]}

  (let [coords (coord/quantum-state-to-bloch-coordinates state)
        {x :x y :y z :z} (:cartesian coords)

        ;; Sphere parameters
        center (/ size 2)
        radius (* size 0.32)

        ;; 3D to 2D projection (isometric view)
        project-3d (fn [x3d y3d z3d]
                     (coord/isometric-projection x3d y3d z3d [center center] radius))

        ;; Reference states
        reference-states coord/reference-state-coordinates

        ;; Sphere outline (wireframe)
        wireframe (coord/generate-sphere-wireframe 8 8 32)
        sphere-circles (map (fn [circle]
                              [:path {:d (str "M " (str/join " L "
                                                             (map (fn [[x3d y3d z3d]]
                                                                    (let [[x2d y2d] (project-3d x3d y3d z3d)]
                                                                      (str x2d "," y2d)))
                                                                  circle)))
                                      :stroke "#475569" :stroke-width 1.4 :fill "none" :opacity 0.8}])
                            (:circles wireframe))

        ;; Meridian lines
        meridians (map (fn [meridian]
                         [:path {:d (str "M " (str/join " L "
                                                        (map (fn [[x3d y3d z3d]]
                                                               (let [[x2d y2d] (project-3d x3d y3d z3d)]
                                                                 (str x2d "," y2d)))
                                                             meridian)))
                                 :stroke "#475569" :stroke-width 1.4 :fill "none" :opacity 0.8}])
                       (:meridians wireframe))

        ;; Coordinate axes
        axis-norm 1.2  ; Extend axes 20% beyond unit sphere
        axes [;; X-axis (red)
              (let [[x1 y1] (project-3d (- axis-norm) 0 0)
                    [x2 y2] (project-3d axis-norm 0 0)]
                [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                        :stroke "#ef4444" :stroke-width 2}])
              ;; Y-axis (green)
              (let [[x1 y1] (project-3d 0 (- axis-norm) 0)
                    [x2 y2] (project-3d 0 axis-norm 0)]
                [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                        :stroke "#22c55e" :stroke-width 2}])
              ;; Z-axis (blue)
              (let [[x1 y1] (project-3d 0 0 (- axis-norm))
                    [x2 y2] (project-3d 0 0 axis-norm)]
                [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                        :stroke "#3b82f6" :stroke-width 2}])]

        ;; Axis labels
        label-dist 1.4  ; Position labels 40% beyond unit sphere
        axis-labels [[:text {:x (first (project-3d label-dist 0 0))
                             :y (second (project-3d label-dist 0 0))
                             :text-anchor "middle" :font-size "14" :fill "#ef4444" :font-weight "bold"}
                      "X"]
                     [:text {:x (first (project-3d 0 label-dist 0))
                             :y (second (project-3d 0 label-dist 0))
                             :text-anchor "middle" :font-size "14" :fill "#22c55e" :font-weight "bold"}
                      "Y"]
                     [:text {:x (first (project-3d 0 0 label-dist))
                             :y (second (project-3d 0 0 label-dist))
                             :text-anchor "middle" :font-size "14" :fill "#3b82f6" :font-weight "bold"}
                      "Z"]]

        ;; Reference state markers
        ref-markers (when show-references
                      (map (fn [[label coords]]
                             (let [{rx :x ry :y rz :z} (:cartesian coords)
                                   [px py] (project-3d rx ry rz)]
                               [:g
                                [:circle {:cx px :cy py :r 3
                                          :fill "#9ca3af" :stroke "#ffffff" :stroke-width 1}]
                                [:text {:x (+ px 8) :y (+ py 3)
                                        :font-size "10" :fill "#6b7280"}
                                 label]]))
                           reference-states))

        ;; Current state vector
        [state-x state-y] (project-3d x y z)

        ;; Prepare display data for tooltips
        display-data (common/prepare-bloch-display-data state coords
                                                        :show-coordinates true
                                                        :show-distances true
                                                        :precision 3
                                                        :format :svg)

        state-vector [:g
                      ;; Vector line from center to state with improved visibility
                      [:line {:x1 center :y1 center :x2 state-x :y2 state-y
                              :stroke "#4c1d95" :stroke-width 4 :opacity 0.9}]
                      [:line {:x1 center :y1 center :x2 state-x :y2 state-y
                              :stroke "#7c3aed" :stroke-width 2.5 :opacity 1}]
                      ;; State point with enhanced visibility
                      [:circle {:cx state-x :cy state-y :r 8
                                :fill "#7c3aed" :stroke "#ffffff" :stroke-width 3}
                       [:title (str "Current State: " (get-in display-data [:coordinates :spherical-text])
                                    "\nBloch Vector: " (get-in display-data [:coordinates :cartesian-text])
                                    "\nState: " (:state-expression display-data))]]
                      ;; State label with shadow for better visibility
                      [:text {:x (+ state-x 14) :y (- state-y 14)
                              :font-size "18" :fill "#4c1d95" :font-weight "bold" :stroke "#ffffff" :stroke-width 0.8 :paint-order "stroke"}
                       "ψ"]]

        ;; Coordinate information
        display-data (common/prepare-bloch-display-data state coords
                                                        :show-coordinates show-coordinates
                                                        :show-distances false  ; SVG uses tooltips instead
                                                        :precision 3
                                                        :format :svg)

        coord-text (when show-coordinates
                     [:g
                      [:text {:x 20 :y (- size 50) :font-size "12" :fill "#374151"}
                       (str "Bloch Vector: " (get-in display-data [:coordinates :cartesian-text]))]
                      [:text {:x 20 :y (- size 35) :font-size "12" :fill "#374151"}
                       (str "Coordinates: " (get-in display-data [:coordinates :spherical-text]))]
                      [:text {:x 20 :y (- size 20) :font-size "12" :fill "#374151"}
                       (str "State: " (:state-expression display-data))]])

        ;; Animation definition
        animations (when animate
                     [:defs
                      [:animateTransform {:attributeName "transform"
                                          :type "rotate"
                                          :values "0 200 200;360 200 200"
                                          :dur "10s"
                                          :repeatCount "indefinite"}]])]

    [:svg {:width size :height size
           :xmlns "http://www.w3.org/2000/svg"
           :style "background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%); border: 1px solid #cbd5e1; border-radius: 8px;"}
     [:defs
      [:style "text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }"]]
     animations
     ;; Equatorial plane (corrected for isometric projection)
     [:ellipse {:cx center :cy center :rx radius :ry (* radius 0.577)
                :fill "#475569" :opacity 0.2}]
     (seq sphere-circles)
     (seq meridians)
     (seq axes)
     (seq axis-labels)
     (seq ref-markers)
     state-vector
     coord-text]))

(defn render-circuit
  [circuit & {:keys [width height gate-spacing qubit-spacing show-measurements interactive]
              :or {gate-spacing 60 qubit-spacing 60 show-measurements true interactive true}}]
  (let [n-qubits (:num-qubits circuit)
        circuit-depth (qc/circuit-depth circuit)
        margin {:top 70 :right 50 :bottom 30 :left 70}

        ;; Calculate default dimensions based on circuit properties
        ;; Width: based on circuit depth (number of sequential layers)
        default-width (+ (* circuit-depth gate-spacing) (* 2 gate-spacing) (:left margin) (:right margin))
        ;; Height: based on number of qubits
        default-height (+ (* n-qubits qubit-spacing) (:top margin) (:bottom margin))

        ;; Use provided dimensions or calculated defaults
        final-width (if (some? width) width default-width)
        final-height (if (some? height) height default-height)

        ;; Calculate circuit area dimensions (excluding margins)
        circuit-width (+ (* circuit-depth gate-spacing) (* 2 gate-spacing))

        ;; Gate visual properties
        operation-styles {:x {:fill "#ef4444" :symbol "X"}
                          :y {:fill "#22c55e" :symbol "Y"}
                          :z {:fill "#3b82f6" :symbol "Z"}
                          :h {:fill "#8b5cf6" :symbol "H"}
                          :s {:fill "#f59e0b" :symbol "S"}
                          :s-dag {:fill "#f59e0b" :symbol "S†"}
                          :t {:fill "#06b6d4" :symbol "T"}
                          :t-dag {:fill "#06b6d4" :symbol "T†"}
                          :rx {:fill "#ec4899" :symbol "RX"}
                          :ry {:fill "#10b981" :symbol "RY"}
                          :rz {:fill "#6366f1" :symbol "RZ"}
                          :phase {:fill "#9333ea" :symbol "P"}
;                          :cx {:fill "#ef4444" :symbol "CX"} ; CNOT alias
                          :cy {:fill "#22c55e" :symbol "CY"}
                          :cz {:fill "#3b82f6" :symbol "CZ"}
                          :swap {:fill "#fb923c" :symbol "×"}
                          :iswap {:fill "#fb923c" :symbol "i×"}
                          :crx {:fill "#ec4899" :symbol "CRX"}
                          :cry {:fill "#10b981" :symbol "CRY"}
                          :crz {:fill "#6366f1" :symbol "CRZ"}
                          :toffoli {:fill "#64748b" :symbol "CCX"}
                          :fredkin {:fill "#fb923c" :symbol "C×"}
                          ;; Rydberg gates
                          :rydberg-cz {:fill "#b91c1c" :symbol "R●"}
                          :rydberg-cphase {:fill "#dc2626" :symbol "R●"}
                          :rydberg-blockade {:fill "#7f1d1d" :symbol "RB"}
                          ;; Global gates
                          :global-x {:fill "#f59e0b" :symbol "GX"}
                          :global-y {:fill "#84cc16" :symbol "GY"}
                          :global-z {:fill "#3b82f6" :symbol "GZ"}
                          :global-h {:fill "#8b5cf6" :symbol "GH"}
                          :global-rx {:fill "#ec4899" :symbol "GRX"}
                          :global-ry {:fill "#10b981" :symbol "GRY"}
                          :global-rz {:fill "#6366f1" :symbol "GRZ"}}

        ;; Qubit lines
        qubit-lines (for [q (range n-qubits)]
                      (let [y (+ (:top margin) (* q qubit-spacing))]
                        [:g
                         ;; Qubit label
                         [:text {:x 30 :y (+ y 5)
                                 :font-size "14" :fill "#374151"}
                          (str "q" q " |0⟩")]
                         ;; Qubit line
                         [:line {:x1 (:left margin) :y1 y
                                 :x2 (+ (:left margin) circuit-width) :y2 y
                                 :stroke "#9ca3af" :stroke-width 2}]]))

        ;; Use common layer assignment functions
        gates (common/extract-circuit-gates circuit)
        gate-layer-assignments (common/assign-gates-to-layers gates n-qubits)

        ;; Generate gate elements
        gate-elements (map
                       (fn [assignment]
                         (let [gate (:gate assignment)
                               layer (:layer assignment)
                               gate-type (:operation-type gate)
                               params (:operation-params gate)
                               control (:control params)
                               target (or (:target params) (first (:targets params)))
                               x (+ (:left margin) (* layer gate-spacing))

                               gate-info (get operation-styles gate-type
                                              {:fill "#6b7280" :symbol (name gate-type)})]

                           (cond
                             ;; CNOT / CX gate
                             (or (= gate-type :cnot) (= gate-type :cx))
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - X gate
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#374151" :stroke-width 2}]
                                [:line {:x1 (- x 8) :y1 target-y :x2 (+ x 8) :y2 target-y
                                        :stroke "#374151" :stroke-width 2}]
                                [:line {:x1 x :y1 (- target-y 8) :x2 x :y2 (+ target-y 8)
                                        :stroke "#374151" :stroke-width 2}]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str "CNOT: control=" control ", target=" target)]])

                             ;; CZ gate
                             (= gate-type :cz)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - Z gate
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#3b82f6" :stroke-width 2}]
                                [:text {:x x :y (+ target-y 5)
                                        :text-anchor "middle" :font-size "12" :fill "#3b82f6" :font-weight "bold"}
                                 "Z"]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str "CZ: control=" control ", target=" target)]])

                             ;; CY gate
                             (= gate-type :cy)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - Y gate
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#22c55e" :stroke-width 2}]
                                [:text {:x x :y (+ target-y 5)
                                        :text-anchor "middle" :font-size "12" :fill "#22c55e" :font-weight "bold"}
                                 "Y"]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str "CY: control=" control ", target=" target)]])

                             ;; Controlled rotation gates (CRX, CRY, CRZ)
                             (#{:crx :cry :crz} gate-type)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - Rotation gate
                                [:rect {:x (- x 15) :y (- target-y 12) :width 30 :height 24
                                        :fill (:fill gate-info) :stroke "#ffffff" :stroke-width 2
                                        :rx 4}]
                                [:text {:x x :y (+ target-y 5)
                                        :text-anchor "middle" :font-size "12" :fill "#ffffff" :font-weight "bold"}
                                 (:symbol gate-info)]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str gate-type " gate: control=" control ", target=" target
                                             (when-let [param-str (format-gate-parameters gate-type params)]
                                               (str ", " param-str)))]])

                             ;; SWAP gate
                             (= gate-type :swap)
                             (let [qubit1 (:qubit1 params)
                                   qubit2 (:qubit2 params)
                                   qubit1-y (+ (:top margin) (* qubit1 qubit-spacing))
                                   qubit2-y (+ (:top margin) (* qubit2 qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Swap symbol on first qubit
                                [:line {:x1 (- x 6) :y1 (- qubit1-y 6) :x2 (+ x 6) :y2 (+ qubit1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ qubit1-y 6) :x2 (+ x 6) :y2 (- qubit1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                ;; Swap symbol on second qubit
                                [:line {:x1 (- x 6) :y1 (- qubit2-y 6) :x2 (+ x 6) :y2 (+ qubit2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ qubit2-y 6) :x2 (+ x 6) :y2 (- qubit2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                ;; Connection line
                                [:line {:x1 x :y1 (min qubit1-y qubit2-y)
                                        :x2 x :y2 (max qubit1-y qubit2-y)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:title (str "SWAP: qubits " qubit1 " and " qubit2)]])

                             ;; iSWAP gate
                             (= gate-type :iswap)
                             (let [qubit1 (:qubit1 params)
                                   qubit2 (:qubit2 params)
                                   qubit1-y (+ (:top margin) (* qubit1 qubit-spacing))
                                   qubit2-y (+ (:top margin) (* qubit2 qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Swap symbols
                                [:line {:x1 (- x 6) :y1 (- qubit1-y 6) :x2 (+ x 6) :y2 (+ qubit1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ qubit1-y 6) :x2 (+ x 6) :y2 (- qubit1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (- qubit2-y 6) :x2 (+ x 6) :y2 (+ qubit2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ qubit2-y 6) :x2 (+ x 6) :y2 (- qubit2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                ;; 'i' indicator
                                [:text {:x (- x 20) :y (/ (+ qubit1-y qubit2-y) 2)
                                        :text-anchor "middle" :font-size "12" :fill "#fb923c" :font-weight "bold"}
                                 "i"]
                                ;; Connection line
                                [:line {:x1 x :y1 (min qubit1-y qubit2-y)
                                        :x2 x :y2 (max qubit1-y qubit2-y)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:title (str "iSWAP: qubits " qubit1 " and " qubit2)]])

                             ;; Toffoli gate (CCX)
                             (= gate-type :toffoli)
                             (let [control1 (:control1 params)
                                   control2 (:control2 params)
                                   control1-y (+ (:top margin) (* control1 qubit-spacing))
                                   control2-y (+ (:top margin) (* control2 qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dots
                                [:circle {:cx x :cy control1-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                [:circle {:cx x :cy control2-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol (same as X/CNOT)
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#374151" :stroke-width 2}]
                                [:line {:x1 (- x 8) :y1 target-y :x2 (+ x 8) :y2 target-y
                                        :stroke "#374151" :stroke-width 2}]
                                [:line {:x1 x :y1 (- target-y 8) :x2 x :y2 (+ target-y 8)
                                        :stroke "#374151" :stroke-width 2}]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control1-y target-y)
                                        :x2 x :y2 (max control1-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:line {:x1 x :y1 (min control2-y target-y)
                                        :x2 x :y2 (max control2-y target-y)
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str "Toffoli (CCX): controls=[" control1 ", " control2 "], target=" target)]])

                             ;; Fredkin gate (CSWAP)
                             (= gate-type :fredkin)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target1 (:target1 params)
                                   target2 (:target2 params)
                                   target1-y (+ (:top margin) (* target1 qubit-spacing))
                                   target2-y (+ (:top margin) (* target2 qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                                ;; Swap symbols on targets
                                [:line {:x1 (- x 6) :y1 (- target1-y 6) :x2 (+ x 6) :y2 (+ target1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ target1-y 6) :x2 (+ x 6) :y2 (- target1-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (- target2-y 6) :x2 (+ x 6) :y2 (+ target2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                [:line {:x1 (- x 6) :y1 (+ target2-y 6) :x2 (+ x 6) :y2 (- target2-y 6)
                                        :stroke "#fb923c" :stroke-width 2}]
                                ;; Connection lines
                                [:line {:x1 x :y1 (min control-y (min target1-y target2-y))
                                        :x2 x :y2 (max control-y (max target1-y target2-y))
                                        :stroke "#374151" :stroke-width 2}]
                                [:title (str "Fredkin (CSWAP): control=" control ", targets=[" target1 ", " target2 "]")]])

                             ;; Rydberg CZ gate
                             (= gate-type :rydberg-cz)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot with Rydberg styling
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#b91c1c" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - Rydberg controlled gate
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#b91c1c" :stroke-width 2}]
                                [:text {:x x :y (+ target-y 5)
                                        :text-anchor "middle" :font-size "10" :fill "#b91c1c" :font-weight "bold"}
                                 "R●"]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#b91c1c" :stroke-width 2}]
                                [:title (str "Rydberg CZ: control=" control ", target=" target)]])

                             ;; Rydberg CPhase gate
                             (= gate-type :rydberg-cphase)
                             (let [control-y (+ (:top margin) (* control qubit-spacing))
                                   target-y (+ (:top margin) (* target qubit-spacing))
                                   phi (:angle params)]
                               [:g {:class (when interactive "gate-group")}
                                ;; Control dot with Rydberg styling
                                [:circle {:cx x :cy control-y :r 6
                                          :fill "#dc2626" :stroke "#ffffff" :stroke-width 2}]
                                ;; Target symbol - Rydberg controlled phase gate
                                [:circle {:cx x :cy target-y :r 12
                                          :fill "none" :stroke "#dc2626" :stroke-width 2}]
                                [:text {:x x :y (+ target-y 2)
                                        :text-anchor "middle" :font-size "9" :fill "#dc2626" :font-weight "bold"}
                                 "R●"]
                                [:text {:x x :y (+ target-y 12)
                                        :text-anchor "middle" :font-size "7" :fill "#dc2626"}
                                 (when phi (str "φ=" (common/format-angle-value phi)))]
                                ;; Connection line
                                [:line {:x1 x :y1 (min control-y target-y)
                                        :x2 x :y2 (max control-y target-y)
                                        :stroke "#dc2626" :stroke-width 2}]
                                [:title (str "Rydberg CPhase: control=" control ", target=" target
                                             (when-let [param-str (or (when phi (str "φ=" (common/format-angle-value phi)))
                                                                      (format-gate-parameters gate-type params))]
                                               (str ", " param-str)))]])

                             ;; Rydberg blockade gate
                             (= gate-type :rydberg-blockade)
                             (let [qubit-indices (:qubit-indices params)
                                   phi (:angle params)
                                   min-qubit (apply min qubit-indices)
                                   max-qubit (apply max qubit-indices)
                                   min-y (+ (:top margin) (* min-qubit qubit-spacing))
                                   max-y (+ (:top margin) (* max-qubit qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                ;; Blockade symbols on each participating qubit
                                (for [q qubit-indices]
                                  (let [qubit-y (+ (:top margin) (* q qubit-spacing))]
                                    [:g {:key (str "blockade-" q)}
                                     [:rect {:x (- x 15) :y (- qubit-y 12) :width 30 :height 24
                                             :fill "#7f1d1d" :stroke "#ffffff" :stroke-width 2
                                             :rx 4}]
                                     [:text {:x x :y (+ qubit-y 5)
                                             :text-anchor "middle" :font-size "9" :fill "#ffffff" :font-weight "bold"}
                                      "RB"]]))
                                ;; Connection line spanning all participating qubits
                                [:line {:x1 x :y1 min-y
                                        :x2 x :y2 max-y
                                        :stroke "#7f1d1d" :stroke-width 3}]
                                [:title (str "Rydberg Blockade: qubits=" qubit-indices
                                             (when phi (str ", φ=" (qmath/round-precision phi 2))))]])

                             ;; Global gates - affect all qubits
                             (#{:global-x :global-y :global-z :global-h :global-rx :global-ry :global-rz} gate-type)
                             (let [gate-info (get operation-styles gate-type)
                                   angle (:angle params)]
                               [:g {:class (when interactive "gate-group")}
                                ;; Global gate indicator - spans all qubits
                                [:rect {:x (- x 20) :y (+ (:top margin) -20)
                                        :width 40 :height (+ (* n-qubits qubit-spacing) 40)
                                        :fill (:fill gate-info) :stroke "#ffffff" :stroke-width 2
                                        :rx 8 :opacity 0.8}]
                                ;; Gate symbol in center
                                [:text {:x x :y (+ (:top margin) (* (/ (dec n-qubits) 2) qubit-spacing) 5)
                                        :text-anchor "middle" :font-size "14" :fill "#ffffff" :font-weight "bold"}
                                 (:symbol gate-info)]
                                ;; Angle annotation for rotation gates
                                (when (and (#{:global-rx :global-ry :global-rz} gate-type) angle)
                                  [:text {:x x :y (+ (:top margin) (* (/ (dec n-qubits) 2) qubit-spacing) 20)
                                          :text-anchor "middle" :font-size "10" :fill "#ffffff"}
                                   (str "θ=" (common/format-angle-value angle))])
                                ;; Global indicator text
                                [:text {:x x :y (+ (:top margin) -10)
                                        :text-anchor "middle" :font-size "8" :fill (:fill gate-info) :font-weight "bold"}
                                 "GLOBAL"]
                                [:title (str "Global " (name gate-type) " gate on all qubits"
                                             (when-let [param-str (or (when angle (str "angle=" (common/format-angle-value angle)))
                                                                      (format-gate-parameters gate-type params))]
                                               (str ", " param-str)))]])

                             ;; Single-qubit gates (default case)
                             :else
                             (let [gate-y (+ (:top margin) (* target qubit-spacing))]
                               [:g {:class (when interactive "gate-group")}
                                [:rect {:x (- x 15) :y (- gate-y 12) :width 30 :height 24
                                        :fill (:fill gate-info) :stroke "#ffffff" :stroke-width 2
                                        :rx 4}]
                                [:text {:x x :y (+ gate-y 5)
                                        :text-anchor "middle" :font-size "12" :fill "#ffffff" :font-weight "bold"}
                                 (:symbol gate-info)]
                                (when (= gate-type :phase)
                                  [:text {:x x :y (+ gate-y 16)
                                          :text-anchor "middle" :font-size "8" :fill "#ffffff"}
                                   (when-let [angle (:angle params)]
                                     (str "φ=" (common/format-angle-value angle)))])
                                [:title (str gate-type " gate on qubit " target
                                             (when-let [param-str (format-gate-parameters gate-type params)]
                                               (str ", " param-str)))]]))))
                       gate-layer-assignments)

        ;; Measurement symbols
        measurements (when show-measurements
                       (for [q (range n-qubits)]
                         (let [x (+ (:left margin) circuit-width 20)
                               y (+ (:top margin) (* q qubit-spacing))]
                           [:g
                            ;; Measurement box
                            [:rect {:x (- x 15) :y (- y 12) :width 30 :height 24
                                    :fill "#fbbf24" :stroke "#f59e0b" :stroke-width 2
                                    :rx 4}]
                            [:text {:x x :y (+ y 5)
                                    :text-anchor "middle" :font-size "10" :fill "#92400e" :font-weight "bold"}
                             "M"]
                            [:title (str "Measure qubit " q)]])))

        ;; Title and circuit info
        title [:text {:x (/ final-width 2) :y 35
                      :text-anchor "middle" :font-size "18" :font-weight "bold" :fill "#111827"}
               (or (:name circuit) "Quantum Circuit")]

        circuit-info [:g
                      [:text {:x 30 :y (- final-height 30)
                              :font-size "12" :fill "#6b7280"}
                       (str "Qubits: " n-qubits " | Gates: " (count gates) " | Depth: " circuit-depth)]
                      (when (:description circuit)
                        [:text {:x 30 :y (- final-height 15)
                                :font-size "12" :fill "#6b7280"}
                         (:description circuit)])]

        ;; Interactive styles
        styles (when interactive
                 [:defs
                  [:style ".gate-group:hover rect { opacity: 1; stroke-width: 3; }
                         .gate-group:hover circle { stroke-width: 3; }
                         text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }"]])]

    [:svg {:width final-width :height final-height
           :xmlns "http://www.w3.org/2000/svg"
           :style "background-color: #ffffff; border: 1px solid #e5e7eb; border-radius: 8px;"}
     styles
     title
     qubit-lines
     gate-elements
     measurements
     circuit-info]))

(defn render-measurement-histogram
  [measurements & {:keys [width height threshold max-bars normalize show-percentages]
                   :or {width 600 height 400 threshold 1 max-bars 16 normalize false show-percentages true}}]
  (let [;; Use common utilities for data preparation
        chart-data (common/prepare-measurement-histogram-data measurements
                                                              :threshold threshold
                                                              :max-bars max-bars
                                                              :normalize normalize)
        counts (:counts chart-data)
        labels (:labels chart-data)
        max-count (:max-count chart-data)
        total-shots (:total-shots chart-data)

        ;; Generate layout parameters
        layout (generate-bar-chart-layout (count counts) width height)

        ;; Generate SVG elements using common components
        bars (generate-svg-bars counts labels layout
                                :max-value max-count
                                :value-formatter identity
                                :tooltip-formatter (fn [label count]
                                                     (str label ": " count " shots"
                                                          (when show-percentages
                                                            (str " (" (qmath/round-precision
                                                                       (* (/ count total-shots) 100) 1) "%)"))))
                                :unit-suffix " shots")

        ;; Y-axis with measurement counts
        y-axis (generate-svg-y-axis layout max-count "Measurement Counts")

        ;; X-axis line
        x-axis (generate-svg-x-axis layout)

        ;; Chart labels
        title (str "Measurement Results (" total-shots " shots)")
        [title-elem y-label-elem x-label-elem] (generate-svg-chart-labels width height title "Measurement Counts" "Basis States")]

    [:svg {:width width :height height
           :xmlns "http://www.w3.org/2000/svg"
           :style "background-color: #f9fafb; border: 1px solid #e5e7eb; border-radius: 8px;"}
     [:defs
      [:style "text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }
                     rect:hover { opacity: 1; stroke-width: 2; }"]]
     title-elem
     y-label-elem
     x-label-elem
     y-axis
     x-axis
     bars]))

(defn render-topology
  [topology & {:keys [width height layout show-labels show-connectivity interactive]
                       :or {layout :auto show-labels true show-connectivity true interactive true}}]
  (let [num-qubits (count topology)
        total-edges (/ (reduce + (map count topology)) 2)

        ;; Auto-select layout if needed
        chosen-layout (if (= layout :auto)
                        (common/auto-select-layout topology)
                        layout)

        ;; Calculate default dimensions based on topology properties
        ;; Base dimensions scale with sqrt of nodes for better space utilization
        base-dimension (max 400 (* 80 (Math/sqrt num-qubits)))

        ;; Layout-specific dimension adjustments
        default-dims (case chosen-layout
                       :linear
                       ;; Linear needs more width than height
                       [(max 600 (* num-qubits 60)) (max 200 (* 40 (Math/sqrt num-qubits)))]

                       :circular
                       ;; Circular needs square canvas with radius for circumference
                       (let [radius (* num-qubits 20)
                             dim (+ (* radius 2) 200)] ; padding for labels
                         [dim dim])

                       :hexagonal
                       ;; Hexagonal layout: estimate layers and calculate radius needed
                       (let [;; Rough estimate of layers in heavy-hex topology
                             estimated-layers (max 2 (Math/ceil (Math/sqrt (/ num-qubits 6))))
                             layer-spacing 80
                             radius (+ (* estimated-layers layer-spacing) 50)
                             dim (+ (* radius 2) 200)] ; padding for labels and info
                         [dim dim])

                       :grid
                       ;; Grid layout: calculate optimal grid dimensions
                       (let [cols (Math/ceil (Math/sqrt num-qubits))
                             rows (Math/ceil (/ num-qubits cols))
                             grid-width (max 400 (+ (* cols 80) 200))
                             grid-height (max 300 (+ (* rows 80) 200))]
                         [grid-width grid-height])

                       :hierarchical
                       ;; Star layout: needs space for center + radius for satellites
                       (let [satellite-count (dec num-qubits)
                             radius (* satellite-count 15)
                             dim (+ (* radius 2) 300)]
                         [dim dim])

                       :force
                       ;; Force-directed: larger space for complex topologies
                       (let [density (/ total-edges (max 1 num-qubits))
                             space-factor (+ 1.0 (* density 0.5)) ; More space for denser graphs
                             dim (* base-dimension space-factor)]
                         [dim dim])

                       ;; Default case
                       [base-dimension base-dimension])

        ;; Use provided dimensions or calculated defaults
        final-width (or width (int (first default-dims)))
        final-height (or height (int (second default-dims)))

        ;; Reserve space for title and info panel
        title-height 50
        info-height 80
        actual-viz-height (- final-height title-height info-height)

        ;; Calculate positions using adjusted dimensions
        positions (calculate-topology-layout topology chosen-layout [final-width actual-viz-height])
        ;; Offset positions to account for title space
        adjusted-positions (mapv (fn [[x y]] [x (+ y title-height)]) positions)

        ;; Generate colors for qubits (quantum color scheme)
        qubit-colors (common/generate-color-palette num-qubits :scheme :quantum)

        ;; Create edges (connections)
        edges (when show-connectivity
                (for [i (range num-qubits)
                      j (get topology i)
                      :when (< i j)] ; Avoid duplicate edges
                  (let [[x1 y1] (nth adjusted-positions i)
                        [x2 y2] (nth adjusted-positions j)]
                    [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                            :stroke "#6b7280" :stroke-width 2 :opacity 0.7}])))

        ;; Create qubit nodes
        qubit-nodes (map-indexed
                     (fn [i [x y]]
                       (let [color (nth qubit-colors (mod i (count qubit-colors)))
                             neighbors (get topology i)
                             degree (count neighbors)]
                         [:g {:class (when interactive "qubit-node")}
                          [:circle {:cx x :cy y :r 16
                                    :fill color :stroke "#ffffff" :stroke-width 3
                                    :opacity 0.9}
                           (when interactive
                             [:title (str "Qubit " i "\nDegree: " degree "\nConnected to: [" (str/join ", " neighbors) "]")])]
                          (when show-labels
                            [:text {:x x :y (+ y 5)
                                    :text-anchor "middle" :font-size "14" :font-weight "bold"
                                    :fill "#ffffff"}
                             (str i)])]))
                     adjusted-positions)

        ;; Topology information
        total-edges (/ (reduce + (map count topology)) 2)
        avg-degree (/ (reduce + (map count topology)) (double num-qubits))
        topology-type (case chosen-layout
                        :hierarchical "Star/Hierarchical"
                        :circular "Ring/Circular"
                        :force "Complex/Force-directed"
                        :grid "Grid/Regular"
                        "Custom")

        ;; Title and info
        title [:text {:x (/ final-width 2) :y 30
                      :text-anchor "middle" :font-size "18" :font-weight "bold" :fill "#111827"}
               (str "Hardware Topology (" topology-type ")")]

        info-panel [:g
                    [:text {:x 20 :y (- final-height 60)
                            :font-size "12" :fill "#374151"}
                     (str "Qubits: " num-qubits)]
                    [:text {:x 20 :y (- final-height 45)
                            :font-size "12" :fill "#374151"}
                     (str "Edges: " total-edges)]
                    [:text {:x 20 :y (- final-height 30)
                            :font-size "12" :fill "#374151"}
                     (str "Avg Degree: " (format "%.1f" avg-degree))]
                    [:text {:x 20 :y (- final-height 15)
                            :font-size "12" :fill "#374151"}
                     (str "Layout: " (name chosen-layout))]]

        ;; Interactive styles
        styles (when interactive
                 [:defs
                  [:style ".qubit-node:hover circle { stroke-width: 4; opacity: 1; }
                           .qubit-node:hover text { font-size: 16px; }
                           text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }"]])]

    [:svg {:width final-width :height final-height
           :xmlns "http://www.w3.org/2000/svg"
           :style "background-color: #f9fafb; border: 1px solid #e5e7eb; border-radius: 8px;"}
     styles
     title
     (seq edges)
     (seq qubit-nodes)
     info-panel]))

;;;
;;; SVG Format Implementations
;;;
(defmethod viz/visualize-bar-chart :hiccup
  [_format state & options]
  (render-bar-chart state options))

(defmethod viz/visualize-bar-chart :svg
  [_format state & options]
  (str (h/html (render-bar-chart state options))))

(defmethod viz/visualize-quantum-state :hiccup
  [_format state & options]
  (viz/visualize-bar-chart :hiccup state options))

(defmethod viz/visualize-quantum-state :svg
  [_format state & options]
  (str (h/html (viz/visualize-bar-chart :svg state options))))

(defmethod viz/visualize-bloch-sphere :hiccup
  [_format state & options]
  (render-bloch-sphere state options))

(defmethod viz/visualize-bloch-sphere :svg
  [_format state & options]
  (str (h/html (render-bloch-sphere state options))))

(defmethod viz/visualize-circuit :hiccup
  [_format circuit & options]
  (render-circuit circuit options))

(defmethod viz/visualize-circuit :svg
  [_format circuit & options]
  (str (h/html (render-circuit circuit options))))

(defmethod viz/visualize-state-evolution :svg
  [_format circuit initial-state & options]
  ;; For now, delegate to HTML which can handle SVG evolution
  ;; In the future, could create animated SVG
  (viz/visualize-state-evolution :html circuit initial-state options))

(defmethod viz/visualize-algorithm-summary :svg
  [_format algorithm-result & options]
  ;; Algorithm summaries are primarily textual, delegate to HTML
  (viz/visualize-algorithm-summary :html algorithm-result options))

(defmethod viz/visualize-measurement-histogram :hiccup
  [_format measurements & options]
  (render-measurement-histogram measurements options))

(defmethod viz/visualize-measurement-histogram :svg
  [_format measurements & options]
  (str (h/html (render-measurement-histogram measurements options))))

(defmethod viz/visualize-topology :hiccup
  [_format topology & options]
  (render-topology topology options))

(defmethod viz/visualize-topology :svg
  [_format topology & options]
  (str (h/html (render-topology topology options))))

(comment
  ;; REPL examples for SVG visualization

  ;; Create SVG bar chart for Bell state
  (require '[org.soulspace.qclojure.domain.state :as qs])
  (require '[org.soulspace.qclojure.domain.gate :as qg])
  (require '[org.soulspace.qclojure.util.io :as qio])

  (def bell-state (-> (qs/zero-state 2)
                      (qg/h-gate 0)
                      (qg/cnot)))

  (def bell-svg (viz/visualize-bar-chart :svg bell-state))
  (qio/save-file bell-svg "bell-state.svg")

  ;; Create SVG bar chart with amplitudes shown in tooltips
  (def bell-svg-detailed (viz/visualize-bar-chart :svg bell-state :show-amplitudes true))
  (qio/save-file bell-svg-detailed "bell-state-detailed.svg")

  ;; Create Bloch sphere visualizations
  (def bloch-0 (viz/visualize-bloch-sphere :svg qs/|0⟩))
  (def bloch-plus (viz/visualize-bloch-sphere :svg qs/|+⟩))
  (def bloch-y (viz/visualize-bloch-sphere :svg (qg/y-gate qs/|0⟩)))

  (qio/save-file bloch-0 "bloch-zero.svg")
  (qio/save-file bloch-plus "bloch-plus.svg")
  (qio/save-file bloch-y "bloch-y-gate.svg")

  ;; Create circuit diagram
  (def circuit-svg (viz/visualize-circuit :svg (qc/bell-state-circuit)))
  (qio/save-file circuit-svg "bell-circuit.svg")

  ;; Create complex circuit with various gates
  (def complex-circuit
    (-> (qc/create-circuit 3 "Complex Circuit")
        (qc/h-gate 0)
        (qc/cz-gate 1 2)
        (qc/s-dag-gate 0)
        (qc/t-dag-gate 1)
        (qc/rx-gate 2 0.5)
        (qc/ry-gate 2 0.25)
        (qc/rz-gate 2 0.75)
        (qc/toffoli-gate 0 1 2)
        (qc/fredkin-gate 1 2 0)))
  (def complex-circuit-svg (viz/visualize-circuit :svg complex-circuit :interactive true))
  (qio/save-file complex-circuit-svg "complex-circuit.svg")

  ;; Test SVG topology visualizations

  ;; Create different topologies
  (def linear-5 (hw/linear-topology 5))
  (def ring-6 (hw/ring-topology 6))
  (def star-7 (hw/star-topology 7))
  (def grid-3x3 (hw/grid-topology 3 3))
  (def hhex-65 (hw/heavy-hex-topology 65))

  ;; Generate SVG visualizations
  (def linear-svg (viz/visualize-topology :svg linear-5))
  (def ring-svg (viz/visualize-topology :svg ring-6))
  (def star-svg (viz/visualize-topology :svg star-7))
  (def grid-svg (viz/visualize-topology :svg grid-3x3))
  (def hhex-svg (viz/visualize-topology :svg hhex-65))

  ;; Save to files
  (qio/save-file linear-svg "linear-topology.svg")
  (qio/save-file ring-svg "ring-topology.svg")
  (qio/save-file star-svg "star-topology.svg")
  (qio/save-file grid-svg "grid-topology.svg")
  (qio/save-file hhex-svg "heavy-hex-topology.svg")

  ;; Test auto layout selection
  (common/auto-select-layout linear-5)  ;=> :grid
  (common/auto-select-layout ring-6)    ;=> :circular  
  (common/auto-select-layout star-7)    ;=> :hierarchical
  (common/auto-select-layout grid-3x3)  ;=> :grid
  (common/auto-select-layout hhex-65)   ;=> :hexagonal
  ;
  )