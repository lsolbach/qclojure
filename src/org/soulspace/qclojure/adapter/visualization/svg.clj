(ns org.soulspace.qclojure.adapter.visualization.svg
  "SVG-based visualization for quantum states and circuits.
  
  This namespace provides scalable vector graphics (SVG) generation using
  Hiccup for high-quality quantum visualization that can be embedded in
  web pages or saved as standalone files."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.adapter.visualization.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coord]
            [org.soulspace.qclojure.adapter.visualization.common :as common]))


;;;
;;; SVG Format Implementations
;;;
(defmethod viz/visualize-quantum-state :svg
  [_format state & options]
  (viz/visualize-bar-chart :svg state options))

(defmethod viz/visualize-bloch-sphere :svg
  [_format state & {:keys [size show-coordinates show-references animate]
                    :or {size 400 show-coordinates true show-references true animate false}}]
  {:pre [(= (:num-qubits state) 1)]}

  (let [coords (coord/quantum-state-to-bloch-coordinates state)
        {x :x y :y z :z} (:cartesian coords)

        ;; Sphere parameters
        center (/ size 2)
        radius (* size 0.35)

        ;; 3D to 2D projection (isometric view)
        project-3d (fn [x3d y3d z3d]
                     (coord/isometric-projection x3d y3d z3d [center center] radius))

        ;; Reference states
        reference-states (coord/reference-state-coordinates)

        ;; Sphere outline (wireframe)
        wireframe (coord/generate-sphere-wireframe 8 8 32)
        sphere-circles (map (fn [circle]
                              [:path {:d (str "M " (str/join " L "
                                                             (map (fn [[x3d y3d z3d]]
                                                                    (let [[x2d y2d] (project-3d x3d y3d z3d)]
                                                                      (str x2d "," y2d)))
                                                                  circle)))
                                      :stroke "#e5e7eb" :stroke-width 1 :fill "none"}])
                            (:circles wireframe))

        ;; Meridian lines
        meridians (map (fn [meridian]
                         [:path {:d (str "M " (str/join " L "
                                                        (map (fn [[x3d y3d z3d]]
                                                               (let [[x2d y2d] (project-3d x3d y3d z3d)]
                                                                 (str x2d "," y2d)))
                                                             meridian)))
                                 :stroke "#e5e7eb" :stroke-width 1 :fill "none"}])
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
                      ;; Vector line from center to state
                      [:line {:x1 center :y1 center :x2 state-x :y2 state-y
                              :stroke "#7c3aed" :stroke-width 3}]
                      ;; State point
                      [:circle {:cx state-x :cy state-y :r 6
                                :fill "#7c3aed" :stroke "#ffffff" :stroke-width 2}
                       [:title (str "Current State: " (get-in display-data [:coordinates :spherical-text])
                                    "\nBloch Vector: " (get-in display-data [:coordinates :cartesian-text])
                                    "\nState: " (:state-expression display-data))]]
                      ;; State label
                      [:text {:x (+ state-x 10) :y (- state-y 10)
                              :font-size "14" :fill "#7c3aed" :font-weight "bold"}
                       "ψ"]]

        ;; Coordinate information
        display-data (common/prepare-bloch-display-data state coords
                                                        :show-coordinates show-coordinates
                                                        :show-distances false  ; SVG uses tooltips instead
                                                        :precision 3
                                                        :format :svg)
        
        coord-text (when show-coordinates
                     [:g
                      [:text {:x 20 :y 30 :font-size "12" :fill "#374151"}
                       (str "Bloch Vector: " (get-in display-data [:coordinates :cartesian-text]))]
                      [:text {:x 20 :y 45 :font-size "12" :fill "#374151"}
                       (str "Coordinates: " (get-in display-data [:coordinates :spherical-text]))]
                      [:text {:x 20 :y 60 :font-size "12" :fill "#374151"}
                       (str "State: " (:state-expression display-data))]])

        ;; Animation definition
        animations (when animate
                     [:defs
                      [:animateTransform {:attributeName "transform"
                                          :type "rotate"
                                          :values "0 200 200;360 200 200"
                                          :dur "10s"
                                          :repeatCount "indefinite"}]])]

    (str
     (h/html
      [:svg {:width size :height size
             :xmlns "http://www.w3.org/2000/svg"
             :style "background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%); border: 1px solid #e5e7eb; border-radius: 8px;"}
       [:defs
        [:style "text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }"]]
       animations
       ;; Background sphere shadow
       [:ellipse {:cx (+ center 5) :cy (+ center 5) :rx radius :ry (* radius 0.3)
                  :fill "#000000" :opacity 0.1}]
       (seq sphere-circles)
       (seq meridians)
       (seq axes)
       (seq axis-labels)
       (seq ref-markers)
       state-vector
       coord-text]))))

(defmethod viz/visualize-circuit :svg
  [_format circuit & {:keys [width gate-spacing qubit-spacing show-measurements interactive]
            :or {width 800 gate-spacing 80 qubit-spacing 60 show-measurements true interactive true}}]

(let [n-qubits (:num-qubits circuit)
      gates (:gates circuit)
      margin {:top 40 :right 40 :bottom 80 :left 100}

      ;; Calculate dimensions
      circuit-width (+ (* (count gates) gate-spacing) (* 2 gate-spacing))
      height (+ (* n-qubits qubit-spacing) (:top margin) (:bottom margin))

      ;; Gate visual properties
      gate-styles {:x {:fill "#ef4444" :symbol "X"}
                   :y {:fill "#22c55e" :symbol "Y"}
                   :z {:fill "#3b82f6" :symbol "Z"}
                   :h {:fill "#8b5cf6" :symbol "H"}
                   :s {:fill "#f59e0b" :symbol "S"}
                   :t {:fill "#06b6d4" :symbol "T"}
                   :rx {:fill "#ec4899" :symbol "RX"}
                   :ry {:fill "#10b981" :symbol "RY"}
                   :rz {:fill "#6366f1" :symbol "RZ"}}

      ;; Qubit lines
      qubit-lines (for [q (range n-qubits)]
                    (let [y (+ (:top margin) (* q qubit-spacing))]
                      [:g
                       ;; Qubit label
                       [:text {:x 20 :y (+ y 5)
                               :font-size "14" :fill "#374151"}
                        (str "q" q " |0⟩")]
                       ;; Qubit line
                       [:line {:x1 (:left margin) :y1 y
                               :x2 (+ (:left margin) circuit-width) :y2 y
                               :stroke "#9ca3af" :stroke-width 2}]]))

      ;; Generate gate elements
      gate-elements (map-indexed
                     (fn [gate-idx gate]
                       (let [gate-type (:gate-type gate)
                             params (:gate-params gate)
                             control (:control params)
                             target (:target params)
                             x (+ (:left margin) gate-spacing (* gate-idx gate-spacing))

                             gate-info (get gate-styles gate-type
                                            {:fill "#6b7280" :symbol (name gate-type)})]

                         (case gate-type
                           :cnot
                           (let [control-y (+ (:top margin) (* control qubit-spacing))
                                 target-y (+ (:top margin) (* target qubit-spacing))]
                             [:g {:class (when interactive "gate-group")}
                              ;; Control dot
                              [:circle {:cx x :cy control-y :r 6
                                        :fill "#374151" :stroke "#ffffff" :stroke-width 2}]
                              ;; Target symbol
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

                           ;; Single-qubit gates
                           (let [gate-y (+ (:top margin) (* target qubit-spacing))]
                             [:g {:class (when interactive "gate-group")}
                              [:rect {:x (- x 15) :y (- gate-y 12) :width 30 :height 24
                                      :fill (:fill gate-info) :stroke "#ffffff" :stroke-width 2
                                      :rx 4}]
                              [:text {:x x :y (+ gate-y 5)
                                      :text-anchor "middle" :font-size "12" :fill "#ffffff" :font-weight "bold"}
                               (:symbol gate-info)]
                              [:title (str gate-type " gate on qubit " target)]]))))
                     gates)

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
      title [:text {:x (/ (+ width (:left margin)) 2) :y 25
                    :text-anchor "middle" :font-size "18" :font-weight "bold" :fill "#111827"}
             (or (:name circuit) "Quantum Circuit")]

      circuit-info [:g
                    [:text {:x 20 :y (- height 40)
                            :font-size "12" :fill "#6b7280"}
                     (str "Qubits: " n-qubits " | Gates: " (count gates) " | Depth: " (qc/circuit-depth circuit))]
                    (when (:description circuit)
                      [:text {:x 20 :y (- height 25)
                              :font-size "12" :fill "#6b7280"}
                       (:description circuit)])]

      ;; Interactive styles
      styles (when interactive
               [:defs
                [:style ".gate-group:hover rect { opacity: 1; stroke-width: 3; }
                         .gate-group:hover circle { stroke-width: 3; }
                         text { font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; }"]])]

  (str
   (h/html
    [:svg {:width (+ width (:left margin) (:right margin)) :height height
           :xmlns "http://www.w3.org/2000/svg"
           :style "background-color: #ffffff; border: 1px solid #e5e7eb; border-radius: 8px;"}
     styles
     title
     qubit-lines
     gate-elements
     measurements
     circuit-info]))))

(defmethod viz/visualize-bar-chart :svg
  [_format state & {:keys [width height threshold max-bars show-amplitudes]
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

        ;; Chart layout
        margin {:top 60 :right 40 :bottom 100 :left 80}
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
                           :text-anchor "middle" :font-size "12" :fill "#374151"}
                    (nth labels i)]
                   [:text {:x (+ x (/ bar-width 2)) :y (- y 5)
                           :text-anchor "middle" :font-size "10" :fill "#6b7280"}
                    (str (qmath/round-precision (* prob 100) 1) "%")]]))
              probabilities)

      ;; Y-axis
      y-ticks (range 0 (inc (* max-prob 100)) (max 10 (/ (* max-prob 100) 5)))
      y-axis [:g
              [:line {:x1 (:left margin) :y1 (:top margin)
                      :x2 (:left margin) :y2 (+ (:top margin) chart-height)
                      :stroke "#9ca3af" :stroke-width 1}]
              (map (fn [tick]
                     (let [y (+ (:top margin) (- chart-height (* (/ tick 100) (/ chart-height max-prob))))]
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

  (str
   (h/html
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
     bars]))))

(defmethod viz/visualize-state-evolution :svg
  [_format circuit initial-state & options]
  ;; For now, delegate to HTML which can handle SVG evolution
  ;; In the future, could create animated SVG
  (viz/visualize-state-evolution :html circuit initial-state options))

(defmethod viz/visualize-algorithm-summary :svg
  [_format algorithm-result & options]
  ;; Algorithm summaries are primarily textual, delegate to HTML
  (viz/visualize-algorithm-summary :html algorithm-result options))

(comment
  ;; REPL examples for SVG visualization
  
  ;; Create SVG bar chart for Bell state
  (require '[org.soulspace.qclojure.domain.quantum-gate :as qg])
  
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
  (def sample-circuit (qc/bell-state-circuit))
  (def circuit-svg (viz/visualize-circuit :svg sample-circuit))
  (qio/save-file circuit-svg "bell-circuit.svg")
  )
