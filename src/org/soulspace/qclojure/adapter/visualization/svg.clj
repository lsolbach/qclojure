(ns org.soulspace.qclojure.adapter.visualization.svg
  "SVG-based visualization for quantum states and circuits.
  
  This namespace provides scalable vector graphics (SVG) generation using
  Hiccup for high-quality quantum visualization that can be embedded in
  web pages or saved as standalone files."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.adapter.visualization :as viz]
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

    (str
     (h/html
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
       coord-text]))))

(defmethod viz/visualize-circuit :svg
  [_format circuit & {:keys [width height gate-spacing qubit-spacing show-measurements interactive]
            :or {gate-spacing 60 qubit-spacing 60 show-measurements true interactive true}}]

(let [n-qubits (:num-qubits circuit)
      operations (:operations circuit)
      gates (filter #(not= (:operation-type %) :measure) operations)
      circuit-depth (qc/circuit-depth circuit)
      margin {:top 40 :right 40 :bottom 80 :left 100}

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
;                   :cx {:fill "#ef4444" :symbol "CX"}
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
                       [:text {:x 20 :y (+ y 5)
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
                             target (:target params)
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
                                 target-y (+ (:top margin) (* target qubit-spacing))
                                 angle (first (:rotation-angles params))]
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
                                           (when angle (str ", angle=" angle)))]])

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
                               (when phi (str "φ=" (qmath/round-precision phi 2)))]
                              ;; Connection line
                              [:line {:x1 x :y1 (min control-y target-y)
                                      :x2 x :y2 (max control-y target-y)
                                      :stroke "#dc2626" :stroke-width 2}]
                              [:title (str "Rydberg CPhase: control=" control ", target=" target
                                           (when phi (str ", φ=" (qmath/round-precision phi 2))))]])

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
                                 (str "θ=" (qmath/round-precision angle 2))])
                              ;; Global indicator text
                              [:text {:x x :y (+ (:top margin) -10)
                                      :text-anchor "middle" :font-size "8" :fill (:fill gate-info) :font-weight "bold"}
                               "GLOBAL"]
                              [:title (str "Global " (name gate-type) " gate on all qubits"
                                           (when angle (str ", angle=" (qmath/round-precision angle 2))))]])

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
                                 (when-let [angle (first (:rotation-angles params))]
                                   (str "φ=" (qmath/round-precision angle 2)))])
                              [:title (str gate-type " gate on qubit " target
                                           (when-let [angle (and (#{:rx :ry :rz :phase} gate-type) 
                                                                (first (:rotation-angles params)))]
                                             (str ", angle=" (qmath/round-precision angle 2))))]]))))
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
      title [:text {:x (/ final-width 2) :y 25
                    :text-anchor "middle" :font-size "18" :font-weight "bold" :fill "#111827"}
             (or (:name circuit) "Quantum Circuit")]

      circuit-info [:g
                    [:text {:x 20 :y (- final-height 40)
                            :font-size "12" :fill "#6b7280"}
                     (str "Qubits: " n-qubits " | Gates: " (count gates) " | Depth: " circuit-depth)]
                    (when (:description circuit)
                      [:text {:x 20 :y (- final-height 25)
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
    [:svg {:width final-width :height final-height
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
  (require '[org.soulspace.qclojure.domain.gate :as qg])

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
  ;
  )