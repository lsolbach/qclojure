(ns org.soulspace.qclojure.adapter.visualization.coordinates
  "Mathematical coordinate transformations for quantum visualization.
  
  This namespace handles 3D to 2D projections for Bloch sphere visualization
  and other coordinate system transformations needed for quantum state graphics."
  (:require [fastmath.core :as m]
            [fastmath.complex :as fc]))

;; Bloch sphere coordinate calculations
(defn quantum-state-to-bloch-coordinates
  "Convert single-qubit quantum state to Bloch sphere coordinates.
  
  For a quantum state |ψ⟩ = α|0⟩ + β|1⟩, calculates the spherical coordinates
  (θ, φ) and Cartesian coordinates (x, y, z) on the Bloch sphere.
  
  Parameters:
  - state: Single-qubit quantum state with :state-vector containing [α β]
  
  Returns:
  Map with :spherical {:theta θ :phi φ} and :cartesian {:x x :y y :z z}"
  [state]
  {:pre [(= (:num-qubits state) 1)]}
  
  (let [amplitudes (:state-vector state)
        α (first amplitudes)   ; amplitude for |0⟩
        β (second amplitudes)  ; amplitude for |1⟩
        
        ;; Calculate Bloch sphere coordinates
        θ (* 2 (m/acos (fc/abs α)))  ; polar angle
        φ (fc/arg β)                 ; azimuthal angle  
        
        ;; Cartesian coordinates
        x (* (m/sin θ) (m/cos φ))
        y (* (m/sin θ) (m/sin φ))
        z (m/cos θ)]
    
    {:spherical {:theta θ :phi φ}
     :cartesian {:x x :y y :z z}}))

;; 3D to 2D projection functions
(defn isometric-projection
  "Apply isometric projection from 3D coordinates to 2D.
  
  Parameters:
  - x3d, y3d, z3d: 3D coordinates
  - center: Center point for projection [cx cy]
  - scale: Scaling factor
  
  Returns:
  Vector [x2d y2d] of projected 2D coordinates"
  [x3d y3d z3d center scale]
  (let [[cx cy] center]
    [(+ cx (* scale x3d 0.866) (* scale y3d -0.5))
     (+ cy (* scale z3d -1) (* scale y3d -0.866))]))

(defn orthographic-projection
  "Apply orthographic projection from 3D coordinates to 2D.
  
  Parameters:
  - x3d, y3d, z3d: 3D coordinates
  - center: Center point for projection [cx cy]
  - scale: Scaling factor
  
  Returns:
  Vector [x2d y2d] of projected 2D coordinates"
  [x3d _y3d z3d center scale]
  (let [[cx cy] center]
    [(+ cx (* scale x3d))
     (+ cy (* scale z3d))]))

(defn perspective-projection
  "Apply perspective projection from 3D coordinates to 2D.
  
  Parameters:
  - x3d, y3d, z3d: 3D coordinates
  - center: Center point for projection [cx cy]
  - scale: Scaling factor
  - distance: Distance from viewer (affects perspective strength)
  
  Returns:
  Vector [x2d y2d] of projected 2D coordinates"
  [x3d y3d z3d center scale distance]
  (let [[cx cy] center
        perspective-factor (/ distance (+ distance y3d))]
    [(+ cx (* scale x3d perspective-factor))
     (+ cy (* scale z3d perspective-factor))]))

;; Sphere geometry helpers
(defn generate-sphere-wireframe
  "Generate wireframe coordinates for a unit sphere.
  
  Parameters:
  - n-circles: Number of latitude circles
  - n-meridians: Number of longitude meridians  
  - n-points: Points per circle/meridian
  
  Returns:
  Map with :circles and :meridians containing coordinate sequences"
  [n-circles n-meridians n-points]
  (let [circles (for [i (range n-circles)]
                  (let [lat-angle (* (/ i (dec n-circles)) m/PI)
                        circle-radius (m/sin lat-angle)
                        circle-z (m/cos lat-angle)]
                    (for [j (range (inc n-points))]
                      (let [lon-angle (* (/ j n-points) 2 m/PI)]
                        [(* circle-radius (m/cos lon-angle))
                         (* circle-radius (m/sin lon-angle))
                         circle-z]))))
        
        meridians (for [i (range n-meridians)]
                    (let [lon-angle (* (/ i n-meridians) 2 m/PI)]
                      (for [j (range (inc n-points))]
                        (let [lat-angle (* (/ j n-points) m/PI)]
                          [(* (m/sin lat-angle) (m/cos lon-angle))
                           (* (m/sin lat-angle) (m/sin lon-angle))
                           (m/cos lat-angle)]))))]
    
    {:circles circles
     :meridians meridians}))

;; Reference point calculations
(defn reference-state-coordinates
  "Get Bloch sphere coordinates for common reference states.
  
  Returns:
  Map from state labels to {:cartesian {:x x :y y :z z}} coordinates"
  []
  {"|0⟩"   {:cartesian {:x 0 :y 0 :z 1}}      ; North pole
   "|1⟩"   {:cartesian {:x 0 :y 0 :z -1}}     ; South pole  
   "|+⟩"   {:cartesian {:x 1 :y 0 :z 0}}      ; +X axis
   "|-⟩"   {:cartesian {:x -1 :y 0 :z 0}}     ; -X axis
   "|+i⟩"  {:cartesian {:x 0 :y 1 :z 0}}      ; +Y axis
   "|-i⟩"  {:cartesian {:x 0 :y -1 :z 0}}})   ; -Y axis

;; Distance and similarity calculations
(defn bloch-distance
  "Calculate Euclidean distance between two points on Bloch sphere.
  
  Parameters:
  - coords1, coords2: Coordinate maps with :cartesian {:x x :y y :z z}
  
  Returns:
  Distance value between 0 and 2"
  [coords1 coords2]
  (let [{x1 :x y1 :y z1 :z} (:cartesian coords1)
        {x2 :x y2 :y z2 :z} (:cartesian coords2)]
    (m/sqrt (+ (* (- x2 x1) (- x2 x1))
               (* (- y2 y1) (- y2 y1))
               (* (- z2 z1) (- z2 z1))))))

(defn find-closest-reference-state
  "Find the reference state closest to given coordinates.
  
  Parameters:
  - coords: Coordinate map with :cartesian {:x x :y y :z z}
  
  Returns:
  Vector [state-label distance] of closest reference state"
  [coords]
  (let [reference-states (reference-state-coordinates)
        distances (map (fn [[label ref-coords]]
                        [label (bloch-distance coords ref-coords)])
                      reference-states)]
    (first (sort-by second distances))))

(comment
  ;; REPL examples for coordinate transformations
  
  ;; Test Bloch coordinate calculation
  (require '[qclojure.domain.quantum-state :as qs])
  
  (def plus-coords (quantum-state-to-bloch-coordinates qs/|+⟩))
  (println "Plus state coordinates:" plus-coords)
  
  ;; Test projections
  (let [coords (:cartesian plus-coords)
        center [200 200]
        scale 100]
    (println "Isometric:" (isometric-projection (:x coords) (:y coords) (:z coords) center scale))
    (println "Orthographic:" (orthographic-projection (:x coords) (:y coords) (:z coords) center scale)))
  
  ;; Generate sphere wireframe
  (def wireframe (generate-sphere-wireframe 6 8 16))
  (println "Generated" (count (:circles wireframe)) "circles and" (count (:meridians wireframe)) "meridians")
  
  ;; Test reference state finding
  (def closest (find-closest-reference-state plus-coords))
  (println "Closest reference state to |+⟩:" closest)
  )
