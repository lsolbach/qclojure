(ns org.soulspace.qclojure.adapter.visualization.coordinates-test
  "Tests for quantum visualization coordinate transformations"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.adapter.visualization.coordinates :as coords]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.util.test :as util]))

;;;
;;; Test Data and Utilities
;;;
(defn coords-approx=
  "Test if two coordinate maps are approximately equal"
  ([coords1 coords2] (coords-approx= coords1 coords2 1e-10))
  ([coords1 coords2 tolerance]
   (let [c1 (:cartesian coords1)
         c2 (:cartesian coords2)]
     (and (util/approx= (:x c1) (:x c2) tolerance)
          (util/approx= (:y c1) (:y c2) tolerance)
          (util/approx= (:z c1) (:z c2) tolerance)))))

;;;
;;; Bloch Sphere Coordinate Tests
;;;

(deftest test-quantum-state-to-bloch-coordinates
  (testing "Bloch sphere coordinate calculation"
    
    (testing "Computational basis states"
      ;; |0⟩ state should map to North pole (0, 0, 1)
      (let [coords-0 (coords/quantum-state-to-bloch-coordinates qs/|0⟩)]
        (is (util/approx= (get-in coords-0 [:cartesian :x]) 0))
        (is (util/approx= (get-in coords-0 [:cartesian :y]) 0))
        (is (util/approx= (get-in coords-0 [:cartesian :z]) 1))
        (is (util/approx= (get-in coords-0 [:spherical :theta]) 0)))
      
      ;; |1⟩ state should map to South pole (0, 0, -1)
      (let [coords-1 (coords/quantum-state-to-bloch-coordinates qs/|1⟩)]
        (is (util/approx= (get-in coords-1 [:cartesian :x]) 0))
        (is (util/approx= (get-in coords-1 [:cartesian :y]) 0))
        (is (util/approx= (get-in coords-1 [:cartesian :z]) -1))
        (is (util/approx= (get-in coords-1 [:spherical :theta]) fm/PI))))
    
    (testing "Superposition states"
      ;; |+⟩ = (|0⟩ + |1⟩)/√2 should map to +X axis (1, 0, 0)
      (let [coords-plus (coords/quantum-state-to-bloch-coordinates qs/|+⟩)]
        (is (util/approx= (get-in coords-plus [:cartesian :x]) 1))
        (is (util/approx= (get-in coords-plus [:cartesian :y]) 0))
        (is (util/approx= (get-in coords-plus [:cartesian :z]) 0))
        (is (util/approx= (get-in coords-plus [:spherical :theta]) (/ fm/PI 2))))
      
      ;; |-⟩ = (|0⟩ - |1⟩)/√2 should map to -X axis (-1, 0, 0)
      (let [coords-minus (coords/quantum-state-to-bloch-coordinates qs/|-⟩)]
        (is (util/approx= (get-in coords-minus [:cartesian :x]) -1))
        (is (util/approx= (get-in coords-minus [:cartesian :y]) 0))
        (is (util/approx= (get-in coords-minus [:cartesian :z]) 0))))
    
    (testing "Complex superposition states"
      ;; |+i⟩ = (|0⟩ + i|1⟩)/√2 should map to +Y axis (0, 1, 0)
      (let [coords-plus-i (coords/quantum-state-to-bloch-coordinates qs/|+i⟩)]
        (is (util/approx= (get-in coords-plus-i [:cartesian :x]) 0))
        (is (util/approx= (get-in coords-plus-i [:cartesian :y]) 1))
        (is (util/approx= (get-in coords-plus-i [:cartesian :z]) 0)))
      
      ;; |-i⟩ = (|0⟩ - i|1⟩)/√2 should map to -Y axis (0, -1, 0)
      (let [coords-minus-i (coords/quantum-state-to-bloch-coordinates qs/|-i⟩)]
        (is (util/approx= (get-in coords-minus-i [:cartesian :x]) 0))
        (is (util/approx= (get-in coords-minus-i [:cartesian :y]) -1))
        (is (util/approx= (get-in coords-minus-i [:cartesian :z]) 0))))
    
    (testing "Normalization constraint"
      ;; All Bloch vectors should have unit length
      (doseq [state [qs/|0⟩ qs/|1⟩ qs/|+⟩ qs/|-⟩ qs/|+i⟩ qs/|-i⟩]]
        (let [coords (coords/quantum-state-to-bloch-coordinates state)
              {x :x y :y z :z} (:cartesian coords)
              length (fm/sqrt (+ (* x x) (* y y) (* z z)))]
          (is (util/approx= length 1.0)))))))

;;;
;;; 3D to 2D Projection Tests
;;;

(deftest test-projections
  (let [center [200 200]
        scale 100]    
    (testing "Isometric projection"
      ;; Test unit vectors along each axis
      (let [x-axis (coords/isometric-projection 1 0 0 center scale)
            y-axis (coords/isometric-projection 0 1 0 center scale)
            z-axis (coords/isometric-projection 0 0 1 center scale)]
        (is (vector? x-axis))
        (is (= (count x-axis) 2))
        (is (vector? y-axis))
        (is (= (count y-axis) 2))
        (is (vector? z-axis))
        (is (= (count z-axis) 2))
        
        ;; Verify that different axes project to different points
        (is (not= x-axis y-axis))
        (is (not= x-axis z-axis))
        (is (not= y-axis z-axis))))
    
    (testing "Orthographic projection"
      ;; Test unit vectors along each axis
      (let [x-axis (coords/orthographic-projection 1 0 0 center scale)
            y-axis (coords/orthographic-projection 0 1 0 center scale)
            z-axis (coords/orthographic-projection 0 0 1 center scale)]
        (is (vector? x-axis))
        (is (= (count x-axis) 2))
        
        ;; In orthographic projection, Y coordinate is ignored
        (is (= x-axis [(+ 200 scale) 200]))
        (is (= z-axis [200 (+ 200 scale)]))))
    
    (testing "Perspective projection"
      (let [distance 500
            ;; Test unit vectors along each axis    
            x-axis (coords/perspective-projection 1 0 0 center scale distance)
            y-axis (coords/perspective-projection 0 1 0 center scale distance)
            z-axis (coords/perspective-projection 0 0 1 center scale distance)]
        (is (vector? x-axis))
        (is (= (count x-axis) 2))
        (is (vector? y-axis))
        (is (= (count y-axis) 2))
        (is (vector? z-axis))
        (is (= (count z-axis) 2))

        ;; Objects closer to viewer (negative Y) should appear larger
        (let [close-point (coords/perspective-projection 1 -100 0 center scale distance)
              far-point (coords/perspective-projection 1 100 0 center scale distance)]
          (is (> (fm/abs (double (- (first close-point) 200)))
                 (fm/abs (double (- (first far-point) 200))))))))))

;;;
;;; Sphere Wireframe Tests
;;;
(deftest test-generate-sphere-wireframe
  (testing "Sphere wireframe generation"
    
    (testing "Basic structure"
      (let [wireframe (coords/generate-sphere-wireframe 4 6 8)]
        (is (map? wireframe))
        (is (contains? wireframe :circles))
        (is (contains? wireframe :meridians))
        (is (= (count (:circles wireframe)) 4))
        (is (= (count (:meridians wireframe)) 6))))
    
    (testing "Circle coordinates"
      (let [wireframe (coords/generate-sphere-wireframe 3 4 4)
            circles (:circles wireframe)]
        ;; Each circle should be a sequence of 3D points
        (doseq [circle circles]
          (is (sequential? circle))
          (is (= (count circle) 5)) ; n-points + 1
          (doseq [point circle]
            (is (vector? point))
            (is (= (count point) 3)) ; [x y z]
            ;; Points should be on unit sphere (approximately)
            (let [[x y z] point
                  radius (fm/sqrt (+ (* x x) (* y y) (* z z)))]
              (is (util/approx= radius 1.0)))))))
    
    (testing "Meridian coordinates"
      (let [wireframe (coords/generate-sphere-wireframe 3 4 4)
            meridians (:meridians wireframe)]
        ;; Each meridian should be a sequence of 3D points
        (doseq [meridian meridians]
          (is (sequential? meridian))
          (is (= (count meridian) 5)) ; n-points + 1
          (doseq [point meridian]
            (is (vector? point))
            (is (= (count point) 3)) ; [x y z]
            ;; Points should be on unit sphere (approximately)
            (let [[x y z] point
                  radius (fm/sqrt (+ (* x x) (* y y) (* z z)))]
              (is (util/approx= radius 1.0)))))))
    
    (testing "Edge cases"
      ;; Test with minimal safe parameters (avoid division by zero in implementation)
      (let [wireframe (coords/generate-sphere-wireframe 2 2 2)]
        (is (= (count (:circles wireframe)) 2))
        (is (= (count (:meridians wireframe)) 2))))))

;;;
;;; Reference State Tests
;;;
(deftest test-reference-state-coordinates
  (testing "Reference state coordinate values"
    
    (testing "All reference states are defined"
      (is (contains? coords/reference-state-coordinates "|0⟩"))
      (is (contains? coords/reference-state-coordinates "|1⟩"))
      (is (contains? coords/reference-state-coordinates "|+⟩"))
      (is (contains? coords/reference-state-coordinates "|-⟩"))
      (is (contains? coords/reference-state-coordinates "|+i⟩"))
      (is (contains? coords/reference-state-coordinates "|-i⟩")))
    
    (testing "Reference coordinates are normalized"
      (doseq [[_label coords] coords/reference-state-coordinates]
        (let [{x :x y :y z :z} (:cartesian coords)
              length (fm/sqrt (+ (* x x) (* y y) (* z z)))]
          (is (util/approx= length 1.0)))))
    
    (testing "Specific coordinate values"
      ;; |0⟩ at North pole
      (let [coords-0 (get coords/reference-state-coordinates "|0⟩")]
        (is (= (get-in coords-0 [:cartesian :x]) 0))
        (is (= (get-in coords-0 [:cartesian :y]) 0))
        (is (= (get-in coords-0 [:cartesian :z]) 1)))
      
      ;; |1⟩ at South pole
      (let [coords-1 (get coords/reference-state-coordinates "|1⟩")]
        (is (= (get-in coords-1 [:cartesian :x]) 0))
        (is (= (get-in coords-1 [:cartesian :y]) 0))
        (is (= (get-in coords-1 [:cartesian :z]) -1)))
      
      ;; |+⟩ on +X axis
      (let [coords-plus (get coords/reference-state-coordinates "|+⟩")]
        (is (= (get-in coords-plus [:cartesian :x]) 1))
        (is (= (get-in coords-plus [:cartesian :y]) 0))
        (is (= (get-in coords-plus [:cartesian :z]) 0))))))

;;;
;;; Distance Calculation Tests
;;;
(deftest test-bloch-distance
  (testing "Bloch sphere distance calculation"
    
    (testing "Distance to self is zero"
      (let [coords {:cartesian {:x 1 :y 0 :z 0}}]
        (is (util/approx= (coords/bloch-distance coords coords) 0))))
    
    (testing "Distance between antipodal points"
      (let [north-pole {:cartesian {:x 0 :y 0 :z 1}}
            south-pole {:cartesian {:x 0 :y 0 :z -1}}]
        (is (util/approx= (coords/bloch-distance north-pole south-pole) 2))))
    
    (testing "Distance between orthogonal points"
      (let [x-axis {:cartesian {:x 1 :y 0 :z 0}}
            y-axis {:cartesian {:x 0 :y 1 :z 0}}
            z-axis {:cartesian {:x 0 :y 0 :z 1}}]
        (is (util/approx= (coords/bloch-distance x-axis y-axis) (fm/sqrt 2)))
        (is (util/approx= (coords/bloch-distance x-axis z-axis) (fm/sqrt 2)))
        (is (util/approx= (coords/bloch-distance y-axis z-axis) (fm/sqrt 2)))))
    
    (testing "Distance is symmetric"
      (let [coords1 {:cartesian {:x 0.6 :y 0.8 :z 0}}
            coords2 {:cartesian {:x 0 :y 0 :z 1}}]
        (is (util/approx= (coords/bloch-distance coords1 coords2)
                     (coords/bloch-distance coords2 coords1)))))))

(deftest test-find-closest-reference-state
  (testing "Finding closest reference state"
    
    (testing "Exact matches"
      (let [north-pole {:cartesian {:x 0 :y 0 :z 1}}
            [closest-label distance] (coords/find-closest-reference-state 
                                     coords/reference-state-coordinates north-pole)]
        (is (= closest-label "|0⟩"))
        (is (util/approx= distance 0))))
    
    (testing "Near |+⟩ state"
      (let [near-plus {:cartesian {:x 0.9 :y 0.1 :z 0.1}}
            [closest-label _distance] (coords/find-closest-reference-state 
                                      coords/reference-state-coordinates near-plus)]
        (is (= closest-label "|+⟩"))))
    
    (testing "Near |1⟩ state"
      (let [near-one {:cartesian {:x 0.1 :y 0.1 :z -0.9}}
            [closest-label _distance] (coords/find-closest-reference-state 
                                      coords/reference-state-coordinates near-one)]
        (is (= closest-label "|1⟩"))))
    
    (testing "Return format"
      (let [test-coords {:cartesian {:x 0 :y 0 :z 1}}
            result (coords/find-closest-reference-state 
                   coords/reference-state-coordinates test-coords)]
        (is (vector? result))
        (is (= (count result) 2))
        (is (string? (first result)))
        (is (number? (second result)))))))

;;;
;;; Integration Tests
;;;
(deftest test-integration-bloch-coordinates-with-reference-states
  (testing "Integration between Bloch coordinates and reference states"
    
    (testing "Quantum states match reference coordinates"
      ;; |0⟩ state should be closest to "|0⟩" reference
      (let [coords-0 (coords/quantum-state-to-bloch-coordinates qs/|0⟩)
            [closest-label distance] (coords/find-closest-reference-state 
                                     coords/reference-state-coordinates coords-0)]
        (is (= closest-label "|0⟩"))
        (is (< distance 1e-10)))
      
      ;; |+⟩ state should be closest to "|+⟩" reference
      (let [coords-plus (coords/quantum-state-to-bloch-coordinates qs/|+⟩)
            [closest-label distance] (coords/find-closest-reference-state 
                                     coords/reference-state-coordinates coords-plus)]
        (is (= closest-label "|+⟩"))
        (is (< distance 1e-10))))
    
    (testing "Projection consistency"
      ;; Same state should project consistently across different projection methods
      (let [coords (coords/quantum-state-to-bloch-coordinates qs/|+⟩)
            {x :x y :y z :z} (:cartesian coords)
            center [200 200]
            scale 100
            iso-proj (coords/isometric-projection x y z center scale)
            ortho-proj (coords/orthographic-projection x y z center scale)
            persp-proj (coords/perspective-projection x y z center scale 500)]
        (is (vector? iso-proj))
        (is (vector? ortho-proj))
        (is (vector? persp-proj))
        (is (= (count iso-proj) 2))
        (is (= (count ortho-proj) 2))
        (is (= (count persp-proj) 2))))))

;;;
;;; Performance and Edge Case Tests
;;;
(deftest test-edge-cases
  (testing "Edge cases and error conditions"
    
    (testing "Coordinate transformation edge cases"
      ;; Zero coordinates
      (let [zero-coords {:cartesian {:x 0 :y 0 :z 0}}]
        (is (number? (coords/bloch-distance zero-coords zero-coords))))
      
      ;; Large coordinates
      (let [large-coords {:cartesian {:x 1000 :y 1000 :z 1000}}
            small-coords {:cartesian {:x 0.001 :y 0.001 :z 0.001}}]
        (is (number? (coords/bloch-distance large-coords small-coords))))
      
      ;; Projection with zero scale
      (let [result (coords/isometric-projection 1 1 1 [0 0] 0)]
        (is (vector? result))
        (is (= result [0.0 0.0]))))
    
    (testing "Sphere wireframe edge cases"
      ;; Test with small but safe wireframe (avoid division by zero)
      (let [wireframe (coords/generate-sphere-wireframe 2 2 1)]
        (is (map? wireframe))
        (is (= (count (:circles wireframe)) 2))
        (is (= (count (:meridians wireframe)) 2)))
      
      ;; Test wireframe structure integrity
      (let [wireframe (coords/generate-sphere-wireframe 3 3 2)]
        (is (map? wireframe))
        (doseq [circle (:circles wireframe)]
          (is (= (count circle) 3))) ; n-points + 1 = 2 + 1 = 3
        (doseq [meridian (:meridians wireframe)]
          (is (= (count meridian) 3)))))))

(comment
  ;; REPL testing examples
  
  ;; Run all tests
  (run-tests)
  
  
  ;; Test Bloch coordinates for |+⟩ state
  (coords/quantum-state-to-bloch-coordinates qs/|+⟩)
  
  ;; Test projection
  (coords/isometric-projection 1 0 0 [200 200] 100))