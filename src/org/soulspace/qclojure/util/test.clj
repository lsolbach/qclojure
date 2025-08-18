(ns org.soulspace.qclojure.util.test
  "Helper functions for testing QClojure."
  (:require [fastmath.complex :as fc]))

(defn vec2?
  "Check if x is a Vec2 object by examining its string representation."
  [x]
  (and x (.startsWith (str (type x)) "class fastmath.vector.Vec2")))

(defn approx=
  "Check if two numbers are approximately equal within tolerance.
  Handles FastMath Vec2 complex numbers and regular numbers."
  ([a b] (approx= a b 1e-10))
  ([a b tolerance]
   (cond
     ;; Both are Vec2 (complex numbers)
     (and (vec2? a) (vec2? b))
     (and (< (Math/abs (- (fc/re a) (fc/re b))) tolerance)
          (< (Math/abs (- (fc/im a) (fc/im b))) tolerance))
     
     ;; One is Vec2, other is number - compare real parts (assuming zero imaginary)
     (vec2? a)
     (and (< (Math/abs (- (fc/re a) b)) tolerance)
          (< (Math/abs (fc/im a)) tolerance))
     
     (vec2? b)
     (and (< (Math/abs (- a (fc/re b))) tolerance)
          (< (Math/abs (fc/im b)) tolerance))
     
     ;; Both are regular numbers
     :else
     (< (Math/abs (- a b)) tolerance))))

(defn real-part
  "Extract real part from number or Vec2 complex."
  [x]
  (if (instance? fastmath.vector.Vec2 x)
    (fc/re x)
    x))

(defn approx-vector=
  "Check if two vectors are approximately equal, handling Vec2 elements."
  ([A B] (approx-vector= A B 1e-10))
  ([A B tolerance]
   (and (= (count A) (count B))
        (every? true? (map (fn [x y] (approx= x y tolerance)) A B)))))

(defn approx-matrix=
  "Check if two matrices (or vectors) are approximately equal, handling Vec2 elements."
  ([A B] (approx-matrix= A B 1e-10))
  ([A B tolerance]
   (cond
     ;; Both are vectors (first element is a number or Vec2, not a nested collection)
     (and (sequential? A) (sequential? B) 
          (or (number? (first A)) (vec2? (first A)))
          (or (number? (first B)) (vec2? (first B))))
     (approx-vector= A B tolerance)
     
     ;; Both are matrices (first element is a collection that's not a Vec2)
     (and (sequential? A) (sequential? B)
          (sequential? (first A)) (not (vec2? (first A)))
          (sequential? (first B)) (not (vec2? (first B))))
     (and (= (count A) (count B))
          (every? true?
                  (map (fn [row-a row-b]
                         (and (= (count row-a) (count row-b))
                              (every? true? 
                                      (map (fn [x y] (approx= x y tolerance)) 
                                           row-a row-b))))
                       A B)))
     
     :else false)))

(defn vec2-matrix->soa
  "Convert a matrix with Vec2 elements to SoA format {:real A :imag B}."
  [matrix]
  {:real (mapv (fn [row] (mapv fc/re row)) matrix)
   :imag (mapv (fn [row] (mapv fc/im row)) matrix)})

(defn approx-complex-matrix=
  "Check if two complex matrices are approximately equal.
  Handles both SoA format {:real A :imag B} and Vec2 matrix format."
  ([A B] (approx-complex-matrix= A B 1e-10))
  ([A B tolerance]
   (cond
     ;; Both are SoA format
     (and (map? A) (map? B) (contains? A :real) (contains? B :real))
     (and (approx-matrix= (:real A) (:real B) tolerance)
          (approx-matrix= (:imag A) (:imag B) tolerance))
     
     ;; A is SoA, B is Vec2 matrix
     (and (map? A) (contains? A :real) (vector? B))
     (approx-complex-matrix= A (vec2-matrix->soa B) tolerance)
     
     ;; A is Vec2 matrix, B is SoA  
     (and (vector? A) (map? B) (contains? B :real))
     (approx-complex-matrix= (vec2-matrix->soa A) B tolerance)
     
     ;; Both are Vec2 matrices
     (and (vector? A) (vector? B))
     (approx-matrix= A B tolerance)
     
     :else false)))

