(ns org.soulspace.qclojure.util.test
  "Helper functions for testing QClojure."
  (:require [fastmath.core :as m]
            [org.soulspace.qclojure.domain.math :as qmath]))

(defn approx=
  "Check if two numbers are approximately equal within tolerance."
  ([a b] (approx= a b 1e-10))
  ([a b tolerance]
   (< (m/abs (- a b)) tolerance)))

(defn approx-matrix=
  "Check if two matrices (vec-of-vecs) are approximately equal within tolerance."
  ([A B] (approx-matrix= A B 1e-10))
  ([A B tolerance]
   (and (= (count A) (count B))
        (every? true?
                (map (fn [ar br]
                       (and (= (count ar) (count br))
                            (every? true? (map (fn [x y] (approx= x y tolerance)) ar br))))
                     A B)))))

(defn approx-complex-matrix=
  "Check if two complex matrices in SoA form {:real A :imag B} are approximately equal."
  ([A B] (approx-complex-matrix= A B 1e-10))
  ([A B tolerance]
   (and (approx-matrix= (:real A) (:real B) tolerance)
        (approx-matrix= (:imag A) (:imag B) tolerance))))

