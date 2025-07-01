(ns org.soulspace.qclojure.util.test
  "Helper functions for testing QClojure."
  (:require [fastmath.core :as m]
            [org.soulspace.qclojure.domain.math :as qmath]))

(defn approx=
  "Check if two numbers are approximately equal within tolerance."
  ([a b] (approx= a b 1e-10))
  ([a b tolerance]
   (< (m/abs (- a b)) tolerance)))

