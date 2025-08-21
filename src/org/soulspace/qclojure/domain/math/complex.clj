(ns org.soulspace.qclojure.domain.math.complex 
  (:require
    [org.soulspace.qclojure.domain.math.protocols :as proto]
    [fastmath.complex :as fc]))

;;;
;;; Complex protocol implementation
;;;

(extend-protocol proto/Complex
  fastmath.vector.Vec2
  (real [x] (fc/re x))
  (imag [x] (fc/im x))
  (conjugate [x] (fc/conjugate x))
  (complex? [_] true)

  java.lang.Number
  (real [x] (double x))
  (imag [_] 0.0)
  (conjugate [x] x)
  (complex? [_] false)

  clojure.lang.IPersistentMap
  (real [x]
    (if (and (contains? x :real) (contains? x :imag))
      (:real x)
      (throw (ex-info "Not a complex map" {:value x}))))
  (imag [x]
    (if (and (contains? x :real) (contains? x :imag))
      (:imag x)
      (throw (ex-info "Not a complex map" {:value x}))))
  (conjugate [x]
    (if (and (contains? x :real) (contains? x :imag))
      {:real (:real x) :imag (- (:imag x))}
      (throw (ex-info "Not a complex map" {:value x}))))
  (complex? [x]
    (and (contains? x :real) (contains? x :imag)
         (number? (:real x)) (number? (:imag x))))

  clojure.lang.IPersistentVector
  (real [v] (mapv proto/real v))
  (imag [v] (mapv proto/imag v))
  (conjugate [v] (mapv proto/conjugate v))
  (complex? [v] (every? proto/complex? v)))

