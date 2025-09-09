(ns org.soulspace.qclojure.domain.math.complex 
  "Protocol and implementations for complex number operations.
   
   This namespace defines a protocol for complex number operations, allowing
   different backends to implement their own representations of complex numbers,
   vectors, and matrices. The protocol provides functions to access the real
   and imaginary parts, compute the complex conjugate, and check if a value
   is a complex number.
   
   Complex Number Protocol:
   - Access real and imaginary parts of complex numbers
   - Compute complex conjugates
   - Check if a value is a complex number"
  (:require [fastmath.complex :as fc]))

;;;
;;; Complex number protocols
;;;

;; Provides a minimal protocol for complex numbers, allowing backends to define their own representations.
;; This protocol is used by matrix algebra operations that may involve complex numbers.
;; Backends can implement this protocol to provide access to the real and imaginary parts of complex numbers, vectors and matrices.
;; The protocol is designed to be flexible, allowing for different representations (e.g., maps, records, or custom types).
;; Implementations should ensure that the real and imaginary parts are accessible in a consistent manner.
(defprotocol Complex
  "Protocol for complex number operations.
  
  Provides access to real and imaginary parts of complex numbers, vectors, and matrices.
  Backends can implement this protocol to support different complex number representations."

  (real [x]
    "Extract the real part of a complex number.
    
    Parameters:
    - x: Complex number, vector, or matrix
    
    Returns:
    Real part of x (scalar, vector, or matrix of real values)")

  (imag [x]
    "Extract the imaginary part of a complex number.
    
    Parameters:
    - x: Complex number, vector, or matrix
    
    Returns:
    Imaginary part of x (scalar, vector, or matrix of real values)")

  (conjugate [x]
    "Compute the complex conjugate of a number.
    
    Parameters:
    - x: Complex number, vector, or matrix
    
    Returns:
    Complex conjugate of x (same structure with imaginary parts negated)")

  (complex? [x]
    "Test if a value represents a complex number.
    
    Parameters:
    - x: Value to test
    
    Returns:
    Boolean indicating whether x is a complex element"))


;;;
;;; Complex protocol implementation
;;;

(extend-protocol Complex
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
  (real [v] (mapv real v))
  (imag [v] (mapv imag v))
  (conjugate [v] (mapv conjugate v))
  (complex? [v] (every? complex? v)))

