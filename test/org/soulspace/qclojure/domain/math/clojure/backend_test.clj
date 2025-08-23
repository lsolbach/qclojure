(ns org.soulspace.qclojure.domain.math.clojure.backend-test
  "Tests for the pure Clojure math backend implementation.
   
   Focuses on the pluggable backend architecture and its compliance with the math protocols.
   Smoke tests for basic operations are included."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.util.test :as util]
            [org.soulspace.qclojure.domain.math.protocols :as proto]
            [org.soulspace.qclojure.domain.math.clojure.backend :as backend]
            [fastmath.complex :as fc]))

(def backend (backend/->ClojureMathComplexBackend 1e-12 {}))

;;;
;;; Helper functions for comparing results
;;;
(defn vec2->real 
  "Extract real part from Vec2 complex number if imaginary part is zero, else return the Vec2"
  [v]
  (if (instance? fastmath.vector.Vec2 v)
    (if (< (Math/abs (fc/im v)) 1e-10)
      (fc/re v)
      v)
    v))

(defn vec2-vector->real-vector 
  "Convert vector of Vec2 to vector of reals if all imaginary parts are zero"
  [v]
  (mapv vec2->real v))

(defn vec2-matrix->real-matrix 
  "Convert matrix of Vec2 to matrix of reals if all imaginary parts are zero"
  [m]
  (mapv vec2-vector->real-vector m))

;;;
;;; Test data
;;;
(def simple-vector [1.0 2.0 3.0])
(def complex-vector [{:real [1.0 2.0] :imag [0.0 0.5]}])
(def simple-matrix [[1.0 2.0] [3.0 4.0]])
(def complex-matrix {:real [[1.0 2.0] [3.0 4.0]] :imag [[0.0 0.5] [0.2 0.0]]})

;;;
;;; BackendAdapter tests
;;;
(deftest test-backend-adapter-vector-conversion
  (testing "vector->backend and backend->vector round-trip"
    (let [converted (proto/vector->backend backend simple-vector)
          back-converted (proto/backend->vector backend converted)
          real-converted (vec2-vector->real-vector back-converted)]
      (is (util/approx-vector= simple-vector real-converted 1e-10)
          "Simple vector round-trip should preserve values")))
  
  (testing "empty vector conversion"
    (let [empty-vec []
          converted (proto/vector->backend backend empty-vec)
          back-converted (proto/backend->vector backend converted)]
      (is (= empty-vec back-converted)
          "Empty vector round-trip should work")))
  
  (testing "complex vector structure validation"
    (let [complex-vec {:real [1.0 2.0] :imag [0.0 0.5]}
          converted (proto/vector->backend backend complex-vec)]
      (is (and (contains? converted :real)
               (contains? converted :imag))
          "Complex vector should maintain SoA structure"))))

(deftest test-backend-adapter-matrix-conversion
  (testing "matrix->backend and backend->matrix round-trip"
    (let [converted (proto/matrix->backend backend simple-matrix)
          back-converted (proto/backend->matrix backend converted)
          real-converted (vec2-matrix->real-matrix back-converted)]
      (is (util/approx-matrix= simple-matrix real-converted 1e-10)
          "Simple matrix round-trip should preserve values")))
  
  (testing "complex matrix structure validation"
    (let [converted (proto/matrix->backend backend complex-matrix)]
      (is (and (contains? converted :real)
               (contains? converted :imag))
          "Complex matrix should maintain SoA structure"))))

;;;
;;; MatrixAlgebra smoke tests
;;;
(deftest test-matrix-algebra-basic-operations
  (testing "matrix addition"
    (let [A (proto/matrix->backend backend simple-matrix)
          B (proto/matrix->backend backend simple-matrix)
          result (proto/add backend A B)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[2.0 4.0] [6.0 8.0]]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix addition should work correctly")))
  
  (testing "matrix scaling"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/scale backend A 2.0)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[2.0 4.0] [6.0 8.0]]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix scaling should work correctly")))
  
  (testing "matrix multiplication"
    (let [A (proto/matrix->backend backend simple-matrix)
          B (proto/matrix->backend backend simple-matrix)
          result (proto/matrix-multiply backend A B)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[7.0 10.0] [15.0 22.0]]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix multiplication should work correctly"))))

(deftest test-matrix-algebra-additional-operations
  (testing "matrix transpose"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/transpose backend A)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[1.0 3.0] [2.0 4.0]]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix transpose should work correctly")))
  
  (testing "matrix trace"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/trace backend A)]
      (is (util/approx= 5.0 (:real result) 1e-10)
          "Matrix trace should work correctly")))
  
  (testing "matrix-vector multiplication"
    (let [A (proto/matrix->backend backend simple-matrix)
          v (proto/vector->backend backend [1.0 2.0])
          result (proto/matrix-vector-product backend A v)
          result-converted (vec2-vector->real-vector (proto/backend->vector backend result))
          expected [5.0 11.0]]
      (is (util/approx-vector= expected result-converted 1e-10)
          "Matrix-vector multiplication should work correctly"))))

;;;
;;; MatrixDecompositions smoke tests
;;;
(deftest test-matrix-decompositions
  (testing "LU decomposition"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/lu-decomposition backend A)]
      (is (map? result)
          "LU decomposition should return a map")
      (is (contains? result :L)
          "LU result should contain L matrix")
      (is (contains? result :U)
          "LU result should contain U matrix")
      (is (contains? result :P)
          "LU result should contain P permutation")))
  
  (testing "QR decomposition"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/qr-decomposition backend A)]
      (is (map? result)
          "QR decomposition should return a map")
      (is (contains? result :Q)
          "QR result should contain Q matrix")
      (is (contains? result :R)
          "QR result should contain R matrix"))))

;;;
;;; MatrixFunctions tests
;;;
(deftest test-matrix-functions
  (testing "matrix exponential"
    (let [A (proto/matrix->backend backend [[0.0 1.0] [0.0 0.0]])  ; nilpotent matrix
          result (proto/matrix-exp backend A)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[1.0 1.0] [0.0 1.0]]]  ; exp([[0 1] [0 0]]) = I + A = [[1 1] [0 1]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix exponential of nilpotent matrix should be correct")))
  
  (testing "matrix square root"
    (let [A (proto/matrix->backend backend [[4.0 0.0] [0.0 9.0]])  ; positive diagonal
          result (proto/matrix-sqrt backend A)
          result-converted (vec2-matrix->real-matrix (proto/backend->matrix backend result))
          expected [[2.0 0.0] [0.0 3.0]]]  ; sqrt([[4 0] [0 9]]) = [[2 0] [0 3]]
      (is (util/approx-matrix= expected result-converted 1e-10)
          "Matrix square root of diagonal matrix should be correct"))))

;;;
;;; MatrixAnalysis tests
;;;
(deftest test-matrix-analysis
  (testing "hermitian predicate"
    (let [hermitian-matrix [[1.0 0.0] [0.0 1.0]]  ; identity is hermitian
          A (proto/matrix->backend backend hermitian-matrix)]
      (is (proto/hermitian? backend A)
          "Identity matrix should be detected as hermitian")))
  
  (testing "unitary predicate"
    (let [unitary-matrix [[1.0 0.0] [0.0 1.0]]  ; identity is unitary
          A (proto/matrix->backend backend unitary-matrix)]
      (is (proto/unitary? backend A)
          "Identity matrix should be detected as unitary")))
  
  (testing "spectral norm"
    (let [A (proto/matrix->backend backend simple-matrix)
          result (proto/spectral-norm backend A)]
      (is (and (number? result) (> result 0))
          "Spectral norm should return a positive number")
      (is (util/approx= 5.464985704219042 result 1e-10)
          "Spectral norm of [[1 2] [3 4]] should be approximately 5.465"))))

;;;
;;; QuantumStateOps smoke tests
;;;
(deftest test-quantum-state-ops
  (testing "inner product"
    (let [v1 (proto/vector->backend backend [1.0 0.0])
          v2 (proto/vector->backend backend [0.0 1.0])
          result (proto/inner-product backend v1 v2)]
      (is (util/approx= 0.0 (:real result) 1e-10)
          "Inner product of orthogonal vectors should be zero")))
  
  (testing "norm"
    (let [v (proto/vector->backend backend [3.0 4.0])
          result (proto/norm2 backend v)]
      (is (util/approx= 5.0 result 1e-10)
          "Norm of [3,4] should be 5")))
  
  (testing "state normalize"
    (let [v (proto/vector->backend backend [3.0 4.0])
          result (proto/state-normalize backend v)
          result-converted (vec2-vector->real-vector (proto/backend->vector backend result))
          expected [0.6 0.8]]
      (is (util/approx-vector= expected result-converted 1e-10)
          "Normalized [3,4] should be [0.6, 0.8]"))))

;;;
;;; Rich comment for running tests
;;;
(comment
  (run-tests)
  ;
  )

