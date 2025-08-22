(ns benchmark
  "Benchmark for QClojure math operations."
  (:require [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.math.core :as mcore]
            [org.soulspace.qclojure.domain.math.clojure.clojure-math :as cmath]
            [org.soulspace.qclojure.domain.math.fastmath.fastmath :as fmath]
            [criterium.core :as crit]
            [fastmath.vector :as v]))

;; Helper functions for generating test data
(defn- random-complex-matrix
  "Generate a random complex matrix using Vec2 representation."
  [rows cols]
  (vec (repeatedly rows 
                   #(vec (repeatedly cols 
                                     (fn [] (fc/complex (- (rand 2.0) 1.0) 
                                                        (- (rand 2.0) 1.0))))))))

(defn- make-hermitian-complex
  "Make a complex matrix Hermitian (A = A†)."
  [A]
  (let [n (count A)]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (if (= i j)
                    ;; Diagonal elements must be real for Hermitian matrices
                    (fc/complex (fc/re (get-in A [i j])) 0.0)
                    (if (< i j)
                      (get-in A [i j])
                      ;; A[i,j] = conj(A[j,i]) for i > j
                      (fc/conjugate (get-in A [j i]))))))))))

(defn- random-complex-vector
  "Generate a random complex vector using Vec2 representation."
  [n]
  (vec (repeatedly n #(fc/complex (- (rand 2.0) 1.0) 
                                  (- (rand 2.0) 1.0)))))

;; Smaller test data for quick benchmarks (10x10)
(def cA10 
  "10x10 complex Hermitian matrix for quick benchmarking."
  (make-hermitian-complex (random-complex-matrix 10 10)))

(def cB10 
  "10x10 complex Hermitian matrix for quick benchmarking."
  (make-hermitian-complex (random-complex-matrix 10 10)))

(def cv10 
  "10-element complex vector for quick benchmarking."
  (random-complex-vector 10))

;; Test data over fastmath.complex numbers (Vec2) of dimension 100
(def cA100 
  "100x100 complex Hermitian matrix using Vec2 representation for benchmarking."
  (make-hermitian-complex (random-complex-matrix 100 100)))

(def cB100 
  "100x100 complex Hermitian matrix using Vec2 representation for benchmarking."
  (make-hermitian-complex (random-complex-matrix 100 100)))

(def cv100 
  "100-element complex vector using Vec2 representation for benchmarking."
  (random-complex-vector 100))

;; Benchmark functions
(defn benchmark-matrix-multiply
  "Benchmark matrix multiplication for different backends."
  [A B backend-name]
  (println (str "Benchmarking matrix multiplication with " backend-name " backend:"))
  (crit/with-progress-reporting
    (crit/bench (mcore/matrix-multiply A B))))

(defn benchmark-eigen-hermitian
  "Benchmark eigendecomposition for different backends."
  [A backend-name]
  (println (str "Benchmarking eigendecomposition with " backend-name " backend:"))
  (crit/with-progress-reporting
    (crit/bench (mcore/eigen-hermitian A))))

(defn benchmark-matrix-vector
  "Benchmark matrix-vector multiplication for different backends."
  [A v backend-name]
  (println (str "Benchmarking matrix-vector multiplication with " backend-name " backend:"))
  (crit/with-progress-reporting
    (crit/bench (mcore/matrix-vector A v))))

(defn run-all-benchmarks
  "Run all benchmarks with current backend settings."
  []
  (let [backend-name (mcore/get-backend)]
    (println (str "Running benchmarks with backend: " backend-name))
    
    ;; Complex matrix operations (10x10 for quick tests)
    (println "Complex matrix operations (10x10):")
    (benchmark-matrix-multiply cA10 cB10 backend-name)
    (benchmark-matrix-vector cA10 cv10 backend-name)
    (benchmark-eigen-hermitian cA10 backend-name)
    
    ;; Complex matrix operations (100x100 for tests)
    (println "Complex matrix operations (100x100):")
    (benchmark-matrix-multiply cA100 cB100 backend-name)
    (benchmark-matrix-vector cA100 cv100 backend-name)
    (benchmark-eigen-hermitian cA100 backend-name)
    
))

(comment
  ;; Example usage:

  ;; Switch to pure backend
  (mcore/set-backend! :pure)
  (run-all-benchmarks)

  ;; Switch to fastmath backend  
  (mcore/set-backend! :fastmath)
  (run-all-benchmarks)

  ;; Manual benchmarks with larger matrices
  (benchmark-matrix-multiply cA100 cB100 "pure")
  (benchmark-matrix-multiply cA100 cB100 "fastmath")
  (benchmark-eigen-hermitian cA100 "pure")
  (benchmark-eigen-hermitian cA100 "fastmath")

  ;; Quick verification that matrices are properly Hermitian
  (mcore/hermitian? cA10)   ; should be true
  )
