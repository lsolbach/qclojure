(ns org.soulspace.qclojure.domain.math.clojure.clojure-math-test
  "Tests for the pure Clojure math backend implementation.

  Focus areas:
  * Basic real & complex arithmetic (add, sub, scale, matmul)
  * Vector/matrix products and outer-product
  * Linear solve & inverse small systems (real)
  * Decomposition smoke tests (LU, QR, Cholesky, SVD 2x2)
  * Predicates (hermitian?, unitary?, positive-semidefinite?)
  * Matrix functions (exp small norm, sqrt small SPD) – smoke only

  NOTE: Advanced numerical accuracy is not asserted here; these tests
  serve as regression guards and shape/contract validation."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            ;[org.soulspace.qclojure.util.test :as util]
            [org.soulspace.qclojure.domain.math.protocols :as proto]
            [org.soulspace.qclojure.domain.math.clojure.clojure-math :as backend]))

(def b (backend/make-backend))

(defn approx= [a b tol]
  (cond (and (number? a) (number? b)) (< (Math/abs (- (double a) (double b))) tol)
        (and (map? a) (contains? a :real) (map? b) (contains? b :real))
        (and (approx= (:real a) (:real b) tol) (approx= (:imag a) (:imag b) tol))
        (and (sequential? a) (sequential? b)) (every? true? (map #(approx= %1 %2 tol) a b))
        :else (= a b)))

(def tol 1.0e-9)

(deftest test-basic-real-arithmetic
  (let [A [[1.0 2.0] [3.0 4.0]]
        B [[5.0 6.0] [7.0 8.0]]]
    (testing "add/subtract"
      (is (= [[6.0 8.0] [10.0 12.0]] (proto/add b A B)))
      (is (= [[-4.0 -4.0] [-4.0 -4.0]] (proto/subtract b A B))))
    (testing "scale/negate"
      (is (= [[2.0 4.0] [6.0 8.0]] (proto/scale b A 2.0)))
      (is (= (proto/scale b A -1.0) (proto/negate b A))))
    (testing "matrix-multiply"
      (is (= [[19.0 22.0] [43.0 50.0]] (proto/matrix-multiply b A B))))))

(deftest test-basic-complex-arithmetic
  (let [A {:real [[1.0 0.0] [0.0 1.0]] :imag [[0.0 1.0] [0.0 0.0]]}
        B {:real [[0.0 2.0] [3.0 0.0]] :imag [[1.0 0.0] [0.0 4.0]]}
        C (proto/add b A B)]
    (testing "complex add"
      (is (= {:real [[1.0 2.0] [3.0 1.0]] :imag [[1.0 1.0] [0.0 4.0]]} C)))
    (testing "complex hermitian? false"
      (is (false? (proto/hermitian? b C))))))

(deftest test-trace-and-transpose
  (let [A [[1 2 3] [4 5 6] [7 8 9]]]
    (is (= 15 (proto/trace b A)))
    (is (= [[1 4 7] [2 5 8] [3 6 9]] (proto/transpose b A)))))

(deftest test-linear-solve-and-inverse
  (let [A [[4.0 7.0] [2.0 6.0]]
        bvec [1.0 0.0]
        x (proto/solve-linear-system b A bvec)
        inv (proto/inverse b A)]
    (testing "solve Ax=b"
      (is (approx= [0.6 -0.2] x tol)))
    (testing "inverse * A ~ I"
      (let [I (proto/matrix-multiply b inv A)]
        (is (approx= [[1.0 0.0] [0.0 1.0]] I 1e-8))))))

(deftest test-complex-linear-solve-and-inverse
  (let [A {:real [[2.0 0.0] [0.0 3.0]] :imag [[0.0 1.0] [0.0 0.0]]}
        bvec {:real [1.0 0.0] :imag [0.0 0.0]}
        x (proto/solve-linear-system b A bvec)
        inv (proto/inverse b A)
        AX (proto/matrix-vector-product b A x)
        IA (proto/matrix-multiply b inv A)]
    (testing "complex solve Ax=b"
      (is (approx= bvec AX 1e-8)))
    (testing "complex inverse * A ~ I"
      (is (approx= {:real [[1.0 0.0] [0.0 1.0]] :imag [[0.0 0.0] [0.0 0.0]]} IA 1e-8)))))

(deftest test-lu-decomposition
  (let [A [[3.0 1.0] [4.0 2.0]]
        {:keys [P L U]} (proto/lu-decomposition b A)]
    (is (= 2 (count P)))
    (is (= [2 2] (proto/shape b L)))
    (is (= [2 2] (proto/shape b U)))))

(deftest test-qr-decomposition
  (let [A [[1.0 1.0] [1.0 -1.0]]
        {:keys [Q R]} (proto/qr-decomposition b A)]
    (is (= [2 2] (proto/shape b Q)))
    (is (= [2 2] (proto/shape b R)))))

(deftest test-cholesky
  (let [A [[4.0 2.0] [2.0 3.0]]
        {:keys [L]} (proto/cholesky-decomposition b A)]
    (is (= [2 2] (proto/shape b L)))
    (is (every? number? (apply concat L)))))

(deftest test-svd-small
  (let [A [[1.0 0.0] [0.0 2.0]]
        {:keys [S]} (proto/svd b A)]
    (is (= 2 (count S)))
    (is (> (first S) (second S)))))

(deftest test-predicates
  (let [H [[2.0 1.0] [1.0 2.0]]
        U [[0.0 1.0] [-1.0 0.0]]
        P (proto/positive-semidefinite? b H)]
    (is (true? (proto/hermitian? b H)))
    (is (true? (proto/unitary? b U)))
    (is (boolean P))))

(deftest test-matrix-functions
  (let [A [[0.0 0.0] [0.0 0.0]]
        E (proto/matrix-exp b A)
        S (proto/matrix-sqrt b [[1.0 0.0] [0.0 1.0]])]
    (is (approx= [[1.0 0.0] [0.0 1.0]] E tol))
    (is (approx= [[1.0 0.0] [0.0 1.0]] S tol))))

;; Additional tests for extended functionality

(deftest test-eigen-hermitian
  (let [A [[2.0 1.0]
           [1.0 2.0]]
        {:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian b A)]
    (is (= 2 (count eigenvalues)))
    ;; Expected eigenvalues 1 and 3 (ascending)
    (is (approx= [1.0 3.0] eigenvalues 1e-6))
    ;; Eigenvectors normalized
    (doseq [v eigenvectors]
      (is (approx= 1.0 (Math/sqrt (reduce + (map #(* % %) v))) 1e-6)))))

(deftest test-eigen-general-upper-triangular
  (let [A [[2.0 1.0]
           [0.0 3.0]]
        {:keys [eigenvalues]} (proto/eigen-general b A)
  ;; Scale & round to avoid tiny numerical drift; eigenvalues of upper triangular are its diagonal.
  evs (set (map #(Math/round (* 1e6 %)) eigenvalues))
  expected (set [2000000 3000000])]
    ;; Cardinality check
    (is (= 2 (count eigenvalues)))
    ;; Set equality check
    (is (= expected evs))
    ;; Individual approximate checks (unscaled) for clarity
    (doseq [λ eigenvalues]
      (is (or (< (Math/abs (- λ 2.0)) 1e-6)
        (< (Math/abs (- λ 3.0)) 1e-6))))))

;; Complex general eigenvalue tests
;;
;; We verify the complex implementation on:
;; 1. Upper triangular (Schur form) matrix: eigenvalues = diagonal entries.
;; 2. Similarity transform A = V D V^{-1} where D diagonal with distinct
;;    complex entries; eigenvalues should match D's diagonal (unordered).

(deftest test-eigen-general-complex-upper-triangular
  (let [A {:real [[2.0 0.0 0.0]
          [0.0 3.0 1.0]
          [0.0 0.0 4.0]]
       :imag [[0.5 0.2 0.0]
          [0.0 0.1 0.3]
          [0.0 0.0 -0.2]]}
    {:keys [eigenvalues]} (proto/eigen-general b A)
    diag (map (fn [i] {:real (get-in (:real A) [i i]) :imag (get-in (:imag A) [i i])}) (range 3))
    ev-set (set (map (fn [{:keys [real imag]}] [(Math/round (* 1e6 real)) (Math/round (* 1e6 imag))]) eigenvalues))
    diag-set (set (map (fn [{:keys [real imag]}] [(Math/round (* 1e6 real)) (Math/round (* 1e6 imag))]) diag))]
  (is (= 3 (count eigenvalues)))
  (is (= diag-set ev-set))))

(deftest test-eigen-general-complex-similarity
  (let [D {:real [[1.0 0.0 0.0]
          [0.0 2.0 0.0]
          [0.0 0.0 3.0]]
       :imag [[0.5 0.0 0.0]
          [0.0 -0.3 0.0]
          [0.0 0.0 0.7]]}
    ;; Simple invertible V (non-unitary) real for ease
    V [[1.0 0.5 0.2]
       [0.0 1.0 0.3]
       [0.0 0.0 1.0]]
    Vinv (proto/inverse b V)
   ;; Promote real V and Vinv to complex (zero imaginary) to leverage complex matmul paths
   zeros [[0.0 0.0 0.0]
     [0.0 0.0 0.0]
     [0.0 0.0 0.0]]
   Vc {:real V :imag zeros}
   Vinvc {:real Vinv :imag zeros}
    ;; A = V D V^{-1} (similarity) for complex D
   A (let [VD (proto/matrix-multiply b Vc D)]
       (proto/matrix-multiply b VD Vinvc))
    {:keys [eigenvalues]} (proto/eigen-general b A)
    expected (map (fn [i] {:real (get-in (:real D) [i i]) :imag (get-in (:imag D) [i i])}) (range 3))
    scale-round (fn [{:keys [real imag]}] [(Math/round (* 1e6 real)) (Math/round (* 1e6 imag))])
    ev-set (set (map scale-round eigenvalues))
    exp-set (set (map scale-round expected))]
  (is (= 3 (count eigenvalues)))
  (is (= exp-set ev-set))))

(deftest test-matrix-exp-nilpotent
  (let [N [[0.0 1.0]
           [0.0 0.0]]
        E (proto/matrix-exp b N)]
    ;; For N^2=0, exp(N) = I + N
    (is (approx= [[1.0 1.0]
                  [0.0 1.0]] E 1e-8))))

(deftest test-matrix-log-identity
  (let [I [[1.0 0.0]
           [0.0 1.0]]
        L (proto/matrix-log b I)]
    (is (approx= [[0.0 0.0]
                  [0.0 0.0]] L 1e-9))))

;; Complex matrix log tests
;;
;; We construct small Hermitian positive definite complex matrices so that
;; eigenvalues are positive and log(A) is well-defined. We validate by
;; exponentiating the result and comparing back to the original matrix.
(deftest test-complex-matrix-log-hermitian-pd
  (let [A {:real [[2.0 0.5]
                  [0.5 1.5]]
           :imag [[0.0 0.2]
                  [-0.2 0.0]]}
        L (proto/matrix-log b A)
        ;; Reconstruct via exp(log(A)) should approximate A
        A* (proto/matrix-exp b L)]
    (is (approx= A A* 1e-6))
    ;; Log should be Hermitian (imag diagonal ~0)
    (let [Lr (:real L) Li (:imag L) n (count Lr)]
      (is (every? #(approx= 0.0 % 1e-8) (map #(get-in Li [% %]) (range n))))
      (doseq [i (range n) j (range n)]
        (is (approx= (get-in Lr [i j]) (get-in Lr [j i]) 1e-8))
        (is (approx= (get-in Li [i j]) (- (get-in Li [j i])) 1e-8))))))

;; Complex matrix sqrt tests
;;
;; Use Hermitian positive definite complex matrix; verify (sqrt A)^2 ≈ A
;; and Hermitian structure of the square root.
(deftest test-complex-matrix-sqrt-hermitian-pd
  (let [A {:real [[3.0 0.4]
                  [0.4 2.5]]
           :imag [[0.0 -0.3]
                  [0.3  0.0]]}
        S (proto/matrix-sqrt b A)
        S2 (proto/matrix-multiply b S S)]
    (is (approx= A S2 1e-6))
    (let [Sr (:real S) Si (:imag S) n (count Sr)]
      (doseq [i (range n) j (range n)]
        (is (approx= (get-in Sr [i j]) (get-in Sr [j i]) 1e-6))
        (is (approx= (get-in Si [i j]) (- (get-in Si [j i])) 1e-6))))))

(deftest test-spectral-norm-diagonal
  (let [A [[3.0 0.0]
           [0.0 4.0]]
        nrm (proto/spectral-norm b A)]
    (is (approx= 4.0 nrm 1e-6))))

(deftest test-complex-matrix-exp-1x1
  (let [A {:real [[0.0]] :imag [[3.141592653589793]]} ; i*pi
        E (proto/matrix-exp b A)]
    (is (approx= {:real [[-1.0]] :imag [[0.0]]} E 1e-6))))

(deftest test-eigen-hermitian-complex
  (let [A {:real [[2.0 0.0]
                  [0.0 3.0]]
           :imag [[0.0 1.0]
                  [-1.0 0.0]]}
        {:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian b A)
        evs (vec (sort eigenvalues))
        expected (let [a 2.0 d 3.0 b 1.0
                       avg (/ (+ a d) 2.0)
                       delta (Math/sqrt (+ (Math/pow (/ (- a d) 2.0) 2.0) (* b b)))]
                   [(- avg delta) (+ avg delta)])]
    (is (= 2 (count eigenvalues)))
    (is (approx= expected evs 1e-6))
    (doseq [v eigenvectors]
      (is (map? v))
      (is (every? number? (:real v)))
      (is (every? number? (:imag v)))
      (let [norm (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) (:real v) (:imag v))))]
        (is (approx= 1.0 norm 1e-6))))))

(deftest test-eigen-hermitian-complex-phase
  (let [A {:real [[3.0  0.5  0.0]
                  [0.5  2.0 -1.0]
                  [0.0 -1.0  1.5]]
           :imag [[0.0  1.0  0.5]
                  [-1.0 0.0  0.0]
                  [-0.5 0.0  0.0]]}
        {:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian b A)
        n (count (:real A))
        tol-phase 1.0e-9]
    ;; Basic shape checks
    (is (= n (count eigenvalues)))
    (is (= n (count eigenvectors)))
    (doseq [v eigenvectors]
      (let [xr (:real v) xi (:imag v)
            ;; Find first significant component index
            idx (or (first (for [i (range n)
                                  :let [a (double (nth xr i)) b (double (nth xi i))
                                        mag2 (+ (* a a) (* b b))]
                                  :when (> mag2 (* tol-phase tol-phase))]
                              i)) -1)
            norm (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) xr xi)))]
        (is (approx= 1.0 norm 1e-6))
        (when (>= idx 0)
          (is (approx= 0.0 (nth xi idx) 1e-8))
          (is (>= (nth xr idx) -1e-12)))))))

;;;
;;; Rich comment for running tests
;;;
(comment
  (run-tests)
  ;
  )

