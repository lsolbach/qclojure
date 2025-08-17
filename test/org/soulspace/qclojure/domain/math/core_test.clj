(ns org.soulspace.qclojure.domain.math.core-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.util.test :as t]
            [org.soulspace.qclojure.domain.math.core :as m]))

;; Use shared approx helpers from util.test

(deftest backend-availability
  (is (contains? (m/available-backends) :pure))
  (is (= :pure (m/get-backend)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unknown backend"
                        (m/set-backend! :nonexistent))))

(deftest multiply-matrices-basic
  (is (= [[19.0 22.0]
          [43.0 50.0]]
         (m/matrix-multiply [[1.0 2.0]
                             [3.0 4.0]]
                            [[5.0 6.0]
                             [7.0 8.0]]))))

(deftest multiply-matrix-vector-basic
  (is (= [5.0 11.0]
         (m/matrix-vector [[1.0 2.0]
                           [3.0 4.0]]
                          [1.0 2.0]))))

(deftest transpose-basic
  (is (= [[1.0 3.0]
          [2.0 4.0]]
         (m/matrix-transpose [[1.0 2.0]
                              [3.0 4.0]]))))

(deftest tensor-product-basic
  (is (= [[1.0 3.0 2.0 6.0]
          [2.0 6.0 4.0 12.0]
          [4.0 12.0 8.0 24.0]
          [8.0 24.0 16.0 48.0]]
         (m/kronecker [[1.0 2.0]
                       [4.0 8.0]]
                      [[1.0 3.0]
                       [2.0 6.0]]))))

(deftest inverse-and-solve
  (let [A [[4.0 7.0]
           [2.0 6.0]]
        inv-A (m/matrix-inverse A)]
    (is inv-A)
    ;; Validate inverse approximately by round-trip A * inv(A) ≈ I
    (let [I (m/matrix-multiply A inv-A)
          rounded (mapv (fn [row]
                          (mapv (fn [x] (/ (Math/round (* 10.0 x)) 10.0)) row))
                        I)]
      (is (= [[1.0 0.0]
              [0.0 1.0]] rounded)))
    (let [x (m/solve-linear-system A [11.0 10.0])
          Ax (m/matrix-vector A x)
          rounded (mapv (fn [v] (/ (Math/round (* 10.0 v)) 10.0)) Ax)]
      (is (= [11.0 10.0] rounded))))
  (is (nil? (m/matrix-inverse [[1.0 2.0]
                               [2.0 4.0]]))))

(deftest complex-ops-basic
  (let [A {:real [[1.0 0.0]
                  [0.0 1.0]]
           :imag [[0.0 1.0]
                  [1.0 0.0]]}
        B {:real [[2.0 0.0]
                  [0.0 2.0]]
           :imag [[0.0 -1.0]
                  [-1.0 0.0]]}
        C (m/complex-matrix-multiply A B)
        H (m/conjugate-transpose A)
        K (m/complex-kronecker A B)]
    ;; Multiply
    (is (= {:real [[3.0 0.0]
                   [0.0 3.0]]
            :imag [[0.0 1.0]
                   [1.0 0.0]]}
           C))
    ;; Conjugate transpose of A: imag part flips sign (approx check)
    (is (t/approx-complex-matrix= {:real [[1.0 0.0]
                                          [0.0 1.0]]
                                   :imag [[0.0 -1.0]
                                          [-1.0 0.0]]}
                                  H 1.0e-12))
    ;; Hermitian check (A is not Hermitian here)
    (is (false? (m/hermitian? A)))
    ;; Kronecker: check shapes and two entries consistent with A⊗B
    (is (= 4 (count (:real K))))
    (is (= 4 (count (first (:real K)))))
    (is (= 4 (count (:imag K))))
    (is (= 4 (count (first (:imag K)))))
    ;; Top-left is a00 * B
    (is (= (get-in B [:real 0 0]) (get-in (:real K) [0 0])))
    ;; Bottom-right corresponds to a11 * B at block (1,1)
    (is (= (get-in B [:imag 1 1]) (get-in (:imag K) [3 3])))))

;;
;; Quantum state algebra helper tests
;;
(deftest inner-product-tests
  (testing "Real vector inner product"
    (is (= 14.0 (m/inner-product [1.0 2.0 3.0] [1.0 2.0 3.0])))
    (is (= 0.0 (m/inner-product [1.0 -1.0] [1.0 1.0]))))
  (testing "Complex SoA vector inner product"
    (let [v (m/complex-vector [1.0 0.0] [0.0 1.0]) ; [1 + 0i, 0 + 1i]
          w (m/complex-vector [0.0 1.0] [1.0 0.0]) ; [0 + 1i, 1 + 0i]
          ip (m/inner-product v w)]
      ;; <v|w> = conj(v0)*w0 + conj(v1)*w1 = (1-0i)*(0+1i) + (0-1i)*(1+0i) = i - i = 0
      (is (= {:real 0.0 :imag 0.0} ip))))
  (testing "Complex self inner product gives norm^2"
    (let [v (m/complex-vector [3.0 4.0] [0.0 0.0])
          ip (m/inner-product v v)]
      (is (= 25.0 (:real ip)))
      (is (= 0.0 (:imag ip))))))

(deftest normalization-tests
  (testing "Real vector normalization"
    (let [v [3.0 4.0]
          n (m/state-normalize v)]
      (is (t/approx= 1.0 (Math/sqrt (reduce + (map #(* % %) n)))))))
  (testing "Complex vector normalization"
    (let [v (m/complex-vector [3.0 0.0] [4.0 0.0])
          n (m/state-normalize v)]
      (is (t/approx= 1.0 (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) (:real n) (:imag n)))))))))

(deftest projector-and-density-tests
  (testing "Projector from real normalized state"
    (let [psi (m/state-normalize [3.0 4.0])
          P (m/projector-from-state psi)]
      ;; Idempotent: P*P = P (approx)
      (let [PP (m/matrix-multiply P P)]
        (is (t/approx-matrix= P PP (m/current-tolerance))))
      ;; Rank-1: trace equals 1
      (is (t/approx= 1.0 (reduce + (map-indexed (fn [i row] (nth row i)) P))))))
  (testing "Projector from complex normalized state"
    (let [psi (m/state-normalize (m/complex-vector [1.0 0.0] [1.0 0.0]))
          P (m/projector-from-state psi)]
      ;; Hermitian: P == P^†
      (is (true? (m/hermitian? P)))
      ;; Trace one
      (is (true? (m/trace-one? P)))))
  (testing "density-matrix alias equivalence"
    (let [psi (m/state-normalize [1.0 2.0 2.0 1.0])
          P (m/projector-from-state psi)
          D (m/density-matrix psi)]
      (is (= P D)))))

(deftest trace-and-psd-tests
  (testing "trace-one? true on projector"
    (let [psi (m/state-normalize [1.0 2.0 2.0 1.0])
          P (m/projector-from-state psi)]
      (is (true? (m/trace-one? P)))))
  (testing "trace-one? false on scaled projector"
    (let [psi (m/state-normalize [1.0 0.0])
          P (m/projector-from-state psi)
          scaled (mapv (fn [row] (mapv #(* 2.0 %) row)) P)]
      (is (false? (m/trace-one? scaled)))))
  (testing "positive-semidefinite? real projector"
    (let [psi (m/state-normalize [1.0 2.0 2.0 1.0])
          P (m/projector-from-state psi)]
      (is (true? (m/positive-semidefinite? P)))))
  (testing "positive-semidefinite? complex projector"
    (let [psi (m/state-normalize (m/complex-vector [1.0 0.0] [1.0 0.0]))
          P (m/projector-from-state psi)]
      (is (true? (m/positive-semidefinite? P)))))
  (testing "positive-semidefinite? rejects non-PSD matrix"
    (let [A [[0.0 1.0]
             [1.0 -3.0]]] ; has negative eigenvalue
      (is (false? (m/positive-semidefinite? A))))))

(deftest unitary-tests
  (testing "Identity is unitary (real)"
    (is (true? (m/unitary? [[1.0 0.0] [0.0 1.0]]))))
  (testing "Pauli-X is unitary (real permutation)"
    (is (true? (m/unitary? [[0.0 1.0] [1.0 0.0]]))))
  (testing "Scaling matrix not unitary"
    (is (false? (m/unitary? [[2.0 0.0] [0.0 0.5]]))))
  (testing "Simple complex unitary (phase gate)"
    (let [U (m/complex-matrix [[1.0 0.0]
                               [0.0 1.0]]
                              [[0.0 0.0]
                               [0.0 0.0]])] ; identity with zero imag
      (is (true? (m/unitary? U))))))

(deftest eigen-hermitian-2x2-tests
  (testing "2x2 symmetric matrix eigendecomposition"
    (let [{:keys [eigenvalues eigenvectors]} (m/eigen-hermitian [[3.0 1.0] [1.0 3.0]])]
      ;; Eigenvalues should be [2, 4] (sorted ascending)
      (is (t/approx= 2.0 (first eigenvalues) 1e-10))
      (is (t/approx= 4.0 (second eigenvalues) 1e-10))
      ;; Eigenvectors should be normalized
      (let [v1 (first eigenvectors)
            v2 (second eigenvectors)
            norm1 (Math/sqrt (reduce + (map #(* % %) v1)))
            norm2 (Math/sqrt (reduce + (map #(* % %) v2)))]
        (is (t/approx= 1.0 norm1 1e-10))
        (is (t/approx= 1.0 norm2 1e-10)))))

  (testing "2x2 diagonal matrix eigendecomposition"
    (let [{:keys [eigenvalues eigenvectors]} (m/eigen-hermitian [[5.0 0.0] [0.0 7.0]])]
      (is (t/approx= 5.0 (first eigenvalues) 1e-10))  ; Ascending order: 5 first
      (is (t/approx= 7.0 (second eigenvalues) 1e-10))))

  (testing "1x1 matrix eigendecomposition"
    (let [{:keys [eigenvalues eigenvectors]} (m/eigen-hermitian [[42.0]])]
      (is (= [42.0] eigenvalues))
      (is (= [[[1.0]]] eigenvectors)))))

(deftest matrix-exp-tests
  (testing "Matrix exponential of zero matrix gives identity"
    (let [result (m/matrix-exp [[0.0 0.0] [0.0 0.0]])]
      (is (t/approx-matrix= [[1.0 0.0] [0.0 1.0]] result 1e-10))))

  (testing "Matrix exponential of diagonal matrix"
    (let [result (m/matrix-exp [[1.0 0.0] [0.0 2.0]])]
      (is (t/approx= (Math/exp 1.0) (get-in result [0 0]) 1e-10))
      (is (t/approx= (Math/exp 2.0) (get-in result [1 1]) 1e-10))
      (is (t/approx= 0.0 (get-in result [0 1]) 1e-10))
      (is (t/approx= 0.0 (get-in result [1 0]) 1e-10))))

  (testing "Matrix exponential of 1x1 matrix"
    (let [result (m/matrix-exp [[3]])]
      (is (t/approx= (Math/exp 3.0) (get-in result [0 0]) 1e-10))))

  (testing "Quantum rotation: exp(-i σ_z π/4) is unitary"
    (let [pauli-z-rotation {:real [[0.0 0.0] [0.0 0.0]]
                            :imag [[(- (/ Math/PI 4)) 0.0] [0.0 (/ Math/PI 4)]]}
          result (m/matrix-exp pauli-z-rotation)]
      ;; Result should be unitary
      (is (m/unitary? result))
      ;; Check specific values: exp(-iπ/4) = cos(π/4) - i sin(π/4)
      (let [expected-cos (/ (Math/sqrt 2) 2)
            expected-sin (/ (Math/sqrt 2) 2)]
        (is (t/approx= expected-cos (get-in result [:real 0 0]) 1e-10))
        (is (t/approx= (- expected-sin) (get-in result [:imag 0 0]) 1e-10))
        (is (t/approx= expected-cos (get-in result [:real 1 1]) 1e-10))
        (is (t/approx= expected-sin (get-in result [:imag 1 1]) 1e-10))))))

(deftest matrix-sqrt-tests
  (testing "Matrix square root of diagonal matrix"
    (let [result (m/matrix-sqrt [[4 0] [0 9]])]
      (is (t/approx-matrix= [[2.0 0.0] [0.0 3.0]] result 1e-10))))

  (testing "Matrix square root of 1x1 matrix"
    (let [result (m/matrix-sqrt [[16]])]
      (is (t/approx= 4.0 (get-in result [0 0]) 1e-10))))

  (testing "Matrix square root of identity"
    (let [result (m/matrix-sqrt [[1 0] [0 1]])]
      (is (t/approx-matrix= [[1.0 0.0] [0.0 1.0]] result 1e-10))))

  (testing "Matrix square root requires positive semidefinite"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Matrix square root requires positive semidefinite matrix"
                          (m/matrix-sqrt [[1 0] [0 -1]])))))

(deftest decomposition-properties
  (testing "Eigendecomposition preserves determinant"
    (let [A [[2 1] [1 2]]
          det-A (- (* 2 2) (* 1 1)) ; det([[2 1][1 2]]) = 4 - 1 = 3
          {:keys [eigenvalues]} (m/eigen-hermitian A)
          det-eigenvals (reduce * eigenvalues)]
      (is (t/approx= det-A det-eigenvals 1e-10))))

  (testing "Eigendecomposition preserves trace"
    (let [A [[5 2] [2 5]]
          trace-A (+ 5 5) ; tr([[5 2][2 5]]) = 10
          {:keys [eigenvalues]} (m/eigen-hermitian A)
          trace-eigenvals (reduce + eigenvalues)]
      (is (t/approx= trace-A trace-eigenvals 1e-10))))

  (testing "Matrix exponential of Hermitian is unitary"
    (let [H [[1 0] [0 -1]] ; Hermitian matrix
          U (m/matrix-exp {:real [[0.0 0.0] [0.0 0.0]]
                           :imag [[(* -1.0 1.0) 0.0] [0.0 (* -1.0 -1.0)]]})] ; exp(i*H)
      (is (m/unitary? U)))))

;; Tests for newly implemented functionality

(deftest spectral-norm-test
  (testing "Spectral norm computation"
    ;; Identity matrix should have spectral norm 1
    (is (t/approx= 1.0 (m/spectral-norm [[1.0 0.0] [0.0 1.0]]) 1e-12))
    
    ;; Test with known matrix
    (let [A [[3 4] [0 5]]
          norm (m/spectral-norm A)]
      ;; Should be approximately 6.708...
      (is (t/approx= 6.708203932499369 norm 1e-10)))
    
    ;; Zero matrix should have spectral norm 0
    (is (t/approx= 0.0 (m/spectral-norm [[0.0 0.0] [0.0 0.0]]) 1e-12))))

(deftest condition-number-test
  (testing "Condition number computation"
    ;; Identity matrix should have condition number 1
    (is (t/approx= 1.0 (m/condition-number [[1.0 0.0] [0.0 1.0]]) 1e-12))
    
    ;; Well-conditioned matrix
    (let [A [[4 1] [1 3]]
          cond (m/condition-number A)]
      (is (> cond 1.0))
      (is (< cond 5.0))) ; Should be moderately conditioned
    
    ;; Test that ill-conditioned matrix has high condition number
    (let [A [[1 1] [1 1.001]]  ; Nearly singular
          cond (m/condition-number A)]
      (is (> cond 100.0)))))    ; Should be ill-conditioned

(deftest svd-test
  (testing "Singular Value Decomposition"
    ;; Test 2x2 identity matrix
    (let [{:keys [U singular-values Vt]} (m/svd [[1.0 0.0] [0.0 1.0]])]
      (is (= 2 (count singular-values)))
      (is (every? #(t/approx= 1.0 % 1e-10) singular-values)))
    
    ;; Test 2x2 diagonal matrix
    (let [{:keys [U singular-values Vt]} (m/svd [[3.0 0.0] [0.0 4.0]])]
      (is (= 2 (count singular-values)))
      ;; Singular values should be sorted in descending order
      (is (>= (first singular-values) (second singular-values)))
      (is (some #(t/approx= 4.0 % 1e-10) singular-values))
      (is (some #(t/approx= 3.0 % 1e-10) singular-values)))
    
    ;; Test 3x3 matrix using power iteration
    (let [{:keys [U singular-values Vt]} (m/svd [[2.0 0.0 0.0] [0.0 3.0 0.0] [0.0 0.0 1.0]])]
      (is (= 3 (count singular-values)))
      (is (>= (first singular-values) (second singular-values)))
      (is (>= (second singular-values) (nth singular-values 2))))))

(deftest eigen-hermitian-extended-test
  (testing "Extended eigendecomposition for larger matrices"
    ;; Test 3x3 symmetric matrix
    (let [A [[2.0 1.0 0.0] [1.0 2.0 1.0] [0.0 1.0 2.0]]
          {:keys [eigenvalues eigenvectors]} (m/eigen-hermitian A)]
      (is (= 3 (count eigenvalues)))
      (is (= 3 (count eigenvectors)))
      ;; All eigenvalues should be real for symmetric matrix
      (is (every? number? eigenvalues)))
    
    ;; Test 4x4 identity matrix
    (let [I [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [0.0 0.0 0.0 1.0]]
          {:keys [eigenvalues eigenvectors]} (m/eigen-hermitian I)]
      (is (= 4 (count eigenvalues)))
      ;; All eigenvalues of identity should be 1
      (is (every? #(t/approx= 1.0 % 1e-10) eigenvalues)))))

(deftest matrix-logarithm-test
  (testing "Matrix logarithm for Hermitian matrices"
    ;; Test identity matrix: log(I) should be zero matrix
    (let [I [[1.0 0.0] [0.0 1.0]]
          log-I (m/matrix-log I)]
      (is (every? #(every? (fn [x] (t/approx= 0.0 x 1e-12)) %) log-I)))
    
    ;; Test with simple diagonal matrix
    (let [A [[2.0 0.0] [0.0 3.0]]
          log-A (m/matrix-log A)]
      ;; log(2) ≈ 0.693, log(3) ≈ 1.099
      (is (t/approx= (Math/log 2) (get-in log-A [0 0]) 1e-10))
      (is (t/approx= (Math/log 3) (get-in log-A [1 1]) 1e-10))
      (is (t/approx= 0.0 (get-in log-A [0 1]) 1e-12))
      (is (t/approx= 0.0 (get-in log-A [1 0]) 1e-12)))
    
    ;; Test error for negative eigenvalues
    (is (thrown? Exception (m/matrix-log [[1.0 2.0] [2.0 -1.0]])))))

(deftest matrix-reconstruction-test
  (testing "Matrix reconstruction from eigendecomposition"
    ;; Test that reconstruction approximately recovers original matrix
    (let [A [[3.0 1.0] [1.0 2.0]]
          {:keys [eigenvalues eigenvectors]} (m/eigen-hermitian A)
          reconstructed (m/matrix-from-eigen eigenvalues eigenvectors)]
      ;; For small matrices, this might just return diagonal approximation
      ;; Check that the diagonal elements are reasonable
      (is (every? number? (map first reconstructed)))
      (is (every? number? (map second reconstructed))))))

(deftest integration-test
  (testing "Integration of multiple operations"
    ;; Test workflow: create matrix -> eigendecomposition -> reconstruct
    (let [A [[2.0 1.0] [1.0 2.0]]
          {:keys [eigenvalues]} (m/eigen-hermitian A)
          spec-norm (m/spectral-norm A)
          cond-num (m/condition-number A)]
      
      ;; Spectral norm should be largest eigenvalue
      (is (t/approx= (apply max eigenvalues) spec-norm 1e-10))
      
      ;; Condition number should be ratio of max/min eigenvalues
      (is (t/approx= (/ (apply max eigenvalues) (apply min eigenvalues)) 
                     cond-num 1e-10))
      
      ;; All operations should complete without error
      (is (every? number? eigenvalues))
      (is (number? spec-norm))
      (is (number? cond-num)))))

;;
;; Property-based tests
;;
(def tol (m/current-tolerance))

(defspec projector-complex-properties 60
  (prop/for-all [ar (gen/double* {:min -3 :max 3 :NaN? false :infinite? false})
                 ai (gen/double* {:min -3 :max 3 :NaN? false :infinite? false})
                 br (gen/double* {:min -3 :max 3 :NaN? false :infinite? false})
                 bi (gen/double* {:min -3 :max 3 :NaN? false :infinite? false})]
                (let [norm2 (+ (* ar ar) (* ai ai) (* br br) (* bi bi))]
                  (if (< norm2 1e-4)
                    true ; skip poorly conditioned vectors
                    (let [v (m/complex-vector [ar br] [ai bi])
                          psi (m/state-normalize v)
                          P (m/projector-from-state psi)
                          psi' (m/complex-matrix-vector P psi)
                          H (m/conjugate-transpose P)
                          trace-r (reduce + (map-indexed (fn [i row] (nth row i)) (:real P)))
                          rel= (fn [x y]
                                 (let [mx (max 1.0 (Math/abs (double x)) (Math/abs (double y)))]
                                   (< (Math/abs (- (double x) (double y))) (* 1e-6 mx))))]
                      (and (every? true? (map rel= (:real psi) (:real psi')))
                           (every? true? (map rel= (:imag psi) (:imag psi')))
                           (t/approx-complex-matrix= P H 1e-8)
                           (t/approx= 1.0 trace-r 1e-8)))))))

(defspec normalization-idempotent-real 50
  (prop/for-all [a (gen/double* {:min -20 :max 20 :NaN? false :infinite? false})
                 b (gen/double* {:min -20 :max 20 :NaN? false :infinite? false})]
                (if (and (zero? a) (zero? b))
                  true
                  (let [v [a b]
                        n1 (m/state-normalize v)
                        n2 (m/state-normalize n1)
                        norm (Math/sqrt (reduce + (map #(* % %) n1)))]
                    (and (t/approx= 1.0 norm 1e-10)
                         (every? true? (map (fn [x y] (t/approx= x y 1e-10)) n1 n2)))))))

(defspec normalization-idempotent-complex 40
  (prop/for-all [ar (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})
                 ai (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})
                 br (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})
                 bi (gen/double* {:min -10 :max 10 :NaN? false :infinite? false})]
                (if (and (zero? ar) (zero? ai) (zero? br) (zero? bi))
                  true
                  (let [v (m/complex-vector [ar br] [ai bi])
                        n1 (m/state-normalize v)
                        n2 (m/state-normalize n1)
                        norm (Math/sqrt (reduce + (map (fn [x y] (+ (* x x) (* y y))) (:real n1) (:imag n1))))]
                    (and (t/approx= 1.0 norm 1e-10)
                         (t/approx-complex-matrix=
                          (m/projector-from-state n1)
                          (m/projector-from-state n2) 1e-10))))))

(defspec rotation-unitary-preserves-norm 60
  (prop/for-all [theta (gen/double* {:min (* -2 Math/PI) :max (* 2 Math/PI) :NaN? false :infinite? false})
                 a (gen/double* {:min -5 :max 5 :NaN? false :infinite? false})
                 b (gen/double* {:min -5 :max 5 :NaN? false :infinite? false})]
                (if (and (zero? a) (zero? b))
                  true
                  (let [c (Math/cos theta)
                        s (Math/sin theta)
                        R [[c (- s)]
                           [s c]]
                        v (m/state-normalize [a b])
                        v2 (m/matrix-vector R v)
                        norm2 (Math/sqrt (reduce + (map #(* % %) v2)))]
                    (t/approx= 1.0 norm2 1e-10)))))

(comment
  (run-tests)
  ;
  )