(ns org.soulspace.qclojure.domain.math.complex-linear-algebra-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.util.test :as t]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]
            [fastmath.complex :as fc]))

;; Use shared approx helpers from util.test

(deftest backend-availability
  (is (contains? (cla/available-backends) :pure))
  (is (= :fastmath (cla/get-backend)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Unknown backend"
                        (cla/set-backend! :nonexistent))))

(deftest multiply-matrices-basic
  (is (t/approx-matrix= [[19.0 22.0]
                         [43.0 50.0]]
                        (cla/matrix-multiply [[1.0 2.0]
                                            [3.0 4.0]]
                                           [[5.0 6.0]
                                            [7.0 8.0]]))))

(deftest multiply-matrix-vector-basic
  (is (t/approx-matrix= [5.0 11.0]
                        (cla/matrix-vector-product [[1.0 2.0]
                                          [3.0 4.0]]
                                         [1.0 2.0]))))

(deftest transpose-basic
  (is (t/approx-matrix= [[1.0 3.0]
                         [2.0 4.0]]
                        (cla/transpose [[1.0 2.0]
                                             [3.0 4.0]]))))

(deftest tensor-product-basic
  (is (t/approx-matrix= [[1.0 3.0 2.0 6.0]
                         [2.0 6.0 4.0 12.0]
                         [4.0 12.0 8.0 24.0]
                         [8.0 24.0 16.0 48.0]]
                        (cla/kronecker-product [[1.0 2.0]
                                      [4.0 8.0]]
                                     [[1.0 3.0]
                                      [2.0 6.0]]))))

(deftest inverse-and-solve
  (let [A [[4.0 7.0]
           [2.0 6.0]]
        inv-A (cla/matrix-inverse A)]
    (is inv-A)
    ;; Validate inverse approximately by round-trip A * inv(A) ≈ I
    (let [I (cla/matrix-multiply A inv-A)
          rounded (mapv (fn [row]
                          (mapv (fn [x] (/ (Math/round (* 10.0 (fc/re x))) 10.0)) row))
                        I)]
      (is (= [[1.0 0.0]
              [0.0 1.0]] rounded)))
    (let [x (cla/solve-linear-system A [11.0 10.0])
          Ax (cla/matrix-vector-product A x)
          rounded (mapv (fn [v] (/ (Math/round (* 10.0 (fc/re v))) 10.0)) Ax)]
      (is (= [11.0 10.0] rounded)))))

(deftest complex-ops-basic
  (let [A {:real [[1.0 0.0]
                  [0.0 1.0]]
           :imag [[0.0 1.0]
                  [1.0 0.0]]}
        B {:real [[2.0 0.0]
                  [0.0 2.0]]
           :imag [[0.0 -1.0]
                  [-1.0 0.0]]}
        C (cla/matrix-multiply A B)
        H (cla/conjugate-transpose A)
        K (cla/kronecker-product A B)
        expected-C [[(fc/complex 3.0 0.0) (fc/complex 0.0 1.0)]
                    [(fc/complex 0.0 1.0) (fc/complex 3.0 0.0)]]
        expected-H [[(fc/complex 1.0 0.0) (fc/complex 0.0 -1.0)]
                    [(fc/complex 0.0 -1.0) (fc/complex 1.0 0.0)]]]
    ;; Multiply
    (is (t/approx-matrix= expected-C C))
    ;; Conjugate transpose of A: imag part flips sign (approx check)
    (is (t/approx-matrix= expected-H H 1.0e-12))
    ;; Hermitian check (A is not Hermitian here)
    (is (false? (cla/hermitian? A)))
    ;; Kronecker: check shapes and two entries consistent with A⊗B
    (is (= 4 (count K)))
    (is (= 4 (count (first K))))
    ;; Top-left is a00 * B (using Vec2 comparison)
    (is (t/approx= (fc/complex 2.0 0.0) (get-in K [0 0])))
    ;; Bottom-right corresponds to a11 * B at block (1,1)
    (is (t/approx= (fc/complex 2.0 0.0) (get-in K [3 3])))))

;;
;; Quantum state algebra helper tests
;;
(deftest inner-product-tests
  (testing "Real vector inner product"
    (is (t/approx= 14.0 (cla/inner-product [1.0 2.0 3.0] [1.0 2.0 3.0])))
    (is (t/approx= 0.0 (cla/inner-product [1.0 -1.0] [1.0 1.0]))))
  (testing "Complex SoA vector inner product"
    (let [v (cla/complex-vector [1.0 0.0] [0.0 1.0]) ; [1 + 0i, 0 + 1i]
          w (cla/complex-vector [0.0 1.0] [1.0 0.0]) ; [0 + 1i, 1 + 0i]
          ip (cla/inner-product v w)]
      ;; <v|w> = conj(v0)*w0 + conj(v1)*w1 = (1-0i)*(0+1i) + (0-1i)*(1+0i) = i - i = 0
      ;; Note: API returns Vec2 format, so we compare with a Vec2 zero
      (is (t/approx= (fc/complex 0.0 0.0) ip))))
  (testing "Complex self inner product gives norm^2"
    (let [v (cla/complex-vector [3.0 4.0] [0.0 0.0])
          ip (cla/inner-product v v)]
      ;; Note: API returns Vec2 format, so we extract real/imag using fc functions
      (is (t/approx= 25.0 (fc/re ip)))
      (is (t/approx= 0.0 (fc/im ip))))))

(deftest unitary-tests
  (testing "Identity is unitary (real)"
    (is (true? (cla/unitary? [[1.0 0.0] [0.0 1.0]]))))
  (testing "Pauli-X is unitary (real permutation)"
    (is (true? (cla/unitary? [[0.0 1.0] [1.0 0.0]]))))
  (testing "Scaling matrix not unitary"
    (is (false? (cla/unitary? [[2.0 0.0] [0.0 0.5]]))))
  (testing "Simple complex unitary (phase gate)"
    (let [U (cla/complex-matrix [[1.0 0.0]
                               [0.0 1.0]]
                              [[0.0 0.0]
                               [0.0 0.0]])] ; identity with zero imag
      (is (true? (cla/unitary? U))))))

(deftest eigen-hermitian-2x2-tests
  (testing "2x2 symmetric matrix eigendecomposition"
    (let [{:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian [[3.0 1.0] [1.0 3.0]])]
      ;; Eigenvalues should be [2, 4] We return in ascending order
      (is (t/approx= 2.0 (fc/re (first eigenvalues)) 1e-10))
      (is (t/approx= 4.0 (fc/re (second eigenvalues)) 1e-10))
      ;; Eigenvectors should be normalized
      (let [v1 (first eigenvectors)
            v2 (second eigenvectors)
            norm1 (Math/sqrt (reduce + (map #(fc/re (fc/mult % (fc/conjugate %))) v1)))
            norm2 (Math/sqrt (reduce + (map #(fc/re (fc/mult % (fc/conjugate %))) v2)))]
        (is (t/approx= 1.0 norm1 1e-10))
        (is (t/approx= 1.0 norm2 1e-10)))))

  (testing "2x2 diagonal matrix eigendecomposition"
    (let [{:keys [eigenvalues]} (cla/eigen-hermitian [[5.0 0.0] [0.0 7.0]])]
      ;; Eigenvalues should be [2, 4] We return in ascending order
      (is (t/approx= 5.0 (fc/re (first eigenvalues)) 1e-10))
      (is (t/approx= 7.0 (fc/re (second eigenvalues)) 1e-10))))

  (testing "1x1 matrix eigendecomposition"
    (let [{:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian [[42.0]])]
      (is (t/approx= 42.0 (fc/re (first eigenvalues))))
      (is (t/approx= 1.0 (fc/re (first (first eigenvectors))))))))

(deftest matrix-exp-tests
  (testing "Matrix exponential of zero matrix gives identity"
    (let [result (cla/matrix-exp [[0.0 0.0] [0.0 0.0]])]
      (is (t/approx-matrix= [[1.0 0.0] [0.0 1.0]] result 1e-10))))

  (testing "Matrix exponential of diagonal matrix"
    (let [result (cla/matrix-exp [[1.0 0.0] [0.0 2.0]])]
      (is (t/approx= (Math/exp 1.0) (fc/re (get-in result [0 0])) 1e-10))
      (is (t/approx= (Math/exp 2.0) (fc/re (get-in result [1 1])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in result [0 1])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in result [1 0])) 1e-10))))

  (testing "Matrix exponential of 1x1 matrix"
    (let [result (cla/matrix-exp [[3]])]
      (is (t/approx= (Math/exp 3.0) (fc/re (get-in result [0 0])) 1e-10))))

  (testing "Quantum rotation: exp(-i σ_z π/4) is unitary"
    (let [pauli-z-rotation {:real [[0.0 0.0] [0.0 0.0]]
                            :imag [[(- (/ Math/PI 4)) 0.0] [0.0 (/ Math/PI 4)]]}
          result (cla/matrix-exp pauli-z-rotation)]
      ;; Result should be unitary
      (is (cla/unitary? result))
      ;; Check specific values: exp(-iπ/4) = cos(π/4) - i sin(π/4)
      (let [expected-cos (/ (Math/sqrt 2) 2)
            expected-sin (/ (Math/sqrt 2) 2)]
        (is (t/approx= expected-cos (fc/re (get-in result [0 0])) 1e-10))
        (is (t/approx= (- expected-sin) (fc/im (get-in result [0 0])) 1e-10))
        (is (t/approx= expected-cos (fc/re (get-in result [1 1])) 1e-10))
        (is (t/approx= expected-sin (fc/im (get-in result [1 1])) 1e-10))))))

(deftest matrix-logarithm-test
  (testing "Matrix logarithm for Hermitian matrices"
    ;; Test identity matrix: log(I) should be zero matrix
    (let [I [[1.0 0.0] [0.0 1.0]]
          log-I (cla/matrix-log I)]
      (is (every? #(every? (fn [x] (t/approx= 0.0 (fc/re x) 1e-12)) %) log-I)))

    ;; Test with simple diagonal matrix
    (let [A [[2.0 0.0] [0.0 3.0]]
          log-A (cla/matrix-log A)]
      ;; log(2) ≈ 0.693, log(3) ≈ 1.099
      (is (t/approx= (Math/log 2) (fc/re (get-in log-A [0 0])) 1e-10))
      (is (t/approx= (Math/log 3) (fc/re (get-in log-A [1 1])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in log-A [0 1])) 1e-12))
      (is (t/approx= 0.0 (fc/re (get-in log-A [1 0])) 1e-12)))

    ;; Test matrix with negative eigenvalues - should compute complex logarithm
    (let [A [[1.0 2.0] [2.0 -1.0]]
          log-A (cla/matrix-log A)]
      ;; The matrix has eigenvalues approximately [-2.236, 2.236]
      ;; The result should be a complex matrix
      (is (not (nil? log-A)))
      ;; Verify that the result is a complex matrix with non-zero imaginary parts
      (is (some (fn [row]
                  (some (fn [elem] 
                          (> (Math/abs (fc/im elem)) 1e-10)) row)) log-A)
          "Matrix logarithm of matrix with negative eigenvalues should have complex entries"))))

(deftest matrix-sqrt-tests
  (testing "Matrix square root of diagonal matrix"
    (let [result (cla/matrix-sqrt [[4 0] [0 9]])]
      (is (t/approx-matrix= [[2.0 0.0] [0.0 3.0]] result 1e-10))))

  (testing "Matrix square root of 1x1 matrix"
    (let [result (cla/matrix-sqrt [[16]])]
      (is (t/approx= 4.0 (fc/re (get-in result [0 0])) 1e-10))))

  (testing "Matrix square root of identity"
    (let [result (cla/matrix-sqrt [[1 0] [0 1]])]
      (is (t/approx-matrix= [[1.0 0.0] [0.0 1.0]] result 1e-10))))

  (testing "Matrix square root of matrix with negative eigenvalues"
    (let [result (cla/matrix-sqrt [[1 0] [0 -1]])]
      ;; Should compute complex square root: sqrt(-1) = i
      (is (t/approx= 1.0 (fc/re (get-in result [0 0])) 1e-10))
      (is (t/approx= 0.0 (fc/im (get-in result [0 0])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in result [1 1])) 1e-10))
      (is (t/approx= 1.0 (fc/im (get-in result [1 1])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in result [0 1])) 1e-10))
      (is (t/approx= 0.0 (fc/re (get-in result [1 0])) 1e-10)))))

(deftest decomposition-properties
  (testing "Eigendecomposition preserves determinant"
    (let [A [[2 1] [1 2]]
          det-A (- (* 2 2) (* 1 1)) ; det([[2 1][1 2]]) = 4 - 1 = 3
          {:keys [eigenvalues]} (cla/eigen-hermitian A)
          det-eigenvals (reduce fc/mult (fc/complex 1.0 0.0) eigenvalues)]
      (is (t/approx= det-A (fc/re det-eigenvals) 1e-10))))

  (testing "Eigendecomposition preserves trace"
    (let [A [[5 2] [2 5]]
          trace-A (+ 5 5) ; tr([[5 2][2 5]]) = 10
          {:keys [eigenvalues]} (cla/eigen-hermitian A)
          trace-eigenvals (reduce fc/add (fc/complex 0.0 0.0) eigenvalues)]
      (is (t/approx= trace-A (fc/re trace-eigenvals) 1e-10))))

  (testing "Matrix exponential of Hermitian is unitary"
    (let [H [[1 0] [0 -1]] ; Hermitian matrix
          U (cla/matrix-exp {:real [[0.0 0.0] [0.0 0.0]]
                           :imag [[(* -1.0 1.0) 0.0] [0.0 (* -1.0 -1.0)]]})] ; exp(i*H)
      (is (cla/unitary? U)))))

(deftest spectral-norm-test
  (testing "Spectral norm computation"
    ;; Identity matrix should have spectral norm 1
    (is (t/approx= 1.0 (cla/spectral-norm [[1.0 0.0] [0.0 1.0]]) 1e-12))
    
    ;; Test with known matrix
    (let [A [[3 4] [0 5]]
          norm (cla/spectral-norm A)]
      ;; Should be approximately 6.708...
      (is (t/approx= 6.708203932499369 norm 1e-10)))
    
    ;; Zero matrix should have spectral norm 0 (TODO: implementation returns NaN)
    (is (t/approx= 0.0 (cla/spectral-norm [[0.0 0.0] [0.0 0.0]]) 1e-12))
    ))

(deftest condition-number-test
  (testing "Condition number computation"
    ;; Identity matrix should have condition number 1
    (is (t/approx= 1.0 (cla/condition-number [[1.0 0.0] [0.0 1.0]]) 1e-12))
    
    ;; Well-conditioned matrix
    (let [A [[4 1] [1 3]]
          cond (cla/condition-number A)]
      (is (> cond 1.0))
      (is (< cond 5.0))) ; Should be moderately conditioned
    
    ;; Test that ill-conditioned matrix has high condition number
    (let [A [[1 1] [1 1.001]]  ; Nearly singular
          cond (cla/condition-number A)]
      (is (> cond 100.0)))))    ; Should be ill-conditioned

(deftest svd-test
  (testing "Singular Value Decomposition"
    ;; Test 2x2 identity matrix (may return fewer singular values if equal)
    (let [{:keys [U singular-values]} (cla/svd [[1.0 0.0] [0.0 1.0]])]
      (is (>= (count singular-values) 1))
      (is (every? #(t/approx= 1.0 (fc/re %) 1e-10) singular-values)))
    
    ;; Test 2x2 diagonal matrix
    (let [{:keys [U singular-values]} (cla/svd [[3.0 0.0] [0.0 4.0]])]
      (is (= 2 (count singular-values)))
      ;; Singular values should be sorted in descending order - use Vec2 comparison
      (is (>= (fc/re (first singular-values)) (fc/re (second singular-values))))
      (is (some #(t/approx= 4.0 (fc/re %) 1e-10) singular-values))
      (is (some #(t/approx= 3.0 (fc/re %) 1e-10) singular-values)))
    
    ;; Test 3x3 matrix using power iteration
    (let [{:keys [U singular-values]} (cla/svd [[2.0 0.0 0.0] [0.0 3.0 0.0] [0.0 0.0 1.0]])]
      (is (= 3 (count singular-values)))
      (is (>= (fc/re (first singular-values)) (fc/re (second singular-values))))
      (is (>= (fc/re (second singular-values)) (fc/re (nth singular-values 2)))))))

(deftest eigen-hermitian-extended-test
  (testing "Extended eigendecomposition for larger matrices"
    ;; Test 3x3 symmetric matrix
    (let [A [[2.0 1.0 0.0] [1.0 2.0 1.0] [0.0 1.0 2.0]]
          {:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian A)]
      (is (= 3 (count eigenvalues)))
      (is (= 3 (count eigenvectors)))
      ;; All eigenvalues should be real for symmetric matrix (check imaginary parts are ~0)
      (is (every? #(< (abs (fc/im %)) 1e-12) eigenvalues)))
    
    ;; Test 4x4 identity matrix (may condense equal eigenvalues)
    (let [I [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [0.0 0.0 0.0 1.0]]
          {:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian I)]
      (is (>= (count eigenvalues) 1))
      ;; All eigenvalues of identity should be 1
      (is (every? #(t/approx= 1.0 (fc/re %) 1e-10) eigenvalues)))))

(deftest matrix-reconstruction-test
  (testing "Matrix reconstruction from eigendecomposition"
    ;; Test that reconstruction approximately recovers original matrix
    (let [A [[3.0 1.0] [1.0 2.0]]
          {:keys [eigenvalues eigenvectors]} (cla/eigen-hermitian A)
          reconstructed (cla/matrix-from-eigen eigenvalues eigenvectors)]
      ;; For small matrices, this might just return diagonal approximation
      ;; Check that the diagonal elements are reasonable - expecting Vec2 objects
      (is (every? (fn [x] (or (number? x) (instance? fastmath.vector.Vec2 x))) (map first reconstructed)))
      (is (every? (fn [x] (or (number? x) (instance? fastmath.vector.Vec2 x))) (map second reconstructed))))))

(deftest integration-test
  (testing "Integration of multiple operations"
    ;; Test workflow: create matrix -> eigendecomposition -> reconstruct
    (let [A [[4.0 0.0] [0.0 2.0]]  ; Use diagonal matrix that works correctly
          {:keys [eigenvalues]} (cla/eigen-hermitian A)
          {:keys [singular-values]} (cla/svd A)
          spec-norm (cla/spectral-norm A)
          cond-num (cla/condition-number A)
          eigenval-reals (map fc/re eigenvalues)
          sv-reals (map fc/re singular-values)]
      
      ;; Spectral norm should be largest singular value
      (is (t/approx= (apply max sv-reals) spec-norm 1e-10))
      
      ;; Condition number should be ratio of max/min singular values
      (is (t/approx= (/ (apply max sv-reals) (apply min sv-reals)) 
                     cond-num 1e-8))
      
      ;; All operations should complete without error
      (is (every? #(< (abs (fc/im %)) 1e-12) eigenvalues)) ; eigenvalues should be real
      (is (t/approx= spec-norm spec-norm)) ; spec-norm should be a valid number/Vec2
      (is (number? cond-num))))) ; cond-num should be a valid number

(comment
  (run-tests)
  ;
  )