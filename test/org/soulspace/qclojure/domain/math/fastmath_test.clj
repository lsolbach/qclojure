(ns org.soulspace.qclojure.domain.math.fastmath-test
  "Tests for the FastMath backend implementation.

  Mirrors the clojure-math-test functionality but uses FastMath Vec2 complex representations.
  
  Focus areas:
  * Basic real & complex arithmetic (add, sub, scale, matmul)
  * Vector/matrix products and outer-product
  * Linear solve & inverse small systems
  * Decomposition tests (LU, QR, Cholesky, SVD)
  * Predicates (hermitian?, unitary?, positive-semidefinite?)
  * Matrix functions (exp, log, sqrt)

  NOTE: Tests serve as regression guards and contract validation for FastMath backend.
  
  Known limitations:
  * SVD returns nil for V-dagger (partial implementation)"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.math.fastmath :as fm-backend]
            [org.soulspace.qclojure.domain.math.protocols :as proto]
            [fastmath.complex :as fc]))

(def backend (fm-backend/->FastMathBackend 1e-12 {}))

(defn approx= 
  "Approximate equality for FastMath Vec2 complex numbers, vectors, and matrices."
  [a b tol]
  (cond 
    ;; Vec2 complex numbers
    (and (instance? fastmath.vector.Vec2 a) (instance? fastmath.vector.Vec2 b))
    (< (fc/abs (fc/sub a b)) tol)
    
    ;; Regular numbers
    (and (number? a) (number? b)) 
    (< (Math/abs (- (double a) (double b))) tol)
    
    ;; Vectors/matrices
    (and (sequential? a) (sequential? b)) 
    (and (= (count a) (count b))
         (every? true? (map #(approx= %1 %2 tol) a b)))
    
    :else (= a b)))

(def tol 1.0e-9)

(deftest test-basic-real-arithmetic
  (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
           [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
        B [[(fc/complex 5.0 0.0) (fc/complex 6.0 0.0)]
           [(fc/complex 7.0 0.0) (fc/complex 8.0 0.0)]]]
    (testing "add/subtract"
      (is (approx= [[(fc/complex 6.0 0.0) (fc/complex 8.0 0.0)]
                    [(fc/complex 10.0 0.0) (fc/complex 12.0 0.0)]] 
                   (proto/add backend A B) tol))
      (is (approx= [[(fc/complex -4.0 0.0) (fc/complex -4.0 0.0)]
                    [(fc/complex -4.0 0.0) (fc/complex -4.0 0.0)]] 
                   (proto/subtract backend A B) tol)))
    (testing "scale/negate"
      (is (approx= [[(fc/complex 2.0 0.0) (fc/complex 4.0 0.0)]
                    [(fc/complex 6.0 0.0) (fc/complex 8.0 0.0)]] 
                   (proto/scale backend A 2.0) tol))
      (is (approx= (proto/scale backend A -1.0) (proto/negate backend A) tol)))
    (testing "matrix-multiply"
      (is (approx= [[(fc/complex 19.0 0.0) (fc/complex 22.0 0.0)]
                    [(fc/complex 43.0 0.0) (fc/complex 50.0 0.0)]] 
                   (proto/matrix-multiply backend A B) tol)))))

(deftest test-basic-complex-arithmetic
  (let [A [[(fc/complex 1.0 0.0) (fc/complex 0.0 1.0)]
           [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]]
        B [[(fc/complex 0.0 1.0) (fc/complex 2.0 0.0)]
           [(fc/complex 3.0 0.0) (fc/complex 0.0 4.0)]]
        C (proto/add backend A B)]
    (testing "complex add"
      (is (approx= [[(fc/complex 1.0 1.0) (fc/complex 2.0 1.0)]
                    [(fc/complex 3.0 0.0) (fc/complex 1.0 4.0)]] C tol)))
    (testing "complex hermitian? false"
      (is (false? (proto/hermitian? backend C))))))

(deftest test-trace-and-transpose
  (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0) (fc/complex 3.0 0.0)]
           [(fc/complex 4.0 0.0) (fc/complex 5.0 0.0) (fc/complex 6.0 0.0)]
           [(fc/complex 7.0 0.0) (fc/complex 8.0 0.0) (fc/complex 9.0 0.0)]]]
    (is (approx= (fc/complex 15.0 0.0) (proto/trace backend A) tol))
    (is (approx= [[(fc/complex 1.0 0.0) (fc/complex 4.0 0.0) (fc/complex 7.0 0.0)]
                  [(fc/complex 2.0 0.0) (fc/complex 5.0 0.0) (fc/complex 8.0 0.0)]
                  [(fc/complex 3.0 0.0) (fc/complex 6.0 0.0) (fc/complex 9.0 0.0)]] 
                 (proto/transpose backend A) tol))))

(deftest test-linear-solve-and-inverse
  (let [A [[(fc/complex 4.0 0.0) (fc/complex 7.0 0.0)]
           [(fc/complex 2.0 0.0) (fc/complex 6.0 0.0)]]
        bvec [(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
        x (proto/solve-linear-system backend A bvec)
        inv (proto/inverse backend A)]
    (testing "solve Ax=b"
      (is (approx= [(fc/complex 0.6 0.0) (fc/complex -0.2 0.0)] x tol)))
    (testing "inverse * A ~ I"
      (let [I (proto/matrix-multiply backend inv A)]
        (is (approx= [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                      [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]] I 1e-8))))))

(deftest test-complex-linear-solve-and-inverse
  (let [A [[(fc/complex 2.0 0.0) (fc/complex 0.0 1.0)]
           [(fc/complex 0.0 0.0) (fc/complex 3.0 0.0)]]
        bvec [(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
        x (proto/solve-linear-system backend A bvec)
        inv (proto/inverse backend A)
        AX (proto/matrix-vector-product backend A x)
        IA (proto/matrix-multiply backend inv A)]
    (testing "complex solve Ax=b"
      (is (approx= bvec AX 1e-8)))
    (testing "complex inverse * A ~ I"
      (is (approx= [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                    [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]] IA 1e-8)))))

(defn verify-cholesky 
  "Verify that L * L† = A within tolerance."
  [backend A L tolerance]
  (let [L-conjugate-transpose (proto/conjugate-transpose backend L)
        reconstructed (proto/matrix-multiply backend L L-conjugate-transpose)
        error (proto/subtract backend A reconstructed)]
    ;; Check that all error elements are within tolerance
    (every? (fn [row]
              (every? (fn [element]
                        (< (fc/abs element) tolerance))
                      row))
            error)))

(deftest test-cholesky-real-matrix
  (testing "Cholesky decomposition of real positive definite matrix"
    (let [A [[(fc/complex 4.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 2.0 0.0) (fc/complex 3.0 0.0)]]
          result (proto/cholesky-decomposition backend A)
          L (:L result)]
      
      ;; Check that result is returned
      (is (some? result))
      (is (contains? result :L))
      
      ;; Check that L is lower triangular
      (is (< (fc/abs (get-in L [0 1])) 1e-12) "L should be lower triangular")
      
      ;; Check that diagonal elements are positive real
      (is (> (fc/re (get-in L [0 0])) 0) "Diagonal element should be positive")
      (is (> (fc/re (get-in L [1 1])) 0) "Diagonal element should be positive")
      (is (< (fc/im (get-in L [0 0])) 1e-12) "Diagonal should be real")
      (is (< (fc/im (get-in L [1 1])) 1e-12) "Diagonal should be real")
      
      ;; Verify that L * L† = A
      (is (verify-cholesky backend A L 1e-10)))))

(deftest test-cholesky-complex-hermitian-matrix
  (testing "Cholesky decomposition of complex Hermitian positive definite matrix"
    (let [A [[(fc/complex 4.0 0.0) (fc/complex 2.0 1.0)]
             [(fc/complex 2.0 -1.0) (fc/complex 3.0 0.0)]]
          result (proto/cholesky-decomposition backend A)
          L (:L result)]
      
      ;; Check that result is returned
      (is (some? result))
      (is (contains? result :L))
      
      ;; Check that L is lower triangular
      (is (< (fc/abs (get-in L [0 1])) 1e-12) "L should be lower triangular")
      
      ;; Check that diagonal elements are positive real
      (is (> (fc/re (get-in L [0 0])) 0) "Diagonal element should be positive")
      (is (> (fc/re (get-in L [1 1])) 0) "Diagonal element should be positive")
      (is (< (fc/im (get-in L [0 0])) 1e-12) "Diagonal should be real")
      (is (< (fc/im (get-in L [1 1])) 1e-12) "Diagonal should be real")
      
      ;; Verify that L * L† = A
      (is (verify-cholesky backend A L 1e-10)))))

(deftest test-cholesky-larger-matrix
  (testing "Cholesky decomposition of larger complex matrix"
    (let [A [[(fc/complex 6.0 0.0) (fc/complex 3.0 1.0) (fc/complex 1.0 0.5)]
             [(fc/complex 3.0 -1.0) (fc/complex 5.0 0.0) (fc/complex 2.0 0.5)]
             [(fc/complex 1.0 -0.5) (fc/complex 2.0 -0.5) (fc/complex 4.0 0.0)]]
          result (proto/cholesky-decomposition backend A)
          L (:L result)]
      
      ;; Check that result is returned
      (is (some? result))
      (is (contains? result :L))
      
      ;; Check that L is lower triangular
      (is (< (fc/abs (get-in L [0 1])) 1e-12) "L should be lower triangular")
      (is (< (fc/abs (get-in L [0 2])) 1e-12) "L should be lower triangular")
      (is (< (fc/abs (get-in L [1 2])) 1e-12) "L should be lower triangular")
      
      ;; Check that diagonal elements are positive real
      (is (> (fc/re (get-in L [0 0])) 0) "Diagonal element should be positive")
      (is (> (fc/re (get-in L [1 1])) 0) "Diagonal element should be positive") 
      (is (> (fc/re (get-in L [2 2])) 0) "Diagonal element should be positive")
      
      ;; Verify that L * L† = A
      (is (verify-cholesky backend A L 1e-10)))))

(deftest test-cholesky-error-cases
  (testing "Cholesky decomposition error cases"
    ;; Non-square matrix
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0) (fc/complex 3.0 0.0)]
             [(fc/complex 4.0 0.0) (fc/complex 5.0 0.0) (fc/complex 6.0 0.0)]]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"square matrix"
                            (proto/cholesky-decomposition backend A))))
    
    ;; Non-Hermitian matrix
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Hermitian matrix"
                            (proto/cholesky-decomposition backend A))))
    
    ;; Non-positive definite matrix (negative eigenvalue)
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 2.0 0.0) (fc/complex 1.0 0.0)]]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not positive definite"
                            (proto/cholesky-decomposition backend A))))))

(deftest test-vector-matrix-products
  (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
           [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
        v [(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
        u [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
    (testing "matrix-vector product Av"
      (is (approx= [(fc/complex 5.0 0.0) (fc/complex 11.0 0.0)] 
                   (proto/matrix-vector-product backend A v) tol)))
    (testing "outer product uv†"
      (is (approx= [[(fc/complex 3.0 0.0) (fc/complex 6.0 0.0)]
                    [(fc/complex 4.0 0.0) (fc/complex 8.0 0.0)]] 
                   (proto/outer-product backend u v) tol)))
    (testing "inner product u†v"
      (is (approx= (fc/complex 11.0 0.0) (proto/inner-product backend u v) tol)))))

(deftest test-basic-predicates
  (let [hermitian [[(fc/complex 2.0 0.0) (fc/complex 1.0 1.0)]
                   [(fc/complex 1.0 -1.0) (fc/complex 3.0 0.0)]]
        non-hermitian [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
                       [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
        identity [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                  [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]]]
    (testing "hermitian?"
      (is (true? (proto/hermitian? backend hermitian)))
      (is (false? (proto/hermitian? backend non-hermitian))))
    (testing "unitary? for identity"
      (is (true? (proto/unitary? backend identity))))
    (testing "positive-semidefinite?"
      (is (true? (proto/positive-semidefinite? backend hermitian))))))

(deftest test-conjugate-transpose-properties
  (let [A [[(fc/complex 1.0 2.0) (fc/complex 3.0 4.0)]
           [(fc/complex 5.0 6.0) (fc/complex 7.0 8.0)]]
        A-dagger (proto/conjugate-transpose backend A)]
    (testing "conjugate transpose values"
      (is (approx= [[(fc/complex 1.0 -2.0) (fc/complex 5.0 -6.0)]
                    [(fc/complex 3.0 -4.0) (fc/complex 7.0 -8.0)]] A-dagger tol)))
    (testing "conjugate transpose involution: (A†)† = A"
      (is (approx= A (proto/conjugate-transpose backend A-dagger) tol)))))

(deftest test-shape-properties
  (let [square [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
                [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
        rectangular [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0) (fc/complex 3.0 0.0)]
                     [(fc/complex 4.0 0.0) (fc/complex 5.0 0.0) (fc/complex 6.0 0.0)]]]
    (testing "shape of square matrix"
      (is (= [2 2] (proto/shape backend square))))
    (testing "shape of rectangular matrix"
      (is (= [2 3] (proto/shape backend rectangular))))))

(defn verify-lu-decomposition 
  "Verify that P * L * U = A within tolerance."
  [backend A L U P tolerance]
  (let [PLU (proto/matrix-multiply backend 
                                   (proto/matrix-multiply backend P L) U)
        error (proto/subtract backend A PLU)]
    (every? (fn [row]
              (every? (fn [element]
                        (< (fc/abs element) tolerance))
                      row))
            error)))

(deftest test-lu-decomposition
  (testing "LU decomposition of real matrix"
    (let [A [[(fc/complex 2.0 0.0) (fc/complex 1.0 0.0)]
             [(fc/complex 1.0 0.0) (fc/complex 3.0 0.0)]]
          result (proto/lu-decomposition backend A)
          L (:L result)
          U (:U result)
          P (:P result)]
      
      ;; Check that all components are returned
      (is (some? L) "L matrix should be returned")
      (is (some? U) "U matrix should be returned")
      (is (some? P) "P matrix should be returned")
      
      ;; Verify reconstruction: P * L * U = A
      (is (verify-lu-decomposition backend A L U P 1e-10)))))

(deftest test-lu-decomposition-complex
  (testing "LU decomposition of complex matrix"
    (let [A [[(fc/complex 3.0 1.0) (fc/complex 2.0 0.0)]
             [(fc/complex 1.0 -1.0) (fc/complex 4.0 2.0)]]
          result (proto/lu-decomposition backend A)
          L (:L result)
          U (:U result) 
          P (:P result)]
      
      ;; Check that all components are returned
      (is (some? L) "L matrix should be returned")
      (is (some? U) "U matrix should be returned")
      (is (some? P) "P matrix should be returned")
      
      ;; Verify reconstruction
      (is (verify-lu-decomposition backend A L U P 1e-10)))))

(defn verify-qr-decomposition 
  "Verify that Q * R = A and Q is orthogonal within tolerance."
  [backend A Q R tolerance]
  (let [QR (proto/matrix-multiply backend Q R)
        error (proto/subtract backend A QR)
        Q-dagger (proto/conjugate-transpose backend Q)
        QQ-dagger (proto/matrix-multiply backend Q Q-dagger)
        I [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
           [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]]
        orthogonality-error (proto/subtract backend I QQ-dagger)]
    (and 
      ;; Check QR = A
      (every? (fn [row]
                (every? (fn [element]
                          (< (fc/abs element) tolerance))
                        row))
              error)
      ;; Check Q is orthogonal: Q Q† = I  
      (every? (fn [row]
                (every? (fn [element]
                          (< (fc/abs element) tolerance))
                        row))
              orthogonality-error))))

(deftest test-qr-decomposition
  (testing "QR decomposition of real matrix"
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
          result (proto/qr-decomposition backend A)
          Q (:Q result)
          R (:R result)]
      
      ;; Check that components are returned
      (is (some? Q) "Q matrix should be returned")
      (is (some? R) "R matrix should be returned")
      
      ;; Verify reconstruction and orthogonality
      (is (verify-qr-decomposition backend A Q R 1e-10)))))

(deftest test-qr-decomposition-complex
  (testing "QR decomposition of complex matrix"
    (let [A [[(fc/complex 1.0 1.0) (fc/complex 0.0 1.0)]
             [(fc/complex 2.0 0.0) (fc/complex 1.0 -1.0)]]
          result (proto/qr-decomposition backend A)
          Q (:Q result)
          R (:R result)]
      
      ;; Check that components are returned
      (is (some? Q) "Q matrix should be returned")
      (is (some? R) "R matrix should be returned")
      
      ;; Verify reconstruction and orthogonality
      (is (verify-qr-decomposition backend A Q R 1e-10))))
  
  (testing "QR decomposition of another complex matrix"
    (let [A [[(fc/complex 3.0 1.0) (fc/complex 1.0 0.0)]
             [(fc/complex 0.0 2.0) (fc/complex 2.0 -1.0)]]
          result (proto/qr-decomposition backend A)
          Q (:Q result)
          R (:R result)]
      
      ;; Check that components are returned
      (is (some? Q) "Q matrix should be returned")
      (is (some? R) "R matrix should be returned")
      
      ;; Verify reconstruction and orthogonality
      (is (verify-qr-decomposition backend A Q R 1e-10)))))

(deftest test-matrix-functions
  (let [A [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
           [(fc/complex 0.0 0.0) (fc/complex 2.0 0.0)]]]
    (testing "matrix exponential of diagonal matrix"
      (let [exp-A (proto/matrix-exp backend A)
            expected [[(fc/complex (Math/exp 1.0) 0.0) (fc/complex 0.0 0.0)]
                      [(fc/complex 0.0 0.0) (fc/complex (Math/exp 2.0) 0.0)]]]
        (is (approx= expected exp-A 1e-10))))
    
    (testing "matrix logarithm of diagonal matrix"
      (let [log-A (proto/matrix-log backend A)
            expected [[(fc/complex 0.0 0.0) (fc/complex 0.0 0.0)]
                      [(fc/complex 0.0 0.0) (fc/complex (Math/log 2.0) 0.0)]]]
        (is (approx= expected log-A 1e-10))))
    
    (testing "matrix square root of diagonal matrix"
      (let [sqrt-A (proto/matrix-sqrt backend A)
            expected [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                      [(fc/complex 0.0 0.0) (fc/complex (Math/sqrt 2.0) 0.0)]]]
        (is (approx= expected sqrt-A 1e-10))))))

(deftest test-matrix-exp-log-identity
  (testing "matrix exp and log are inverses"
    (let [A [[(fc/complex 0.5 0.0) (fc/complex 0.1 0.0)]
             [(fc/complex 0.1 0.0) (fc/complex 0.3 0.0)]]
          exp-A (proto/matrix-exp backend A)
          log-exp-A (proto/matrix-log backend exp-A)]
      ;; log(exp(A)) should equal A for suitable matrices
      (is (approx= A log-exp-A 1e-8)))))

(deftest test-svd-decomposition
  (testing "SVD decomposition of real matrix"
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
          result (proto/svd backend A)
          U (:U result)
          S (:S result)]
      
      ;; Check that components are returned
      (is (some? U) "U matrix should be returned")
      (is (some? S) "S diagonal should be returned")
      ;; Note: V-dagger may be nil in FastMath implementation
      
      ;; Check that singular values are non-negative and sorted
      (is (>= (fc/re (first S)) 0) "First singular value should be non-negative")
      (is (>= (fc/re (second S)) 0) "Second singular value should be non-negative")
      (is (>= (fc/re (first S)) (fc/re (second S))) "Singular values should be sorted"))))

(deftest test-eigenvalue-decompositions
  (testing "eigen-hermitian for real symmetric matrix"
    (let [A [[(fc/complex 2.0 0.0) (fc/complex 1.0 0.0)]
             [(fc/complex 1.0 0.0) (fc/complex 3.0 0.0)]]
          result (proto/eigen-hermitian backend A)
          eigenvals (:eigenvalues result)
          eigenvecs (:eigenvectors result)]
      
      ;; Check that components are returned
      (is (some? eigenvals) "Eigenvalues should be returned")
      (is (some? eigenvecs) "Eigenvectors should be returned")
      (is (= 2 (count eigenvals)) "Should have 2 eigenvalues")
      (is (= 2 (count eigenvecs)) "Should have 2 eigenvectors")
      
      ;; Eigenvalues should be real for Hermitian matrices
      ;; Note: eigen-hermitian returns eigenvalues as {:real ... :imag ...} maps
      (is (every? #(< (Math/abs (:imag %)) 1e-12) eigenvals) "Eigenvalues should be real")))
  
  (testing "eigen-general for upper triangular matrix"
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 0.0 0.0) (fc/complex 3.0 0.0)]]
          result (proto/eigen-general backend A)
          eigenvals (:eigenvalues result)
          eigenvecs (:eigenvectors result)]
      
      ;; Check that components are returned
      (is (some? eigenvals) "Eigenvalues should be returned")
      (is (some? eigenvecs) "Eigenvectors should be returned")
      (is (= 2 (count eigenvals)) "Should have 2 eigenvalues")
      
      ;; For upper triangular matrix, eigenvalues should be diagonal elements
      ;; Note: eigen-general returns eigenvalues as Vec2 complex numbers
      (is (approx= [(fc/complex 1.0 0.0) (fc/complex 3.0 0.0)] eigenvals 1e-10)))))

(deftest test-spectral-norm
  (testing "spectral norm of real matrix"
    (let [A [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
             [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]]
          norm (proto/spectral-norm backend A)]
      
      ;; Check that norm is returned and positive
      (is (number? norm) "Spectral norm should be a number")
      (is (> norm 0) "Spectral norm should be positive")
      
      ;; For this specific matrix, we can check the known value
      (is (approx= 5.464985704219042 norm 1e-10)))))

(deftest test-advanced-matrix-properties
  (testing "spectral norm of diagonal matrix"
    (let [A [[(fc/complex 3.0 0.0) (fc/complex 0.0 0.0)]
             [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]]
          norm (proto/spectral-norm backend A)]
      ;; Spectral norm of diagonal matrix should be max diagonal element
      (is (approx= 3.0 norm 1e-10))))
  
  (testing "positive-semidefinite detection"
    (let [psd [[(fc/complex 2.0 0.0) (fc/complex 1.0 0.0)]
               [(fc/complex 1.0 0.0) (fc/complex 3.0 0.0)]]
          non-psd [[(fc/complex 1.0 0.0) (fc/complex 2.0 0.0)]
                   [(fc/complex 2.0 0.0) (fc/complex 1.0 0.0)]]]
      (is (true? (proto/positive-semidefinite? backend psd)))
      (is (false? (proto/positive-semidefinite? backend non-psd))))))

(comment
  "Run tests"
  (run-tests)  
  ;
  )