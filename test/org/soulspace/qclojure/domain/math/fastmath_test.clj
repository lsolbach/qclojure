(ns org.soulspace.qclojure.domain.math.fastmath-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.math.fastmath :as fm-backend]
            [org.soulspace.qclojure.domain.math.protocols :as proto]
            [fastmath.complex :as fc]))

(def backend (fm-backend/->FastMathBackend 1e-12))

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

(comment
  "Run tests"
  (run-tests)  
  ;
  )