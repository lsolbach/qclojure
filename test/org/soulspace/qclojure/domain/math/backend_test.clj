(ns org.soulspace.qclojure.domain.math.backend-test
  "Tests for Math Backend Adapters
   
   This namespace contains unit tests for the Math Backend Adapters, ensuring correct conversion
   between Clojure data structures and backend-specific formats, as well as validating core
   mathematical operations such as vector and matrix manipulations.
   
   It also compares results across different backends to ensure consistency and correctness.
   The tests include edge cases and typical use cases to verify the robustness of the backend implementations."
  (:require
   [clojure.test :refer [deftest is testing run-tests]]
   [fastmath.complex :as fc]
   [org.soulspace.qclojure.util.test :as util]
   [org.soulspace.qclojure.domain.math.protocols :as proto]
   [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]))

;;;
;;; Test Data
;;;

(def real-vector [1.0 2.0 3.0])
(def real-matrix [[1.0 2.0 3.0]
                  [4.0 5.0 6.0]
                  [7.0 8.0 9.0]])

(def complex-vector [(fc/complex 1.0 0.0) 
                     (fc/complex 0.0 1.0) 
                     (fc/complex 1.0 1.0)])

(def complex-matrix [[(fc/complex 1.0 0.0) (fc/complex 0.0 -1.0)]
                     [(fc/complex 0.0 1.0) (fc/complex 1.0 0.0)]])

(def hermitian-matrix [[(fc/complex 1.0 0.0) (fc/complex 0.0 -1.0)]
                       [(fc/complex 0.0 1.0) (fc/complex 1.0 0.0)]])

(def identity-2x2 [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                   [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]])

;;;
;;; Helper Functions
;;;

(defn test-backend-consistency
  "Test that all backends produce consistent results for a given operation."
  [operation & args]
  (let [backends (cla/available-backends)
        results (into {} 
                      (map (fn [backend-key]
                             [backend-key 
                              (try
                                (cla/with-backend backend-key
                                  (apply operation args))
                                (catch Exception e
                                  {:error (.getMessage e)}))])
                           backends))
        ;; Filter out error results for comparison
        valid-results (into {} (filter #(not (and (map? (second %)) (:error (second %)))) results))
        errors (into {} (filter #(and (map? (second %)) (:error (second %))) results))]
    
    (when (seq errors)
      (println "Backend errors:" errors))
    
    (if (< (count valid-results) 2)
      [false results] ; Can't compare if most backends failed
      (let [first-result (second (first valid-results))
            all-equal? (every? (fn [[_ result]]
                                 (cond
                                   ;; Handle boolean results
                                   (boolean? result) (= first-result result)
                                   ;; Handle scalar results
                                   (not (coll? result)) (util/approx= first-result result)
                                   ;; Handle vector results
                                   (and (vector? result) (not (vector? (first result))))
                                   (util/approx-vector= first-result result)
                                   ;; Handle matrix results
                                   (and (vector? result) (vector? (first result)))
                                   (util/approx-matrix= first-result result)
                                   ;; Handle maps (like eigendecomposition results) - compare eigenvalues for now
                                   (map? result) 
                                   (and (map? first-result)
                                        (util/approx-vector= (:eigenvalues first-result) (:eigenvalues result)))
                                   :else false))
                               valid-results)]
        [all-equal? results]))))

;;;
;;; Backend Selection Tests
;;;

(deftest test-backend-selection
  (testing "Available backends"
    (let [backends (cla/available-backends)]
      (is (set? backends))
      (is (contains? backends :pure))
      (is (contains? backends :fastmath))))

  (testing "Current backend"
    (is (keyword? (cla/get-backend)))
    (is (contains? (cla/available-backends) (cla/get-backend))))

  (testing "Backend switching"
    (let [original-backend (cla/get-backend)]
      ;; Test with-backend macro
      (cla/with-backend :pure
        (is (= :pure (cla/get-backend))))
      
      ;; Should restore original backend
      (is (= original-backend (cla/get-backend)))))

  (testing "Backend creation"
    (let [pure-backend (cla/create-backend :pure)
          fastmath-backend (cla/create-backend :fastmath)]
      (is (satisfies? proto/MatrixAlgebra pure-backend))
      (is (satisfies? proto/MatrixAlgebra fastmath-backend)))))

;;;
;;; Data Conversion Tests
;;;

(deftest test-data-conversion
  (testing "Complex vector creation"
    (let [cv (cla/complex-vector [1 2] [3 4])]
      (is (= 2 (count cv)))
      (is (util/approx= (fc/complex 1.0 3.0) (first cv)))
      (is (util/approx= (fc/complex 2.0 4.0) (second cv)))))

  (testing "Complex matrix creation"
    (let [cm (cla/complex-matrix [[1 2] [3 4]] [[5 6] [7 8]])]
      (is (= 2 (count cm)))
      (is (= 2 (count (first cm))))
      (is (util/approx= (fc/complex 1.0 5.0) (get-in cm [0 0])))
      (is (util/approx= (fc/complex 4.0 8.0) (get-in cm [1 1]))))))

;;;
;;; Basic Operations Tests
;;;

(deftest test-basic-operations
  (testing "Matrix shape"
    (is (= [3 3] (cla/shape real-matrix)))
    (is (= [2 2] (cla/shape complex-matrix))))

  (testing "Matrix addition consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/add identity-2x2 identity-2x2)]
      (is consistent? (str "Backend results differ: " results))
      (let [result (second (first results))]
        (is (util/approx= (fc/complex 2.0 0.0) (get-in result [0 0]))))))

  (testing "Matrix subtraction consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/subtract identity-2x2 identity-2x2)]
      (is consistent? (str "Backend results differ: " results))
      (let [result (second (first results))]
        (is (util/approx= (fc/complex 0.0 0.0) (get-in result [0 0]))))))

  (testing "Scalar multiplication consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/scale identity-2x2 2.0)]
      (is consistent? (str "Backend results differ: " results))
      (let [result (second (first results))]
        (is (util/approx= (fc/complex 2.0 0.0) (get-in result [0 0]))))))

  (testing "Matrix negation consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/negate identity-2x2)]
      (is consistent? (str "Backend results differ: " results))
      (let [result (second (first results))]
        (is (util/approx= (fc/complex -1.0 0.0) (get-in result [0 0])))))))

;;;
;;; Product Operations Tests
;;;

(deftest test-product-operations
  (testing "Matrix multiplication consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/matrix-multiply identity-2x2 complex-matrix)]
      (is consistent? (str "Backend results differ: " results))
      ;; Identity * A should equal A
      (let [result (second (first results))]
        (is (util/approx-complex-matrix= complex-matrix result)))))

  (testing "Matrix-vector product consistency across backends"
    (let [vector-2d [(fc/complex 1.0 0.0) (fc/complex 0.0 1.0)]
          [consistent? results] (test-backend-consistency cla/matrix-vector-product complex-matrix vector-2d)]
      (is consistent? (str "Backend results differ: " results))))

  (testing "Inner product consistency across backends"
    (let [v1 [(fc/complex 1.0 0.0) (fc/complex 0.0 1.0)]
          v2 [(fc/complex 1.0 0.0) (fc/complex 1.0 0.0)]
          [consistent? results] (test-backend-consistency cla/inner-product v1 v2)]
      (is consistent? (str "Backend results differ: " results))))

  (testing "Outer product consistency across backends"
    (let [v1 [(fc/complex 1.0 0.0) (fc/complex 0.0 1.0)]
          v2 [(fc/complex 1.0 0.0) (fc/complex 1.0 0.0)]
          [consistent? results] (test-backend-consistency cla/outer-product v1 v2)]
      (is consistent? (str "Backend results differ: " results))))

  (testing "Kronecker product consistency across backends"
    (let [small-matrix [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                        [(fc/complex 0.0 0.0) (fc/complex 1.0 0.0)]]
          [consistent? results] (test-backend-consistency cla/kronecker-product small-matrix small-matrix)]
      (is consistent? (str "Backend results differ: " results))
      ;; Result should be 4x4 matrix
      (let [result (second (first results))]
        (is (= [4 4] (cla/shape result)))))))

;;;
;;; Transformation Tests
;;;

(deftest test-transformations
  (testing "Transpose consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/transpose complex-matrix)]
      (is consistent? (str "Backend results differ: " results))))

  (testing "Conjugate transpose consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/conjugate-transpose complex-matrix)]
      (is consistent? (str "Backend results differ: " results))
      ;; For Hermitian matrices, A† should equal A
      (let [hermitian-conj-transpose (cla/conjugate-transpose hermitian-matrix)]
        (is (util/approx-matrix= hermitian-matrix hermitian-conj-transpose))))))

;;;
;;; Reduction and Scalar Tests
;;;

(deftest test-reductions
  (testing "Trace consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/trace complex-matrix)]
      (is consistent? (str "Backend results differ: " results))))

  (testing "Vector norm consistency across backends"
    (let [vector-2d [(fc/complex 3.0 0.0) (fc/complex 4.0 0.0)]
          [consistent? results] (test-backend-consistency cla/norm2 vector-2d)]
      (is consistent? (str "Backend results differ: " results))
      ;; ||[3, 4]|| should be 5
      (let [result (second (first results))]
        (is (util/approx= 5.0 result))))))

;;;
;;; Predicate Tests
;;;

(deftest test-predicates
  (testing "Hermitian test consistency across backends"
    (let [[consistent? results] (test-backend-consistency cla/hermitian? hermitian-matrix)]
      (is consistent? (str "Backend results differ: " results))
      (is (every? true? (vals results)) "Hermitian matrix should be detected as Hermitian")))

  (testing "Unitary test consistency across backends"
    ;; Use identity matrix which is unitary
    (let [[consistent? results] (test-backend-consistency cla/unitary? identity-2x2)]
      (is consistent? (str "Backend results differ: " results))
      (is (every? true? (vals results)) "Identity matrix should be detected as unitary"))))

;;;
;;; Eigendecomposition Tests
;;;

(deftest test-eigendecomposition
  (testing "Hermitian eigendecomposition consistency across backends"
    (let [pauli-z [[(fc/complex 1.0 0.0) (fc/complex 0.0 0.0)]
                   [(fc/complex 0.0 0.0) (fc/complex -1.0 0.0)]]
          [consistent? results] (test-backend-consistency cla/eigen-hermitian pauli-z)]
      (is consistent? (str "Backend results differ: " results))
      
      ;; Test eigenvalue equation A*v = λ*v for each backend
      (doseq [[backend-key result] results]
        (let [{:keys [eigenvalues eigenvectors]} result]
          (is (= 2 (count eigenvalues)) (str "Should have 2 eigenvalues for " backend-key))
          (is (= 2 (count eigenvectors)) (str "Should have 2 eigenvectors for " backend-key))
          
          ;; Test eigenvalue equation for each eigenvector
          (doseq [i (range (count eigenvalues))]
            (let [lambda (nth eigenvalues i)
                  v (nth eigenvectors i)
                  av (cla/with-backend backend-key (cla/matrix-vector-product pauli-z v))
                  lambda-v (mapv #(fc/mult lambda %) v)
                  diff (mapv fc/sub av lambda-v)
                  norm-diff (Math/sqrt (apply + (map #(let [abs-val (fc/abs %)]
                                                        (* abs-val abs-val)) diff)))]
              (is (< norm-diff 1e-10) 
                  (str "Eigenvalue equation not satisfied for " backend-key 
                       " eigenvector " i ": ||A*v - λ*v|| = " norm-diff)))))))))


(comment
  (run-tests)
  ;
  )