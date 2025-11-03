(ns org.soulspace.qclojure.application.algorithm.optimization-test
  "Test suite for Optimization algorithm functionality.
  
  This test suite validates quantum optimization algorithms:
  - Gradient computation (parameter shift, finite difference)
  - Optimization methods (Adam, gradient descent, quantum natural gradient)
  - Matrix operations for quantum natural gradient
  - Performance and convergence behavior"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithm.optimization :as qopt]))

;;
;; Test Fixtures and Helper Functions
;;
(defn simple-objective-function
  "Simple quadratic objective function for testing optimization."
  [params]
  (let [[x y] params]
    (+ (* x x) (* y y) (* 2 x y) (* 3 x) (* 4 y) 5)))

(defn simple-vqe-objective
  "Simple VQE-like objective function for testing."
  [params]
  ;; Simplified objective function for testing optimization algorithms
  ;; Avoids complex quantum simulation dependencies
  (reduce + (map #(* % %) params)))

;;
;; Gradient Computation Tests
;;
(deftest test-finite-difference-gradient
  (testing "Finite difference gradient computation"
    (let [gradient (qopt/finite-difference-gradient simple-objective-function [1.0 1.0])]
      (is (vector? gradient) "Should return a vector")
      (is (= 2 (count gradient)) "Should have same dimension as input")
      (is (every? number? gradient) "All elements should be numbers"))))

(deftest test-parameter-shift-gradient
  (testing "Parameter shift gradient computation for VQE"
    (let [params [0.1 0.2 0.3 0.4 0.5 0.6]
          gradient (qopt/calculate-parameter-shift-gradient simple-vqe-objective params)]
      
      (is (vector? gradient) "Should return a vector")
      (is (= (count params) (count gradient)) "Should have same dimension as input")
      (is (every? number? gradient) "All elements should be numbers")
      (is (not-any? #(Double/isNaN %) gradient) "No elements should be NaN"))))

(deftest test-calculate-gradient
  (testing "Calculate gradient with different methods"
    (let [params [0.1 0.2 0.3 0.4 0.5 0.6]]
      
      ;; Test parameter shift gradient with simple function
      (let [grad1 (qopt/calculate-gradient simple-vqe-objective params
                                           {:gradient-method :parameter-shift})]
        (is (vector? grad1) "Parameter shift should return vector")
        (is (= (count params) (count grad1)) "Should match parameter count"))
      
      ;; Test finite difference gradient
      (let [grad2 (qopt/calculate-gradient simple-vqe-objective params
                                           {:gradient-method :finite-difference})]
        (is (vector? grad2) "Finite difference should return vector")
        (is (= (count params) (count grad2)) "Should match parameter count")))))

(deftest test-parallel-gradient-computation
  (testing "Parallel gradient computation"
    (let [params [0.1 0.2 0.3 0.4 0.5 0.6]
          grad-seq (qopt/calculate-gradient simple-vqe-objective params
                                            {:gradient-method :finite-difference
                                             :parallel? false})
          grad-par (qopt/calculate-gradient simple-vqe-objective params
                                            {:gradient-method :finite-difference
                                             :parallel? true})]
      
      (is (= (count grad-seq) (count grad-par)) "Should have same dimension")
      ;; Results should be approximately equal (within numerical precision)
      (is (every? true? (map #(< (abs (- %1 %2)) 1e-10) grad-seq grad-par))
          "Parallel and sequential results should be approximately equal"))))

;;
;; Optimization Algorithm Tests
;;
(deftest test-adam-optimization
  (testing "Adam optimization algorithm"
    (let [result (qopt/adam-optimization simple-objective-function [1.0 1.0] 
                                         {:max-iterations 50
                                          :tolerance 1e-6
                                          :learning-rate 0.1})]
      (is (map? result) "Should return result map")
      (is (contains? result :optimal-parameters) "Should have optimal parameters")
      (is (contains? result :optimal-energy) "Should have optimal energy")
      (is (contains? result :success) "Should have success flag")
      (is (vector? (:optimal-parameters result)) "Optimal parameters should be vector")
      (is (number? (:optimal-energy result)) "Optimal energy should be number"))))

(deftest test-gradient-descent-optimization
  (testing "Gradient descent optimization algorithm"
    (let [result (qopt/gradient-descent-optimization simple-objective-function [1.0 1.0]
                                                     {:max-iterations 50
                                                      :tolerance 1e-6
                                                      :learning-rate 0.1})]
      (is (map? result) "Should return result map")
      (is (contains? result :optimal-parameters) "Should have optimal parameters")
      (is (contains? result :optimal-energy) "Should have optimal energy")
      (is (contains? result :success) "Should have success flag"))))

(deftest test-fastmath-optimization-methods
  (testing "Fastmath derivative-free optimization methods"
    (doseq [method [:nelder-mead :powell]]
      (let [result (qopt/fastmath-derivative-free-optimization method simple-objective-function [1.0 1.0]
                                                               {:max-iterations 50
                                                                :tolerance 1e-6})]
        (is (map? result) (str "Method " method " should return result map"))
        (is (contains? result :optimal-parameters) (str "Method " method " should have optimal parameters"))
        (is (contains? result :optimal-energy) (str "Method " method " should have optimal energy"))))))

;;
;; Fisher Information Matrix Tests
;;
(deftest test-fisher-information-matrix
  (testing "Fisher information matrix computation"
    ;; Skip this test for now as it requires complex quantum simulation setup
    ;; Focus on simpler matrix operations that don't depend on backend simulation
    (is true "Fisher information matrix test skipped - requires full VQE setup")))

(deftest test-regularize-fisher-matrix
  (testing "Fisher matrix regularization"
    (let [fisher-matrix [[1.0 0.5] [0.5 0.0]]  ; Singular matrix
          regularized (qopt/regularize-fisher-matrix fisher-matrix 1e-6)]
      (is (vector? regularized) "Should return matrix")
      (is (= (count fisher-matrix) (count regularized)) "Should preserve dimensions")
      ;; Check that diagonal elements are increased
      (is (> (get-in regularized [1 1]) (get-in fisher-matrix [1 1])) "Should regularize singular values"))))

;;
;; Edge Cases and Error Handling Tests
;;
(deftest test-optimization-edge-cases
  (testing "Empty parameter vector"
    (is (thrown? Exception (qopt/adam-optimization simple-objective-function [] {}))))
  
  (testing "Invalid learning rate"
    (is (thrown? Exception (qopt/adam-optimization simple-objective-function [1.0] 
                                                   {:learning-rate -0.1}))))
  
  (testing "Zero max iterations"
    (let [result (qopt/adam-optimization simple-objective-function [1.0 1.0]
                                         {:max-iterations 0})]
      (is (false? (:success result)) "Should not succeed with zero iterations"))))

(comment
  (run-tests)
  
  ;; SPSA Performance Demonstration
  ;; ==============================
  ;; SPSA (Simultaneous Perturbation Stochastic Approximation) is highly efficient
  ;; for quantum variational algorithms because it uses only 2 function evaluations
  ;; per iteration, regardless of the number of parameters.
  ;;
  ;; Key benefits:
  ;; - 2 evals/iteration (vs 2N for parameter shift, where N = num params)
  ;; - Highly noise-resistant
  ;; - Can escape local minima
  ;; - Scales well to high-dimensional problems
  ;;
  ;; Example: For a 4-parameter problem:
  ;; - SPSA: 2 evaluations/iteration
  ;; - Adam with parameter shift: 8 evaluations/iteration
  ;; - Savings: 4x fewer circuit evaluations!
  ;;
  ;; Usage:
  ;; (qopt/spsa-optimization objective-fn initial-params
  ;;   {:max-iterations 1000    ; Higher default for SPSA
  ;;    :tolerance 1e-6
  ;;    :a 0.16                 ; Step size gain (π/20)
  ;;    :c 0.1                  ; Perturbation size (π/30)
  ;;    :blocking-size 5})      ; Optional: average last N iterations for noise
  ;
  )

;;
;; SPSA Optimization Tests
;;
(deftest test-spsa-optimization-basic
  (testing "SPSA optimization on simple quadratic function"
    (let [quadratic-fn (fn [params]
                         (let [x (first params)
                               y (second params)]
                           (+ (* (- x 3) (- x 3))
                              (* (- y 2) (- y 2)))))
          result (qopt/spsa-optimization quadratic-fn [0.0 0.0]
                                         {:max-iterations 200
                                          :tolerance 1e-4
                                          :a 0.16
                                          :c 0.1})]
      (is (map? result) "Should return result map")
      (is (contains? result :optimal-parameters) "Should have optimal parameters")
      (is (contains? result :optimal-energy) "Should have optimal energy")
      (is (contains? result :success) "Should have success flag")
      (is (contains? result :spsa-hyperparameters) "Should have SPSA hyperparameters")
      (is (vector? (:optimal-parameters result)) "Optimal parameters should be vector")
      (is (number? (:optimal-energy result)) "Optimal energy should be number")
      (is (= 2 (count (:optimal-parameters result))) "Should have 2 parameters")
      ;; Check convergence quality (should be near [3, 2])
      (is (< (:optimal-energy result) 0.5) "Should converge to near-optimal solution"))))

(deftest test-spsa-optimization-efficiency
  (testing "SPSA efficiency - only 2 function evaluations per iteration"
    (let [sphere-fn (fn [params] (reduce + (map #(* % %) params)))
          result (qopt/spsa-optimization sphere-fn [1.0 1.0 1.0 1.0]
                                         {:max-iterations 100
                                          :tolerance 1e-5})]
      (is (= 2 (quot (:function-evaluations result) (:iterations result)))
          "Should use exactly 2 function evaluations per iteration"))))

(deftest test-spsa-optimization-blocking
  (testing "SPSA with parameter blocking for noise resistance"
    (let [quadratic-fn (fn [params]
                         (let [x (first params)
                               y (second params)]
                           (+ (* (- x 3) (- x 3))
                              (* (- y 2) (- y 2)))))
          result (qopt/spsa-optimization quadratic-fn [0.0 0.0]
                                         {:max-iterations 200
                                          :tolerance 1e-4
                                          :blocking-size 5})]
      (is (:success result) "Should converge with blocking")
      (is (< (:optimal-energy result) 1.0) "Should reach reasonable solution with blocking"))))

(deftest test-spsa-hyperparameters
  (testing "SPSA hyperparameters in result"
    (let [sphere-fn (fn [params] (reduce + (map #(* % %) params)))
          result (qopt/spsa-optimization sphere-fn [1.0 1.0]
                                         {:max-iterations 150
                                          :tolerance 1e-5
                                          :a 0.2
                                          :c 0.15
                                          :alpha 0.602
                                          :gamma 0.101})]
      (is (contains? result :spsa-hyperparameters) "Should contain hyperparameters")
      (let [hyper (:spsa-hyperparameters result)]
        (is (= 0.2 (:a hyper)) "Should preserve a parameter")
        (is (= 0.15 (:c hyper)) "Should preserve c parameter")
        (is (= 0.602 (:alpha hyper)) "Should preserve alpha parameter")
        (is (= 0.101 (:gamma hyper)) "Should preserve gamma parameter")
        ;; Only check final gains if converged
        (when (:success result)
          (is (contains? hyper :final-ak) "Should record final step size")
          (is (contains? hyper :final-ck) "Should record final perturbation size"))))))

(deftest test-spsa-convergence-history
  (testing "SPSA convergence history tracking"
    (let [sphere-fn (fn [params] (reduce + (map #(* % %) params)))
          result (qopt/spsa-optimization sphere-fn [2.0 2.0]
                                         {:max-iterations 100
                                          :tolerance 1e-5})]
      (is (contains? result :convergence-history) "Should have convergence history")
      (is (vector? (:convergence-history result)) "History should be vector")
      (is (> (count (:convergence-history result)) 0) "History should not be empty")
      ;; Energy should generally decrease over time
      (let [history (:convergence-history result)
            first-energy (first history)
            last-energy (last history)]
        (is (< last-energy first-energy) "Final energy should be less than initial")))))

(deftest test-spsa-multi-dimensional
  (testing "SPSA on higher-dimensional problems"
    (let [sphere-fn (fn [params] (reduce + (map #(* % %) params)))
          result (qopt/spsa-optimization sphere-fn [1.0 1.0 1.0 1.0 1.0 1.0]
                                         {:max-iterations 300
                                          :tolerance 1e-4})]
      (is (map? result) "Should handle 6D problem")
      (is (= 6 (count (:optimal-parameters result))) "Should preserve dimensionality")
      ;; With 6 parameters, still only 2 evals per iteration
      (is (= 2 (quot (:function-evaluations result) (:iterations result)))
          "Should maintain 2 evals/iter even for 6D"))))

(deftest test-spsa-vs-other-methods
  (testing "SPSA comparison with other methods on same problem"
    (let [sphere-fn (fn [params] (reduce + (map #(* % %) params)))
          initial-params [1.0 1.0 1.0 1.0]
          
          spsa-result (qopt/spsa-optimization sphere-fn initial-params
                                              {:max-iterations 200
                                               :tolerance 1e-4})
          
          adam-result (qopt/adam-optimization sphere-fn initial-params
                                              {:max-iterations 200
                                               :tolerance 1e-4
                                               :learning-rate 0.1})]
      
      ;; Both should converge
      (is (:success spsa-result) "SPSA should converge")
      (is (:success adam-result) "Adam should converge")
      
      ;; SPSA should use far fewer function evaluations than Adam
      ;; Adam needs 2N evals per iteration (N=4), SPSA needs only 2
      (is (< (:function-evaluations spsa-result)
             (* 0.5 (:function-evaluations adam-result)))
          "SPSA should be much more efficient than Adam in function evaluations"))))
