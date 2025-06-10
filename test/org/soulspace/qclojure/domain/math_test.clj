(ns org.soulspace.qclojure.domain.math-test
  "Unit tests for mathematical operations in the qclojure domain"
  (:require [clojure.test :refer [deftest is testing]]
            [org.soulspace.qclojure.domain.math :as qmath]
            [fastmath.core :as fc]))

;;
;; Test continued fraction expansion
;;
(deftest test-continued-fraction
  (testing "Continued fraction expansion of simple rational numbers"
    ;; 3/2 = 1 + 1/2 => [1, 2]
    (is (= (qmath/continued-fraction 3 2) [1 2]))
    
    ;; 7/3 = 2 + 1/3 => [2, 3]
    (is (= (qmath/continued-fraction 7 3) [2 3])))
     
  (testing "Continued fraction handles integer inputs"
    (is (= (qmath/continued-fraction 5 1) [5]))
    (is (= (qmath/continued-fraction 0 1) [0])))

  (testing "Continued fraction with custom depth limit"
    ;; Should stop at specified depth
    (is (<= (count (qmath/continued-fraction 22 7 3)) 3))
    (is (<= (count (qmath/continued-fraction 355 113 5)) 5)))

  (testing "Continued fraction with epsilon precision"
    ;; With larger epsilon, should converge faster
    (let [cf-high-precision (qmath/continued-fraction 355 113 100 1e-15)
          cf-low-precision (qmath/continued-fraction 355 113 100 1e-6)]
      (is (>= (count cf-high-precision) (count cf-low-precision)))))
  
  (testing "Edge cases for continued fraction"
    ;; Zero numerator
    (is (= (qmath/continued-fraction 0 5) [0]))

    ;; Equal numerator and denominator
    (is (= (qmath/continued-fraction 7 7) [1]))))

;;
;; Test convergents calculation
;;
(deftest test-convergents
  (testing "Convergents of simple continued fractions"
    ;; CF [1, 2] should give convergents [[1, 1], [3, 2]]
    (let [convs (qmath/convergents [1 2])]
      (is (= convs [[1 1] [3 2]])))
    
    ;; CF [3, 7, 15] for 22/7 approximation
    (let [convs (qmath/convergents [3 7 15])]
      (is (= (first convs) [3 1]))
      (is (= (second convs) [22 7]))))
  
  (testing "Convergents for single-term continued fraction"
    (is (= (qmath/convergents [5]) [[5 1]])))

  (testing "Convergents for longer continued fractions"
    ;; Golden ratio φ = [1; 1, 1, 1, ...] 
    (let [golden-cf (repeat 5 1)
          convs (qmath/convergents golden-cf)]
      (is (= (count convs) 5))
      ;; First few convergents should be 1/1, 2/1, 3/2, 5/3, 8/5 (Fibonacci ratios)
      (is (= (first convs) [1 1]))
      (is (= (second convs) [2 1]))
      (is (= (nth convs 2) [3 2]))
      (is (= (nth convs 3) [5 3]))))
  
  (testing "Empty continued fraction"
    (is (= (qmath/convergents []) []))))

