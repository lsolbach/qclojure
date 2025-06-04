(ns org.soulspace.qclojure.application.shors-algorithm-test
  "Tests for Shor's factoring algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithms :as qa]
            [org.soulspace.qclojure.domain.math :as qmath]))

;; Test Shor's Algorithm with small composite numbers
#_(deftest test-shors-algorithm
  (testing "Factoring N=15"
    (let [N 15
          expected-factors #{3 5}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 5")))
  
  (testing "Factoring N=21"
    (let [N 21
          expected-factors #{3 7}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 7")))
  
  (testing "Factoring even number N=14"
    (let [N 14
          expected-factors #{2 7}
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 2 and 7")
      (is (= :classical-even (:method result)) "Should use classical method for even numbers")))
  
  (testing "Perfect power factorization N=27"
    (let [N 27  ; 3^3
          result (qa/shor-algorithm N)]
      
      (is (:success result) "Factorization should succeed")
      (is (= [3 3 3] (:factors result)) "Should find factors [3 3 3]"))))

(comment 
  (run-tests)

  ;
  )