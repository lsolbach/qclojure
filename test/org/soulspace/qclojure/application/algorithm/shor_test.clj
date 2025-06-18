(ns org.soulspace.qclojure.application.algorithm.shor-test
  "Tests for Shor's factoring algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.algorithm.shor :as shor]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

;; Test Shor's Algorithm with small composite numbers
(deftest test-shors-algorithm
  #_(testing "Factoring N=15"
    (let [N 15
          expected-factors #{3 5}
          result (shor/shor-algorithm (sim/create-simulator) N {:shots 50})]
      
      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 5")))
  
  #_(testing "Factoring N=21"
    (let [N 21
          expected-factors #{3 7}
          result (shor/shor-algorithm (sim/create-simulator) N {:shots 10})]

      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 3 and 7")))
  
  #_(testing "Factoring even number N=14"
    (let [N 14
          expected-factors #{2 7}
          result (shor/shor-algorithm (sim/create-simulator) N {:shots 10})]

      (is (:success result) "Factorization should succeed")
      (is (= 2 (count (:factors result))) "Should find 2 factors")
      (is (= expected-factors (set (:factors result))) "Should find factors 2 and 7")
      (is (= :classical-even (:method result)) "Should use classical method for even numbers")))
  
  (testing "Perfect power factorization N=27"
    (let [N 27  ; 3^3
          result (shor/shor-algorithm (sim/create-simulator) N {:shots 10})]

      (is (:success result) "Factorization should succeed")
      (is (= [3 9] (:factors result)) "Should find factors [3 3 3]"))))

(comment 
  (run-tests)

  ;
  )