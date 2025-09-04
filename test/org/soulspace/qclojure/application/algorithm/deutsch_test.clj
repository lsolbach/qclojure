(ns org.soulspace.qclojure.application.algorithm.deutsch-test
  "Tests for the Deutsch algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithm.deutsch :as deutsch]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]))

;;
;; Test Deutsch Algorithm
;;
(deftest test-deutsch-algorithm
  (testing "Deutsch algorithm correctly identifies constant functions"
    (let [simulator (sim/create-simulator)
          constant-true (constantly true)
          constant-false (constantly false)
          result-true (deutsch/deutsch-algorithm simulator constant-true)
          result-false (deutsch/deutsch-algorithm simulator constant-false)]
      (is (= (:result result-true) :constant))
      (is (= (:result result-false) :constant))))
  
  (testing "Deutsch algorithm correctly identifies balanced functions"
    (let [simulator (sim/create-simulator)
          identity-fn identity
          not-fn (comp not boolean)
          result-id (deutsch/deutsch-algorithm simulator identity-fn)
          result-not (deutsch/deutsch-algorithm simulator not-fn)]
      (is (= (:result result-id) :balanced))
      (is (= (:result result-not) :balanced))))
  
  (testing "Deutsch algorithm includes proper metadata"
    (let [simulator (sim/create-simulator)
          result (deutsch/deutsch-algorithm simulator identity)]
      (is (contains? result :circuit))
      (is (contains? result :execution-result)))))

(comment
  ;; Run all tests in namespace
  (run-tests)

  ;
  )
