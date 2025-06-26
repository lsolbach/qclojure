(ns org.soulspace.qclojure.application.algorithm.bernstein-vazirani-test
  "Tests for the Bernstein-Vazirani algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]))

;;
;; Test Bernstein-Vazirani Algorithm
;;
(deftest test-bernstein-vazirani-algorithm
  (testing "BV algorithm finds hidden string correctly"
    (let [hidden-strings [[1 0 1 0]
                          [1 1 0 1 1]
                          [0 1 0]
                          [1]]
          test-hidden-string (fn [s]
                               (let [result (bv/bernstein-vazirani-algorithm (sim/create-simulator) s)]
                                 (is (= (:hidden-string result) s))
                                 (is (contains? result :success))
                                 (is (contains? result :algorithm))
                                 (is (= (:algorithm result) "Bernstein-Vazirani"))))]
      (doseq [s hidden-strings]
        (test-hidden-string s))))
  
  (testing "BV algorithm includes circuit information"
    (let [result (bv/bernstein-vazirani-algorithm (sim/create-simulator) [1 0 1])]
      (is (contains? result :circuit))
      (is (= (get-in result [:circuit :name]) "Bernstein-Vazirani"))
      (is (contains? (:circuit result) :qubits))
      (is (contains? (:circuit result) :operations)))))

;; Property-based tests using test.check
(def bernstein-vazirani-correctness
  (prop/for-all [hidden-string (gen/not-empty (gen/vector (gen/elements [0 1]) 1 6))]
    (let [result (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string)]
      (= (:hidden-string result) hidden-string))))

(comment
  (run-tests)

  ;; Run property-based tests
  (tc/quick-check 20 bernstein-vazirani-correctness)
  
  ;
  )
