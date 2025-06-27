(ns org.soulspace.qclojure.application.algorithm.simon-test
  "Tests for the Simon's algorithm implementation"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithm.simon :as simon]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]))

;;
;; Test Simon's Algorithm  
;;
(deftest test-simon-algorithm
  (testing "Simon's algorithm structure and metadata"
    (let [hidden-period [1 0 1]
          result (simon/simon-algorithm (sim/create-simulator) hidden-period)]
      (is (= (:hidden-period result) hidden-period))
      (is (contains? result :measurements))
      (is (contains? result :found-period))
      (is (contains? result :linear-system))
      (is (= (:algorithm result) "Simon"))
      (is (contains? result :complexity))))
  
  (testing "Simon's algorithm with different period lengths"
    (let [periods [[1 0] [1 1 0] [0 1 0 1]]
          test-period (fn [p]
                        (let [result (simon/simon-algorithm (sim/create-simulator) p)]
                          (is (= (:hidden-period result) p))
                          (is (= (count (:measurements result)) (dec (count p))))))]
      (doseq [p periods]
        (test-period p))))
  
  (testing "Simon's algorithm complexity information"
    (let [result (simon/simon-algorithm (sim/create-simulator) [1 0 1])]
      (is (= (get-in result [:complexity :classical]) "O(2^(n/2))"))
      (is (= (get-in result [:complexity :quantum]) "O(n)"))
      (is (= (get-in result [:complexity :speedup]) "Exponential")))))

;; Property-based tests using test.check
(def simon-algorithm-valid-structure
  (prop/for-all [period-length (gen/choose 2 4)]
    (let [period (vec (concat [1] (repeatedly (dec period-length) #(rand-int 2))))
          result (simon/simon-algorithm (sim/create-simulator) period)]
      (and (= (:hidden-period result) period)
           (= (count (:measurements result)) (dec period-length))
           (= (:algorithm result) "Simon")))))

(comment
  (run-tests)

  ;; Run property-based tests
  (tc/quick-check 10 simon-algorithm-valid-structure)
  ;
  )
