(ns org.soulspace.qclojure.application.algorithms-test
  "Tests for quantum algorithms in the application layer"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.application.algorithm.deutsch :as deutsch]
            [org.soulspace.qclojure.application.algorithm.grover :as grover]
            [org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv]
            [org.soulspace.qclojure.application.algorithm.simon :as simon]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]))

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
      (is (= (:result result-false) :constant))
      (is (= (:measurement-outcome result-true) 0))
      (is (= (:measurement-outcome result-false) 0))))
  
  (testing "Deutsch algorithm correctly identifies balanced functions"
    (let [simulator (sim/create-simulator)
          identity-fn identity
          not-fn (comp not boolean)
          result-id (deutsch/deutsch-algorithm simulator identity-fn)
          result-not (deutsch/deutsch-algorithm simulator not-fn)]
      (is (= (:result result-id) :balanced))
      (is (= (:result result-not) :balanced))
      (is (= (:measurement-outcome result-id) 1))
      (is (= (:measurement-outcome result-not) 1))))
  
  (testing "Deutsch algorithm includes proper metadata"
    (let [simulator (sim/create-simulator)
          result (deutsch/deutsch-algorithm simulator identity)]
      (is (contains? result :circuit))
      (is (contains? result :execution-result))
      (is (contains? result :oracle-function))
      (is (fn? (:oracle-function result))))))

;;
;; Test Grover's Algorithm
;;
(deftest test-grover-algorithm
  (testing "Grover search for single target"
    (let [target-item 3
          search-size 8
          oracle-fn #(= % target-item)
          result (grover/grover-algorithm (sim/create-simulator) search-size oracle-fn)]
      (is (contains? result :result))
      (is (contains? result :target-indices))
      (is (contains? result :probability))
      (is (= (:search-space-size result) search-size))
      (is (fn? (:oracle-function result)))))
  
  (testing "Grover search iterations calculation"
    (let [n4-optimal (grover/optimal-grover-iterations 4 1)
          n16-optimal (grover/optimal-grover-iterations 16 1)]
      ;; For N=4, optimal is π√N/4 ≈ π√4/4 = π/2 ≈ 1.57 → 2 iterations
      (is (>= n4-optimal 1))
      (is (<= n4-optimal 3))
      ;; For N=16, optimal is π√16/4 = π ≈ 3.14 → 3 iterations  
      (is (>= n16-optimal 2))
      (is (<= n16-optimal 4))))
  
  (testing "Grover search with multiple targets"
    (let [oracle-fn #(or (= % 1) (= % 3) (= % 5))
          result (grover/grover-algorithm (sim/create-simulator) 8 oracle-fn)]
      (is (>= (:probability result) 0.0))
      (is (<= (:probability result) 1.0)))))

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
    (let [periods [[1 0] [1 1 0 1] [0 1 0 1 0]]
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
(def deutsch-algorithm-deterministic
  (prop/for-all [input gen/boolean]
    (let [constant-fn (constantly input)
          simulator (sim/create-simulator)
          result (deutsch/deutsch-algorithm simulator constant-fn)]
      (= (:result result) :constant))))

(def bernstein-vazirani-correctness
  (prop/for-all [hidden-string (gen/not-empty (gen/vector (gen/elements [0 1]) 1 6))]
    (let [result (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string)]
      (= (:hidden-string result) hidden-string))))

(def simon-algorithm-valid-structure
  (prop/for-all [period-length (gen/choose 2 4)]
    (let [period (vec (concat [1] (repeatedly (dec period-length) #(rand-int 2))))
          result (simon/simon-algorithm (sim/create-simulator) period)]
      (and (= (:hidden-period result) period)
           (= (count (:measurements result)) (dec period-length))
           (= (:algorithm result) "Simon")))))

;; Integration tests
(deftest test-algorithm-integration
  (testing "All algorithms can be run in sequence"
    (let [constant-fn (constantly true)
          oracle-fn #(= % 2)
          hidden-string [1 0 1]

          deutsch-result (deutsch/deutsch-algorithm (sim/create-simulator) constant-fn)
          grover-result (grover/grover-algorithm (sim/create-simulator) 8 oracle-fn)
          bv-result (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string)
          simon-result (simon/simon-algorithm (sim/create-simulator) hidden-string)]

      ;; Ensure all algorithms return valid results
      (is (contains? deutsch-result :result))
      (is (contains? grover-result :result))
      (is (contains? bv-result :result))
      (is (contains? simon-result :result)))))

;; Benchmark and performance tests  
(deftest test-algorithm-performance
  (testing "Grover iterations scale with square root"
    (let [n4-iter (grover/optimal-grover-iterations 4 1)
          n16-iter (grover/optimal-grover-iterations 16 1)
          n64-iter (grover/optimal-grover-iterations 64 1)]
      ;; Check that iterations scale roughly as √N
      (is (< n4-iter (* 2 n16-iter)))   ; 4 vs 16 → 2x vs 4x
      (is (< n16-iter (* 2 n64-iter))))) ; 16 vs 64 → 4x vs 8x
  
  (testing "Algorithm complexity metadata is consistent"
    (let [results [(deutsch/deutsch-algorithm (sim/create-simulator) identity)
                   (grover/grover-algorithm (sim/create-simulator) 4 #(= % 1))
                   (bv/bernstein-vazirani-algorithm (sim/create-simulator) [1 0])
                   (simon/simon-algorithm (sim/create-simulator) [1 0])]]
      
      ;; All results should have algorithm names
      (doseq [result results]
        (is (or (contains? result :algorithm)
                (contains? result :circuit)))))))

(comment
  ;; REPL testing examples

  ;; Run all tests
  (run-tests)

  ;; Run specific test categories
  (test-deutsch-algorithm)
  (test-grover-algorithm)
  (test-bernstein-vazirani-algorithm)
  (test-simon-algorithm)

  ;; Run property-based tests
  (tc/quick-check 50 deutsch-algorithm-deterministic)
  (tc/quick-check 20 bernstein-vazirani-correctness)

  ;; Manual algorithm verification
  (deutsch/deutsch-algorithm (sim/create-simulator) (constantly true))
  (grover/grover-algorithm (sim/create-simulator) 8 #(= % 3))
  (bv/bernstein-vazirani-algorithm (sim/create-simulator) [1 0 1 0])
  (simon/simon-algorithm (sim/create-simulator) [1 0 1])

  ;
  )

