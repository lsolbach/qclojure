(ns org.soulspace.qclojure.application.quantum-algorithms-test
  "Tests for quantum algorithms in the application layer"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.application.quantum-algorithms :as qa]
            [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-gate :as qg]
            [org.soulspace.qclojure.domain.math :as qmath]
            [fastmath.core :as m]
            [fastmath.complex :as fc]))

;; Test Deutsch Algorithm
(deftest test-deutsch-algorithm
  (testing "Deutsch algorithm correctly identifies constant functions"
    (let [constant-true (constantly true)
          constant-false (constantly false)
          result-true (qa/deutsch-algorithm constant-true)
          result-false (qa/deutsch-algorithm constant-false)]
      (is (= (:result result-true) :constant))
      (is (= (:result result-false) :constant))
      (is (= (:measurement-outcome result-true) 0))
      (is (= (:measurement-outcome result-false) 0))))
  
  (testing "Deutsch algorithm correctly identifies balanced functions"
    (let [identity-fn identity
          not-fn (comp not boolean)
          result-id (qa/deutsch-algorithm identity-fn)
          result-not (qa/deutsch-algorithm not-fn)]
      (is (= (:result result-id) :balanced))
      (is (= (:result result-not) :balanced))
      (is (= (:measurement-outcome result-id) 1))
      (is (= (:measurement-outcome result-not) 1))))
  
  (testing "Deutsch algorithm includes proper metadata"
    (let [result (qa/deutsch-algorithm identity)]
      (is (contains? result :circuit))
      (is (contains? result :final-state))
      (is (contains? result :oracle-function))
      (is (fn? (:oracle-function result))))))

;; Test Grover's Algorithm
(deftest test-grover-algorithm
  (testing "Grover search for single target"
    (let [target-item 3
          search-size 8
          oracle-fn #(= % target-item)
          result (qa/grover-algorithm search-size oracle-fn)]
      (is (contains? result :measurements))
      (is (contains? result :target-indices))
      (is (contains? result :probability))
      (is (= (:search-space-size result) search-size))
      (is (fn? (:oracle-function result)))))
  
  (testing "Grover search iterations calculation"
    (let [n4-optimal (qa/optimal-grover-iterations 4 1)
          n16-optimal (qa/optimal-grover-iterations 16 1)]
      ;; For N=4, optimal is π√N/4 ≈ π√4/4 = π/2 ≈ 1.57 → 2 iterations
      (is (>= n4-optimal 1))
      (is (<= n4-optimal 3))
      ;; For N=16, optimal is π√16/4 = π ≈ 3.14 → 3 iterations  
      (is (>= n16-optimal 2))
      (is (<= n16-optimal 4))))
  
  (testing "Grover search with multiple targets"
    (let [oracle-fn #(or (= % 1) (= % 3) (= % 5))
          result (qa/grover-algorithm 8 oracle-fn)]
      (is (>= (:probability result) 0.0))
      (is (<= (:probability result) 1.0)))))

;; Test Bernstein-Vazirani Algorithm
(deftest test-bernstein-vazirani-algorithm
  (testing "BV algorithm finds hidden string correctly"
    (let [hidden-strings [[1 0 1 0]
                          [1 1 0 1 1]
                          [0 1 0]
                          [1]]
          test-hidden-string (fn [s]
                               (let [result (qa/bernstein-vazirani-algorithm s)]
                                 (is (= (:hidden-string result) s))
                                 (is (contains? result :success))
                                 (is (contains? result :algorithm))
                                 (is (= (:algorithm result) "Bernstein-Vazirani"))))]
      (doseq [s hidden-strings]
        (test-hidden-string s))))
  
  (testing "BV algorithm includes circuit information"
    (let [result (qa/bernstein-vazirani-algorithm [1 0 1])]
      (is (contains? result :circuit))
      (is (= (get-in result [:circuit :name]) "Bernstein-Vazirani"))
      (is (contains? (:circuit result) :qubits))
      (is (contains? (:circuit result) :operations)))))

;; Test Simon's Algorithm  
(deftest test-simon-algorithm
  (testing "Simon's algorithm structure and metadata"
    (let [hidden-period [1 0 1]
          result (qa/simon-algorithm hidden-period 3)]
      (is (= (:hidden-period result) hidden-period))
      (is (contains? result :measurements))
      (is (contains? result :found-period))
      (is (contains? result :linear-system))
      (is (= (:algorithm result) "Simon"))
      (is (contains? result :complexity))))
  
  (testing "Simon's algorithm with different period lengths"
    (let [periods [[1 0] [1 1 0 1] [0 1 0 1 0]]
          test-period (fn [p]
                        (let [result (qa/simon-algorithm p (count p))]
                          (is (= (:hidden-period result) p))
                          (is (= (count (:measurements result)) (dec (count p))))))]
      (doseq [p periods]
        (test-period p))))
  
  (testing "Simon's algorithm complexity information"
    (let [result (qa/simon-algorithm [1 0 1] 3)]
      (is (= (get-in result [:complexity :classical]) "O(2^(n/2))"))
      (is (= (get-in result [:complexity :quantum]) "O(n)"))
      (is (= (get-in result [:complexity :speedup]) "Exponential")))))

;; Test Quantum Phase Estimation
(deftest test-quantum-phase-estimation
  (testing "QPE estimates simple phases correctly"
    (let [phases [0.0 0.25 0.5 0.75]
          test-phase (fn [φ]
                       (let [result (qa/quantum-phase-estimation φ 4)]
                         (is (= (:actual-phase result) φ))
                         (is (contains? result :estimated-phase))
                         (is (contains? result :error))
                         (is (>= (:estimated-phase result) 0))
                         (is (< (:estimated-phase result) 1))))]
      (doseq [φ phases]
        (test-phase φ))))
  
  (testing "QPE precision increases with more qubits"
    (let [phase 0.375  ; 3/8
          result-3bit (qa/quantum-phase-estimation phase 3)
          result-6bit (qa/quantum-phase-estimation phase 6)]
      (is (<= (:error result-6bit) (:error result-3bit)))
      (is (= (:precision-qubits result-3bit) 3))
      (is (= (:precision-qubits result-6bit) 6))))
  
  (testing "QPE includes proper algorithm metadata"
    (let [result (qa/quantum-phase-estimation 0.125 4)]
      (is (= (:algorithm result) "Quantum Phase Estimation"))
      (is (contains? result :complexity))
      (is (contains? result :circuit))
      (is (= (get-in result [:circuit :qubits]) 5)))))  ; 4 counting + 1 eigenstate

;; Test Shor's Algorithm
#_(deftest test-shor-algorithm
  (testing "Shor's algorithm can factor small composite numbers"
    (let [;; Test with N = 15 (3 * 5)
          result-15 (qa/shor-algorithm 15)]
      (is (= (count (:factors result-15)) 2))
      (is (contains? (set (:factors result-15)) 3))
      (is (contains? (set (:factors result-15)) 5))
      (is (:success result-15))
      (is (= (:N result-15) 15))))
  
  (testing "Shor's algorithm handles even numbers trivially"
    (let [result-14 (qa/shor-algorithm 14)]
      (is (= (count (:factors result-14)) 2))
      (is (contains? (set (:factors result-14)) 2))
      (is (contains? (set (:factors result-14)) 7))
      (is (:success result-14))
      (is (= (:method result-14) :classical-even))))
  
  (testing "Shor's algorithm correctly implements period finding"
    (let [;; We'll test period finding directly with known values
          ;; For a = 2, N = 15, the period should be 4 (since 2^4 mod 15 = 1)
          period-result (qa/quantum-period-finding 2 15 8)]
      ;; The period finding might not be deterministic in all runs
      ;; due to quantum measurement randomness, but we can verify:
      (is (contains? period-result :measured-value))
      (is (contains? period-result :circuit))
      
      ;; If we found a period, check that it's valid
      (when-let [period (:estimated-period period-result)]
        (is (= 1 (qmath/mod-exp 2 period 15))
            "Verify that a^r mod N = 1 for the found period")))))

;; Property-based tests using test.check
(defspec deutsch-algorithm-deterministic 20
  (prop/for-all [input gen/boolean]
    (let [constant-fn (constantly input)
          result (qa/deutsch-algorithm constant-fn)]
      (= (:result result) :constant))))

(defspec bernstein-vazirani-correctness 10
  (prop/for-all [hidden-string (gen/not-empty (gen/vector (gen/elements [0 1]) 1 6))]
    (let [result (qa/bernstein-vazirani-algorithm hidden-string)]
      (= (:hidden-string result) hidden-string))))

(defspec simon-algorithm-valid-structure 10
  (prop/for-all [period-length (gen/choose 2 4)]
    (let [period (vec (concat [1] (repeatedly (dec period-length) #(rand-int 2))))
          result (qa/simon-algorithm period period-length)]
      (and (= (:hidden-period result) period)
           (= (count (:measurements result)) (dec period-length))
           (= (:algorithm result) "Simon")))))

(defspec quantum-phase-estimation-bounds 15
  (prop/for-all [phase (gen/such-that #(and (not (Double/isNaN %)) 
                                            (>= % 0.0) 
                                            (< % 1.0))
                                      (gen/double* {:min 0.0 :max 0.999 :NaN? false :infinite? false}))
                 precision-bits (gen/choose 2 6)]
    (let [result (qa/quantum-phase-estimation phase precision-bits)]
      (and (>= (:estimated-phase result) 0.0)
           (< (:estimated-phase result) 1.0)
           (= (:precision-qubits result) precision-bits)))))

;; Integration tests
(deftest test-algorithm-integration
  (testing "All algorithms can be run in sequence"
    (let [constant-fn (constantly true)
          oracle-fn #(= % 2)
          hidden-string [1 0 1]
          phase 0.25

          deutsch-result (qa/deutsch-algorithm constant-fn)
          grover-result (qa/grover-algorithm 8 oracle-fn)
          bv-result (qa/bernstein-vazirani-algorithm hidden-string)
          simon-result (qa/simon-algorithm hidden-string 3)
          qpe-result (qa/quantum-phase-estimation phase 4)]

      ;; Ensure all algorithms return valid results
      (is (contains? deutsch-result :result))
      (is (contains? grover-result :measurements))
      (is (contains? bv-result :success))
      (is (contains? simon-result :found-period))
      (is (contains? qpe-result :estimated-phase)))))

;; Benchmark and performance tests  
(deftest test-algorithm-performance
  (testing "Grover iterations scale with square root"
    (let [n4-iter (qa/optimal-grover-iterations 4 1)
          n16-iter (qa/optimal-grover-iterations 16 1) 
          n64-iter (qa/optimal-grover-iterations 64 1)]
      ;; Check that iterations scale roughly as √N
      (is (< n4-iter (* 2 n16-iter)))   ; 4 vs 16 → 2x vs 4x
      (is (< n16-iter (* 2 n64-iter))))) ; 16 vs 64 → 4x vs 8x
  
  (testing "Algorithm complexity metadata is consistent"
    (let [algorithms [qa/deutsch-algorithm
                      qa/grover-algorithm  
                      qa/bernstein-vazirani-algorithm
                      qa/simon-algorithm
                      qa/quantum-phase-estimation]
          results [(qa/deutsch-algorithm identity)
                   (qa/grover-algorithm 4 #(= % 1))
                   (qa/bernstein-vazirani-algorithm [1 0])
                   (qa/simon-algorithm [1 0] 2)
                   (qa/quantum-phase-estimation 0.5 3)]]
      
      ;; All results should have algorithm names
      (doseq [result results]
        (is (or (contains? result :algorithm)
                (contains? result :circuit)))))))

(defn run-all-algorithm-tests
  "Run all quantum algorithm tests."
  []
  (println "Running quantum algorithm tests...")
  (run-tests))

(comment
  ;; REPL testing examples
  
  ;; Run all tests
  (run-all-algorithm-tests)
  
  ;; Run specific test categories
  (test-deutsch-algorithm)
  (test-grover-algorithm)
  (test-bernstein-vazirani-algorithm)
  (test-simon-algorithm)
  (test-quantum-phase-estimation)
  
  ;; Run property-based tests
  (tc/quick-check 50 deutsch-algorithm-deterministic)
  (tc/quick-check 20 bernstein-vazirani-correctness)

  ;; Performance testing
  (time (qa/grover-algorithm 256 #(= % 100)))
  (time (qa/simon-algorithm [1 0 1 1 0] 5))
  
  ;; Manual algorithm verification
  (qa/deutsch-algorithm (constantly true))
  (qa/grover-algorithm 8 #(= % 3))
  (qa/bernstein-vazirani-algorithm [1 0 1 0])
  (qa/simon-algorithm [1 0 1] 3)
  (qa/quantum-phase-estimation 0.375 4)

  (qa/shor-algorithm 14)
  (qa/shor-algorithm 15)
  (qa/shor-algorithm 21)
  (qa/shor-algorithm 77)
  )
