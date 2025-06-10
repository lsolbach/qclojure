(ns org.soulspace.qclojure.application.algorithms-test
  "Tests for quantum algorithms in the application layer"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.application.algorithms :as qa]
            [org.soulspace.qclojure.application.algorithm.deutsch :as deutsch]
            [org.soulspace.qclojure.application.algorithm.grover :as grover]
            [org.soulspace.qclojure.application.algorithm.bernstein-vazirani :as bv]
            [org.soulspace.qclojure.application.algorithm.simon :as simon]
            [org.soulspace.qclojure.application.algorithm.quantum-phase-estimation :as qpe]
            [org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft]
            [org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf]
            [org.soulspace.qclojure.application.algorithm.modular-arithmetic :as ma]
            [org.soulspace.qclojure.application.algorithm.shor :as shor]
            [org.soulspace.qclojure.adapter.backend.simulator :as sim]
            [clojure.string :as s]))

;; Test continued fraction expansion
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

;; Test convergents calculation
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

;; Test Deutsch Algorithm
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

;; Test Grover's Algorithm
(deftest test-grover-algorithm
  (testing "Grover search for single target"
    (let [target-item 3
          search-size 8
          oracle-fn #(= % target-item)
          result (grover/grover-algorithm search-size oracle-fn (sim/create-simulator))]
      (is (contains? result :measurements))
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
          result (grover/grover-algorithm 8 oracle-fn (sim/create-simulator))]
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

;; Test Simon's Algorithm  
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

;; Test Quantum Phase Estimation
(deftest test-quantum-phase-estimation
  (testing "QPE estimates simple phases correctly"
    (let [phases [0.0 0.25 0.5 0.75]
          test-phase (fn [φ]
                       (let [result (qpe/quantum-phase-estimation (sim/create-simulator) φ 4)]
                         (is (= (:actual-phase result) φ))
                         (is (contains? result :estimated-phase))
                         (is (contains? result :error))
                         (is (>= (:estimated-phase result) 0))
                         (is (< (:estimated-phase result) 1))))]
      (doseq [φ phases]
        (test-phase φ))))
  
  (testing "QPE precision increases with more qubits"
    (let [phase 0.375  ; 3/8
          result-3bit (qpe/quantum-phase-estimation (sim/create-simulator) phase 3)
          result-6bit (qpe/quantum-phase-estimation (sim/create-simulator) phase 6)]
      (is (<= (:error result-6bit) (:error result-3bit)))
      (is (= (:precision-qubits result-3bit) 3))
      (is (= (:precision-qubits result-6bit) 6))))
  
  (testing "QPE includes proper algorithm metadata"
    (let [result (qpe/quantum-phase-estimation (sim/create-simulator) 0.125 4)]
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

(def quantum-phase-estimation-bounds
  (prop/for-all [phase (gen/such-that #(and (not (Double/isNaN %)) 
                                            (>= % 0.0) 
                                            (< % 1.0))
                                      (gen/double* {:min 0.0 :max 0.999 :NaN? false :infinite? false}))
                 precision-bits (gen/choose 2 6)]
    (let [result (qpe/quantum-phase-estimation (sim/create-simulator) phase precision-bits)]
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

          deutsch-result (deutsch/deutsch-algorithm (sim/create-simulator) constant-fn)
          grover-result (grover/grover-algorithm 8 oracle-fn (sim/create-simulator))
          bv-result (bv/bernstein-vazirani-algorithm (sim/create-simulator) hidden-string)
          simon-result (simon/simon-algorithm (sim/create-simulator) hidden-string)
          qpe-result (qpe/quantum-phase-estimation (sim/create-simulator) phase 4)]

      ;; Ensure all algorithms return valid results
      (is (contains? deutsch-result :result))
      (is (contains? grover-result :measurements))
      (is (contains? bv-result :success))
      (is (contains? simon-result :found-period))
      (is (contains? qpe-result :estimated-phase)))))

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
                   (grover/grover-algorithm 4 #(= % 1) (sim/create-simulator))
                   (bv/bernstein-vazirani-algorithm (sim/create-simulator) [1 0])
                   (simon/simon-algorithm (sim/create-simulator) [1 0])
                   (qpe/quantum-phase-estimation (sim/create-simulator) 0.5 3)]]
      
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
  (test-quantum-phase-estimation)

  ;; Run property-based tests
  (tc/quick-check 50 deutsch-algorithm-deterministic)
  (tc/quick-check 20 bernstein-vazirani-correctness)

  ;; Manual algorithm verification
  (deutsch/deutsch-algorithm (sim/create-simulator) (constantly true))
  (grover/grover-algorithm 8 #(= % 3) (sim/create-simulator))
  (bv/bernstein-vazirani-algorithm (sim/create-simulator) [1 0 1 0])
  (simon/simon-algorithm (sim/create-simulator) [1 0 1])
  (qpe/quantum-phase-estimation (sim/create-simulator) 0.375 4)

  (shor/shor-algorithm (sim/create-simulator) 14)
  (shor/shor-algorithm (sim/create-simulator) 15)
  (shor/shor-algorithm (sim/create-simulator) 21)
  (shor/shor-algorithm (sim/create-simulator) 77)
  ;
  )

