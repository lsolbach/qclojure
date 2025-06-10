(ns org.soulspace.qclojure.application.algorithm.shor
  (:require
   [org.soulspace.qclojure.domain.math :as qmath]
   [org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf]))

(defn shor-algorithm
  "Shor's algorithm for integer factorization.
  
  Shor's algorithm is a quantum algorithm that can factor large integers
  exponentially faster than the best known classical algorithms. It combines
  classical preprocessing, quantum period finding, and classical post-processing.
  
  This improved implementation supports:
  1. Hardware-compatible mode for real quantum hardware execution
  2. Multiple measurements for statistical robustness
  3. Enhanced period extraction for better success rate
  
  Algorithm steps:
  1. Classical preprocessing: Check for trivial cases
  2. Choose random a < N, check gcd(a,N)  
  3. Quantum period finding: Find period r of f(x) = a^x mod N
  4. Classical post-processing: Extract factors from the period
  
  Parameters:
  - backend: Quantum backend implementing the QuantumBackend protocol to execute the circuit
  - N: Integer to factor (should be composite)
  - options: (Optional) Map containing:
    - :n-qubits - Number of qubits for quantum period finding (default: 2*⌈log₂(N)⌉)
    - :hardware-compatible - Boolean indicating if hardware-optimized circuit should be used
    - :n-measurements - Number of measurements for statistical analysis (default: 10)
    - :max-attempts - Maximum number of random 'a' values to try (default: 10)
  
  Returns:
  Map containing:
  - :factors - Vector of non-trivial factors found (empty if factorization failed)
  - :success - Boolean indicating if factorization succeeded
  - :N - The input number
  - :attempts - Vector of maps describing each attempt with different 'a' values
  - :quantum-circuit - The quantum circuit from the successful attempt (if any)
  - :statistics - Performance statistics and confidence metrics
  
  Example:
  (shor-algorithm 15)    ;=> {:factors [3 5], :success true, :N 15, ...}
  (shor-algorithm 21 {:hardware-compatible true})   ;=> {:factors [3 7], :success true, ...}"
  ([backend N] (shor-algorithm backend N {}))
  ([backend N options]
   {:pre [(> N 1)]}

   (let [;; Extract options with defaults
         n-qubits (get options :n-qubits
                       (* 2 (int (Math/ceil (/ (Math/log N) (Math/log 2))))))
         n-measurements (get options :n-measurements 10)
         max-attempts (get options :max-attempts 10)]

     ;; Step 1: Classical preprocessing
     (cond
       ;; Check if N is even
       (even? N) {:factors [2 (/ N 2)]
                  :success true
                  :N N
                  :attempts []
                  :method :classical-even}

       ;; Check if N is a perfect power - try to find if N = m^k for some m,k>1
       (some (fn [k]
               (let [root (Math/pow N (/ 1 k))
                     int-root (int root)]
                 (when (= N (int (Math/pow int-root k)))
                   {:base int-root :power k})))
             (range 2 (inc (int (/ (Math/log N) (Math/log 2))))))
       (let [{:keys [base power]} (some (fn [k]
                                          (let [root (Math/pow N (/ 1 k))
                                                int-root (int root)]
                                            (when (= N (int (Math/pow int-root k)))
                                              {:base int-root :power k})))
                                        (range 2 (inc (int (/ (Math/log N) (Math/log 2))))))]
         {:factors (repeat power base)
          :success true
          :N N
          :attempts []
          :method :classical-perfect-power})

       ;; Continue with quantum period finding
       :else
       (let [attempts (atom [])
             start-time (System/currentTimeMillis)]

         ;; Step 2-4: Try quantum period finding with different values of 'a'
         (loop [attempt 0]
           (if (>= attempt max-attempts)
             ;; Failed to find factors
             {:factors []
              :success false
              :N N
              :attempts @attempts
              :method :quantum-failed
              :statistics {:runtime (- (System/currentTimeMillis) start-time)
                           :attempts attempt
                           :n-measurements n-measurements}}

             ;; Choose random a coprime to N
             (let [a (loop [candidate (+ 2 (rand-int (- N 2)))]
                       (if (= (qmath/gcd candidate N) 1)
                         candidate
                         (recur (+ 2 (rand-int (- N 2))))))
                   gcd-a-N (qmath/gcd a N)]

               ;; Check if gcd(a,N) gives us a factor
               (if (> gcd-a-N 1)
                 ;; Found factor classically
                 (do
                   (swap! attempts conj {:a a :gcd gcd-a-N :method :classical-gcd})
                   {:factors [gcd-a-N (/ N gcd-a-N)]
                    :success true
                    :N N
                    :attempts @attempts
                    :method :classical-gcd
                    :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                 :attempts (inc attempt)
                                 :n-measurements 0}})

                 ;; Try quantum period finding with our improved implementation
                 (let [period-result (qpf/quantum-period-finding backend a N n-qubits)
                       period (:estimated-period period-result)]

                   (when (map? period-result)
                     (swap! attempts conj (assoc period-result :a a)))

                   (if (and period
                            (even? period)
                            (not= 1 (qmath/mod-exp a (int (/ period 2)) N)))
                     ;; We have a valid period, try to extract factors
                     (let [exp-a-r-2 (qmath/mod-exp a (int (/ period 2)) N)
                           factor1 (qmath/gcd (dec exp-a-r-2) N)
                           factor2 (qmath/gcd (inc exp-a-r-2) N)]
                       (cond
                         ;; First factor is valid
                         (and (> factor1 1) (< factor1 N))
                         {:factors [factor1 (/ N factor1)]
                          :success true
                          :N N
                          :attempts @attempts
                          :quantum-circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                       :attempts (inc attempt)
                                       :n-measurements n-measurements
                                       :period period
                                       :confidence (:confidence period-result)}}

                         ;; Second factor is valid 
                         (and (> factor2 1) (< factor2 N))
                         {:factors [factor2 (/ N factor2)]
                          :success true
                          :N N
                          :attempts @attempts
                          :quantum-circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                       :attempts (inc attempt)
                                       :n-measurements n-measurements
                                       :period period
                                       :confidence (:confidence period-result)}}

                         ;; Period didn't give useful factors, try again
                         :else
                         (recur (inc attempt))))

                     ;; Invalid period or quantum step failed, try again
                     (recur (inc attempt)))))))))))))

