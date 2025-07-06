(ns org.soulspace.qclojure.application.algorithm.shor
  (:require
   [org.soulspace.qclojure.domain.math :as qmath]
   [org.soulspace.qclojure.application.algorithm.quantum-period-finding :as qpf]))

(defn generate-unique-coprime-values 
  "Generate all values a where 2 <= a < N and gcd(a,N) = 1, in random order.
   This ensures no duplicate values are attempted during factorization."
  [N]
  (->> (range 2 N)
       (filter #(= 1 (qmath/gcd % N)))
       (shuffle)))

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
  - :circuit - The quantum circuit from the successful attempt (if any)
  - :statistics - Performance statistics and confidence metrics
  
  Example:
  (shor-algorithm 15)    ;=> {:factors [3 5], :success true, :N 15, ...}
  (shor-algorithm 21 {:hardware-compatible true})   ;=> {:factors [3 7], :success true, ...}"
  ([backend N] (shor-algorithm backend N {}))
  ([backend N options]
   {:pre [(> N 1)]}

   (let [;; Extract options with defaults - use smaller defaults for better performance
         n-qubits (get options :n-qubits
                       (* 2 (int (Math/ceil (/ (Math/log N) (Math/log 2))))))
         n-measurements (get options :n-measurements 3)  ; Reduced from 10 to 3
         max-attempts (get options :max-attempts 5)      ; Reduced from 10 to 5
         ]

     ;; Step 1: Classical preprocessing
     (cond
       ;; Check if N is prime (can't be factored)
       (qmath/prime? N) {:factors []
                      :success false
                      :N N
                      :attempts []
                      :method :classical-prime-check
                      :message "Cannot factor prime numbers"
                      :statistics {:runtime 0
                                   :attempts 0
                                   :n-measurements 0}}

       ;; Check if N is even
       (even? N) {:factors [2 (/ N 2)]
                  :success true
                  :N N
                  :attempts []
                  :method :classical-even
                  :statistics {:runtime 0
                               :attempts 0
                               :n-measurements 0}}

       ;; Check if N is a perfect power
       (let [perfect-power-factor (qmath/perfect-power-factor N)]
         (> perfect-power-factor 1))
       (let [factor (qmath/perfect-power-factor N)]
         {:factors [factor (/ N factor)]
          :success true
          :N N
          :attempts []
          :method :classical-perfect-power})

       ;; Continue with quantum period finding
       :else
       (let [attempts (atom [])
             start-time (System/currentTimeMillis)
             ;; Generate unique coprime values to avoid trying the same 'a' multiple times
             coprime-values (generate-unique-coprime-values N)]

         ;; Step 2-4: Try quantum period finding with different values of 'a'
         (loop [attempt 0
                remaining-values coprime-values]
           (if (or (>= attempt max-attempts)
                   (empty? remaining-values))
             ;; Failed to find factors or exhausted all values
             {:factors []
              :result []
              :success false
              :N N
              :attempts @attempts
              :method (if (empty? remaining-values) :exhausted-values :quantum-failed)
              :statistics {:runtime (- (System/currentTimeMillis) start-time)
                           :attempts attempt
                           :n-measurements n-measurements
                           :total-coprime-values (count coprime-values)}}

             ;; Use the next unique a value
             (let [a (first remaining-values)
                   gcd-a-N (qmath/gcd a N)]

               ;; Check if gcd(a,N) gives us a factor
               (if (> gcd-a-N 1)
                 ;; Found factor classically
                 (do
                   (swap! attempts conj {:a a :gcd gcd-a-N :method :classical-gcd})
                   {:factors [gcd-a-N (/ N gcd-a-N)]
                    :result [gcd-a-N (/ N gcd-a-N)]
                    :success true
                    :N N
                    :attempts @attempts
                    :method :classical-gcd
                    :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                 :attempts (inc attempt)
                                 :n-measurements 0}})

                 ;; Try quantum period finding with our improved implementation
                 (let [period-result (qpf/quantum-period-finding backend a N n-qubits n-measurements options)
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
                          :result [factor1 (/ N factor1)]
                          :success true
                          :N N
                          :attempts @attempts
                          :circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                       :attempts (inc attempt)
                                       :n-measurements n-measurements
                                       :period period
                                       :confidence (:confidence period-result)}}

                         ;; Second factor is valid 
                         (and (> factor2 1) (< factor2 N))
                         {:factors [factor2 (/ N factor2)]
                          :result [factor2 (/ N factor2)]
                          :success true
                          :N N
                          :attempts @attempts
                          :circuit (:circuit period-result)
                          :method :quantum-period-finding
                          :statistics {:runtime (- (System/currentTimeMillis) start-time)
                                       :attempts (inc attempt)
                                       :n-measurements n-measurements
                                       :period period
                                       :confidence (:confidence period-result)}}

                         ;; Period didn't give useful factors, try again
                         :else
                         (recur (inc attempt) (rest remaining-values))))

                     ;; Invalid period or quantum step failed, try again
                     (recur (inc attempt) (rest remaining-values)))))))))))))

(defn complete-factorization
  "Perform complete prime factorization using Shor's algorithm recursively.
  
  Unlike the basic shor-algorithm which finds only one factorization,
  this function continues factoring until all factors are prime.
  
  Parameters:
  - backend: Quantum backend for executing quantum circuits
  - N: Integer to factor completely
  - options: (Optional) Options map passed to shor-algorithm
  
  Returns:
  Map containing:
  - :prime-factors - Vector of all prime factors (sorted)
  - :success - Boolean indicating if complete factorization succeeded
  - :N - The input number
  - :factorization-tree - Tree showing the factorization process
  - :total-attempts - Total number of Shor algorithm calls made
  - :statistics - Combined statistics from all factorization steps
  
  Example:
  (complete-factorization backend 27)   ;=> {:prime-factors [3 3 3], :success true, ...}
  (complete-factorization backend 15)   ;=> {:prime-factors [3 5], :success true, ...}"
  ([backend N] (complete-factorization backend N {}))
  ([backend N options]
   {:pre [(> N 1)]}
   
   (let [start-time (System/currentTimeMillis)
         total-attempts (atom 0)
         all-statistics (atom [])]
     
     (letfn [(factor-completely [n path]
               ;; Base case: if n is prime, return it
               (if (qmath/prime? n)
                 {:factors [n]
                  :tree {:value n :prime true :path path}}
                 
                 ;; Factor n using Shor's algorithm
                 (let [result (shor-algorithm backend n options)]
                   (swap! total-attempts inc)
                   (swap! all-statistics conj (:statistics result))
                   
                   (if (:success result)
                     ;; Successfully factored, now recursively factor each part
                     (let [[f1 f2] (:factors result)
                           left-result (factor-completely f1 (conj path :left))
                           right-result (factor-completely f2 (conj path :right))]
                       {:factors (concat (:factors left-result) (:factors right-result))
                        :tree {:value n
                               :prime false
                               :path path
                               :factorization [f1 f2]
                               :method (:method result)
                               :left (:tree left-result)
                               :right (:tree right-result)}})
                     
                     ;; Factorization failed
                     {:factors []
                      :tree {:value n :prime nil :failed true :path path}}))))]
       
       (let [result (factor-completely N [])
             end-time (System/currentTimeMillis)]
         {:prime-factors (sort (:factors result))
          :result (sort (:factors result))
          :success (seq (:factors result))
          :N N
          :factorization-tree (:tree result)
          :total-attempts @total-attempts
          :statistics {:runtime (- end-time start-time)
                       :total-shor-calls @total-attempts
                       :individual-results @all-statistics}})))))


(comment
  
  (require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])
  (shor-algorithm (sim/create-simulator) 5 {:shots 1})

  ;
  )
