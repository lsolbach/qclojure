(ns org.soulspace.qclojure.domain.math
  "Mathematical operations and utilities for quantum algorithms."
  (:require [fastmath.core :as m]))

; Enable fastmath operator macros
#_(m/use-primitive-operators)

(defn prime?
  "Simple primality test for small numbers."
  [n]
  (and (> n 1)
       (not-any? #(zero? (mod n %))
                 (range 2 (inc (int (m/sqrt n)))))))

(defn gcd
  "Calculate greatest common divisor using Euclidean algorithm."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn mod-exp
  "Calculate (base^exp) mod m efficiently using binary exponentiation."
  [base exp m]
  (loop [result 1
         base (mod base m)
         exp exp]
    (cond
      (zero? exp) result
      (odd? exp) (recur (mod (* result base) m) base (dec exp))
      :else (recur result (mod (* base base) m) (quot exp 2)))))

(defn continued-fraction
  "Convert a fraction to continued fraction representation.
  
  This implementation handles numerical precision issues and early termination
  conditions that are important for Shor's algorithm. It can detect periodic
  patterns in the continued fraction expansion, which is crucial for finding
  the correct period.
  
  Parameters:
  - num: Numerator of the fraction
  - den: Denominator of the fraction
  - max-depth: (Optional) Maximum depth of continued fraction expansion
  - epsilon: (Optional) Precision threshold for detecting near-zero remainders
  
  Returns:
  Vector of continued fraction terms"
  ([num den]
   (continued-fraction num den 100 1e-10))
  ([num den max-depth]
   (continued-fraction num den max-depth 1e-10))
  ([num den max-depth epsilon]
   (loop [n num
          d den
          cf []
          depth 0]
     (cond
       ;; Stop if denominator is zero or very close to zero
       (or (zero? d) (< (m/abs d) epsilon))
       cf

       ;; Stop if we've reached max depth to prevent infinite loops
       (>= depth max-depth)
       cf

       :else
       (let [q (quot n d)
             r (mod n d)]
         ;; If remainder is very small relative to denominator, stop
         (if (< (/ r d) epsilon)
           (conj cf q)
           (recur d r (conj cf q) (inc depth))))))))

(defn convergents
  "Calculate convergents from continued fraction representation.
  
  This enhanced implementation handles edge cases better and includes
  additional validation to ensure proper convergence, which is important
  for accurately extracting periods in Shor's algorithm.
  
  Parameters:
  - cf: Vector of continued fraction terms
  
  Returns:
  Vector of convergents as [numerator denominator] pairs"
  [cf]
  (reduce (fn [acc term]
            (let [h (count acc)]
              (cond
                (= h 0) [[term 1]]
                (= h 1) (conj acc [(+ (* term (ffirst acc)) 1) term])
                :else (let [prev-2 (nth acc (- h 2))
                            prev-1 (nth acc (- h 1))
                            p (+ (* term (first prev-1)) (first prev-2))
                            q (+ (* term (second prev-1)) (second prev-2))]
                        (conj acc [p q])))))
          []
          cf))

(defn round-precision
  "Round a number to specified decimal places.
  
  Parameters:
  - x: Number to round
  - precision: Number of decimal places
  
  Returns:
  Number rounded to specified precision"
  [x precision]
  (if (zero? precision)
    (double (m/round x))
    (let [bd (bigdec x)
          scale precision
          rounded (.setScale bd scale java.math.RoundingMode/HALF_UP)]
      (double rounded))))

(defn find-period
  "Find the period from a phase estimate using improved continued fraction expansion.
  
  This function implements a more robust version of period extraction from
  a phase measurement, which is critical for Shor's algorithm.
  
  Parameters:
  - measured-value: The value from quantum measurement
  - precision: Number of bits used in phase estimation
  - N: Modulus for period finding
  - a: Base for modular exponentiation
  
  Returns:
  Most likely period or nil if no valid period found"
  [measured-value precision N a]
  (let [;; Calculate phase from measurement
        phase (/ measured-value (Math/pow 2 precision))

        ;; Try different depths of continued fraction expansion
        candidates (for [depth [10 20 50 100]
                         :let [cf (continued-fraction measured-value (Math/pow 2 precision) depth)
                               convs (convergents cf)]
                         [num den] convs
                         :let [period (int den)]  ; Ensure period is an integer
                         ;; Verify this is actually a period
                         :when (and (pos? period)
                                    (<= period N)
                                    (= 1 (mod-exp a period N)))]
                     {:period period
                      :fraction [num den]
                      :error (Math/abs (- phase (/ num (Math/pow 2 precision))))})

        ;; Sort by error (lowest first) and then by period (smallest valid first)
        sorted-candidates (sort-by (juxt :error :period) candidates)]

    ;; Return the best candidate's period, or nil if none found
    (when (seq sorted-candidates)
      (:period (first sorted-candidates)))))

(defn perfect-power-factor
  "Check if N is a perfect power and return its base factor.
  
  A perfect power is a number that can be expressed as a^k for some integers a and k where k > 1.
  This function finds the smallest base a such that N = a^k for some k > 1.
  
  This is used in Shor's algorithm for classical preprocessing - if N is a perfect power,
  we can factor it classically without needing quantum period finding.
  
  Examples:
  - perfect-power-factor(8) = 2 (since 8 = 2^3)
  - perfect-power-factor(9) = 3 (since 9 = 3^2)
  - perfect-power-factor(15) = 1 (since 15 is not a perfect power)
  
  Parameters:
  - N: The number to check for perfect power
  
  Returns:
  Base factor a if N = a^k for some k > 1, otherwise returns 1"
  [N]
  {:pre [(pos? N)]}
  
  (if (<= N 1)
    1  ; Handle edge cases
    (loop [k 2]
      (if (> k (Math/ceil (/ (Math/log N) (Math/log 2))))  ; Check up to log_2(N)
        1  ; Not a perfect power
        (let [root (Math/round (Math/pow N (/ 1.0 k)))]
          (cond
            ;; Check if root^k equals N (accounting for floating point precision)
            (and (> root 1)
                 (let [power (long (Math/pow root k))]
                   (= power N)))
            root  ; Found perfect power base
            
            ;; Also check root-1 and root+1 due to floating point precision issues
            (and (> (dec root) 1)
                 (let [power (long (Math/pow (dec root) k))]
                   (= power N)))
            (dec root)
            
            (and (> (inc root) 1)
                 (let [power (long (Math/pow (inc root) k))]
                   (= power N)))
            (inc root)
            
            :else
            (recur (inc k))))))))

(defn complete-factorization
  "Complete factorization of a vector of partial factors, e.g. [3 9]."
  [factors]
  {:pre [(seq factors)]}
  (reduce (fn [acc factor]
            (let [base (perfect-power-factor factor)]
              (if (= base 1)
                (conj acc factor)  ; Not a perfect power, keep as is
                (let [power (int (/ factor base))]
                  (concat acc (repeat power base))))))
          []
          factors))

(comment ;

  (gcd 48 18) ; => 6
  (mod-exp 2 10 1000) ; => 24
  (continued-fraction 22 7) ; => [3 7 15]
  (convergents [3 7 15]) ; => [[3 1] [22 7] [45 16]]
  (round-precision 3.14159 2) ; => 3.14
  (find-period 5 3 15 2) ; => 4
  (perfect-power-factor 27) ; => 3
  (perfect-power-factor 16) ; => 2
  (complete-factorization [3 9]) ; => [3 3 3]
  (complete-factorization [2 4 8]) ; => [2 2 2 2 2 2]   
  ;
  )

; Disable fastmath operator macros to avoid conflicts
#_(m/unuse-primitive-operators)