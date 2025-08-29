(ns org.soulspace.qclojure.domain.math-test
  "Unit tests for mathematical operations in the qclojure domain"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.math :as qmath]
            [fastmath.core :as fm]
            [fastmath.complex :as fc]))

(defn factors-equal?
  "Compare two factorization results regardless of order"
  [expected actual]
  (= (sort expected) (sort actual)))

;;
;; Test complex number handling functions
;;
(deftest test-complex?
  (testing "Complex number type detection"
    ;; Test with fastmath complex numbers
    (is (qmath/complex? (fc/complex 1.0 2.0)) "FastMath complex number should be detected")
    (is (qmath/complex? (fc/complex 0 0)) "Zero complex number should be detected")
    (is (qmath/complex? (fc/complex -1.5 3.7)) "Negative real complex number should be detected")
    
    ;; Test with non-complex types
    (is (not (qmath/complex? 5)) "Regular integer should not be complex")
    (is (not (qmath/complex? 3.14)) "Regular double should not be complex")
    (is (not (qmath/complex? [1 2])) "Vector should not be complex")
    (is (not (qmath/complex? "string")) "String should not be complex")
    (is (not (qmath/complex? nil)) "Nil should not be complex")
    (is (not (qmath/complex? {})) "Map should not be complex")))

(deftest test-complex-magnitude-squared  
  (testing "Complex magnitude squared calculation"
    
    (testing "FastMath complex numbers"
      ;; Simple cases
      (is (= 5.0 (qmath/complex-magnitude-squared (fc/complex 1 2))) "|1+2i|² = 5")
      (is (= 25.0 (qmath/complex-magnitude-squared (fc/complex 3 4))) "|3+4i|² = 25")
      (is (= 0.0 (qmath/complex-magnitude-squared (fc/complex 0 0))) "|0|² = 0")
      (is (= 1.0 (qmath/complex-magnitude-squared (fc/complex 1 0))) "|1|² = 1")
      (is (= 1.0 (qmath/complex-magnitude-squared (fc/complex 0 1))) "|i|² = 1")
      
      ;; Negative components
      (is (= 5.0 (qmath/complex-magnitude-squared (fc/complex -1 -2))) "|-1-2i|² = 5")
      (is (= 25.0 (qmath/complex-magnitude-squared (fc/complex -3 4))) "|-3+4i|² = 25"))
    
    (testing "Clojure vector format [real imag]"
      (is (= 5.0 (qmath/complex-magnitude-squared [1 2])) "[1, 2] magnitude squared")
      (is (= 25.0 (qmath/complex-magnitude-squared [3 4])) "[3, 4] magnitude squared")  
      (is (= 0.0 (qmath/complex-magnitude-squared [0 0])) "[0, 0] magnitude squared")
      (is (= 9.0 (qmath/complex-magnitude-squared [-3 0])) "[-3, 0] magnitude squared")
      (is (= 16.0 (qmath/complex-magnitude-squared [0 -4])) "[0, -4] magnitude squared"))
    
    (testing "Regular numbers (treated as real)"
      (is (= 25.0 (qmath/complex-magnitude-squared 5)) "Real number 5")
      (is (= 9.0 (qmath/complex-magnitude-squared -3)) "Negative real number")
      (is (= 0.0 (qmath/complex-magnitude-squared 0)) "Zero")
      (is (= 6.25 (qmath/complex-magnitude-squared 2.5)) "Decimal number"))
    
    (testing "Error conditions"
      (is (thrown? IllegalArgumentException (qmath/complex-magnitude-squared "invalid")))
      (is (thrown? IllegalArgumentException (qmath/complex-magnitude-squared {})))
      (is (thrown? IllegalArgumentException (qmath/complex-magnitude-squared []))))))

(deftest test-max-coeff-magnitude-squared
  (testing "Maximum coefficient magnitude squared from matrix"
    
    (testing "Simple 2x2 matrix"
      (let [matrix [[(fc/complex 1 0) (fc/complex 0 1)]
                    [(fc/complex 2 0) (fc/complex 0 2)]]]
        (is (= 4.0 (qmath/max-coeff-magnitude-squared matrix)) "Max should be |2|² = 4")))
    
    (testing "Matrix with mixed magnitudes"
      (let [matrix [[(fc/complex 3 4) (fc/complex 1 1)]  ; |3+4i|² = 25, |1+1i|² = 2
                    [(fc/complex 0 5) (fc/complex 2 0)]]] ; |5i|² = 25, |2|² = 4
        (is (= 25.0 (qmath/max-coeff-magnitude-squared matrix)) "Max should be 25")))
    
    (testing "Single element matrix"
      (let [matrix [[(fc/complex 1.5 2.5)]]]  ; |1.5+2.5i|² = 2.25 + 6.25 = 8.5  
        (is (= 8.5 (qmath/max-coeff-magnitude-squared matrix)))))
    
    (testing "Matrix with negative components"
      (let [matrix [[(fc/complex -3 -4) (fc/complex 1 -1)]
                    [(fc/complex 0 -2) (fc/complex -1 0)]]]
        (is (= 25.0 (qmath/max-coeff-magnitude-squared matrix)) "|-3-4i|² = 25")))
    
    (testing "Larger matrix"
      (let [matrix [[(fc/complex 1 0) (fc/complex 0 1) (fc/complex 2 1)]
                    [(fc/complex 1 1) (fc/complex 3 0) (fc/complex 1 2)]
                    [(fc/complex 0 4) (fc/complex 2 2) (fc/complex 0 0)]]]
        (is (= 16.0 (qmath/max-coeff-magnitude-squared matrix)) "|4i|² = 16")))))

(deftest test-prime?
  (testing "Primality testing"
    
    (testing "Known primes"
      (is (qmath/prime? 2) "2 is prime")
      (is (qmath/prime? 3) "3 is prime") 
      (is (qmath/prime? 5) "5 is prime")
      (is (qmath/prime? 7) "7 is prime")
      (is (qmath/prime? 11) "11 is prime")
      (is (qmath/prime? 13) "13 is prime")
      (is (qmath/prime? 17) "17 is prime")
      (is (qmath/prime? 19) "19 is prime")
      (is (qmath/prime? 23) "23 is prime")
      (is (qmath/prime? 29) "29 is prime")
      (is (qmath/prime? 31) "31 is prime"))
    
    (testing "Known composites"
      (is (not (qmath/prime? 4)) "4 is not prime (2×2)")
      (is (not (qmath/prime? 6)) "6 is not prime (2×3)")
      (is (not (qmath/prime? 8)) "8 is not prime (2×4)")
      (is (not (qmath/prime? 9)) "9 is not prime (3×3)")
      (is (not (qmath/prime? 10)) "10 is not prime (2×5)")
      (is (not (qmath/prime? 12)) "12 is not prime (3×4)")
      (is (not (qmath/prime? 14)) "14 is not prime (2×7)")
      (is (not (qmath/prime? 15)) "15 is not prime (3×5)")
      (is (not (qmath/prime? 16)) "16 is not prime (4×4)")
      (is (not (qmath/prime? 18)) "18 is not prime (2×9)")
      (is (not (qmath/prime? 20)) "20 is not prime (4×5)")
      (is (not (qmath/prime? 21)) "21 is not prime (3×7)")
      (is (not (qmath/prime? 25)) "25 is not prime (5×5)"))
    
    (testing "Edge cases"
      (is (not (qmath/prime? 0)) "0 is not prime")
      (is (not (qmath/prime? 1)) "1 is not prime")
      (is (not (qmath/prime? -5)) "Negative numbers are not prime"))
    
    (testing "Larger primes for performance"
      (is (qmath/prime? 97) "97 is prime")
      (is (qmath/prime? 101) "101 is prime")
      (is (qmath/prime? 103) "103 is prime")
      (is (qmath/prime? 107) "107 is prime") 
      (is (qmath/prime? 109) "109 is prime")
      (is (qmath/prime? 113) "113 is prime")
      
      ;; Test some larger composites
      (is (not (qmath/prime? 99)) "99 is not prime (9×11)")
      (is (not (qmath/prime? 100)) "100 is not prime (10×10)")
      (is (not (qmath/prime? 102)) "102 is not prime (2×51)")
      (is (not (qmath/prime? 104)) "104 is not prime (8×13)")
      (is (not (qmath/prime? 105)) "105 is not prime (3×5×7)")
      (is (not (qmath/prime? 111)) "111 is not prime (3×37)")))) 

(deftest test-complete-factorization
  (testing "Complete factorization of partial factors"
    
    (testing "Basic perfect power factorization"
      ;; 9 = 3²
      (is (factors-equal? [3 3] (qmath/complete-factorization [9])) "9 should factor to [3 3]")
      ;; 4 = 2²  
      (is (factors-equal? [2 2] (qmath/complete-factorization [4])) "4 should factor to [2 2]")
      ;; 8 = 2³
      (is (factors-equal? [2 2 2] (qmath/complete-factorization [8])) "8 should factor to [2 2 2]")
      ;; 27 = 3³
      (is (factors-equal? [3 3 3] (qmath/complete-factorization [27])) "27 should factor to [3 3 3]"))
    
    (testing "Mixed factorization"
      ;; [3 9] → [3 3 3] (9 = 3²)
      (is (factors-equal? [3 3 3] (qmath/complete-factorization [3 9])) "[3 9] should factor to [3 3 3]")
      ;; [2 4 8] → [2 2 2 2 2 2] (4 = 2², 8 = 2³)
      (is (factors-equal? [2 2 2 2 2 2] (qmath/complete-factorization [2 4 8])) "[2 4 8] should factor completely")
      ;; [5 25] → [5 5 5] (25 = 5²)
      (is (factors-equal? [5 5 5] (qmath/complete-factorization [5 25])) "[5 25] should factor to [5 5 5]"))
    
    (testing "Non-perfect powers remain unchanged"  
      ;; Prime numbers should remain as-is
      (is (factors-equal? [7] (qmath/complete-factorization [7])) "Prime 7 should remain unchanged")
      (is (factors-equal? [11] (qmath/complete-factorization [11])) "Prime 11 should remain unchanged") 
      ;; Products of primes should remain as-is
      (is (factors-equal? [3 5] (qmath/complete-factorization [15])) "15 (3×5) should factor to [3 5]") 
      (is (factors-equal? [3 7] (qmath/complete-factorization [21])) "21 (3×7) should factor to [3 7]"))
    
    (testing "Complex mixed cases"
      ;; [2 3 16] → [2 3 2 2 2 2] (16 = 2⁴)
      (is (factors-equal? [2 3 2 2 2 2] (qmath/complete-factorization [2 3 16])) "[2 3 16] should factor 16 to powers of 2")
      ;; [7 49 11] → [7 7 7 11] (49 = 7²)
      (is (factors-equal? [7 7 7 11] (qmath/complete-factorization [7 49 11])) "[7 49 11] should factor 49 to powers of 7"))
    
    (testing "Higher powers"
      ;; 32 = 2⁵
      (is (factors-equal? [2 2 2 2 2] (qmath/complete-factorization [32])) "32 should factor to five 2s")
      ;; 81 = 3⁴
      (is (factors-equal? [3 3 3 3] (qmath/complete-factorization [81])) "81 should factor to four 3s")
      ;; 125 = 5³
      (is (factors-equal? [5 5 5] (qmath/complete-factorization [125])) "125 should factor to three 5s"))
    
    (testing "Edge cases"
      ;; Single prime
      (is (factors-equal? [2] (qmath/complete-factorization [2])) "Prime 2 should remain unchanged")
      (is (factors-equal? [3] (qmath/complete-factorization [3])) "Prime 3 should remain unchanged")
      
      ;; Multiple primes
      (is (factors-equal? [2 3 5] (qmath/complete-factorization [2 3 5])) "Multiple primes should remain unchanged"))
    
    (testing "Invalid inputs should throw exceptions"
      (is (thrown? AssertionError (qmath/complete-factorization [])) "Empty vector should throw")
      (is (thrown? AssertionError (qmath/complete-factorization nil)) "Nil should throw"))))
  (testing "Continued fraction expansion of simple rational numbers"
    ;; 3/2 = 1 + 1/2 => [1, 2]
    (is (= (qmath/continued-fraction 3 2) [1 2]))
    
    ;; 7/3 = 2 + 1/3 => [2, 3]
    (is (= (qmath/continued-fraction 7 3) [2 3])))
  
    ;; 22/7 = 3 + 1/7 => [3, 7]
    (is (= (qmath/continued-fraction 22 7) [3 7]))
  
    ;; 355/113 = 3 + 1/(113/16) => [3, 7, 16]
    (is (= (qmath/continued-fraction 355 113) [3 7 16]))
     
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
    (is (= (qmath/continued-fraction 7 7) [1])))

;;
;; Test convergents calculation
;;
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

;;
;; Test GCD function - fundamental for Shor's algorithm
;;
(deftest test-gcd
  (testing "Basic GCD calculations"
    (is (= 6 (qmath/gcd 48 18)) "GCD of 48 and 18 should be 6")
    (is (= 1 (qmath/gcd 25 9)) "GCD of coprime numbers should be 1")
    (is (= 7 (qmath/gcd 21 14)) "GCD of 21 and 14 should be 7")
    (is (= 1 (qmath/gcd 17 13)) "GCD of two primes should be 1"))
  
  (testing "GCD edge cases"
    (is (= 5 (qmath/gcd 5 0)) "GCD with zero should return the other number")
    (is (= 7 (qmath/gcd 0 7)) "GCD with zero should return the other number")
    (is (= 12 (qmath/gcd 12 12)) "GCD of equal numbers should be the number itself"))
  
  (testing "GCD with larger numbers - performance test"
    (is (= 9 (qmath/gcd 123456789 987654321)) "Large numbers with common factor")
    (is (= 1 (qmath/gcd 1000003 1000033)) "Large coprime numbers")))

;;
;; Test modular exponentiation - critical for Shor's algorithm
;;
(deftest test-mod-exp
  (testing "Basic modular exponentiation"
    (is (= 24 (qmath/mod-exp 2 10 1000)) "2^10 mod 1000 should be 24")
    (is (= 1 (qmath/mod-exp 7 4 15)) "7^4 mod 15 should be 1 (period verification)")
    (is (= 1 (qmath/mod-exp 2 4 15)) "2^4 mod 15 should be 1 (period verification)"))
  
  (testing "Modular exponentiation edge cases"
    (is (= 1 (qmath/mod-exp 5 0 7)) "Any number to power 0 mod m should be 1")
    (is (= 0 (qmath/mod-exp 0 5 7)) "0 to any positive power mod m should be 0")
    (is (= 1 (qmath/mod-exp 1 100 7)) "1 to any power mod m should be 1"))
  
  (testing "Large modular exponentiation - Shor's algorithm cases"
    (is (= 1 (qmath/mod-exp 3 2 8)) "3^2 mod 8 should be 1 (period 2)")
    (is (= 1 (qmath/mod-exp 4 2 15)) "4^2 mod 15 should be 1 (period 2)")
    (is (number? (qmath/mod-exp 123 456789 1000000)) "Should handle large exponents")))

;;
;; Test round-precision function
;;
(deftest test-round-precision
  (testing "Basic precision rounding"
    (is (= 3.14 (qmath/round-precision 3.14159 2)) "Round π to 2 decimal places")
    (is (= 2.718 (qmath/round-precision 2.71828 3)) "Round e to 3 decimal places")
    (is (= 1.0 (qmath/round-precision 1.49999 0)) "Round to whole number"))
  
  (testing "Edge cases for rounding"
    (is (= 0.0 (qmath/round-precision 0.0001 3)) "Small numbers")
    (is (= 1000.0 (qmath/round-precision 999.999 0)) "Rounding up at boundary")
    (is (= -3.14 (qmath/round-precision -3.14159 2)) "Negative numbers")))

;;
;; Test find-period function - crucial for quantum period finding
;;
(deftest test-find-period
  (testing "Period finding from measurements"
    ;; These test cases simulate quantum measurement results
    (let [period (qmath/find-period 5 3 15 7)]
      (is (or (nil? period) (pos-int? period)) "Should return positive integer or nil"))
    
    ;; Test with known period cases
    (let [period (qmath/find-period 4 3 15 2)] ; For a=2, N=15, period should be 4
      (is (or (nil? period) (= 4 period)) "Should find period 4 for a=2, N=15")))
  
  (testing "Edge cases for period finding"
    (is (nil? (qmath/find-period 0 3 15 2)) "Zero measurement should return nil")
    (let [period (qmath/find-period 7 4 15 2)]
      (is (or (nil? period) (pos-int? period)) "Valid measurement should return period or nil"))))

;;
;; Test continued fraction with quantum algorithm specific cases
;;
(deftest test-continued-fraction-quantum-cases
  (testing "Continued fraction for quantum phase estimation results"
    ;; Test cases that appear in Shor's algorithm
    (let [cf (qmath/continued-fraction 355 113)] ; Pi approximation
      (is (vector? cf) "Should return vector")
      (is (= (first cf) 3) "First term should be 3"))
    
    ;; Test for small fractions that might appear in period finding
    (is (= [2] (qmath/continued-fraction 4 2)) "4/2 = 2")
    (is (= [0 2] (qmath/continued-fraction 1 2)) "1/2 = [0; 2]")
    (is (= [0 3] (qmath/continued-fraction 1 3)) "1/3 = [0; 3]")
    (is (= [0 4] (qmath/continued-fraction 1 4)) "1/4 = [0; 4]"))
  
  (testing "Continued fraction numerical precision for quantum measurements"
    ;; Test with values that might come from quantum measurements
    (let [cf1 (qmath/continued-fraction 5 8)  ; 5/8
          cf2 (qmath/continued-fraction 3 8)] ; 3/8
      (is (every? integer? cf1) "All terms should be integers")
      (is (every? integer? cf2) "All terms should be integers"))))

;;
;; Test convergents with quantum algorithm specific cases
;;
(deftest test-convergents-quantum-cases
  (testing "Convergents for period finding applications"
    ;; Test convergents that would help in period extraction
    (let [convs (qmath/convergents [0 2])]  ; For 1/2
      (is (= convs [[0 1] [1 2]]) "Convergents of [0; 2] should be [[0 1] [1 2]]"))
    
    (let [convs (qmath/convergents [0 4])]  ; For 1/4
      (is (= convs [[0 1] [1 4]]) "Convergents of [0; 4] should be [[0 1] [1 4]]"))
    
    ;; Test for more complex cases
    (let [convs (qmath/convergents [1 2 3])]
      (is (= (count convs) 3) "Should have 3 convergents")
      (is (every? #(= (count %) 2) convs) "Each convergent should be [num den] pair")))
  
  (testing "Convergents accuracy for continued fractions"
    ;; Verify that convergents properly approximate the original fraction
    (let [cf (qmath/continued-fraction 22 7)
          convs (qmath/convergents cf)]
      (is (pos? (count convs)) "Should have at least one convergent")
      ;; The last convergent should be close to the original fraction
      (when-let [last-conv (last convs)]
        (let [[num den] last-conv
              approx (/ (double num) den)
              actual (/ 22.0 7)]
          (is (< (abs (- approx actual)) 0.1) "Last convergent should approximate original"))))))

;;
;; Integration tests - test functions working together for Shor's algorithm
;;
(deftest test-shor-integration
  (testing "GCD and coprime generation patterns"
    ;; Test cases that would be used in Shor's algorithm
    (is (= 1 (qmath/gcd 7 15)) "7 and 15 are coprime")
    (is (= 1 (qmath/gcd 2 15)) "2 and 15 are coprime")
    (is (= 1 (qmath/gcd 4 15)) "4 and 15 are coprime")
    (is (= 3 (qmath/gcd 3 15)) "3 and 15 share factor 3"))
  
  (testing "Period verification for Shor's algorithm"
    ;; These are actual periods that Shor's algorithm would find
    (is (= 1 (qmath/mod-exp 7 4 15)) "Period 4 for a=7, N=15")
    (is (= 1 (qmath/mod-exp 2 4 15)) "Period 4 for a=2, N=15")
    (is (= 1 (qmath/mod-exp 3 2 8)) "Period 2 for a=3, N=8")
    (is (= 1 (qmath/mod-exp 4 2 15)) "Period 2 for a=4, N=15"))
  
  (testing "Classical preprocessing edge cases"
    ;; Cases that should be handled classically before quantum algorithm
    (is (= 2 (qmath/perfect-power-factor 4)) "4 = 2^2")
    (is (= 3 (qmath/perfect-power-factor 9)) "9 = 3^2")
    (is (= 1 (qmath/perfect-power-factor 15)) "15 needs quantum algorithm")))

;;
;; Performance and stress tests
;;
(deftest test-performance-edge-cases
  (testing "Large number handling"
    ;; Test with numbers that might appear in real Shor's algorithm usage
    (is (pos? (qmath/gcd 1000000007 1000000009)) "Large prime-like numbers")
    (is (= 1 (qmath/mod-exp 2 0 1000000007)) "Large modulus with zero exponent")
    (is (number? (qmath/mod-exp 2 1000 1000000007)) "Large exponent"))
  
  (testing "Numerical stability"
    ;; Test cases that might cause floating point issues
    (is (number? (qmath/round-precision 1.0000000001 10)) "High precision rounding")
    (is (vector? (qmath/continued-fraction 355 113)) "Pi approximation CF")
    (is (vector? (qmath/convergents [3 7 15 1 292])) "Complex convergents")))

(comment
  (run-tests)
  ;
  )