(ns org.soulspace.qclojure.domain.hamiltonian-test
  "Test suite for Hamiltonian domain functionality.
  
  This test suite validates Hamiltonian construction, Pauli string operations,
  and expectation value calculations using clojure.test macros:
  - Pauli term creation and validation
  - Hamiltonian construction and validation 
  - Pauli string expectation values
  - Commuting term grouping
  - Measurement-based expectation calculations"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [fastmath.core :as fm]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.application.algorithm.vqe :as vqe]))

;;
;; Test Fixtures and Helper Functions
;;
(defn test-hamiltonian
  "Create a simple test Hamiltonian for testing."
  []
  [(ham/pauli-term -1.5 "IIII")
   (ham/pauli-term 0.5 "ZIII")
   (ham/pauli-term 0.3 "IZII")])

;;
;; Core Hamiltonian Component Tests
;;
(deftest test-pauli-term-creation
  (testing "Pauli term creation and validation"
    (let [term (ham/pauli-term -1.5 "XYZI")]
      (is (= -1.5 (:coefficient term)))
      (is (= "XYZI" (:pauli-string term)))
      (is (s/valid? ::ham/pauli-term term)))))

(deftest test-hamiltonian-validation
  (testing "Hamiltonian validation function"
    (testing "Valid Hamiltonian"
      (let [valid-h [(ham/pauli-term 1.0 "IIII")
                     (ham/pauli-term -0.5 "ZIII")]]
        (is (ham/validate-hamiltonian valid-h))))
    
    (testing "Invalid Hamiltonian - wrong structure"
      (let [invalid-h [{:wrong-key 1.0 :pauli-string "IIII"}]]
        (is (not (ham/validate-hamiltonian invalid-h)))))
    
    (testing "Empty Hamiltonian"
      (is (ham/validate-hamiltonian [])))))

(deftest test-commuting-terms-grouping
  (testing "Grouping of commuting Pauli terms"
    (let [hamiltonian [(ham/pauli-term 1.0 "IIII")
                       (ham/pauli-term 0.5 "ZIII")
                       (ham/pauli-term 0.3 "ZIII")]
          groups (ham/group-commuting-terms hamiltonian)]
      (is (>= (count groups) 1) "Should have at least one group")
      (is (every? coll? groups) "Each group should be a collection")
      (is (= (reduce + (map count groups)) (count hamiltonian))
          "All terms should be assigned to groups"))))

;;
;; Expectation Value Tests
;;
(deftest test-pauli-string-expectation
  (testing "Pauli string expectation values"
    (let [zero-state (qs/zero-state 1)
          one-state (qs/one-state)
          plus-state (qs/plus-state)
          minus-state (qs/minus-state)]
      
      ;; Z expectation values
      (is (= 1.0 (ham/pauli-string-expectation "Z" zero-state)) "Z|0⟩ should be +1")
      (is (= -1.0 (ham/pauli-string-expectation "Z" one-state)) "Z|1⟩ should be -1") 
      (is (< (abs (ham/pauli-string-expectation "Z" plus-state)) 1e-10) "Z|+⟩ should be ~0")
      
      ;; X expectation values
      (is (< (abs (ham/pauli-string-expectation "X" zero-state)) 1e-10) "X|0⟩ should be ~0")
      (is (< (abs (ham/pauli-string-expectation "X" one-state)) 1e-10) "X|1⟩ should be ~0")
      (is (fm/approx= 1.0 (ham/pauli-string-expectation "X" plus-state) 1e-10) "X|+⟩ should be +1")
      (is (fm/approx= -1.0 (ham/pauli-string-expectation "X" minus-state) 1e-10) "X|-⟩ should be -1")
      
      ;; Identity expectation values
      (is (fm/approx= 1.0 (ham/pauli-string-expectation "I" zero-state) 1e-10) "I always gives +1")
      (is (fm/approx= 1.0 (ham/pauli-string-expectation "I" one-state) 1e-10) "I always gives +1")
      (is (fm/approx= 1.0 (ham/pauli-string-expectation "I" plus-state) 1e-10) "I always gives +1")))
  
  (testing "Multi-qubit Pauli strings"
    (let [|00⟩ (qs/zero-state 2)
          |11⟩ (qs/tensor-product (qs/one-state) (qs/one-state))]
      
      (is (= 1.0 (ham/pauli-string-expectation "ZZ" |00⟩)) "ZZ|00⟩ should be +1")
      (is (= 1.0 (ham/pauli-string-expectation "ZZ" |11⟩)) "ZZ|11⟩ should be +1")
      (is (= 1.0 (ham/pauli-string-expectation "II" |00⟩)) "II always gives +1")
      (is (= 1.0 (ham/pauli-string-expectation "ZI" |00⟩)) "ZI|00⟩ should be +1"))))

(deftest test-hamiltonian-expectation
  (testing "Hamiltonian expectation value calculation"
    (let [simple-h [(ham/pauli-term 2.0 "I")     ; Identity: always 2.0
                    (ham/pauli-term 1.0 "Z")]]   ; Z: +1 for |0⟩, -1 for |1⟩
          
      (is (= 3.0 (ham/hamiltonian-expectation simple-h qs/|0⟩)) "2*I + 1*Z for |0⟩ = 2 + 1 = 3")
      (is (= 1.0 (ham/hamiltonian-expectation simple-h qs/|1⟩)) "2*I + 1*Z for |1⟩ = 2 - 1 = 1")))
  
  (testing "Multi-qubit Hamiltonian expectation"
    (let [|00⟩ (qs/zero-state 2)
          multi-h [(ham/pauli-term 1.0 "II")   ; Identity: 1.0
                   (ham/pauli-term 0.5 "ZI")   ; Z⊗I: 0.5 for |00⟩
                   (ham/pauli-term -0.3 "IZ")  ; I⊗Z: -0.3 for |00⟩
                   (ham/pauli-term 0.2 "ZZ")] ; Z⊗Z: 0.2 for |00⟩
          
          expected (+ 1.0 0.5 -0.3 0.2)]  ; 1.4
      
      (is (= expected (ham/hamiltonian-expectation multi-h |00⟩)) "Multi-term Hamiltonian calculation"))))

(deftest test-molecular-hydrogen-hamiltonian
  (testing "Molecular hydrogen Hamiltonian construction"
    (let [h2-hamiltonian (vqe/molecular-hydrogen-hamiltonian)]
      (is (> (count h2-hamiltonian) 0) "Should have terms")
      (is (ham/validate-hamiltonian h2-hamiltonian))
      (is (every? #(string? (:pauli-string %)) h2-hamiltonian))
      (is (every? #(number? (:coefficient %)) h2-hamiltonian)))))

(deftest test-heisenberg-hamiltonian
  (testing "Heisenberg model Hamiltonian construction"
    (let [heisenberg-h (vqe/heisenberg-hamiltonian 4 1.0 true)]
      (is (> (count heisenberg-h) 0) "Should have terms")
      (is (ham/validate-hamiltonian heisenberg-h))
      (is (every? #(= 1.0 (:coefficient %)) heisenberg-h)))))

(deftest test-measurement-based-expectation
  (testing "Expectation from measurement statistics"
    (let [;; Simulate measuring |0⟩ state in Z basis: all measurements are "0"
          measurements-0 {"Z" {"0" 1000}}
          ;; Simulate measuring |1⟩ state in Z basis: all measurements are "1" 
          measurements-1 {"Z" {"1" 1000}}
          ;; Simulate measuring |+⟩ state in Z basis: 50/50 split
          measurements-plus {"Z" {"0" 500 "1" 500}}
          
          z-hamiltonian [(ham/pauli-term 1.0 "Z")]]
      
      (is (= 1.0 (vqe/measurement-based-expectation z-hamiltonian measurements-0 1000))
          "Z expectation from |0⟩ measurements should be +1")
      (is (= -1.0 (vqe/measurement-based-expectation z-hamiltonian measurements-1 1000))
          "Z expectation from |1⟩ measurements should be -1")
      (is (< (abs (vqe/measurement-based-expectation z-hamiltonian measurements-plus 1000)) 0.1)
          "Z expectation from |+⟩ measurements should be ~0")))
  
  (testing "Multi-qubit measurement statistics"
    (let [measurements-00 {"ZZ" {"00" 1000}}
          measurements-11 {"ZZ" {"11" 1000}}
          zz-hamiltonian [(ham/pauli-term 1.0 "ZZ")]]
      
      (is (= 1.0 (vqe/measurement-based-expectation zz-hamiltonian measurements-00 1000))
          "ZZ expectation from |00⟩ measurements")
      (is (= 1.0 (vqe/measurement-based-expectation zz-hamiltonian measurements-11 1000))
          "ZZ expectation from |11⟩ measurements"))))

;;
;; Edge Case and Error Handling Tests  
;;
(deftest test-hamiltonian-edge-cases
  (testing "Hamiltonian validation edge cases"
    (is (true? (ham/validate-hamiltonian [])) "Empty Hamiltonian should be valid")
    (is (false? (ham/validate-hamiltonian [{:coefficient 1.0}])) "Missing pauli-string should be invalid")
    (is (false? (ham/validate-hamiltonian [(ham/pauli-term 1.0 "XY") (ham/pauli-term 0.5 "XYZ")]))
        "Inconsistent string lengths should be invalid"))
  
  (testing "Invalid Pauli characters"
    (is (thrown? AssertionError (ham/pauli-term 1.0 "ABC"))
        "Should reject invalid Pauli characters"))
  
  (testing "Hamiltonian expectation with empty Hamiltonian"
    (let [empty-h []
          zero-state (qs/zero-state 1)]
      (is (= 0 (ham/hamiltonian-expectation empty-h zero-state)) "Empty Hamiltonian should give zero energy"))))

(comment
  (run-tests)
  ;
  )