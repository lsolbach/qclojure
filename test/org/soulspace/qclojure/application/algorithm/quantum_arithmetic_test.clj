(ns org.soulspace.qclojure.application.algorithm.quantum-arithmetic-test
  "Tests for quantum arithmetic circuits."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.algorithm.quantum-arithmetic :as qarith]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.test :as t]))

;; Test basic quantum carry operation
(deftest test-quantum-carry
  (testing "Quantum carry operation with minimal resources"
    (let [circuit (qc/create-circuit 4)
          ;; Test basic carry: a=1, b=1, c_in=0 should produce c_out=1
          result-circuit (qarith/quantum-carry circuit 0 1 2 3)]
      
      ;; Should have exactly 3 gates: CNOT, Toffoli, CNOT
      (is (= 3 (count (:operations result-circuit))))
      
      ;; Check gate types
      (let [ops (:operations result-circuit)]
        (is (= :cnot (:operation-type (nth ops 0))))
        (is (= :toffoli (:operation-type (nth ops 1))))
        (is (= :cnot (:operation-type (nth ops 2))))))))

;; Test resource estimation
(deftest test-resource-estimation
  (testing "Resource estimation for quantum arithmetic operations"
    ;; Addition should need minimal ancilla: n + 1 (carry qubits + final carry)
    (is (= 2 (qarith/estimate-auxiliary-qubits 1 :addition)))
    
    ;; For 2-bit addition: 2 + 1 = 3
    (is (= 3 (qarith/estimate-auxiliary-qubits 2 :addition)))
    
    ;; Multiplication needs more resources
    (is (= 3 (qarith/estimate-auxiliary-qubits 1 :multiplication)))
    
    ;; Exponentiation needs the most
    (is (= 5 (qarith/estimate-auxiliary-qubits 1 :exponentiation)))))

;; Test quantum adder
(deftest test-quantum-adder
  (testing "Quantum ripple-carry adder with minimal resources"
    (let [n 2                                    ; 2-bit addition
          total-qubits (+ (* 3 n) 1)            ; 3n+1 = 7 qubits total
          circuit (qc/create-circuit total-qubits)
          a-qubits [0 1]                         ; First number
          b-qubits [2 3]                         ; Second number (becomes sum)
          carry-qubits [4 5]                     ; n carry qubits
          c-out 6                                ; Final carry
          result-circuit (qarith/quantum-adder-ripple-carry 
                          circuit a-qubits b-qubits carry-qubits c-out)]
      
      ;; Should have reasonable number of gates (forward + final + sum + backward)
      (is (< (count (:operations result-circuit)) 20))
      
      ;; Circuit should be valid
      (is (= total-qubits (:num-qubits result-circuit))))))

;; Test modular arithmetic
(deftest test-quantum-modular-adder
  (testing "Quantum modular addition with minimal resources"
    (let [n 2                                    ; 2-bit numbers
          ancilla-needed (qarith/estimate-auxiliary-qubits n :modular-addition)
          total-qubits (+ (* 3 n) ancilla-needed) ; a + b + N + ancilla
          circuit (qc/create-circuit total-qubits)
          a-qubits [0 1]                         ; First number
          b-qubits [2 3]                         ; Second number (becomes result)
          n-qubits [4 5]                         ; Modulus N
          ancilla-qubits (vec (range (* 3 n) total-qubits)) ; Properly sized ancilla
          result-circuit (qarith/quantum-modular-adder 
                          circuit a-qubits b-qubits n-qubits ancilla-qubits)]
      
      ;; Should produce a valid circuit
      (is (= total-qubits (:num-qubits result-circuit)))
      
      ;; Should have reasonable gate count
      (is (> (count (:operations result-circuit)) 5)))))

;; Test controlled modular exponentiation
(deftest test-controlled-modular-exponentiation
  (testing "Complete controlled modular exponentiation circuit"
    (let [n-exp-qubits 2                         ; 2 exponent qubits
          n-result-qubits 2                      ; 2 result qubits
          base 2
          modulus 5
          circuit (qarith/controlled-modular-exponentiation-circuit 
                   n-exp-qubits n-result-qubits base modulus "test")]
      
      ;; Should create a valid circuit
      (is (>= (:num-qubits circuit) (+ n-exp-qubits n-result-qubits)))
      
      ;; Should have some operations
      (is (> (count (:operations circuit)) 0))
      
      ;; Circuit should be valid for quantum period finding
      (is (:name circuit)))))

(comment
  (run-tests)
  ;
  )