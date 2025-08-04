(ns org.soulspace.qclojure.domain.ansatz-test
  "Test suite for Ansatz domain functionality.
  
  This test suite validates ansatz circuit generation for VQE algorithms:
  - Hardware-efficient ansatz
  - UCCSD-inspired ansatz  
  - Symmetry-preserving ansatz
  - Chemistry-inspired ansatz
  - Parameter validation and circuit structure"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.ansatz :as ansatz]))

;;
;; Ansatz Circuit Tests
;;
(deftest test-hardware-efficient-ansatz
  (testing "Hardware-efficient ansatz circuit generation"
    (let [num-qubits 4
          num-layers 2
          expected-params (* num-qubits num-layers 3) ; 3 rotations per qubit per layer
          ansatz-fn (ansatz/hardware-efficient-ansatz num-qubits num-layers)
          params (vec (repeatedly expected-params #(* 0.1 (rand))))
          circuit (ansatz-fn params)]
      
      (is (fn? ansatz-fn) "Should return a function")
      (is (= num-qubits (:num-qubits circuit)))
      (is (= "Hardware Efficient Ansatz" (:name circuit)))
      (is (> (count (:operations circuit)) 0) "Should have operations")
      
      ;; Test with wrong parameter count
      (try
        (ansatz-fn [0.1 0.2])  ; Too few parameters
        (is false "Should throw exception for wrong parameter count")
        (catch AssertionError _e
          (is true "Should throw AssertionError for wrong parameter count"))
        (catch Exception _e
          (is true "Should throw exception for wrong parameter count"))))))

(deftest test-uccsd-inspired-ansatz
  (testing "UCCSD-inspired ansatz circuit generation"
    (let [num-qubits 4
          num-excitations 2
          ansatz-fn (ansatz/uccsd-inspired-ansatz num-qubits num-excitations)
          params [0.1 0.2]
          circuit (ansatz-fn params)]
      
      (is (fn? ansatz-fn) "Should return a function")
      (is (= num-qubits (:num-qubits circuit)))
      (is (= "UCCSD Inspired Ansatz" (:name circuit)))
      (is (> (count (:operations circuit)) 0) "Should have operations"))))

(deftest test-symmetry-preserving-ansatz
  (testing "Symmetry-preserving ansatz circuit generation"
    (let [num-qubits 4
          num-particles 2
          num-layers 2
          ansatz-fn (ansatz/symmetry-preserving-ansatz num-qubits num-particles num-layers)
          params (vec (repeatedly (* num-qubits num-layers 2) #(* 0.1 (rand))))
          circuit (ansatz-fn params)]
      
      (is (fn? ansatz-fn) "Should return a function")
      (is (= num-qubits (:num-qubits circuit)))
      (is (= "Symmetry Preserving Ansatz" (:name circuit)))
      (is (> (count (:operations circuit)) 0) "Should have operations"))))

(deftest test-chemistry-inspired-ansatz
  (testing "Chemistry-inspired ansatz circuit generation"
    (let [num-qubits 4
          num-excitation-layers 2
          num-electron-pairs (/ num-qubits 2)  ; 2
          params-per-layer (+ num-qubits                    ; 4 
                             num-electron-pairs              ; 2
                             (/ (* num-electron-pairs (dec num-electron-pairs)) 2)) ; 1
          expected-params (* num-excitation-layers params-per-layer)  ; 2 * 7 = 14
          ansatz-fn (ansatz/chemistry-inspired-ansatz num-qubits num-excitation-layers)
          params (vec (repeatedly expected-params #(* 0.1 (rand))))
          circuit (ansatz-fn params)]
      
      (is (fn? ansatz-fn) "Should return a function")
      (is (= num-qubits (:num-qubits circuit)))
      (is (= "Chemistry Inspired Ansatz" (:name circuit)))
      (is (> (count (:operations circuit)) 0) "Should have operations"))))

(deftest test-hartree-fock-initialization
  (testing "Hartree-Fock state initialization"
    (let [num-qubits 4
          num-electrons 2
          initialization (ansatz/hartree-fock-initialization :hardware-efficient num-qubits num-electrons)]
      
      (is (vector? initialization) "Should return parameter vector")
      (is (> (count initialization) 0) "Should have parameters"))))

;;
;; Edge Case and Error Handling Tests  
;;
(deftest test-ansatz-edge-cases
  (testing "Parameter validation in ansatz functions"
    ;; Test wrong parameter count
    (let [ansatz-fn (ansatz/hardware-efficient-ansatz 2 1)]  ; expects 6 params
      (is (thrown? AssertionError (ansatz-fn [0.1 0.2]))  ; only 2 params
          "Should throw error for wrong parameter count")))
  
  (testing "Zero qubit systems"
    (is (thrown? AssertionError (ansatz/hardware-efficient-ansatz 0 1))
        "Should reject zero qubits"))
  
  (testing "Invalid parameters"
    (is (thrown? AssertionError (ansatz/symmetry-preserving-ansatz 4 6 2))
        "Should reject more particles than qubits")))

(comment
  (run-tests)
  ;
  )