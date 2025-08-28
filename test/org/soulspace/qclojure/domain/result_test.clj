(ns org.soulspace.qclojure.domain.result-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.result :as result]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]))

;;
;; Helper functions for testing
;;
(defn approx=
  "Test if two values are approximately equal within tolerance"
  ([a b] (approx= a b 1e-10))
  ([a b tolerance]
   (< (Math/abs (- a b)) tolerance)))

(defn create-test-result
  "Create a minimal test result structure"
  [quantum-state]
  {:final-state quantum-state})

;;
;; Basic tests for core result functions
;;
(deftest test-extract-measurement-results
  (testing "Basic measurement extraction with correct keyword args"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-measurement-results zero-state :shots 10)]
      (is (= (:shot-count result) 10))
      (is (vector? (:measurement-outcomes result)))
      (is (= (count (:measurement-outcomes result)) 10))
      (is (vector? (:measurement-probabilities result)))
      (is (map? (:empirical-probabilities result)))
      (is (map? (:frequencies result)))))
  
  (testing "Default shots parameter"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-measurement-results zero-state)]
      (is (= (:shot-count result) 1))
      (is (= (count (:measurement-outcomes result)) 1))))
  
  (testing "Measurement with specific qubits"
    (let [two-qubit-state (qs/zero-state 2)
          result (result/extract-measurement-results two-qubit-state 
                                                    :shots 5 
                                                    :measurement-qubits [0])]
      (is (= (:shot-count result) 5))
      (is (= (:measurement-qubits result) [0])))))

(deftest test-extract-expectation-results
  (testing "Z expectation for |0⟩ state"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-expectation-results zero-state [obs/pauli-z])]
      (is (vector? result))
      (is (= (count result) 1))
      (let [first-result (first result)]
        (is (approx= (:expectation-value first-result) 1.0))
        (is (= (:observable first-result) obs/pauli-z)))))
  
  (testing "X expectation for |+⟩ state"
    (let [plus-state (qs/plus-state)
          result (result/extract-expectation-results plus-state [obs/pauli-x])]
      (is (= (count result) 1))
      (let [first-result (first result)]
        (is (approx= (:expectation-value first-result) 1.0)))))
  
  (testing "Multiple observables"
    (let [plus-state (qs/plus-state)
          result (result/extract-expectation-results plus-state [obs/pauli-x obs/pauli-z])]
      (is (= (count result) 2))
      (is (approx= (:expectation-value (first result)) 1.0))
      (is (approx= (:expectation-value (second result)) 0.0)))))

(deftest test-extract-variance-results
  (testing "Z variance for |0⟩ state"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-variance-results zero-state [obs/pauli-z])]
      (is (vector? result))
      (is (= (count result) 1))
      (let [first-result (first result)]
        (is (approx= (:variance-value first-result) 0.0))
        (is (approx= (:standard-deviation first-result) 0.0)))))
  
  (testing "Z variance for |+⟩ state"
    (let [plus-state (qs/plus-state)
          result (result/extract-variance-results plus-state [obs/pauli-z])]
      (is (= (count result) 1))
      (let [first-result (first result)]
        (is (approx= (:variance-value first-result) 1.0))
        (is (approx= (:standard-deviation first-result) 1.0))))))

(deftest test-extract-hamiltonian-expectation
  (testing "Z Hamiltonian expectation"
    (let [zero-state (qs/zero-state 1)
          z-hamiltonian [(ham/pauli-term 1.0 "Z")]
          result (result/extract-hamiltonian-expectation zero-state z-hamiltonian)]
      (is (approx= (:energy-expectation result) 1.0))
      (is (= (:hamiltonian result) z-hamiltonian))
      (is (coll? (:measurement-groups result)))
      (is (map? (:measurement-bases result)))))
  
  (testing "Multi-term Hamiltonian"
    (let [plus-state (qs/plus-state)
          mixed-hamiltonian [(ham/pauli-term 0.5 "Z") (ham/pauli-term 0.5 "X")]
          result (result/extract-hamiltonian-expectation plus-state mixed-hamiltonian)]
      ;; For |+⟩: Z expectation = 0, X expectation = 1, so total = 0.5*0 + 0.5*1 = 0.5
      (is (approx= (:energy-expectation result) 0.5)))))

(deftest test-extract-probability-results
  (testing "Probability extraction for |0⟩ state"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-probability-results zero-state)]
      (is (contains? result :probability-outcomes))
      (is (contains? result :all-probabilities))
      (let [all-probs (:all-probabilities result)]
        (is (approx= (first all-probs) 1.0))
        (is (approx= (second all-probs) 0.0)))))
  
  (testing "Probability extraction with specific targets"
    (let [bell-state (:final-state (qc/execute-circuit (qc/bell-state-circuit) (qs/zero-state 2)))
          result (result/extract-probability-results bell-state :target-states [[0 0] [1 1]])]
      (is (contains? result :probability-outcomes))
      (is (= (:target-states result) [[0 0] [1 1]]))
      (let [outcomes (:probability-outcomes result)]
        ;; Bell state should have equal probability for |00⟩ and |11⟩
        (is (approx= (get outcomes [0 0]) 0.5 0.1))
        (is (approx= (get outcomes [1 1]) 0.5 0.1))))))

(deftest test-extract-amplitude-results
  (testing "Amplitude extraction for |0⟩ state"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-amplitude-results zero-state [0 1])]
      (is (= (:basis-states result) [0 1]))
      (let [amplitudes (:amplitude-values result)]
        (is (approx= (fc/abs (get amplitudes 0)) 1.0))
        (is (approx= (fc/abs (get amplitudes 1)) 0.0)))))
  
  (testing "Amplitude extraction for |+⟩ state"
    (let [plus-state (qs/plus-state)
          result (result/extract-amplitude-results plus-state [0 1])
          amplitudes (:amplitude-values result)]
      (is (approx= (fc/abs (get amplitudes 0)) (/ 1.0 (Math/sqrt 2))))
      (is (approx= (fc/abs (get amplitudes 1)) (/ 1.0 (Math/sqrt 2)))))))

(deftest test-extract-state-vector-result  
  (testing "State vector extraction"
    (let [plus-state (qs/plus-state)
          result (result/extract-state-vector-result plus-state)]
      (is (= (:num-qubits result) 1))
      (is (vector? (:state-vector result)))
      (is (= (count (:state-vector result)) 2))
      (is (vector? (:basis-labels result)))
      (is (= (:basis-labels result) ["|0⟩" "|1⟩"])))))

(deftest test-extract-density-matrix-result
  (testing "Density matrix extraction"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-density-matrix-result zero-state)]
      (is (= (:num-qubits result) 1))
      (is (vector? (:density-matrix result)))
      (is (:trace-valid result)))))

(deftest test-extract-fidelity-result
  (testing "Fidelity with identical states"
    (let [zero-state (qs/zero-state 1)
          another-zero (qs/zero-state 1)
          result (result/extract-fidelity-result zero-state [another-zero])]
      (is (contains? result :fidelities))
      (is (= (:reference-states result) [another-zero]))
      (let [fidelities (:fidelities result)]
        (is (approx= (get fidelities "reference-0") 1.0)))))
  
  (testing "Fidelity with orthogonal states"
    (let [zero-state (qs/zero-state 1)
          one-state (qs/one-state)
          result (result/extract-fidelity-result zero-state [one-state])
          fidelities (:fidelities result)]
      (is (approx= (get fidelities "reference-0") 0.0)))))

(deftest test-extract-sample-results
  (testing "Sample results for Z observable on |0⟩"
    (let [zero-state (qs/zero-state 1)
          result (result/extract-sample-results zero-state [obs/pauli-z] 100)]
      (is (vector? result))
      (is (= (count result) 1))
      (let [first-result (first result)]
        (is (= (:shot-count first-result) 100))
        (is (= (:observable first-result) obs/pauli-z))
        (is (= (count (:sample-outcomes first-result)) 100))
        ;; For |0⟩ state, Z measurements should always give +1
        (is (every? #(= % 1.0) (:sample-outcomes first-result)))))))

(deftest test-compute-results-basic
  (testing "Basic compute-results functionality"
    (let [zero-state (qs/zero-state 1)
          test-result (create-test-result zero-state)
          specs {:measurements {:shots 10}
                 :expectation {:observables [obs/pauli-z]}
                 :state-vector true}
          result (result/compute-results test-result specs)]
      
      (is (contains? result :final-state))
      (is (contains? result :measurement-results))
      (is (contains? result :expectation-results))
      (is (contains? result :state-vector-result))
      
      (is (= (:shot-count (:measurement-results result)) 10))
      (is (= (count (:expectation-results result)) 1))
      (is (approx= (:expectation-value (first (:expectation-results result))) 1.0)))))

(deftest test-compute-results-comprehensive
  (testing "Compute results with Bell state and multiple result types"
    (let [bell-state (:final-state (qc/execute-circuit (qc/bell-state-circuit) (qs/zero-state 2)))
          test-result (create-test-result bell-state)
          specs {:measurements {:shots 100}
                 :expectation {:observables [obs/pauli-z] :targets [0]}
                 :variance {:observables [obs/pauli-z] :targets [0]}
                 :probabilities {:targets [[0 0] [1 1]]}
                 :amplitudes {:basis-states [0 3]}  ; |00⟩ and |11⟩
                 :state-vector true
                 :density-matrix true}
          result (result/compute-results test-result specs)]
      
      (is (contains? result :measurement-results))
      (is (contains? result :expectation-results))
      (is (contains? result :variance-results))
      (is (contains? result :probability-results))
      (is (contains? result :amplitude-results))
      (is (contains? result :state-vector-result))
      (is (contains? result :density-matrix-result))
      
      ;; Bell state specific checks
      (let [prob-results (:probability-results result)
            amp-results (:amplitude-results result)]
        (is (approx= (get (:probability-outcomes prob-results) [0 0]) 0.5 0.1))
        (is (approx= (get (:probability-outcomes prob-results) [1 1]) 0.5 0.1))
        (is (= (:basis-states amp-results) [0 3]))))))

(deftest test-compute-results-hamiltonian
  (testing "Hamiltonian energy computation"
    (let [plus-state (qs/plus-state)
          test-result (create-test-result plus-state)
          hamiltonian [(ham/pauli-term 1.0 "X")]
          specs {:hamiltonian hamiltonian}
          result (result/compute-results test-result specs)]
      
      (is (contains? result :hamiltonian-result))
      (let [ham-result (:hamiltonian-result result)]
        (is (approx= (:energy-expectation ham-result) 1.0))))))

(deftest test-compute-results-empty-specs
  (testing "Empty result specs"
    (let [zero-state (qs/zero-state 1)
          test-result (create-test-result zero-state)
          result (result/compute-results test-result {})]
      
      (is (contains? result :final-state))
      (is (= (count (keys result)) 1)))))

(deftest test-summarize-results
  (testing "Result summarization"
    (let [zero-state (qs/zero-state 1)
          test-result (create-test-result zero-state)
          specs {:measurements {:shots 100}
                 :expectation {:observables [obs/pauli-z]}
                 :variance {:observables [obs/pauli-z]}}
          computed-result (result/compute-results test-result specs)
          summary (result/summarize-results computed-result)]
      
      (is (contains? summary :measurement-summary))
      (is (contains? summary :expectation-summary))
      (is (contains? summary :variance-summary))
      
      (let [measurement-summary (:measurement-summary summary)]
        (is (= (:total-shots measurement-summary) 100)))
      
      (let [expectation-summary (:expectation-summary summary)]
        (is (= (count expectation-summary) 1))
        (is (approx= (:expectation (first expectation-summary)) 1.0)))
      
      (let [variance-summary (:variance-summary summary)]
        (is (= (count variance-summary) 1))
        (is (approx= (:variance (first variance-summary)) 0.0))))))

(deftest test-real-circuit-integration
  (testing "Bell state circuit comprehensive analysis"
    (let [bell-circuit (qc/bell-state-circuit)
          initial-state (qs/zero-state 2)
          final-state (:final-state (qc/execute-circuit bell-circuit initial-state))
          test-result (create-test-result final-state)
          specs {:measurements {:shots 1000}
                 :expectation {:observables [obs/pauli-z obs/pauli-x] :targets [0 1]}
                 :probabilities {:targets [[0 0] [0 1] [1 0] [1 1]]}
                 :state-vector true}
          result (result/compute-results test-result specs)]
      
      (is (contains? result :measurement-results))
      (is (contains? result :expectation-results))
      (is (contains? result :probability-results))
      (is (contains? result :state-vector-result))
      
      ;; Bell state should have no probability for |01⟩ and |10⟩
      (is (approx= (get (:probability-outcomes (:probability-results result)) [0 1]) 0.0 0.1))
      (is (approx= (get (:probability-outcomes (:probability-results result)) [1 0]) 0.0 0.1))))

  (testing "GHZ state circuit analysis"
    (let [ghz-circuit (qc/ghz-state-circuit 3)
          initial-state (qs/zero-state 3)
          final-state (:final-state (qc/execute-circuit ghz-circuit initial-state))
          test-result (create-test-result final-state)
          specs {:measurements {:shots 500}
                 :probabilities {:targets [[0 0 0] [1 1 1]]}
                 :state-vector true}
          result (result/compute-results test-result specs)]
      
      ;; GHZ state should have equal probability for |000⟩ and |111⟩
      (is (approx= (get (:probability-outcomes (:probability-results result)) [0 0 0]) 0.5 0.1))
      (is (approx= (get (:probability-outcomes (:probability-results result)) [1 1 1]) 0.5 0.1)))))

(comment
  ;; Run all tests in this namespace
  (run-tests)
  
  ;; Run specific test
  (test-extract-measurement-results)
  (test-compute-results-comprehensive)
  (test-real-circuit-integration)
  )
