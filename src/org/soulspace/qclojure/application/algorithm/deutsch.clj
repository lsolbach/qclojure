(ns org.soulspace.qclojure.application.algorithm.deutsch
  (:require
   [clojure.spec.alpha :as s]
   [org.soulspace.qclojure.application.algorithms :as qa]
   [org.soulspace.qclojure.application.backend :as qb]
   [org.soulspace.qclojure.adapter.backend.simulator :as sim]
   [org.soulspace.qclojure.domain.circuit :as qc]))

;; Oracle function type - takes computational basis state index, returns boolean
(s/def ::deutsch-oracle ::qa/oracle-function)

;;;
;;; Deutsch Algorithm
;;;
(defn deutsch-oracle-circuit
  "Build the quantum circuit for the Deutsch oracle Uf.

  Parameters:
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  Returns:
  A function that takes a quantum circuit and applies the Deutsch oracle Uf
   to it based on the behavior of the oracle function.

   The oracle function should behave as follows:
   - If oracle-fn false, returns false for both inputs (constant function f(x) = 0)
   - If oracle-fn true, returns true for both inputs (constant function f(x) = 1)
   - If oracle-fn false for input 0 and true for input 1 (balanced function f(x) = x)
   - If oracle-fn true for input 0 and false for input 1 (balanced function f(x) = NOT x)" 
  [oracle-fn & _]
  (let [;; Determine oracle type by evaluating the function
        f-false (oracle-fn false)
        f-true (oracle-fn true)
        is-constant? (= f-false f-true)]
    (fn [c]
      (cond
        ;; Constant function f(x) = 0: no gates needed
        (and is-constant? (= f-false false))
        c

        ;; Constant function f(x) = 1: apply X to ancilla 
        (and is-constant? (= f-false true))
        (qc/x-gate c 1)

        ;; Balanced function f(x) = x: apply CNOT 
        (and (not is-constant?) (= f-false false) (= f-true true))
        (qc/cnot-gate c 0 1)

        ;; Balanced function f(x) = NOT x: apply X to ancilla then CNOT
        (and (not is-constant?) (= f-false true) (= f-true false))
        (-> c
            (qc/x-gate 1)
            (qc/cnot-gate 0 1))

        :else
        (throw (ex-info "Invalid oracle function"
                        {:f-false f-false :f-true f-true}))))))

(defn deutsch-circuit
  "Build the quantum circuit for the Deutsch algorithm.
  
  Parameters:
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  Returns:
  A quantum circuit implementing the Deutsch algorithm using the provided oracle function."
  [oracle-fn]
  {:pre [(fn? oracle-fn)]}

  (-> (qc/create-circuit 2 "Deutsch Algorithm"
                         "Determines if function is constant or balanced")
      ;; Initialize ancilla qubit to |1⟩
      (qc/x-gate 1)
      ;; Apply Hadamard to both qubits
      (qc/h-gate 0)
      (qc/h-gate 1)
      ;; Implement oracle based on function behavior
      ((deutsch-oracle-circuit oracle-fn))
      ;; Final Hadamard on input qubit
      (qc/h-gate 0)))

(defn deutsch-algorithm
  "Implement the Deutsch algorithm to determine if a function is constant or balanced.
  
  The Deutsch algorithm solves the problem: Given a function f: {0,1} → {0,1},
  determine whether f is constant (f(0) = f(1)) or balanced (f(0) ≠ f(1))
  using only one quantum query, compared to 2 classical queries needed.
  
  Algorithm steps:
  1. Initialize |0⟩|1⟩ state (input qubit |0⟩, ancilla qubit |1⟩)
  2. Apply Hadamard to both qubits: |+⟩|-⟩
  3. Apply oracle function Uf
  4. Apply Hadamard to input qubit
  5. Measure input qubit: 0 = constant, 1 = balanced
  
  Parameters:
  - backend: Quantum backend implementing the QuantumBackend protocol to execute the circuit
  - oracle-fn: Function that takes a boolean input and returns boolean output
               Represents the quantum oracle Uf
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - :constant or :balanced  
  - :measurement-outcome - measurement outcome from backend
  - :circuit - The quantum circuit used
  - :execution-result - Full backend execution result
  
  Example:
  (deutsch-algorithm (fn [x] true) simulator)     ;=> {:result :constant}
  (deutsch-algorithm (fn [x] x) simulator)        ;=> {:result :balanced}"
  ([backend oracle-fn]
   (deutsch-algorithm backend oracle-fn {:shots 1024}))
  ([backend oracle-fn options]
   {:pre [(fn? oracle-fn)
          (satisfies? qb/QuantumBackend backend)]}
   
   (let [;; Build the complete quantum circuit
         circuit (deutsch-circuit oracle-fn)

         ;; Execute circuit on backend
         execution-result (qb/execute-circuit backend circuit options)

         ;; Extract measurement results and determine outcome
         measurements (:measurement-results execution-result)

         ;; For Deutsch algorithm, we only care about the measurement of qubit 0
         ;; Parse measurement outcomes to determine if qubit 0 was measured as 0 or 1
         outcome-0-count (+ (get measurements "00" 0) (get measurements "01" 0))
         outcome-1-count (+ (get measurements "10" 0) (get measurements "11" 0))
         total-shots (+ outcome-0-count outcome-1-count)

         ;; Determine most likely outcome based on measurement statistics
         measurement-outcome (if (> outcome-0-count outcome-1-count) 0 1)
         result (if (= measurement-outcome 0) :constant :balanced)]

     {:result result
      :measurement-outcome measurement-outcome
      :circuit circuit
      :execution-result execution-result
      :oracle-function oracle-fn
      :measurement-statistics {:outcome-0-count outcome-0-count
                               :outcome-1-count outcome-1-count
                               :total-shots total-shots}})))

(s/fdef deutsch-algorithm
  :args (s/cat :oracle-fn ::deutsch-oracle
               :backend ::qb/quantum-backend
               :options (s/? map?))
  :ret (s/keys :req-un [:org.soulspace.qclojure.application.algorithms/result
                        :org.soulspace.qclojure.application.algorithms/measurement-outcome
                        :org.soulspace.qclojure.application.algorithms/circuit
                        :org.soulspace.qclojure.application.algorithms/execution-result]))
