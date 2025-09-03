(ns org.soulspace.qclojure.application.algorithm.deutsch
  "Deutsch Algorithm
   
   The Deutsch algorithm is a quantum algorithm that determines whether a given
   function f: {0,1} → {0,1} is constant (f(0) = f(1)) or balanced (f(0) ≠ f(1))
   using only one quantum query, compared to 2 classical queries needed.
   
   This implementation builds the quantum circuit for the Deutsch algorithm
   and executes it on a specified quantum backend."
  (:require
   [clojure.spec.alpha :as s]
   [org.soulspace.qclojure.application.algorithms :as qa]
   [org.soulspace.qclojure.application.backend :as qb]
   [org.soulspace.qclojure.domain.circuit :as qc]))

;; Oracle function type - takes computational basis state index, returns boolean
(s/def ::deutsch-oracle ::qa/oracle-function)

;;;
;;; Deutsch Algorithm
;;;
(defn add-oracle-fn
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
      ((add-oracle-fn oracle-fn))
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
  - :algorithm 
  - :result - :constant or :balanced  
  - :probability-zero - the probability of the first qubit being |0⟩
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

         ;; Result specifications for the Deutsch algorithm
         result-specs {:result-specs {:measurements {:shots (:shots options)}
                                      :probabilities {:qubits [0]}}}
         options (merge options result-specs)

         ;; Execute circuit on backend
         execution-result (qb/execute-circuit backend circuit options)
         results (:results execution-result)

         ;; Extract algorithm-specific results
         prob-results (:probability-results results)
         prob-outcomes (:probability-outcomes prob-results)

         ;; Deutsch algorithm decision logic:
         ;; Probability of |0⟩ on first qubit indicates constant vs balanced
         ;; Sum probabilities where first qubit is 0: states 0 (00) and 1 (01)
         prob-zero (+ (get prob-outcomes 0)  ; state |00⟩
                      (get prob-outcomes 1)) ; state |01⟩

         ;; Use threshold for noisy systems
         result (if (> prob-zero 0.75) :constant :balanced)]
     {:algorithm "Deutsch"
      :result result
      :probability-zero prob-zero
      :circuit circuit
      :execution-result execution-result})))

(comment
  (require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
  
  
  (def constant-true (constantly true))
  (def constant-false (constantly false))
  (def identity-fn identity)
  (def not-fn (comp not boolean))

  (deutsch-algorithm (sim/create-simulator) constant-true {:shots 100})
  (deutsch-algorithm (sim/create-simulator) constant-false {:shots 100})
  (deutsch-algorithm (sim/create-simulator) identity-fn {:shots 100})
  (deutsch-algorithm (sim/create-simulator) not-fn {:shots 100})
  ;
  )