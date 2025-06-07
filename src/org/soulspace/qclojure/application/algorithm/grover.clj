(ns org.soulspace.qclojure.application.algorithm.grover
  (:require
   [clojure.spec.alpha :as s]
   [org.soulspace.qclojure.application.algorithms :as qa]
   [org.soulspace.qclojure.application.backend :as qb]
   [org.soulspace.qclojure.domain.state :as qs]
   [org.soulspace.qclojure.domain.gate :as qg]
   [fastmath.core :as m]
   [fastmath.complex :as fc]
   [org.soulspace.qclojure.domain.circuit :as qc]))

;; Oracle function type - takes computational basis state index, returns boolean
(s/def ::grover-oracle ::qa/oracle-function)

;;;
;;; Grover's Search Algorithm
;;;
(defn grover-oracle-circuit
  "Build the quantum circuit for Grover's oracle Uf.
  
  Parameters:
  - oracle-fn: Function that takes a basis state index and returns true if it's a target state
               Represents the Grover oracle Uf
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the Grover oracle Uf to it."
  [oracle-fn n-qubits]
  (let [target-states (filter oracle-fn (range (bit-shift-left 1 n-qubits)))]
    (fn [circuit]
      (reduce (fn [c idx]
                ;; Apply phase flip to target states
                (qc/z-gate c idx))
              circuit
              target-states))))

(defn grover-diffusion-circuit
  "Build the quantum circuit for Grover's diffusion operator.
  
  The diffusion operator applies inversion about the average amplitude.
  
  Parameters:
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the diffusion operator."
  [n-qubits]
  (fn [circuit]
    ;; Apply Hadamard to all qubits
    (reduce (fn [c idx] (qc/h-gate c idx)) circuit (range n-qubits))
    ;; Apply conditional phase shift (|0...0⟩ → -|0...0⟩)
    (qc/z-gate circuit 0)
    ;; Apply Hadamard again to all qubits
    (reduce (fn [c idx] (qc/h-gate c idx)) circuit (range n-qubits))))

(defn grover-iteration
  "Perform one iteration of Grover's algorithm.
  
  A Grover iteration consists of:
  1. Apply the oracle (marks the target state with a phase flip)
  2. Apply the diffusion operator (inversion about average)
  
  Parameters:
  - state: Current quantum state
  - oracle-fn: Function that returns true for marked items
  - n-qubits: Number of qubits in the system
  
  Returns:
  Quantum state after one Grover iteration"
  [state oracle-fn n-qubits]
  (let [;; Apply oracle (simplified - just demonstrate the concept)
        ;; In practice, oracle would be implemented as a quantum circuit
        state-vector (:state-vector state)
        
        ;; Mark the target states by applying phase flip
        marked-state-vector 
        (mapv (fn [i amplitude]
                (if (oracle-fn i)
                  (fc/mult amplitude (fc/complex -1 0))  ; Phase flip
                  amplitude))
              (range (count state-vector))
              state-vector)
        
        marked-state (assoc state :state-vector marked-state-vector)
        
        ;; Apply diffusion operator (inversion about average)
        ;; 1. Apply H gates to all qubits
        after-hadamards (reduce (fn [s qubit-idx]
                                  (qg/h-gate s qubit-idx))
                                marked-state
                                (range n-qubits))
        
        ;; 2. Apply conditional phase shift (|0...0⟩ → -|0...0⟩)
        ;; Simplified: just flip phase of |0...0⟩ state
        diffusion-state-vector (:state-vector after-hadamards)
        after-phase-flip (update diffusion-state-vector 0 
                                 #(fc/mult % (fc/complex -1 0)))
        
        after-phase-state (assoc after-hadamards :state-vector after-phase-flip)
        
        ;; 3. Apply H gates again
        final-state (reduce (fn [s qubit-idx]
                              (qg/h-gate s qubit-idx))
                            after-phase-state
                            (range n-qubits))]
    
    final-state))

; TODO use backend and circuit functions
(defn grover-algorithm
  "Implement Grover's search algorithm.
  
  Grover's algorithm provides a quadratic speedup for searching unsorted databases.
  For N items, classical search requires O(N) queries, while Grover's requires O(√N).
  
  Algorithm steps:
  1. Initialize uniform superposition |+⟩^⊗n
  2. Repeat ~π√N/4 times:
     a. Apply oracle (marks target items)
     b. Apply diffusion operator (inversion about average)
  3. Measure to find target item with high probability
  
  Parameters:
  - search-space-size: Number of items to search through (must be power of 2)
  - oracle-fn: Function that returns true for target items
               Takes basis state index as input
  - backend: Quantum backend implementing the QuantumBackend protocol
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - Most likely measurement outcome
  - :probability - Probability of measuring the target
  - :iterations - Number of Grover iterations performed
  - :final-state - Final quantum state before measurement
  - :circuit - Description of the quantum circuit used
  
  Example:
  (grover-algorithm 4 #(= % 2))  ; Search for item at index 2 in 4-item space"
  ([search-space-size oracle-fn backend]
   (grover-algorithm search-space-size oracle-fn backend {:shots 1024}))
  ([search-space-size oracle-fn _backend _options]
   {:pre [(pos-int? search-space-size)
          (= search-space-size (bit-shift-left 1 (m/log2int search-space-size)))  ; Power of 2
          (fn? oracle-fn)]}

   (let [n-qubits (m/log2int search-space-size)

         ;; Calculate optimal number of iterations: π√N/4
         n-iterations (max 1 (int (* (/ m/PI 4) (m/sqrt search-space-size))))

         ;; Initialize uniform superposition state
         initial-state (reduce (fn [state qubit-idx]
                                 (qg/h-gate state qubit-idx))
                               (qs/zero-state n-qubits)
                               (range n-qubits))

         ;; Perform Grover iterations
         final-state (reduce (fn [state _iteration]
                               (grover-iteration state oracle-fn n-qubits))
                             initial-state
                             (range n-iterations))

         ;; Measure the final state
         measurement (qs/measure-state final-state)
         result (:outcome measurement)

         ;; Calculate probability of measuring the target
         target-indices (filter oracle-fn (range search-space-size))
         target-probability (reduce + (map #(qs/probability final-state %) target-indices))]

     {:result result
      :probability target-probability
      :iterations n-iterations
      :final-state final-state
      :target-indices target-indices
      :search-space-size search-space-size
      :oracle-function oracle-fn
      :measurements [measurement]  ; Include the measurement data
      :circuit {:name "Grover Search"
                :description (str "Search " search-space-size " items using " n-iterations " iterations")
                :qubits n-qubits
                :operations ["Initialize superposition"
                             (str "Apply " n-iterations " Grover iterations")
                             "Measure result"]}})))

(defn optimal-grover-iterations
  "Calculate the optimal number of iterations for Grover's algorithm.
  
  For N items with M marked items, optimal iterations ≈ π√(N/M)/4
  
  Parameters:
  - N: Total number of items in search space
  - M: Number of marked (target) items
  
  Returns:
  Optimal number of iterations (integer)"
  [N M]
  {:pre [(pos-int? N) (pos-int? M) (<= M N)]}
  (max 1 (int (* (/ m/PI 4) (m/sqrt (/ N M))))))

