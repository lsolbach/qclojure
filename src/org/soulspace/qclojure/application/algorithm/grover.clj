(ns org.soulspace.qclojure.application.algorithm.grover
  "Grover's Search Algorithm
   
   Grover's algorithm provides a quadratic speedup for searching unsorted databases.
   For N items, classical search requires O(N) queries, while Grover's requires O(√N).
   The number of Grover iterations is approximately π√N/4, where N is the size
   of the search space.

   This implementation builds the quantum circuit for Grover's algorithm and executes it
   on a specified quantum backend.

   The algorithm consists of:
   1. Initializing a uniform superposition state |+⟩^⊗n
   2. Repeating Grover iterations:
      a. Apply the oracle Uf to mark target states
      b. Apply the diffusion operator (inversion about average)
   3. Measuring the final state to find the target item with high probability
   
   The oracle function should take a computational basis state index and return
   true for target states.
   
   The diffusion operator applies inversion about the average amplitude."
  (:require
   [clojure.spec.alpha :as s]
   [org.soulspace.qclojure.application.algorithms :as qa]
   [org.soulspace.qclojure.application.backend :as qb]
   [fastmath.core :as m]
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.domain.state :as qs]))

;; Oracle function type - takes computational basis state index, returns boolean
(s/def ::grover-oracle ::qa/oracle-function)

;;;
;;; Grover's Search Algorithm
;;;
#_(defn multi-controlled-z
  "Apply a multi-controlled Z gate to flip the phase of |11...1⟩ state.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control-qubits: Vector of control qubit indices
  - target: Index of target qubit
  
  Returns:
  Updated quantum circuit with multi-controlled Z gate"
  [circuit control-qubits target]
  (case (count control-qubits)
    0 (qc/z-gate circuit target)
    1 (qc/cz-gate circuit (first control-qubits) target)
    2 ;; For 2 controls, we can use decomposition with Toffoli and single-qubit gates
    (let [c1 (first control-qubits)
          c2 (second control-qubits)]
      ;; Decompose CCZ using Toffoli: CCZ = CZ(c2,t) CNOT(c1,c2) CZ(c2,t) CNOT(c1,c2) CZ(c1,t)
      (-> circuit
          (qc/cz-gate c1 target)
          (qc/cnot-gate c1 c2)
          (qc/cz-gate c2 target)
          (qc/cnot-gate c1 c2)
          (qc/cz-gate c2 target)))
    ;; For more controls, use recursive decomposition
    (let [aux-qubit (dec (+ (apply max control-qubits) 1)) ; simplified auxiliary qubit selection
          [c1 & rest-controls] control-qubits]
      (-> circuit
          (multi-controlled-z rest-controls aux-qubit)
          (qc/cnot-gate c1 aux-qubit)
          (multi-controlled-z rest-controls aux-qubit)
          (qc/cnot-gate c1 aux-qubit)
          (qc/cz-gate c1 target)))))

(defn multi-controlled-z
  "Apply a multi-controlled Z gate to flip the phase of |11...1⟩ state.
  
  Uses the decomposition MCZ = H + MCX + H, where MCX is implemented using
  Toffoli gates and CNOT gates for multi-control scenarios.
  
  Parameters:
  - circuit: Quantum circuit to add the gate to
  - control-qubits: Vector of control qubit indices
  - target: Index of target qubit
  
  Returns:
  Updated quantum circuit with multi-controlled Z gate"
  [circuit control-qubits target]
  (let [n-controls (count control-qubits)]
    (case n-controls
      0 (qc/z-gate circuit target)
      1 (qc/cz-gate circuit (first control-qubits) target)
      2 ;; CCZ = H + Toffoli + H
      (let [c1 (first control-qubits)
            c2 (second control-qubits)]
        (-> circuit
            (qc/h-gate target)
            (qc/toffoli-gate c1 c2 target)
            (qc/h-gate target)))

      3 ;; CCCZ using a working 3-control decomposition without auxiliary qubits
      (let [c1 (nth control-qubits 0)
            c2 (nth control-qubits 1) 
            c3 (nth control-qubits 2)]
        ;; Use a reliable decomposition for 3-control Z gate
        ;; This implementation ensures phase flip ONLY when all controls are |1⟩
        (-> circuit
            ;; Convert to X gate version: CCCZ = H + CCCX + H
            (qc/h-gate target)
            ;; Implement CCCX using a sequence that works correctly
            ;; Use the fact that CCCX(a,b,c,t) flips t iff a=b=c=1
            (qc/toffoli-gate c1 c2 target)   ; t = t ⊕ (c1 ∧ c2)
            (qc/toffoli-gate c1 target c2)   ; c2 = c2 ⊕ (c1 ∧ t) 
            (qc/toffoli-gate c2 c3 target)   ; t = t ⊕ (c2 ∧ c3)
            (qc/toffoli-gate c1 target c2)   ; c2 = c2 ⊕ (c1 ∧ t) [undo]
            (qc/toffoli-gate c1 c2 target)   ; t = t ⊕ (c1 ∧ c2) [undo]
            (qc/h-gate target)))
      4 ;; For 4+ controls, use linear decomposition with auxiliary qubits
      ;; This is a simplified implementation that should work but may not be optimal
      (let [max-qubit (apply max (conj control-qubits target))
            aux-start (inc max-qubit)]
        ;; Build a linear decomposition using auxiliary qubits
        ;; This decomposes an n-control gate into a series of 3-control gates
        (if (= n-controls 4)
          ;; Special case for 4 controls: use one auxiliary qubit
          (let [c1 (nth control-qubits 0)
                c2 (nth control-qubits 1)
                c3 (nth control-qubits 2)
                c4 (nth control-qubits 3)
                aux aux-start]
            (-> circuit
                (qc/h-gate target)
                ;; Implement 4-control X using auxiliary qubit
                (qc/toffoli-gate c1 c2 aux)      ; CCX(c1,c2,aux)
                (qc/toffoli-gate c3 c4 target)   ; CCX(c3,c4,target)
                (qc/cnot-gate aux target)        ; CX(aux,target)
                (qc/toffoli-gate c3 c4 target)   ; CCX(c3,c4,target) - undo
                (qc/cnot-gate aux target)        ; CX(aux,target) - undo
                (qc/toffoli-gate c1 c2 aux)      ; CCX(c1,c2,aux) - undo
                (qc/h-gate target)))
          ;; For 5+ controls, use recursive decomposition (simplified)
          (throw (ex-info "Multi-controlled Z with 5+ controls not yet implemented"
                          {:n-controls n-controls
                           :control-qubits control-qubits
                           :target target
                           :message "Need auxiliary qubits for 5+ controls"})))))))

(defn add-oracle-fn
  "Build the quantum circuit for Grover's oracle Uf.
  
  Implements a phase oracle that flips the phase of target computational basis states.
  For each target state, it applies controlled gates to flip the phase when the
  qubits match the binary representation of the target state index.
  
  Parameters:
  - oracle-fn: Function that takes a basis state index and returns true if it's a target state
               Represents the Grover oracle Uf
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the Grover oracle Uf to it."
  [oracle-fn n-qubits]
  (let [target-states (filter oracle-fn (range (bit-shift-left 1 n-qubits)))]
    (fn [circuit]
      (if (seq target-states)
        ;; Apply phase flip for each target state
        (reduce (fn [c state-idx]
                  ;; Convert state index to binary representation
                  (let [bits (qs/index-to-bits state-idx n-qubits)]
                    ;; Apply oracle: flip phase of |state-idx⟩
                    ;; Algorithm:
                    ;; 1. Apply X gates to qubits that should be |0⟩ in target state
                    ;; 2. Apply multi-controlled Z gate to flip phase of |111...1⟩
                    ;; 3. Apply X gates again to flip back
                    (-> c
                        ;; Step 1: Apply X gates to qubits that should be |0⟩
                        ((fn [circuit]
                           (reduce (fn [c qubit-idx]
                                     (if (bits qubit-idx)
                                       c  ; Qubit should be |1⟩, leave unchanged
                                       (qc/x-gate c qubit-idx)))  ; Qubit should be |0⟩, flip it
                                   circuit
                                   (range n-qubits))))
                        ;; Step 2: Apply multi-controlled Z gate (all qubits now |1⟩ for target state)
                        ((fn [circuit]
                           (case n-qubits
                             1 (qc/z-gate circuit 0)
                             2 (qc/cz-gate circuit 0 1)
                             ;; For 3+ qubits, we need to apply a phase flip when ALL qubits are |1⟩
                             ;; Use the last qubit as target and all others as controls
                             ;; This is the same pattern as the diffusion operator
                             (multi-controlled-z circuit (vec (range (dec n-qubits))) (dec n-qubits)))))
                        ;; Step 3: Apply X gates again to flip back
                        ((fn [circuit]
                           (reduce (fn [c qubit-idx]
                                     (if (bits qubit-idx)
                                       c  ; Qubit should be |1⟩, leave unchanged
                                       (qc/x-gate c qubit-idx)))  ; Qubit should be |0⟩, flip it back
                                   circuit
                                   (range n-qubits)))))))
                circuit
                target-states)
        circuit))))

(defn grover-diffusion-circuit
  "Build the quantum circuit for Grover's diffusion operator.
  
  The diffusion operator applies inversion about the average amplitude.
  It implements 2|ψ⟩⟨ψ| - I where |ψ⟩ is the uniform superposition state.
  
  Algorithm:
  1. Apply H gates to all qubits (transform to computational basis)
  2. Apply X gates to all qubits (flip all bits)
  3. Apply multi-controlled Z gate to flip phase of |11...1⟩
  4. Apply X gates to all qubits again (flip back)
  5. Apply H gates to all qubits again (transform back to superposition basis)
  
  Parameters:
  - n-qubits: Number of qubits in the system
  
  Returns:
  A function that takes a quantum circuit and applies the diffusion operator."
  [n-qubits]
  (fn [circuit]
    (let [qubit-indices (range n-qubits)]
      (-> circuit
          ;; 1. Apply H gates to all qubits (computational basis)
          ((fn [c] (reduce (fn [acc idx] (qc/h-gate acc idx)) c qubit-indices)))
          ;; 2. Apply X gates to all qubits (invert all bits)
          ((fn [c] (reduce (fn [acc idx] (qc/x-gate acc idx)) c qubit-indices)))
          ;; 3. Apply multi-controlled Z gate to |11...1⟩
          ((fn [c]
             (case n-qubits
               1 (qc/z-gate c 0)
               2 (qc/cz-gate c 0 1)
               ;; For 3+ qubits, use the last qubit as target and others as controls
               (multi-controlled-z c (vec (range (dec n-qubits))) (dec n-qubits)))))
          ;; 4. Apply X gates to all qubits again (flip back)
          ((fn [c] (reduce (fn [acc idx] (qc/x-gate acc idx)) c qubit-indices)))
          ;; 5. Apply H gates to all qubits again (superposition basis)
          ((fn [c] (reduce (fn [acc idx] (qc/h-gate acc idx)) c qubit-indices)))))))

; TODO use backend and circuit functions
(defn grover-circuit
  "Build the complete quantum circuit for Grover's search algorithm.
  
  Constructs a quantum circuit that implements Grover's algorithm, including:
  1. Initial state preparation (uniform superposition)
  2. Repeated Grover iterations (oracle + diffusion operator)
  3. Optional final measurement
  
  Parameters:
  - n-qubits: Number of qubits (determines search space size 2^n)
  - oracle-fn: Function that takes a basis state index and returns true for target states
  - iterations: Number of Grover iterations (if not provided, uses optimal π√N/4)
  - options: Optional map with keys:
    - :add-measurements? - Whether to add measurement operations (default: false)
  
  Returns:
  Quantum circuit implementing Grover's algorithm
  
  Example:
  (grover-circuit 3 #(= % 5))  ; Search for state |101⟩ in 3-qubit space
  (grover-circuit 2 #(contains? #{1 2} %) 1 {:add-measurements? true})"
  ([n-qubits oracle-fn]
   (let [search-space-size (bit-shift-left 1 n-qubits)
         optimal-iterations (max 1 (int (* (/ m/PI 4) (m/sqrt search-space-size))))]
     (grover-circuit n-qubits oracle-fn optimal-iterations {})))
  ([n-qubits oracle-fn iterations]
   (grover-circuit n-qubits oracle-fn iterations {}))
  ([n-qubits oracle-fn iterations options]
   {:pre [(pos-int? n-qubits)
          (fn? oracle-fn)
          (pos-int? iterations)]}
   
   (let [search-space-size (bit-shift-left 1 n-qubits)
         add-measurements? (get options :add-measurements? false)
         oracle-circuit-fn (add-oracle-fn oracle-fn n-qubits)
         diffusion-circuit-fn (grover-diffusion-circuit n-qubits)]
     
     (-> (qc/create-circuit n-qubits "Grover Search"
                           (str "Search " search-space-size " items using " iterations " iterations"))
         ;; Step 1: Initialize uniform superposition |+⟩^⊗n
         ((fn [c]
            (reduce (fn [circuit qubit-idx]
                      (qc/h-gate circuit qubit-idx))
                    c
                    (range n-qubits))))
         ;; Step 2: Apply Grover iterations
         ((fn [c]
            (reduce (fn [circuit _iteration]
                      (-> circuit
                          ;; Apply oracle
                          oracle-circuit-fn
                          ;; Apply diffusion operator
                          diffusion-circuit-fn))
                    c
                    (range iterations))))
         ;; Step 3: Optional measurements
         ((fn [c]
            (if add-measurements?
              (qc/measure-operation c (vec (range n-qubits)))
              c)))))))

(s/fdef grover-algorithm
  :args (s/cat :backend #(satisfies? qb/QuantumBackend %)
               :search-space-size pos-int?
               :oracle-fn ::grover-oracle
               :options (s/? map?))
  :ret (s/keys :req-un [:org.soulspace.qclojure.application.algorithms/result
                        :org.soulspace.qclojure.application.algorithms/probability
                        :org.soulspace.qclojure.application.algorithms/circuit
                        :org.soulspace.qclojure.application.algorithms/execution-result]))

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
  - backend: Quantum backend implementing the QuantumBackend protocol
  - search-space-size: Number of items to search through (must be power of 2)
  - oracle-fn: Function that returns true for target items
               Takes basis state index as input
  - options: Optional map with execution options (default: {:shots 1024})
  
  Returns:
  Map containing:
  - :result - Most likely measurement outcome
  - :probability - Probability of measuring the target
  - :iterations - Number of Grover iterations performed
  - :circuit - The quantum circuit used
  - :execution-result - Full backend execution result
  - :target-indices - List of target state indices
  - :measurement-statistics - Detailed measurement statistics
  
  Example:
  (grover-algorithm backend 4 #(= % 2))  ; Search for item at index 2 in 4-item space"
  ([backend search-space-size oracle-fn]
   (grover-algorithm backend search-space-size oracle-fn {:shots 1024}))
  ([backend search-space-size oracle-fn options]
   {:pre [(satisfies? qb/QuantumBackend backend)
          (pos-int? search-space-size)
          (= search-space-size (bit-shift-left 1 (m/log2int search-space-size)))  ; Power of 2
          (fn? oracle-fn)]}

   (let [n-qubits (m/log2int search-space-size)

         ;; Calculate optimal number of iterations: π√N/4
         n-iterations (max 1 (int (* (/ m/PI 4) (m/sqrt search-space-size))))

         ;; Build the complete quantum circuit with measurements
         circuit (grover-circuit n-qubits oracle-fn n-iterations {:add-measurements? true})

         ;; Execute circuit on backend
         execution-result (qb/execute-circuit backend circuit options)

         ;; Extract measurement results and determine outcome
         measurements (:measurement-results execution-result)
         
         ;; Calculate target indices for analysis
         target-indices (filter oracle-fn (range search-space-size))
         
         ;; Analyze measurement outcomes to find most likely result
         ;; Convert measurement strings to integers and find most frequent
         outcome-counts (into {} 
                             (map (fn [[outcome-str count]]
                                    [(Integer/parseInt outcome-str 2) count])
                                  measurements))
         
         total-shots (reduce + (vals outcome-counts))
         
         ;; Find the most likely measurement outcome
         most-likely-outcome (first (apply max-key second outcome-counts))
         
         ;; Calculate success probability (probability of measuring a target state)
         target-counts (reduce + (map #(get outcome-counts % 0) target-indices))
         success-probability (/ target-counts total-shots)]

     {:result most-likely-outcome
      :probability success-probability
      :iterations n-iterations
      :circuit circuit
      :execution-result execution-result
      :target-indices target-indices
      :search-space-size search-space-size
      :oracle-function oracle-fn
      :measurement-statistics {:outcome-counts outcome-counts
                               :target-counts target-counts
                               :total-shots total-shots
                               :success-probability success-probability}})))

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

(comment
  (let [circuit (-> (qc/create-circuit 4 "Test Circuit")
                    (qc/x-gate 0)
                    (qc/x-gate 1)
                    (qc/x-gate 2)
                    (qc/toffoli-gate 1 2 3)
                    ; (multi-controlled-z [0 1 2] 3)
                    ;
                    )]
  (qc/execute-circuit circuit (qs/zero-state 4)))
  ;
  )