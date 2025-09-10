(ns org.soulspace.qclojure.domain.gate-optimization
  "Gate optimization functions for quantum circuits
  
  This namespace provides functions to optimize quantum circuits by removing
  redundant gates and simplifying gate sequences. The primary optimization
  implemented is cancellation of consecutive self-inverse gates.
  
  Self-inverse gates include:
  - Single-qubit gates: X, Y, Z, H (Pauli gates and Hadamard)
  - Two-qubit gates: CNOT (controlled-X gate), CX, CY, CZ, SWAP
  - Three-qubit gates: Toffoli (CCX), Fredkin (CSWAP)
   
  When two identical self-inverse gates are applied consecutively to the same
  qubit(s), they cancel out and can be removed from the circuit without
  changing the quantum computation."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit :as circuit]))

;; TODO rotation gates with a combined rotation of multiples of 2 * PI

;;
;; Gate Optimization - Circuit simplification through gate cancellation
;;

;; Set of gates that are self-inverse (G² = I)
(def self-inverse-gates
  "Set of quantum gates that are their own inverse.
  
  These gates satisfy the property G² = I, meaning applying the same gate
  twice in succession results in the identity operation and can be removed
  from the circuit without affecting the quantum computation.
  
  Single-qubit self-inverse gates:
  - :x (Pauli-X): Bit flip gate, X² = I
  - :y (Pauli-Y): Bit and phase flip gate, Y² = I  
  - :z (Pauli-Z): Phase flip gate, Z² = I
  - :h (Hadamard): Superposition gate, H² = I
  
  Two-qubit self-inverse gates:
  - :cnot (Controlled-NOT): Controlled bit flip, CNOT² = I
  - :cx (Controlled-X): Alias for CNOT, CX² = I  
  - :cy (Controlled-Y): Controlled bit and phase flip, CY² = I
  - :cz (Controlled-Z): Controlled phase flip, CZ² = I
  - :swap (SWAP): Exchange states of two qubits, SWAP² = I
  
  Three-qubit self-inverse gates:
  - :toffoli (Toffoli/CCX): Controlled-controlled-X gate, Toffoli² = I
  - :ccx (CCX): Alias for Toffoli, CCX² = I
  - :fredkin (Fredkin/CSWAP): Controlled SWAP gate, Fredkin² = I  
  - :cswap (CSWAP): Alias for Fredkin, CSWAP² = I
  
  Note: iSWAP is NOT self-inverse (iSWAP² ≠ I) and is not included."
  #{:x :y :z :h :cnot :cx :cy :cz :swap :toffoli :ccx :fredkin :cswap
    ;; Rydberg gates - some are self-inverse
    :rydberg-cz
    ;; Global gates - self-inverse single-qubit gates applied globally  
    :global-x :global-y :global-z :global-h})

(defn gate-qubits
  "Extract the qubits that a gate operation acts upon.
  
  Returns a set of qubit indices for comparison purposes. This function
  handles both single-qubit and multi-qubit gates by extracting the
  relevant qubit parameters from the operation.
  
  Parameters:
  - operation: Gate operation map with :operation-type and :operation-params
  
  Returns:
  Set of qubit indices that the gate operates on
  
  Examples:
  (gate-qubits {:operation-type :x, :operation-params {:target 0}})
  ;=> #{0}
  
  (gate-qubits {:operation-type :cnot, :operation-params {:control 0, :target 1}})
  ;=> #{0 1}"
  [operation]
  (let [params (:operation-params operation)
        op-type (:operation-type operation)]
    (case op-type
      ;; Single-qubit gates
      (:x :y :z :h :s :s-dag :t :t-dag :rx :ry :rz :phase)
      #{(:target params)}

      ;; Two-qubit controlled gates
      (:cnot :cx :cz :cy :crx :cry :crz)
      #{(:control params) (:target params)}

      ;; Rydberg two-qubit gates
      (:rydberg-cz :rydberg-cphase)
      #{(:control params) (:target params)}

      ;; Rydberg multi-qubit blockade gate
      :rydberg-blockade
      (set (:qubit-indices params))

      ;; SWAP gates
      (:swap :iswap)
      #{(:qubit1 params) (:qubit2 params)}

      ;; Multi-qubit gates
      :toffoli
      #{(:control1 params) (:control2 params) (:target params)}

      :fredkin
      #{(:control params) (:target1 params) (:target2 params)}

      ;; Global gates - affect all qubits in the circuit (need circuit context)
      (:global-x :global-y :global-z :global-h :global-rx :global-ry :global-rz)
      ;; For global gates, we need the circuit context to determine num-qubits
      ;; For now, return empty set - this will be handled by circuit analysis
      (if-let [circuit-qubits (:circuit-qubits params)]
        (set (range circuit-qubits))
        #{})

      ;; Default: empty set for unknown operations
      #{})))

(defn gates-equivalent?
  "Check if two gate operations are equivalent for cancellation purposes.
  
  Two gates are considered equivalent if they:
  1. Have the same operation type
  2. Act on the same set of qubits with the same roles
  3. Have the same parameters (for parametric gates)
  4. Are self-inverse gates (gates that cancel when applied twice)
  
  Special handling for symmetric gates:
  - SWAP gates: SWAP(a,b) is equivalent to SWAP(b,a)
  
  This function is used to identify consecutive gates that can cancel each other.
  
  Parameters:
  - gate1: First gate operation map
  - gate2: Second gate operation map
  
  Returns:
  Boolean indicating whether the gates are equivalent and can cancel
  
  Examples:
  (gates-equivalent? 
    {:operation-type :x, :operation-params {:target 0}}
    {:operation-type :x, :operation-params {:target 0}})
  ;=> true
  
  (gates-equivalent?
    {:operation-type :cnot, :operation-params {:control 0, :target 1}}
    {:operation-type :cnot, :operation-params {:control 0, :target 1}})
  ;=> true
  
  (gates-equivalent?
    {:operation-type :swap, :operation-params {:qubit1 0, :qubit2 1}}
    {:operation-type :swap, :operation-params {:qubit1 1, :qubit2 0}})
  ;=> true (SWAP is symmetric)
  
  (gates-equivalent?
    {:operation-type :rx, :operation-params {:target 0, :angle 1.5708}}
    {:operation-type :rx, :operation-params {:target 0, :angle 1.5708}})
  ;=> false (RX is not self-inverse)"
  [gate1 gate2]
  (and (= (:operation-type gate1) (:operation-type gate2))
       (contains? self-inverse-gates (:operation-type gate1))
       (let [op-type (:operation-type gate1)
             params1 (:operation-params gate1)
             params2 (:operation-params gate2)]
         (case op-type
           ;; SWAP gates are symmetric: SWAP(a,b) == SWAP(b,a)
           :swap (or (= params1 params2)
                     (and (= (:qubit1 params1) (:qubit2 params2))
                          (= (:qubit2 params1) (:qubit1 params2))))

           ;; For all other gates, require exact parameter match
           (= params1 params2)))))

(defn find-next-gate-on-same-qubits
  "Find the next gate that acts on the same qubits with no interfering gates.
  
  This function looks ahead from a given position to find the next operation
  that affects the same set of qubits, but ONLY if there are no intervening
  gates that act on ANY of those qubits. This ensures that gates can only
  cancel when they are truly consecutive in quantum mechanical terms.
  
  The key insight: Gates can only cancel if ALL intervening gates commute
  with both canceling gates. In practice, this means no intervening gates
  can share ANY qubits with the canceling gates.
  
  Parameters:
  - operations: Vector of gate operations
  - start-index: Index to start searching from
  - target-qubits: Set of qubits to match
  
  Returns:
  Index of next gate acting on same qubits with no interference, or nil if none found"
  [operations start-index target-qubits]
  (loop [i (inc start-index)]
    (cond
      (>= i (count operations)) nil

      ;; Found a gate on the same qubits - check if we can cancel
      (= (gate-qubits (nth operations i)) target-qubits) i

      ;; Found a gate that shares ANY qubits with target - cannot cancel
      (seq (set/intersection (gate-qubits (nth operations i)) target-qubits)) nil

      ;; Gate acts on completely different qubits - continue searching
      :else (recur (inc i)))))

(defn find-cancellation-pairs
  "Find consecutive gate pairs that cancel each other in a circuit.
  
  This function finds pairs of self-inverse gates that can cancel each other,
  but ONLY when there are no intervening gates that act on any of the same
  qubits. This ensures quantum mechanical correctness.
  
  Key principle: Two gates can only cancel if all gates between them commute
  with both canceling gates. In practice, this means no intervening gates
  can share ANY qubits with the canceling gates.
  
  The algorithm works by:
  1. For each self-inverse operation, finding the next operation on the same qubits
  2. Ensuring no intervening operations share qubits with the canceling gates
  3. Checking if both operations are equivalent self-inverse gates
  4. Collecting pairs that can safely cancel each other
  5. Ensuring no operation is used in multiple pairs
  
  Parameters:
  - operations: Vector of gate operations from a quantum circuit
  
  Returns:
  Vector of [i j] index pairs where operations i and j cancel each other
  
  Examples:
  ;; Literally adjacent gates - CAN cancel
  (find-cancellation-pairs 
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 0}}])
  ;=> [[0 1]]
  
  ;; Gates separated by operations on different qubits - CAN cancel
  (find-cancellation-pairs 
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}
     {:operation-type :h, :operation-params {:target 0}}])
  ;=> [[0 2]]
  
  ;; Gates separated by operations on shared qubits - CANNOT cancel
  (find-cancellation-pairs 
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :cnot, :operation-params {:control 0, :target 1}}
     {:operation-type :h, :operation-params {:target 0}}])
  ;=> [] (no cancellation - CNOT shares qubit 0 with H gates)"
  [operations]
  (loop [i 0
         pairs []
         used-indices #{}]
    (if (>= i (count operations))
      pairs
      (if (contains? used-indices i)
        (recur (inc i) pairs used-indices)  ; Skip if already used in a pair
        (let [op1 (nth operations i)
              type1 (:operation-type op1)]
          (if (contains? self-inverse-gates type1)
            (let [target-qubits (gate-qubits op1)
                  next-index (find-next-gate-on-same-qubits operations i target-qubits)]
              (if (and next-index
                       (not (contains? used-indices next-index))
                       (gates-equivalent? op1 (nth operations next-index)))
                (recur (inc i)
                       (conj pairs [i next-index])
                       (conj used-indices i next-index))
                (recur (inc i) pairs used-indices)))
            (recur (inc i) pairs used-indices)))))))

(defn remove-cancellation-pairs
  "Remove consecutive canceling gate pairs from circuit operations.
  
  Takes a vector of operations and a collection of index pairs representing
  gates that cancel each other, then returns a new operations vector with
  those pairs removed.
  
  The function processes pairs in reverse order to maintain correct indices
  during removal operations.
  
  Parameters:
  - operations: Vector of gate operations
  - pairs: Collection of [i j] index pairs to remove
  
  Returns:
  New vector of operations with canceling pairs removed
  
  Example:
  (remove-cancellation-pairs
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :h, :operation-params {:target 0}}
     {:operation-type :x, :operation-params {:target 1}}]
    [[0 1]])
  ;=> [{:operation-type :x, :operation-params {:target 1}}]"
  [operations pairs]
  (if (empty? pairs)
    operations
    (let [indices-to-remove (set (mapcat identity pairs))
          filtered-ops (vec (keep-indexed
                             #(when-not (contains? indices-to-remove %1) %2)
                             operations))]
      filtered-ops)))

(defn optimize-gates
  "Optimize a quantum circuit by removing consecutive self-canceling gates.
  
  This is the main optimization function that repeatedly applies cancellation
  optimization until no more improvements can be made. The function:
  
  1. Identifies consecutive gates that cancel each other (e.g., H-H, X-X)
  2. Ensures no intervening gates share qubits with the canceling gates  
  3. Removes these safe-to-cancel gate pairs
  4. Repeats until no more cancellations are possible
  5. Returns the optimized circuit
  
  IMPORTANT: Gates can only cancel when all intervening gates act on completely
  different qubits. This ensures quantum mechanical correctness and prevents
  incorrect optimizations like canceling H gates across CNOT gates.
  
  The optimization preserves the quantum computation while reducing the
  number of gates, which can improve:
  - Circuit depth and execution time
  - Gate fidelity on noisy quantum devices
  - Resource requirements for simulation
  
  Parameters:
  - ctx: Map containing:
      :circuit - Quantum circuit to optimize
      :options - Map with optimization options, including:
          :optimize-gates? - Boolean to enable/disable gate optimization (default: true)  

  Returns:
  Updated context map with optimized circuit under :circuit key

  Example:
  (optimize-gates {:circuit my-circuit, :options {:optimize-gates? true}})
  ;=> {:circuit <optimized-circuit>, :options {...}}"
  [ctx]
  {:pre [(s/valid? ::qc/circuit (:circuit ctx))]}
  (if-not (get-in ctx [:options :optimize-gates?])
    ; No optimization requested, return original context
    ctx
    ; Perform optimization and return updated context
    (let [circuit (:circuit ctx)
          optimized-circuit (loop [current-circuit circuit
                                   iteration 0]
                              (let [operations (:operations current-circuit)
                                    pairs (find-cancellation-pairs operations)]
                                (if (empty? pairs)
                                  current-circuit  ; No more optimizations possible
                                  (let [optimized-ops (remove-cancellation-pairs operations pairs)
                                        optimized-circuit (assoc current-circuit :operations optimized-ops)]
                                    (recur optimized-circuit (inc iteration))))))]
      (if (qc/empty-circuit? optimized-circuit)
        (throw (ex-info "Optimization resulted in an empty circuit"
                        {:circuit circuit
                         :optimized optimized-circuit}))
        (assoc ctx :circuit optimized-circuit)))))


(comment
  ;; Example 1: Hadamard gate cancellation
  ;; H-H gates cancel because H² = I
  (def hadamard-test-circuit
    (-> (qc/create-circuit 1 "Hadamard Test")
        (qc/h-gate 0)
        (qc/h-gate 0)))

  ;; Should result in exception because circuit becomes empty
  (try
    (optimize-gates {:circuit hadamard-test-circuit
                     :options {:optimize-gates? true}})
    (catch Exception e
      (println "Caught exception:" (.getMessage e))
      (println "Data:" (ex-data e))))

  ;; Example 2: Circuit with multiple consecutive gate cancellations
  (def circuit-with-multiple-cancellations
    (-> (qc/create-circuit 3 "Multiple Cancellations")
        (qc/h-gate 0)      ; H on qubit 0
        (qc/h-gate 0)      ; H on qubit 0 - cancels with first H
        (qc/x-gate 1)      ; X on qubit 1
        (qc/x-gate 1)      ; X on qubit 1 - cancels with first X
        (qc/y-gate 2)      ; Y on qubit 2
        (qc/z-gate 0)      ; Z on qubit 0 - remains
        (qc/y-gate 2)))    ; Y on qubit 2 - cancels with first Y

  ; Check optimization, should leave only Z gate on qubit 0
  (def optimized-multiple
    (:circuit (optimize-gates {:circuit circuit-with-multiple-cancellations
                               :options {:optimize-gates? true}})))
  (println "Original operations:" (count (:operations circuit-with-multiple-cancellations)))
  (println "Optimized operations:" (count (:operations optimized-multiple)))
  (println "Final operations:" (:operations optimized-multiple))
  ;; Expected: H-H and X-X pairs cancel, Y-Y pair also cancels, leaving only Z(0)

  ;; Example 3: Circuit with qubit-wise consecutive gates (should be optimized)
  (def circuit-with-qubit-wise-consecutive-gates
    (-> (qc/create-circuit 2 "Qubit-wise Consecutive Gates")
        (qc/h-gate 0)      ; H on qubit 0
        (qc/x-gate 1)      ; X on qubit 1
        (qc/h-gate 0)      ; H on qubit 0 - cancels with first H
        (qc/x-gate 1)))    ; X on qubit 1 - cancels with first X

  ;; Expected: All gates cancelled because H gates on qubit 0 are consecutive 
  ;; and X gates on qubit 1 are consecutive (gates on different qubits can be reordered)
  (try (def optimized-qubit-wise
         (:circuit (optimize-gates {:circuit circuit-with-qubit-wise-consecutive-gates
                                    :options {:optimize-gates? true}})))
       (catch Exception e
         (println "Caught exception:" (.getMessage e))
         (println "Data:" (ex-data e))))

  ;; Example 4: Pauli gate cancellations with qubit-wise consecutive gates
  (def pauli-circuit
    (-> (qc/create-circuit 2 "Pauli Test")
        (qc/x-gate 0)      ; X gate on qubit 0
        (qc/y-gate 1)      ; Y gate on qubit 1
        (qc/x-gate 0)      ; X gate on qubit 0 - cancels with first X
        (qc/z-gate 0)      ; Z gate on qubit 0 - remains
        (qc/y-gate 1)))    ; Y gate on qubit 1 - cancels with first Y

  (def optimized-pauli
    (:circuit (optimize-gates {:circuit pauli-circuit
                               :options {:optimize-gates? true}})))

  ;; Should have only Z gate remaining
  (:operations optimized-pauli)
  ;=> [{:operation-type :z, :operation-params {:target 0}}]
  ;; The X gates on qubit 0 are consecutive (indices 0,2) and cancel
  ;; The Y gates on qubit 1 are consecutive (indices 1,4) and cancel
  ;; The Z gate on qubit 0 (index 3) remains

  ;; Example 5: Complex circuit with Bell state preparation 
  ;; This should NOT be optimized away
  (def bell-circuit (qc/bell-state-circuit))
  (def optimized-bell
    (:circuit (optimize-gates {:circuit bell-circuit
                               :options {:optimize-gates? true}})))

  ;; Bell circuit should remain unchanged (H + CNOT are not self-canceling)
  (:operations optimized-bell)
  ;=> Same as original Bell circuit

  ;; Example 6: Test gate-qubits helper function
  (gate-qubits {:operation-type :x, :operation-params {:target 0}})
  ;=> #{0}

  (gate-qubits {:operation-type :cnot, :operation-params {:control 0, :target 1}})
  ;=> #{0 1}

  ;; Example 7: Test gates-equivalent? function
  (gates-equivalent?
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 0}})
  ;=> true

  (gates-equivalent?
   {:operation-type :h, :operation-params {:target 0}}
   {:operation-type :h, :operation-params {:target 1}})
  ;=> false

  ;; Test with non-self-inverse gate (should be false)
  (gates-equivalent?
   {:operation-type :rx, :operation-params {:target 0, :angle 1.5708}}
   {:operation-type :rx, :operation-params {:target 0, :angle 1.5708}})
  ;=> false (RX is not self-inverse)

  ;; Example 8: When gates should NOT cancel - same qubit, interfering operations
  (def no-cancel-circuit
    {:num-qubits 1
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :x :operation-params {:target 0}}  ; Interferes with H gates
                  {:operation-type :h :operation-params {:target 0}}]})

  (def optimized-no-cancel
    (:circuit (optimize-gates {:circuit no-cancel-circuit
                               :options {:optimize-gates? true}})))
  (:operations optimized-no-cancel)
  ;=> All 3 gates remain - H gates cannot cancel because X gate interferes

  ;; Example 10: Consecutive vs non-consecutive cancellation
  ;; This WILL be optimized (consecutive H gates)
  (def consecutive-circuit (-> (qc/create-circuit 1)
                               (qc/h-gate 0)
                               (qc/h-gate 0)
                               (qc/x-gate 0)
                               (qc/x-gate 0)))

  (try
    (:circuit (optimize-gates {:circuit consecutive-circuit
                               :options {:optimize-gates? true}}))
    (catch Exception e
      (println "Caught exception:" (.getMessage e))
      (println "Data:" (ex-data e))))

  ;; This will NOT be optimized (non-consecutive H gates)
  (def non-consecutive-circuit (-> (qc/create-circuit 1)
                                   (qc/h-gate 0)
                                   (qc/x-gate 0)
                                   (qc/h-gate 0)
                                   (qc/x-gate 0)))
  
  (def optimized-non-consecutive
    (:circuit (optimize-gates {:circuit non-consecutive-circuit
                               :options {:optimize-gates? true}})))
  (:operations optimized-non-consecutive)
  ;=> [{:operation-type :h, ...} {:operation-type :x, ...} 
  ;    {:operation-type :h, ...} {:operation-type :x, ...}] (unchanged)

  ;; Evaluate individual functions for testing
  (find-cancellation-pairs (:operations hadamard-test-circuit))
  ;=> [[0 1]]

  self-inverse-gates

  ;
  ) 
