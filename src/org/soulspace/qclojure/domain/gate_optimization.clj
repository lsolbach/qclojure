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
  changing the quantum computation.
  
  Additionally, rotation folding is implemented to combine consecutive rotation
  gates on the same axis, such as RX(θ₁) followed by RX(θ₂) becoming RX(θ₁+θ₂).
  This optimization reduces circuit depth and can eliminate rotations that
  sum to multiples of 2π (effectively identity operations)."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as circuit]))

;;;
;;; Gate Optimization - Circuit simplification through gate cancellation
;;;

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

;;;
;;; Rotation Folding - Combine consecutive rotation gates on same axis
;;;

;; Tolerance for angle comparison (from backend precision requirements)
(def ^:const angle-tolerance 1e-12)

;; Set of rotation gates that can be combined
(def rotation-gates
  "Set of rotation gates that can be folded together when applied consecutively
  on the same qubit and same rotation axis.
  
  Rotation gates that can be combined:
  - :rx (X-axis rotation): RX(θ₁) · RX(θ₂) = RX(θ₁ + θ₂)
  - :ry (Y-axis rotation): RY(θ₁) · RY(θ₂) = RY(θ₁ + θ₂)  
  - :rz (Z-axis rotation): RZ(θ₁) · RZ(θ₂) = RZ(θ₁ + θ₂)
  
  Phase gates (Z-axis rotations with specific angles):
  - :phase (General phase): Phase(θ₁) · Phase(θ₂) = Phase(θ₁ + θ₂) = RZ(θ₁ + θ₂)
  - :s (S gate): S = RZ(π/2) = Phase(π/2)
  - :s-dag (S† gate): S† = RZ(-π/2) = Phase(-π/2)  
  - :t (T gate): T = RZ(π/4) = Phase(π/4)
  - :t-dag (T† gate): T† = RZ(-π/4) = Phase(-π/4)
  
  All phase gates can be combined with each other and with RZ gates since
  they are all rotations around the Z-axis."
  #{:rx :ry :rz :phase :s :s-dag :t :t-dag})

(defn angle-equivalent?
  "Check if two angles are equivalent within tolerance.
  
  This function handles the periodic nature of rotations by comparing
  angles modulo 2π and considering floating-point precision.
  
  Parameters:
  - angle1: First angle in radians
  - angle2: Second angle in radians
  
  Returns:
  Boolean indicating if angles are equivalent within tolerance"
  [angle1 angle2]
  (let [diff (Math/abs (- angle1 angle2))
        two-pi (* 2 Math/PI)
        normalized-diff (rem diff two-pi)]
    (or (< normalized-diff angle-tolerance)
        (< (- two-pi normalized-diff) angle-tolerance))))

(defn normalize-angle
  "Normalize angle to [0, 2π) range and handle near-zero values.
  
  Parameters:
  - angle: Angle in radians
  
  Returns:
  Normalized angle, with values within tolerance of 0 or 2π set to 0"
  [angle]
  (let [two-pi (* 2 Math/PI)
        normalized (rem (mod angle two-pi) two-pi)]
    (cond
      (< normalized angle-tolerance) 0.0
      (< (- two-pi normalized) angle-tolerance) 0.0
      :else normalized)))

(defn gate-rotation-angle
  "Extract the rotation angle from a gate operation.
  
  For parametric rotation gates (:rx, :ry, :rz, :phase), extracts the :angle parameter.
  For fixed-angle phase gates, returns the equivalent rotation angle:
  - :s → π/2 (S gate)
  - :s-dag → -π/2 (S† gate)  
  - :t → π/4 (T gate)
  - :t-dag → -π/4 (T† gate)
  
  Parameters:
  - operation: Gate operation map
  
  Returns:
  Rotation angle in radians, or nil if not a rotation gate
  
  Examples:
  (gate-rotation-angle {:operation-type :rx, :operation-params {:target 0, :angle 1.5}})
  ;=> 1.5
  
  (gate-rotation-angle {:operation-type :s, :operation-params {:target 0}})
  ;=> 1.5707963267948966 (π/2)
  
  (gate-rotation-angle {:operation-type :t-dag, :operation-params {:target 0}})
  ;=> -0.7853981633974483 (-π/4)"
  [operation]
  (case (:operation-type operation)
    ;; Parametric rotation gates
    (:rx :ry :rz :phase) (get-in operation [:operation-params :angle])
    
    ;; Fixed-angle phase gates (Z-axis rotations)
    :s (/ Math/PI 2)           ; S = RZ(π/2)
    :s-dag (- (/ Math/PI 2))   ; S† = RZ(-π/2)  
    :t (/ Math/PI 4)           ; T = RZ(π/4)
    :t-dag (- (/ Math/PI 4))   ; T† = RZ(-π/4)
    
    ;; Not a rotation gate
    nil))

(defn gate-rotation-axis
  "Determine the rotation axis for a gate operation.
  
  Returns the axis of rotation for rotation gates:
  - :rx → :x
  - :ry → :y  
  - :rz, :phase, :s, :s-dag, :t, :t-dag → :z (all phase gates are Z-axis rotations)
  
  Parameters:
  - operation: Gate operation map
  
  Returns:
  Keyword representing rotation axis (:x, :y, :z), or nil if not a rotation gate"
  [operation]
  (case (:operation-type operation)
    :rx :x
    :ry :y
    (:rz :phase :s :s-dag :t :t-dag) :z
    nil))

(defn rotation-gates-combinable?
  "Check if two rotation gates can be combined into a single rotation.
  
  Two rotation gates can be combined if:
  1. They are both rotation gates on the same axis (X, Y, or Z)
  2. They act on the same target qubit
  3. No intervening gates act on the same qubit
  
  Phase gates (:s, :s-dag, :t, :t-dag, :phase) can combine with each other
  and with :rz gates since they are all Z-axis rotations.
  
  Parameters:
  - gate1: First rotation gate operation
  - gate2: Second rotation gate operation
  
  Returns:
  Boolean indicating whether gates can be combined"
  [gate1 gate2]
  (and (contains? rotation-gates (:operation-type gate1))
       (contains? rotation-gates (:operation-type gate2))
       (= (gate-rotation-axis gate1) (gate-rotation-axis gate2))
       (= (get-in gate1 [:operation-params :target])
          (get-in gate2 [:operation-params :target]))))

(defn combine-rotation-gates
  "Combine two rotation gates on the same axis into a single rotation.
  
  Combines rotation gates on the same axis by adding their angles.
  Phase gates (:s, :s-dag, :t, :t-dag, :phase) are treated as Z-axis
  rotations and can combine with each other and with :rz gates.
  
  The result is always returned as the more general parametric form:
  - X-axis rotations → :rx gate  
  - Y-axis rotations → :ry gate
  - Z-axis rotations → :rz gate (even for combined phase gates)
  
  Parameters:
  - gate1: First rotation gate with angle θ₁
  - gate2: Second rotation gate with angle θ₂
  
  Returns:
  New parametric rotation gate with combined angle θ₁ + θ₂, or nil if result is identity
  
  Examples:
  (combine-rotation-gates 
    {:operation-type :s, :operation-params {:target 0}}
    {:operation-type :t, :operation-params {:target 0}})
  ;=> {:operation-type :rz, :operation-params {:target 0, :angle 2.356...}} ; π/2 + π/4 = 3π/4"
  [gate1 gate2]
  {:pre [(rotation-gates-combinable? gate1 gate2)]}
  (let [angle1 (gate-rotation-angle gate1)
        angle2 (gate-rotation-angle gate2)
        combined-angle (+ angle1 angle2)
        normalized-angle (normalize-angle combined-angle)
        axis (gate-rotation-axis gate1)
        target-qubit (get-in gate1 [:operation-params :target])]
    (if (< normalized-angle angle-tolerance)
      ;; Combined rotation is effectively identity, return nil
      nil
      ;; Return combined rotation as parametric gate
      (case axis
        :x {:operation-type :rx, :operation-params {:target target-qubit, :angle normalized-angle}}
        :y {:operation-type :ry, :operation-params {:target target-qubit, :angle normalized-angle}}  
        :z {:operation-type :rz, :operation-params {:target target-qubit, :angle normalized-angle}}))))

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

;;;
;;; Inverse Pair Cancellation - Cancel gates that are inverses of each other
;;;

;; Map of gates to their inverses (non-self-inverse pairs)
(def inverse-gate-pairs
  "Map of quantum gates to their inverse gates.
  
  These are gates where G₁ · G₂ = I but G₁ ≠ G₂. This allows cancellation
  of different gates that are inverses of each other.
  
  Inverse pairs included:
  - S and S† (S-dagger): S gate and its inverse
  - T and T† (T-dagger): T gate and its inverse
  
  Note: Rotation gates RX(θ)/RX(-θ) are handled by rotation folding,
  not by this mechanism, since they can combine more generally."
  {:s :s-dag
   :s-dag :s
   :t :t-dag  
   :t-dag :t})

(defn gates-are-inverses?
  "Check if two gates are inverses of each other (G₁ · G₂ = I).
  
  This checks both self-inverse gates (where G₁ = G₂) and explicit
  inverse pairs (where G₁ ≠ G₂ but they cancel each other).
  
  Parameters:
  - gate1: First gate operation
  - gate2: Second gate operation
  
  Returns:
  Boolean indicating whether the gates are inverses and can cancel"
  [gate1 gate2]
  (let [op1 (:operation-type gate1)
        op2 (:operation-type gate2)
        params1 (:operation-params gate1)
        params2 (:operation-params gate2)]
    (and
     ;; Must act on same qubits
     (= (gate-qubits gate1) (gate-qubits gate2))
     
     ;; Check for inverse relationship
     (or
      ;; Self-inverse gates (existing functionality)
      (and (= op1 op2)
           (contains? self-inverse-gates op1)
           (= params1 params2))
      
      ;; Explicit inverse pairs
      (and (= (get inverse-gate-pairs op1) op2)
           (= params1 params2))))))

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

(defn find-inverse-cancellation-pairs
  "Find consecutive gate pairs that are inverses of each other.
  
  This function identifies pairs of gates that cancel each other through
  inverse relationships (G₁ · G₂ = I), including both self-inverse gates
  and explicit inverse pairs like S+S†, T+T†.
  
  Like other cancellation functions, this only finds pairs when no
  intervening gates act on the same qubits.
  
  Parameters:
  - operations: Vector of gate operations from a quantum circuit
  
  Returns:
  Vector of [i j] index pairs where operations i and j are inverses"
  [operations]
  (loop [i 0
         pairs []
         used-indices #{}]
    (if (>= i (count operations))
      pairs
      (if (contains? used-indices i)
        (recur (inc i) pairs used-indices)  ; Skip if already used
        
        (let [gate1 (nth operations i)
              gate1-qubits (gate-qubits gate1)
              next-index (find-next-gate-on-same-qubits operations i gate1-qubits)]
          
          (if (and next-index
                   (not (contains? used-indices next-index))
                   (gates-are-inverses? gate1 (nth operations next-index)))
            ;; Found an inverse pair
            (recur (inc i) 
                   (conj pairs [i next-index]) 
                   (conj used-indices i next-index))
            ;; No inverse found, continue
            (recur (inc i) pairs used-indices)))))))

(defn optimize-inverse-cancellations
  "Optimize gates by canceling inverse pairs.
  
  This function repeatedly finds and removes inverse gate pairs until
  no more cancellations are possible.
  
  Parameters:
  - operations: Vector of gate operations
  
  Returns:
  Vector of optimized operations with inverse pairs canceled"
  [operations]
  (loop [current-operations operations
         iteration 0]
    (let [cancellation-pairs (find-inverse-cancellation-pairs current-operations)]
      (if (empty? cancellation-pairs)
        current-operations  ; No more cancellations possible
        (let [optimized-ops (remove-cancellation-pairs current-operations cancellation-pairs)]
          (recur optimized-ops (inc iteration)))))))

(defn find-rotation-folding-pairs
  "Find consecutive rotation gate pairs that can be combined into single rotations.
  
  This function identifies pairs of rotation gates (RX, RY, RZ) on the same qubit
  that can be combined into a single rotation with the sum of their angles.
  Like gate cancellation, rotation folding only occurs when no intervening
  gates act on the same qubit.
  
  Algorithm:
  1. For each rotation gate, find the next rotation gate of the same type on the same qubit
  2. Ensure no intervening gates act on the same qubit
  3. Check if both gates can be combined
  4. Collect pairs and ensure no gate is used in multiple pairs
  
  Parameters:
  - operations: Vector of gate operations from a quantum circuit
  
  Returns:
  Vector of [i j] index pairs where operations i and j can be folded together,
  along with the combined gate operation that should replace them
  
  Examples:
  (find-rotation-folding-pairs
    [{:operation-type :rx, :operation-params {:target 0, :angle 0.5}}
     {:operation-type :rx, :operation-params {:target 0, :angle 1.0}}])
  ;=> [[[0 1] {:operation-type :rx, :operation-params {:target 0, :angle 1.5}}]]"
  [operations]
  (loop [i 0
         pairs []
         used-indices #{}]
    (if (>= i (count operations))
      pairs
      (if (contains? used-indices i)
        (recur (inc i) pairs used-indices)
        (let [op1 (nth operations i)
              type1 (:operation-type op1)]
          (if (contains? rotation-gates type1)
            (let [target-qubits (gate-qubits op1)
                  next-index (find-next-gate-on-same-qubits operations i target-qubits)]
              (if (and next-index
                       (not (contains? used-indices next-index))
                       (rotation-gates-combinable? op1 (nth operations next-index)))
                (let [combined-gate (combine-rotation-gates op1 (nth operations next-index))]
                  (if combined-gate
                    ;; Non-identity result, add to folding pairs
                    (recur (inc i)
                           (conj pairs [[i next-index] combined-gate])
                           (conj used-indices i next-index))
                    ;; Identity result, add to cancellation (removal) pairs
                    (recur (inc i)
                           (conj pairs [[i next-index] nil])
                           (conj used-indices i next-index))))
                (recur (inc i) pairs used-indices)))
            (recur (inc i) pairs used-indices)))))))

(defn apply-rotation-folding
  "Apply rotation folding transformations to circuit operations.
  
  Takes operations and folding pairs, then returns new operations with
  rotation gates folded together or removed (if they sum to identity).
  
  Parameters:
  - operations: Vector of gate operations
  - folding-pairs: Vector of [[i j] combined-gate] pairs
  
  Returns:
  New vector of operations with rotation folding applied"
  [operations folding-pairs]
  (if (empty? folding-pairs)
    operations
    (let [;; Separate pairs into replacements and removals  
          replacement-pairs (filter #(some? (second %)) folding-pairs)
          
          ;; Get indices to remove (including those being replaced)
          indices-to-remove (set (mapcat #(first %) folding-pairs))
          
          ;; Create replacement map: first-index -> new-gate
          replacements (into {} 
                             (map (fn [[[i _j] new-gate]] [i new-gate]) 
                                  replacement-pairs))
          
          ;; Apply transformations
          new-ops (vec (keep-indexed
                        (fn [idx op]
                          (cond
                            ;; This index should be replaced
                            (contains? replacements idx)
                            (get replacements idx)
                            
                            ;; This index should be removed
                            (contains? indices-to-remove idx)
                            nil
                            
                            ;; Keep unchanged
                            :else op))
                        operations))]
      new-ops)))

(defn optimize-rotations
  "Optimize rotation gates by folding consecutive rotations on same axis.
  
  This function repeatedly applies rotation folding until no more improvements
  can be made. It combines consecutive rotation gates of the same type on the
  same qubit, reducing circuit depth and eliminating identity rotations.
  
  Parameters:
  - operations: Vector of gate operations
  
  Returns:
  Vector of optimized operations with rotation folding applied"
  [operations]
  (loop [current-operations operations
         iteration 0]
    (let [folding-pairs (find-rotation-folding-pairs current-operations)]
      (if (empty? folding-pairs)
        current-operations  ; No more optimizations possible
        (let [optimized-ops (apply-rotation-folding current-operations folding-pairs)]
          (recur optimized-ops (inc iteration)))))))

(defn is-identity-rotation?
  "Check if a rotation gate is effectively an identity operation.
  
  A rotation gate is considered an identity operation if its angle
  is normalized to 0.0, which includes:
  - Explicit zero angles (0.0)
  - Angles that are multiples of 2π (e.g., 2π, 4π, -2π)
  - Angles very close to zero or multiples of 2π (within tolerance)
  
  Parameters:
  - operation: Gate operation to check
  
  Returns:
  Boolean indicating whether the rotation is an identity operation"
  [operation]
  (and (contains? rotation-gates (:operation-type operation))
       (let [angle (get-in operation [:operation-params :angle])]
         (when angle
           (< (Math/abs (normalize-angle angle)) angle-tolerance)))))

(defn remove-identity-rotations
  "Remove rotation gates that are effectively identity operations.
  
  This function filters out rotation gates (RX, RY, RZ) that have angles
  normalized to zero, which means they don't change the quantum state and
  can be safely removed from the circuit.
  
  This optimization is particularly useful after rotation folding, where
  consecutive rotations may combine to create identity operations.
  
  Parameters:
  - operations: Vector of gate operations
  
  Returns:
  Vector of operations with identity rotations removed
  
  Examples:
  (remove-identity-rotations
    [{:operation-type :h, :operation-params {:target 0}}
     {:operation-type :rx, :operation-params {:target 0, :angle 0.0}}
     {:operation-type :cnot, :operation-params {:control 0, :target 1}}])
  ;=> [{:operation-type :h, :operation-params {:target 0}}
  ;    {:operation-type :cnot, :operation-params {:control 0, :target 1}}]"
  [operations]
  (filterv #(not (is-identity-rotation? %)) operations))

(defn optimize-gates
  "Optimize a quantum circuit by removing consecutive self-canceling gates
  and folding consecutive rotation gates.
  
  This is the main optimization function that repeatedly applies gate
  cancellation and rotation folding optimizations until no more improvements
  can be made. The function:
  
  1. Applies inverse pair cancellation (S+S†, T+T†) for direct cancellations
  2. Applies rotation folding to combine consecutive rotations (RX, RY, RZ, phase gates)
  3. Removes identity rotations (gates with angle 0 or 2π)
  4. Identifies consecutive gates that cancel each other (e.g., H-H, X-X)
  5. Ensures no intervening gates share qubits with the canceling gates  
  6. Removes these safe-to-cancel gate pairs
  7. Repeats until no more optimizations are possible
  8. Returns the optimized circuit
  
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
          :optimize-rotations? - Boolean to enable/disable rotation folding (default: true)
          :optimize-inverse-pairs? - Boolean to enable/disable inverse pair cancellation (default: true)

  Returns:
  Updated context map with optimized circuit under :circuit key

  Example:
  (optimize-gates {:circuit my-circuit, :options {:optimize-gates? true}})
  ;=> {:circuit <optimized-circuit>, :options {...}}"
  [ctx]
  {:pre [(s/valid? ::circuit/circuit (:circuit ctx))]}
  (if (get-in ctx [:options :optimize-gates?] true)
    ; Perform optimization and return updated context
    (let [circuit (:circuit ctx)
          optimize-rotations? (get-in ctx [:options :optimize-rotations?] true)
          optimize-inverse-pairs? (get-in ctx [:options :optimize-inverse-pairs?] true)
          optimized-circuit (loop [current-circuit circuit
                                   iteration 0]
                              (let [operations (:operations current-circuit)

                                    ;; Apply inverse pair cancellation first
                                    inverse-optimized-ops (if optimize-inverse-pairs?
                                                            (optimize-inverse-cancellations operations)
                                                            operations)

                                    ;; Then apply rotation folding 
                                    rotation-optimized-ops (if optimize-rotations?
                                                             (optimize-rotations inverse-optimized-ops)
                                                             inverse-optimized-ops)

                                    ;; Remove identity rotations (after folding)
                                    identity-removed-ops (if optimize-rotations?
                                                           (remove-identity-rotations rotation-optimized-ops)
                                                           rotation-optimized-ops)

                                    ;; Finally apply self-inverse gate cancellation
                                    pairs (find-cancellation-pairs identity-removed-ops)]
                                (if (empty? pairs)
                                  (assoc current-circuit :operations identity-removed-ops)  ; No more optimizations possible
                                  (let [optimized-ops (remove-cancellation-pairs identity-removed-ops pairs)
                                        optimized-circuit (assoc current-circuit :operations optimized-ops)]
                                    (recur optimized-circuit (inc iteration))))))]
      (if (circuit/empty-circuit? optimized-circuit)
        (throw (ex-info "Optimization resulted in an empty circuit"
                        {:circuit circuit
                         :optimized optimized-circuit}))
        (assoc ctx :circuit optimized-circuit)))
    ; No optimization requested, return original context
    ctx))

(comment
  ;; Example 1: Hadamard gate cancellation
  ;; H-H gates cancel because H² = I
  (def hadamard-test-circuit
    (-> (circuit/create-circuit 1 "Hadamard Test")
        (circuit/h-gate 0)
        (circuit/h-gate 0)))

  ;; Should result in exception because circuit becomes empty
  (try
    (optimize-gates {:circuit hadamard-test-circuit
                     :options {:optimize-gates? true}})
    (catch Exception e
      (println "Caught exception:" (.getMessage e))
      (println "Data:" (ex-data e))))

  ;; Example 1.5: Rotation folding examples
  ;; Test basic rotation combination
  (def rotation-test-circuit
    {:num-qubits 2
     :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
                  {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
                  {:operation-type :ry :operation-params {:target 1 :angle 0.3}}
                  {:operation-type :ry :operation-params {:target 1 :angle 0.7}}]})

  (def optimized-rotation
    (:circuit (optimize-gates {:circuit rotation-test-circuit
                               :options {:optimize-gates? true :optimize-rotations? true}})))
  ;; Expected: RX(0.5) + RX(1.0) = RX(1.5) on qubit 0
  ;;           RY(0.3) + RY(0.7) = RY(1.0) on qubit 1
  (:operations optimized-rotation)

  ;; Example 1.6: Inverse pair cancellation examples
  ;; Test S and S† cancellation
  (def inverse-pair-circuit
    {:num-qubits 2
     :operations [{:operation-type :s :operation-params {:target 0}}
                  {:operation-type :s-dag :operation-params {:target 0}}
                  {:operation-type :t :operation-params {:target 1}}
                  {:operation-type :t-dag :operation-params {:target 1}}
                  {:operation-type :x :operation-params {:target 0}}]})

  (def optimized-inverse
    (:circuit (optimize-gates {:circuit inverse-pair-circuit
                               :options {:optimize-gates? true :optimize-inverse-pairs? true}})))
  ;; Expected: S + S† cancels, T + T† cancels, only X gate remains
  (:operations optimized-inverse)

  ;; Test comprehensive optimization (all three types)
  (def comprehensive-circuit
    {:num-qubits 3
     :operations [{:operation-type :h :operation-params {:target 0}}    ; Self-inverse
                  {:operation-type :h :operation-params {:target 0}}    ; cancellation
                  {:operation-type :rx :operation-params {:target 1 :angle 0.5}}  ; Rotation
                  {:operation-type :rx :operation-params {:target 1 :angle 1.0}}  ; folding
                  {:operation-type :s :operation-params {:target 2}}     ; Inverse
                  {:operation-type :s-dag :operation-params {:target 2}} ; pairs
                  {:operation-type :x :operation-params {:target 0}}]})  ; Remaining

  (def optimized-comprehensive
    (:circuit (optimize-gates {:circuit comprehensive-circuit
                               :options {:optimize-gates? true 
                                        :optimize-rotations? true 
                                        :optimize-inverse-pairs? true}})))
  ;; Expected: Only RX(1.5) on qubit 1 and X on qubit 0 remain
  (:operations optimized-comprehensive)

  ;; Test rotation folding to identity (2π elimination)
  (def identity-rotation-circuit
    {:num-qubits 2
     :operations [{:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
                  {:operation-type :rx :operation-params {:target 0 :angle Math/PI}}
                  {:operation-type :h :operation-params {:target 1}}]})

  (def optimized-identity
    (:circuit (optimize-gates {:circuit identity-rotation-circuit
                               :options {:optimize-gates? true :optimize-rotations? true}})))
  ;; Expected: RX(π) + RX(π) = RX(2π) ≡ I (removed), only H gate remains
  (:operations optimized-identity)

  ;; Test mixed optimization (rotation folding + gate cancellation)
  (def mixed-optimization-circuit
    {:num-qubits 2
     :operations [{:operation-type :rx :operation-params {:target 0 :angle 0.5}}
                  {:operation-type :rx :operation-params {:target 0 :angle 1.0}}
                  {:operation-type :h :operation-params {:target 1}}
                  {:operation-type :h :operation-params {:target 1}}
                  {:operation-type :ry :operation-params {:target 0 :angle 0.25}}]})

  (def optimized-mixed
    (:circuit (optimize-gates {:circuit mixed-optimization-circuit
                               :options {:optimize-gates? true :optimize-rotations? true}})))
  ;; Expected: RX(0.5) + RX(1.0) = RX(1.5), H+H cancels, RY(0.25) remains
  (:operations optimized-mixed)

  ;; Example 2: Circuit with multiple consecutive gate cancellations
  (def circuit-with-multiple-cancellations
    (-> (circuit/create-circuit 3 "Multiple Cancellations")
        (circuit/h-gate 0)      ; H on qubit 0
        (circuit/h-gate 0)      ; H on qubit 0 - cancels with first H
        (circuit/x-gate 1)      ; X on qubit 1
        (circuit/x-gate 1)      ; X on qubit 1 - cancels with first X
        (circuit/y-gate 2)      ; Y on qubit 2
        (circuit/z-gate 0)      ; Z on qubit 0 - remains
        (circuit/y-gate 2)))    ; Y on qubit 2 - cancels with first Y

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
    (-> (circuit/create-circuit 2 "Qubit-wise Consecutive Gates")
        (circuit/h-gate 0)      ; H on qubit 0
        (circuit/x-gate 1)      ; X on qubit 1
        (circuit/h-gate 0)      ; H on qubit 0 - cancels with first H
        (circuit/x-gate 1)))    ; X on qubit 1 - cancels with first X

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
    (-> (circuit/create-circuit 2 "Pauli Test")
        (circuit/x-gate 0)      ; X gate on qubit 0
        (circuit/y-gate 1)      ; Y gate on qubit 1
        (circuit/x-gate 0)      ; X gate on qubit 0 - cancels with first X
        (circuit/z-gate 0)      ; Z gate on qubit 0 - remains
        (circuit/y-gate 1)))    ; Y gate on qubit 1 - cancels with first Y

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
  (def bell-circuit (circuit/bell-state-circuit))
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
  (def consecutive-circuit (-> (circuit/create-circuit 1)
                               (circuit/h-gate 0)
                               (circuit/h-gate 0)
                               (circuit/x-gate 0)
                               (circuit/x-gate 0)))

  (try
    (:circuit (optimize-gates {:circuit consecutive-circuit
                               :options {:optimize-gates? true}}))
    (catch Exception e
      (println "Caught exception:" (.getMessage e))
      (println "Data:" (ex-data e))))

  ;; This will NOT be optimized (non-consecutive H gates)
  (def non-consecutive-circuit (-> (circuit/create-circuit 1)
                                   (circuit/h-gate 0)
                                   (circuit/x-gate 0)
                                   (circuit/h-gate 0)
                                   (circuit/x-gate 0)))
  
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
