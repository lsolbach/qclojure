(ns org.soulspace.qclojure.domain.gate-decomposition
  "Quantum gate decomposition and circuit transformation for quantum hardware compatibility.
  
  This namespace provides functionality to decompose complex quantum gates into simpler,
  hardware-native operations. It enables quantum circuits to be transformed to match the
  specific gate sets supported by different quantum hardware platforms and simulators.
    
  Gate Decomposition:
  - Decomposes complex gates (Toffoli, Fredkin, etc.) into universal gate sets
  - Handles parametric gates (rotation gates with arbitrary angles)
  - Supports multiple decomposition strategies based on target hardware
  
  Circuit Transformation:
  - Transforms entire quantum circuits to use only supported operations
  - Preserves quantum circuit semantics while changing implementation
  - Provides iterative decomposition with cycle detection
  
  Gate Decomposition Examples
  
  ```clojure
  ;; Decompose Toffoli gate for CNOT+T gate set
  (decompose-operation 
    {:operation-type :toffoli 
     :operation-params {:control1 0 :control2 1 :target 2}}
    :cnot-t)
  
  ;; Decompose rotation gate with specific angle
  (decompose-operation
    {:operation-type :ry :operation-params {:target 0 :angle (/ Math/PI 3)}}
    #{:rz :x :cnot})
  ```
  
  See also: `org.soulspace.qclojure.domain.operation-registry` for gate definitions."
  (:require [org.soulspace.qclojure.domain.operation-registry :as gr]))

(defn decompose-swap-if-needed
  "Decompose a SWAP gate if it's not natively supported.
   
   Parameters:
   - swap-op: SWAP operation map to decompose
   - supported-operations: Set of operation types supported by the backend
   
   Returns:
   Vector of operation maps representing the decomposition, or the original SWAP operation if supported"
  [swap-op supported-operations]
  (if (contains? supported-operations :swap)
    [swap-op]  ; SWAP is native
    ;; Decompose SWAP into 3 CNOTs
    (let [target1 (get-in swap-op [:operation-params :target1])
          target2 (get-in swap-op [:operation-params :target2])]
      [{:operation-type :cnot :operation-params {:control target1 :target target2}}
       {:operation-type :cnot :operation-params {:control target2 :target target1}}
       {:operation-type :cnot :operation-params {:control target1 :target target2}}])))

(defn decompose-operation
  "Decompose an operation into a sequence of more primitive operations using the operation catalog.
  
  Parameters:
  - operation: operation map to decompose
  - supported-operations: Set of operation types supported or keyword for hardware target

  Returns:
  Vector of operation maps representing the decomposition, or the original operation 
  if no decomposition is available or if it cannot be fully decomposed to supported operations"
  [operation supported-operations]
  (let [operation-type (:operation-type operation)
        operation-params (:operation-params operation)]

    ;; If the operation is already supported, no need to decompose
    (if (contains? supported-operations operation-type)
      [operation]

      ;; Try to get decomposition from the operation catalog
      (let [gate-info (get gr/operation-catalog operation-type)
            decompositions (:decomposition gate-info)]

        (if (nil? decompositions)
          ;; No decomposition available in catalog - check for special cases
          (cond
            ;; T gate to RZ(π/4) when RZ is supported but T is not
            (and (= operation-type :t)
                 (contains? supported-operations :rz)
                 (not (contains? supported-operations :t)))
            [{:operation-type :rz
              :operation-params (assoc operation-params :angle (/ Math/PI 4))}]

            ;; Rydberg CZ to standard CZ when supported
            (and (= operation-type :rydberg-cz)
                 (contains? supported-operations :cz)
                 (not (contains? supported-operations :rydberg-cz)))
            [{:operation-type :cz
              :operation-params operation-params}]

            ;; Rydberg CPhase to CRZ when supported  
            (and (= operation-type :rydberg-cphase)
                 (contains? supported-operations :crz)
                 (not (contains? supported-operations :rydberg-cphase)))
            [{:operation-type :crz
              :operation-params operation-params}]

            ;; Global gates decomposition to individual gates when circuit context available
            (and (contains? #{:global-x :global-y :global-z :global-h} operation-type)
                 (not (contains? supported-operations operation-type))
                 (:circuit-qubits operation-params))
            (let [n-qubits (:circuit-qubits operation-params)
                  base-gate (case operation-type
                              :global-x :x
                              :global-y :y  
                              :global-z :z
                              :global-h :h)]
              (mapv (fn [i] {:operation-type base-gate
                             :operation-params {:target i}})
                    (range n-qubits)))

            ;; Global rotation gates decomposition when circuit context available
            (and (contains? #{:global-rx :global-ry :global-rz} operation-type)
                 (not (contains? supported-operations operation-type))
                 (:circuit-qubits operation-params))
            (let [n-qubits (:circuit-qubits operation-params)
                  angle (:angle operation-params)
                  base-gate (case operation-type
                              :global-rx :rx
                              :global-ry :ry
                              :global-rz :rz)]
              (mapv (fn [i] {:operation-type base-gate
                             :operation-params {:target i :angle angle}})
                    (range n-qubits)))

            ;; Otherwise return original operation
            :else
            [operation])

          ;; Process different types of decompositions from the catalog
          (let [decomposed-gates (cond
                                   ;; Try parametric function decomposition for gates with explicit angles
                                   (and (:angle operation-params) (:parametric-fn decompositions))
                                   ((:parametric-fn decompositions) (:angle operation-params))

                                   ;; Try parametric function decomposition for fixed-angle gates (T, S, etc.)
                                   (and (:parametric-fn decompositions)
                                        (not (:angle operation-params))
                                        (contains? supported-operations :rz))
                                   ;; Use parametric decomposition with the gate's standard angle
                                   ((:parametric-fn decompositions) nil)

                                   ;; Try universal function decomposition for gates with explicit angles
                                   (and (:angle operation-params) (:universal-fn decompositions))
                                   ((:universal-fn decompositions) (:angle operation-params))

                                   ;; Try specific target decomposition (e.g., :cnot-t for Toffoli)
                                   (keyword? supported-operations)
                                   (let [target-key (keyword (str (name supported-operations) "-"
                                                                  (str (first (sort supported-operations)))))]
                                     (or (get decompositions target-key)
                                         (get decompositions supported-operations)
                                         (:universal decompositions)))

                                   ;; Try to find a decomposition that uses only supported operations
                                   (set? supported-operations)
                                   (or
                                    ;; Check if we have a specific decomposition for common gate sets
                                    (when (contains? supported-operations :cnot)
                                      (or (get decompositions :cnot)
                                          (get decompositions :cnot-h)
                                          (get decompositions :cnot-t)
                                          (get decompositions :cnot-s)))
                                    ;; Check universal decomposition
                                    (:universal decompositions)
                                    ;; If decomposition is just a vector, use it
                                    (when (vector? decompositions) decompositions))

                                   ;; Default: try universal decomposition
                                   :else
                                   (:universal decompositions))]

            (if (nil? decomposed-gates)
              ;; No suitable decomposition found
              [operation]

              ;; Convert decomposed gates to operation maps
              (mapv (fn [decomp-step]
                      (cond
                        ;; Handle vector format: [gate-type param-spec1 param-spec2] 
                        (vector? decomp-step)
                        (let [gate-type (first decomp-step)
                              param-specs (rest decomp-step)]
                          {:operation-type gate-type
                           :operation-params
                           (cond
                             ;; For parametric gates like [:rz angle] - handle angle parameter
                             (and (contains? #{:rz :rx :ry} gate-type) 
                                  (= 1 (count param-specs))
                                  (number? (first param-specs)))
                             {:target (get operation-params :target)
                              :angle (first param-specs)}

                             ;; For gates like [:cnot :target1 :target2] - map positional params
                             (= gate-type :cnot)
                             (let [control-param (first param-specs)
                                   target-param (second param-specs)]
                               {:control (get operation-params (keyword (name control-param)))
                                :target (get operation-params (keyword (name target-param)))})

                             ;; For single-qubit gates like [:h :target] - map target param
                             (contains? #{:h :x :y :z :s :s-dag :t :t-dag} gate-type)
                             (let [target-param (first param-specs)]
                               (if target-param
                                 {:target (get operation-params (keyword (name target-param)))}
                                 {:target (get operation-params :target)}))

                             ;; Default: just use the operation params as-is
                             :else
                             (select-keys operation-params [:target :control :control1 :control2 :target1 :target2]))})

                        ;; Handle map format: {:gate :h, :target :target2}
                        (map? decomp-step)
                        (let [gate-type (:gate decomp-step)]
                          {:operation-type gate-type
                           :operation-params
                           (merge
                            ;; Map the parameters from the decomposition spec to actual values
                            (reduce-kv (fn [acc param-key param-spec]
                                         (if (= param-key :gate)
                                           acc  ; Skip the :gate key
                                           (assoc acc param-key
                                                  (get operation-params (keyword (name param-spec))))))
                                       {}
                                       decomp-step))})

                        ;; Handle keyword format: just the gate name
                        (keyword? decomp-step)
                        {:operation-type decomp-step
                         :operation-params (select-keys operation-params [:target :control :control1 :control2 :target1 :target2])}

                        ;; Default fallback
                        :else
                        {:operation-type decomp-step
                         :operation-params operation-params}))
                    decomposed-gates))))))))

(defn transform-operations
  "Transform the operations in a circuit to use only supported operations.

  Parameters:
  - operations: Original vector of operation maps
  - supported-operations: Set of operation types supported
  - max-iterations: Maximum decomposition iterations to prevent infinite loops
  
  Returns:
  A vector of transformed operations that are all supported or
  operations that couldn't be further decomposed"
  [operations supported-operations max-iterations]
  (loop [current-operations operations
         iteration 0
         processed-operations #{}]  ;; Track operations we've already tried to decompose
    (if (>= iteration max-iterations)
      ;; Safety check to prevent infinite loops
      (throw (ex-info "Maximum iterations reached during circuit transformation"
                      {:operations current-operations
                       :iteration iteration}))

      (let [;; Find any operations that need decomposition
            needs-decomposition? (fn [operation]
                                   (and (not (contains? processed-operations (:operation-type operation)))
                                        (not (contains? supported-operations (:operation-type operation)))))
            unsupported (filterv needs-decomposition? current-operations)]

        (if (empty? unsupported)
          ;; All operations are either supported or can't be decomposed further
          current-operations

          ;; Replace the first unsupported operation with its decomposition
          (let [unsupported-operation (first unsupported)
                operation-type (:operation-type unsupported-operation)
                unsupported-index (.indexOf current-operations unsupported-operation)
                decomposed-operations (decompose-operation unsupported-operation supported-operations)

                ;; Check if the operation was actually decomposed
                was-decomposed? (not= [unsupported-operation] decomposed-operations)

                ;; If the operation wasn't decomposed, mark it as processed so we don't try again
                new-processed-operations (if was-decomposed?
                                           processed-operations
                                           (conj processed-operations operation-type))

                ;; Create new operations vector with decomposition replacing original operation
                new-operations (into []
                                     (concat
                                      (subvec current-operations 0 unsupported-index)
                                      decomposed-operations
                                      (subvec current-operations (inc unsupported-index))))]

            (recur new-operations (inc iteration) new-processed-operations)))))))

