(ns org.soulspace.qclojure.domain.gate-decomposition
  (:require [org.soulspace.qclojure.domain.operation-registry :as gr]))

(defn- resolve-supported-operations
  "Resolve supported operations - converts keyword targets to gate sets or returns the set as-is.
  
  Parameters:
  - supported-operations: Either a keyword (hardware target) or a set of operation types
  
  Returns:
  Set of supported operation types"
  [supported-operations]
  (if (keyword? supported-operations)
    ;; If it's a keyword, get the corresponding gate set
    (case supported-operations
      :braket-ionq gr/braket-ionq-gates
      :braket-rigetti gr/braket-rigetti-gates
      :braket-simulator gr/braket-simulator-gates
      :superconducting gr/superconducting-hardware-gates
      :trapped-ion gr/trapped-ion-hardware-gates
      :universal gr/universal-gate-set
      #{})
    ;; If it's already a set, return as-is
    supported-operations))

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

(defn- decompose-operation
  "Decompose an operation into a sequence of more primitive operations using the operation catalog.
  
  Parameters:
  - operation: operation map to decompose
  - supported-operations: Set of operation types supported or keyword for hardware target

  Returns:
  Vector of operation maps representing the decomposition, or the original operation 
  if no decomposition is available or if it cannot be fully decomposed to supported operations"
  [operation supported-operations]
  (let [operation-type (:operation-type operation)
        operation-params (:operation-params operation)

        ;; Resolve the supported operations if it's a keyword
        resolved-supported-ops (resolve-supported-operations supported-operations)]

    ;; If the operation is already supported, no need to decompose
    (if (contains? resolved-supported-ops operation-type)
      [operation]

      ;; Try to get decomposition from the operation catalog
      (let [gate-info (get gr/operation-catalog operation-type)
            decompositions (:decomposition gate-info)]

        (if (nil? decompositions)
          ;; No decomposition available in catalog - check for special cases
          (cond
            ;; T gate to RZ(π/4) when RZ is supported but T is not
            (and (= operation-type :t)
                 (contains? resolved-supported-ops :rz)
                 (not (contains? resolved-supported-ops :t)))
            [{:operation-type :rz
              :operation-params (assoc operation-params :angle (/ Math/PI 4))}]

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
                                        (contains? resolved-supported-ops :rz))
                                   ;; Use parametric decomposition with the gate's standard angle
                                   ((:parametric-fn decompositions) nil)

                                   ;; Try universal function decomposition for gates with explicit angles
                                   (and (:angle operation-params) (:universal-fn decompositions))
                                   ((:universal-fn decompositions) (:angle operation-params))

                                   ;; Try specific target decomposition (e.g., :cnot-t for Toffoli)
                                   (keyword? supported-operations)
                                   (let [target-key (keyword (str (name supported-operations) "-"
                                                                  (str (first (sort resolved-supported-ops)))))]
                                     (or (get decompositions target-key)
                                         (get decompositions supported-operations)
                                         (:universal decompositions)))

                                   ;; Try to find a decomposition that uses only supported operations
                                   (set? resolved-supported-ops)
                                   (or
                                    ;; Check if we have a specific decomposition for common gate sets
                                    (when (contains? resolved-supported-ops :cnot)
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
  - supported-operations: Set of operation types supported or keyword for hardware target
  - max-iterations: Maximum decomposition iterations to prevent infinite loops
  
  Returns:
  A vector of transformed operations that are all supported or
  operations that couldn't be further decomposed"
  [operations supported-operations max-iterations]
  (let [resolved-supported-ops (resolve-supported-operations supported-operations)]
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
                                          (not (contains? resolved-supported-ops (:operation-type operation)))))
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

              (recur new-operations (inc iteration) new-processed-operations))))))))

