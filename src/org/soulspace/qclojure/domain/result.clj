(ns org.soulspace.qclojure.domain.result
  "Enhanced quantum circuit execution results with comprehensive result types.
   
   This namespace provides a systematic way to extract different types of
   results from quantum circuit executions, supporting both hardware and
   simulation backends with QASM 3 and Amazon Braket compatible result types.
   
   Design Philosophy:
   - Leverage existing functions from state, observables, and hamiltonian namespaces
   - Support QASM 3.0 and Amazon Braket result type specifications
   - Provide both simulation-only and hardware-compatible result types
   - Enable systematic extraction without code duplication
   
   Result Types Supported:
   - Measurement results (sample outcomes and probabilities)
   - Expectation values for observables
   - Variance values for observables
   - Hamiltonian energy measurements
   - Probability distributions for specific basis states
   - Amplitude extraction for basis states
   - Complete state vector (simulation only)
   - Density matrix representation (simulation only)
   - Fidelity measurements against reference states
   - Sample results for observables (hardware measurement simulation)

    Future Extensions:
   - Adjoint gradient computations (for parameterized circuits) (TODO)

   The result specification format is flexible, allowing users to request
   multiple result types in a single execution. Each result type is extracted
   using dedicated functions that utilize existing domain logic, ensuring
   consistency and maintainability.

   Example Result Spec:
   {:measurements {:shots 1000 :qubits [0 1]}
    :expectation {:observables [obs/pauli-z obs/pauli-x] :targets [0]}
    :variance {:observables [obs/pauli-z] :targets [0]}  
    :hamiltonian my-hamiltonian
    :probabilities {:targets [[1 0] [0 1]] :qubits [0 1]}
    :amplitudes {:basis-states [0 1 2 3]}
    :state-vector true
    :density-matrix true
    :fidelity {:references [|0⟩ |1⟩]}
    :sample {:observables [obs/pauli-z] :shots 1000 :targets [0]}
   "
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.state :as state]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.qubit-mapping :as mapping]))

;;;
;;; Specs for result specs and results (QASM 3.0 / Braket compatible)
;;;
(s/def ::shots pos-int?)
(s/def ::measurement-qubits (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::measurement-outcomes (s/coll-of string? :kind vector? :min-count 1))
(s/def ::measurement-probabilities (s/coll-of number? :kind vector? :min-count 1))
(s/def ::empirical-probabilities (s/map-of string? number?))
(s/def ::frequencies (s/map-of string? pos-int?))
(s/def ::shot-count pos-int?)
(s/def ::observable any?) ;; Placeholder, should be a proper observable spec
(s/def ::expectation-value number?)
(s/def ::variance-value number?)
(s/def ::standard-deviation number?)
(s/def ::target-qubits (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::probability-outcomes (s/map-of (s/or :index pos-int? :bit-pattern (s/coll-of #{0 1} :kind vector? :min-count 1)) number?))
(s/def ::target-states (s/coll-of (s/or :index pos-int? :bit-pattern (s/coll-of #{0 1} :kind vector? :min-count 1)) :kind vector? :min-count 1))
(s/def ::all-probabilities (s/coll-of number? :kind vector? :min-count 1))
(s/def ::amplitude-values (s/map-of pos-int? qmath/complex?))
(s/def ::basis-states (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::state-vector (s/coll-of qmath/complex? :kind vector? :min-count 1))
(s/def ::num-qubits pos-int?)
(s/def ::density-matrix (s/coll-of (s/coll-of qmath/complex? :kind vector? :min-count 1) :kind vector? :min-count 1))
(s/def ::trace-valid boolean?)
(s/def ::fidelity number?)
(s/def ::fidelities (s/map-of string? ::fidelity))
(s/def ::reference-states (s/coll-of any? :kind vector? :min-count 1)) ;; Placeholder
(s/def ::sample-outcomes (s/coll-of string? :kind vector? :min-count 1))
(s/def ::final-state ::state/state)
(s/def ::execution-metadata (s/map-of keyword? any?))
(s/def ::result-types (s/coll-of ::result-type :kind set? :min-count 1))
(s/def ::measurement-results (s/coll-of ::measurement-result :kind vector? :min-count 1))
(s/def ::expectation-results (s/coll-of ::expectation-result :kind vector? :min-count 1))
(s/def ::variance-results (s/coll-of ::variance-result :kind vector? :min-count 1))

(s/def ::measurements (s/keys :opt-un [::shots ::measurement-qubits]))
(s/def ::expectation (s/keys :req-un [::observables] :opt-un [::target-qubits]))
(s/def ::variance (s/keys :req-un [::observables] :opt-un [::target-qubits]))
(s/def ::hamiltonian any?) ;; Placeholder
(s/def ::probabilities (s/keys :opt-un [::target-states ::target-qubits]))
(s/def ::amplitudes (s/keys :req-un [::basis-states]))
(s/def ::state-vector boolean?)
(s/def ::density-matrix boolean?)
(s/def ::fidelity (s/keys :req-un [::reference-states]))
(s/def ::sample (s/keys :req-un [::observables ::shots] :opt-un [::target-qubits]))

(s/def ::result-type #{:measurement :expectation :variance :probability 
                       :amplitude :state-vector :density-matrix :fidelity
                       :sample :adjoint-gradient})

(s/def ::measurement-result
  (s/keys :req-un [::measurement-outcomes ::measurement-probabilities]
          :opt-un [::shot-count ::measurement-qubits]))

(s/def ::expectation-result  
  (s/keys :req-un [::expectation-value ::observable]
          :opt-un [::variance-value ::target-qubits]))

(s/def ::probability-result
  (s/keys :req-un [::probability-outcomes ::target-qubits]))

(s/def ::amplitude-result
  (s/keys :req-un [::amplitude-values ::basis-states]))

(s/def ::state-vector-result
  (s/keys :req-un [::state-vector ::num-qubits]))

(s/def ::density-matrix-result
  (s/keys :req-un [::density-matrix ::num-qubits]))

(s/def ::variance-result
  (s/keys :req-un [::variance-value ::observable]
          :opt-un [::target-qubits]))

(s/def ::sample-result
  (s/keys :req-un [::sample-outcomes ::observable ::shot-count]
          :opt-un [::target-qubits]))

(s/def ::fidelity-result
  (s/keys :req-un [::fidelities]))

(s/def ::enhanced-circuit-result
  (s/keys :req-un [::final-state ::result-types]
          :opt-un [::measurement-results ::expectation-results 
                   ::probability-results ::amplitude-results
                   ::state-vector-result ::density-matrix-result
                   ::variance-results ::sample-results
                   ::execution-metadata]))

;;;
;;; Result type extractors - systematic use of existing functions
;;;
(defn extract-measurement-results
  "Extract measurement results from circuit execution using existing state functions.
   Supports reverse mapping to present results in terms of original circuit qubits.
   
   Leverages: qs/measure-state, qs/measurement-probabilities
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - measurement-qubits: Qubits that were measured (optional, defaults to all)
   - shots: Number of measurement shots (default 1)
   - ctx: (optional) Optimization context with reverse mappings
     {:inverse-qubit-mapping {...}, :physical-to-logical {...}, :ancilla-to-logical {...}}
   
   Returns:
   Map with measurement outcomes and probabilities (Braket Sample format)
   If ctx with reverse mappings is provided, also includes:
   - :mapped-measurements - Measurements mapped to original circuit qubits
   - :qubit-mappings - The reverse mappings used"
  [final-state & {:keys [measurement-qubits shots ctx] :or {shots 1}}]
  {:pre [(s/valid? ::state/state final-state)]}
  (let [num-qubits (:num-qubits final-state)
        measured-qubits (or measurement-qubits (range num-qubits))
        ;; Perform multiple shots for statistical results
        shot-results (repeatedly shots #(state/measure-state final-state))
        outcomes (mapv :outcome shot-results)
        theoretical-probs (state/measurement-probabilities final-state)
        frequencies (frequencies outcomes)
        empirical-probs (into {} (map (fn [[outcome count]]
                                       [outcome (/ count shots)])
                                     frequencies))
        base-results {:measurement-outcomes outcomes
                      :measurement-probabilities theoretical-probs
                      :empirical-probabilities empirical-probs
                      :shot-count shots
                      :measurement-qubits measured-qubits
                      :frequencies frequencies}]
    ;; Add reverse mapping if context is provided
    (if (and ctx (:inverse-qubit-mapping ctx))
      (let [;; Convert measurement outcomes to qubit-index -> value maps for mapping
            measurement-map (into {}
                                 (map-indexed (fn [idx value]
                                               [idx value])
                                             (first outcomes)))  ; Use first shot as example
            mapped-measurements (mapping/map-measurements-to-original measurement-map ctx)]
        (assoc base-results
               :mapped-measurements mapped-measurements
               :qubit-mappings {:inverse-qubit-mapping (:inverse-qubit-mapping ctx)
                               :physical-to-logical (:physical-to-logical ctx)
                               :ancilla-to-logical (:ancilla-to-logical ctx)}))
      base-results)))

(defn extract-expectation-results
  "Extract expectation value results for observables.
   
   Leverages: obs/expectation-value, obs/tensor-product, obs/identity-op
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - observables: Collection of observables to measure
   - target-qubits: (optional) Specific qubits for each observable
   
   Returns:
   Vector of expectation value results (Braket Expectation format)"
  [final-state observables & {:keys [target-qubits]}]
  {:pre [(s/valid? ::state/state final-state)
         (coll? observables)]}
  (let [num-qubits (:num-qubits final-state)]
    (mapv (fn [observable target-qubit]
            (let [full-observable (if target-qubit
                                   ;; Expand single-qubit observable to multi-qubit
                                   (let [ops (mapv (fn [i]
                                                    (if (= i target-qubit)
                                                      observable
                                                      obs/identity-op))
                                                  (range num-qubits))]
                                     (obs/tensor-product ops))
                                   ;; Use observable as-is (should match state dimensionality)
                                   observable)
                  exp-val (obs/expectation-value full-observable final-state)]
              {:expectation-value exp-val
               :observable observable
               :target-qubits (when target-qubit [target-qubit])}))
          observables
          (if target-qubits
            target-qubits
            (repeat (count observables) nil)))))

(defn extract-variance-results
  "Extract variance results for observables.
   
   Leverages: obs/variance
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - observables: Collection of observables to measure
   - target-qubits: (optional) Specific qubits for each observable
   
   Returns:
   Vector of variance results (Braket Variance format)"
  [final-state observables & {:keys [target-qubits]}]
  {:pre [(s/valid? ::state/state final-state)
         (coll? observables)]}
  (let [num-qubits (:num-qubits final-state)]
    (mapv (fn [observable target-qubit]
            (let [full-observable (if target-qubit
                                   ;; Expand single-qubit observable to multi-qubit
                                   (let [ops (mapv (fn [i]
                                                    (if (= i target-qubit)
                                                      observable
                                                      obs/identity-op))
                                                  (range num-qubits))]
                                     (obs/tensor-product ops))
                                   ;; Use observable as-is (should match state dimensionality)
                                   observable)
                  var-val (obs/variance full-observable final-state)]
              {:variance-value var-val
               :standard-deviation (Math/sqrt var-val)
               :observable observable
               :target-qubits (when target-qubit [target-qubit])}))
          observables
          (if target-qubits
            target-qubits
            (repeat (count observables) nil)))))

(defn extract-hamiltonian-expectation
  "Extract expectation value for a Hamiltonian (energy measurement).
   
   Leverages: ham/hamiltonian-expectation, ham/group-commuting-terms
   
   Parameters:
   - final-state: Final quantum state after circuit execution  
   - hamiltonian: Hamiltonian to measure
   
   Returns:
   Map with energy expectation value and measurement optimization info"
  [final-state hamiltonian]
  {:pre [(s/valid? ::state/state final-state)
         (ham/validate-hamiltonian hamiltonian)]}
  {:energy-expectation (ham/hamiltonian-expectation hamiltonian final-state)
   :hamiltonian hamiltonian
   :measurement-groups (ham/group-commuting-terms hamiltonian)
   :measurement-bases (ham/group-pauli-terms-by-measurement-basis hamiltonian)})

(defn extract-probability-results
  "Extract probability results for specific computational basis states.
   
   Leverages: qs/probability, qs/measurement-probabilities
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - target-qubits: (optional) Specific qubits to measure (defaults to all)
   - target-states: (optional) Specific target basis state indices or bit patterns
   
   Returns:
   Map with probabilities (Braket Probability format)"
  [final-state & {:keys [target-qubits target-states]}]
  {:pre [(s/valid? ::state/state final-state)]}
  (let [num-qubits (:num-qubits final-state)]
    (if target-states
      ;; Specific target states requested
      (let [probability-outcomes 
            (into {} (map (fn [target]
                           (let [prob (if (vector? target)
                                       ;; Target is bit pattern like [1 0 1]
                                       (state/probability final-state (state/bits-to-index target))
                                       ;; Target is basis state index
                                       (state/probability final-state target))]
                             [target prob]))
                         target-states))]
        {:probability-outcomes probability-outcomes
         :target-states target-states
         :target-qubits target-qubits})
      ;; All probabilities for specified qubits (or all qubits)
      (let [used-qubits (or target-qubits (range num-qubits))
            all-probs (state/measurement-probabilities final-state)]
        {:probability-outcomes (into {} (map-indexed vector all-probs))
         :target-qubits used-qubits
         :all-probabilities all-probs}))))

(defn extract-amplitude-results
  "Extract amplitude results for specific computational basis states.
   
   Leverages: qs/state-vector (direct access to amplitudes)
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - basis-states: Vector of basis state indices to extract amplitudes for
   
   Returns:
   Map with complex amplitudes (Braket Amplitude format)"
  [final-state basis-states]
  {:pre [(s/valid? ::state/state final-state)
         (vector? basis-states)]}
  (let [state-vector (:state-vector final-state)
        amplitude-values
        (into {} (map (fn [basis-idx]
                       [basis-idx (nth state-vector basis-idx)])
                     basis-states))]
    {:amplitude-values amplitude-values
     :basis-states basis-states}))

(defn extract-state-vector-result
  "Extract complete state vector (simulation only).
   
   Leverages: qs/state-vector, qs/basis-labels
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   
   Returns:
   Complete state vector representation (Braket StateVector format)"
  [final-state]
  {:pre [(s/valid? ::state/state final-state)]}
  {:state-vector (:state-vector final-state)
   :num-qubits (:num-qubits final-state)
   :basis-labels (state/basis-labels (:num-qubits final-state))})

(defn extract-density-matrix-result  
  "Extract density matrix representation (simulation only).
   
   Leverages: qs/density-matrix, qs/trace-one?
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   
   Returns:
   Density matrix representation (Braket style)"
  [final-state]
  {:pre [(s/valid? ::state/state final-state)]}
  (let [rho (state/density-matrix final-state)]
    {:density-matrix rho
     :num-qubits (:num-qubits final-state)
     :trace-valid (state/trace-one? rho)}))

(defn extract-fidelity-result
  "Extract fidelity between final state and reference states.
   
   Leverages: qs/state-fidelity
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - reference-states: Vector of reference states to compare against
   
   Returns:
   Map with fidelity values for each reference state"
  [final-state reference-states]
  {:pre [(s/valid? ::state/state final-state)
         (coll? reference-states)]}
  (let [fidelities
        (into {} (map-indexed (fn [idx ref-state]
                               [(str "reference-" idx) 
                                (state/state-fidelity final-state ref-state)])
                             reference-states))]
    {:fidelities fidelities
     :reference-states reference-states}))

(defn extract-sample-results
  "Extract sample results for observables (hardware measurement simulation).
   
   Leverages: obs/measurement-probabilities, simulated sampling
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - observables: Collection of observables to sample
   - shots: Number of measurement shots
   - target-qubits: (optional) Specific qubits for each observable
   
   Returns:
   Vector of sample results (Braket Sample format)"
  [final-state observables shots & {:keys [target-qubits]}]
  {:pre [(s/valid? ::state/state final-state)
         (coll? observables)
         (pos-int? shots)]}
  (mapv (fn [observable]
          (let [measurement-probs (obs/measurement-probabilities observable final-state)
                ;; Simulate sampling from the probability distribution
                sample-outcomes (repeatedly shots 
                                           (fn []
                                             (let [rand-val (rand)]
                                               ;; Find the eigenvalue corresponding to this random sample
                                               (loop [[[eigenval prob] & rest] (seq measurement-probs)
                                                      cumulative 0.0]
                                                 (let [new-cumulative (+ cumulative prob)]
                                                   (if (or (< rand-val new-cumulative) (empty? rest))
                                                     eigenval
                                                     (recur rest new-cumulative)))))))]
            {:sample-outcomes sample-outcomes
             :observable observable
             :shot-count shots
             :target-qubits target-qubits
             :frequencies (frequencies sample-outcomes)}))
        observables))

;;;
;;; Common Result Processing Helpers
;;;
(defn calculate-approximation-ratio
  "Calculate approximation ratio for optimization problems.
  
  Parameters:
  - qaoa-solution: Solution value from QAOA
  - classical-optimum: Known or estimated classical optimum
  - problem-type: Type of problem (:max-cut, :max-sat, :tsp)
  
  Returns:
  Approximation ratio (for maximization: qaoa/classical, for minimization: classical/qaoa)"
  [qaoa-solution classical-optimum problem-type]
  (when (and qaoa-solution classical-optimum (pos? classical-optimum))
    (case problem-type
      (:max-cut :max-sat) (/ qaoa-solution classical-optimum)  ; Maximization problems
      :tsp (/ classical-optimum qaoa-solution)                 ; Minimization problem
      nil)))

(defn extract-best-solution
  "Extract the best solution from a collection of decoded solutions.
  
  Parameters:
  - solutions: Collection of solution maps with objective values
  - problem-type: Type of problem to determine maximization vs minimization
  
  Returns:
  Best solution according to the problem objective"
  [solutions problem-type]
  (when (seq solutions)
    (case problem-type
      (:max-cut :max-sat) (apply max-key :objective-value solutions)  ; Maximization
      :tsp (apply min-key :objective-value solutions)                 ; Minimization
      (first solutions))))                                            ; Default: first solution

;;;
;;; Result extraction and processing
;;;
(defn extract-results
  "Extract specified results from the final quantum state after circuit execution.
   
   Parameters:
   - result: Result map containing :final-state (the final quantum state after executinga circuit)
   - result-specs: Map specifying which results to extract
   
   Result specs format (all optional):
   {:measurements {:shots 1000 :qubits [0 1]}
    :expectation {:observables [pauli-z pauli-x] :targets [0]}
    :variance {:observables [pauli-z] :targets [0]}  
    :hamiltonian my-hamiltonian
    :probabilities {:targets [[1 0] [0 1]] :qubits [0 1]}
    :amplitudes {:basis-states [0 1 2 3]}
    :state-vector true
    :density-matrix true
    :fidelity {:references [|0⟩ |1⟩]}
    :sample {:observables [pauli-z] :shots 1000 :targets [0]}}
   
   Returns:
   Map of extracted results based on requested types"
  [result result-specs]
  {:pre [(s/valid? ::state/state (:final-state result))
         (map? result-specs)]}

  (let [final-state (:final-state result)]

    ;; TODO align with extract-noisy-results structure
    ;; Extract each requested result type systematically
    (cond-> result

      ;; Basic measurements (Sample result type)
      (:measurements result-specs)
      (assoc :measurement-results
             (let [specs (:measurements result-specs)]
               (extract-measurement-results
                final-state
                :measurement-qubits (:qubits specs)
                :shots (or (:shots specs) 1))))

      ;; Observable expectation values (Expectation result type)
      (:expectation result-specs)
      (assoc :expectation-results
             (let [specs (:expectation result-specs)]
               (extract-expectation-results
                final-state
                (:observables specs)
                :target-qubits (:targets specs))))

      ;; Observable variances (Variance result type)
      (:variance result-specs)
      (assoc :variance-results
             (let [specs (:variance result-specs)]
               (extract-variance-results
                final-state
                (:observables specs)
                :target-qubits (:targets specs))))

      ;; Hamiltonian energy measurement (specialized Expectation)
      (:hamiltonian result-specs)
      (assoc :hamiltonian-result
             (extract-hamiltonian-expectation final-state (:hamiltonian result-specs)))

      ;; Specific probability outcomes (Probability result type)
      (:probabilities result-specs)
      (assoc :probability-results
             (let [specs (:probabilities result-specs)]
               (extract-probability-results
                final-state
                :target-qubits (:qubits specs)
                :target-states (:targets specs))))

      ;; Amplitude extraction (Amplitude result type)
      (:amplitudes result-specs)
      (assoc :amplitude-results
             (extract-amplitude-results
              final-state
              (:basis-states (:amplitudes result-specs))))

      ;; Complete state vector (StateVector result type - simulation only)
      (:state-vector result-specs)
      (assoc :state-vector-result
             (extract-state-vector-result final-state))

      ;; Density matrix (simulation only)  
      (:density-matrix result-specs)
      (assoc :density-matrix-result
             (extract-density-matrix-result final-state))

      ;; Fidelity measurements (custom analysis)
      (:fidelity result-specs)
      (assoc :fidelity-results
             (extract-fidelity-result
              final-state
              (:references (:fidelity result-specs))))

      ;; Sample results (Sample result type for observables)
      (:sample result-specs)
      (assoc :sample-results
             (let [specs (:sample result-specs)]
               (extract-sample-results
                final-state
                (:observables specs)
                (or (:shots specs) 1000)
                :target-qubits (:targets specs)))))))

(defn extract-noisy-results
  "Extract comprehensive results from noisy simulation data.
  
  This function processes the raw simulation results and extracts various
  result types based on the result specifications, similar to the ideal
  simulator but adapted for noisy hardware simulation with trajectories.
  
  Parameters:
  - base-result: Raw simulation result from hardware simulator
  - result-specs: Map specifying what types of results to extract
  - circuit: Original circuit for context
  
  Returns:
  Map of extracted results based on requested types"
  [base-result result-specs circuit]
  ; TODO job-status check/handling should be done in caller (e.g. hardware simulator)
  (if (not= (:job-status base-result) :completed)
    base-result  ; Return error results as-is
    (let [final-state (:final-state base-result)
          density-matrix (:density-matrix base-result)
;          measurement-results (:measurement-results base-result)
          shots (:shots-executed base-result)
          enhanced-result (assoc base-result :result-types (set (keys result-specs)))]

      ;; TODO align with extract-results structure
      ;; Systematic extraction of each requested result type
      (reduce-kv
       (fn [acc-result result-type spec]
         (try
           (case result-type
             :measurements
             (let [measurement-data (extract-measurement-results
                                     final-state
                                     :measurement-qubits (:measurement-qubits spec)
                                     :shots (or (:shots spec) shots))]
               (assoc acc-result :measurement-results measurement-data))

             :expectation
             (let [observables (:observables spec)
                   target-qubits (:target-qubits spec)
                   expectations (if density-matrix
                                  ;; Use density matrix for expectation values if available
                                  (mapv #(obs/expectation-value-density-matrix % density-matrix) observables)
                                  ;; Fall back to state-based calculation
                                  (extract-expectation-results final-state observables
                                                                      :target-qubits target-qubits))]
               (assoc acc-result :expectation-results expectations))

             :variance
             (let [observables (:observables spec)
                   target-qubits (:target-qubits spec)
                   variances (extract-variance-results final-state observables
                                                              :target-qubits target-qubits)]
               (assoc acc-result :variance-results variances))

             :hamiltonian
             (let [hamiltonian (:hamiltonian spec)
                   energy-result (if density-matrix
                                   ;; Use density matrix for Hamiltonian expectation if available
                                   (ham/hamiltonian-expectation-density-matrix hamiltonian density-matrix)
                                   ;; Fall back to state-based calculation
                                   (extract-hamiltonian-expectation final-state hamiltonian))]
               (assoc acc-result :hamiltonian-expectation energy-result))

             :probability
             (let [target-qubits (:target-qubits spec)
                   target-states (:target-states spec)
                   probs (extract-probability-results final-state
                                                      :target-qubits target-qubits
                                                      :target-states target-states)]
               (assoc acc-result :probability-results probs))

             :amplitude
             (let [basis-states (:basis-states spec)
                   amps (extract-amplitude-results final-state basis-states)]
               (assoc acc-result :amplitude-results amps))

             :state-vector
             (if (true? spec)  ; Simple boolean flag for state vector
               (let [state-data (extract-state-vector-result final-state)]
                 (assoc acc-result :state-vector-result state-data))
               acc-result)

             :density-matrix
             (if (true? spec)  ; Simple boolean flag for density matrix
               (let [dm-data (if density-matrix
                               {:density-matrix density-matrix
                                :num-qubits (:num-qubits circuit)
                                :trace (:density-matrix-trace base-result)
                                :from-trajectories true
                                :trajectory-count (:trajectory-count base-result)}
                               ;; Generate from final state if no trajectory-based DM
                               (extract-density-matrix-result final-state))]
                 (assoc acc-result :density-matrix-result dm-data))
               acc-result)

             :fidelity
             (let [reference-states (:reference-states spec)
                   fidelities (extract-fidelity-result final-state reference-states)]
               (assoc acc-result :fidelity-results fidelities))

             :sample
             (let [observables (:observables spec)
                   sample-shots (:shots spec)
                   target-qubits (:target-qubits spec)
                   samples (extract-sample-results final-state observables sample-shots
                                                          :target-qubits target-qubits)]
               (assoc acc-result :sample-results samples))

             ;; Default case - unknown result type
             (do
               (println (str "Warning: Unknown result type " result-type " in hardware simulator"))
               acc-result))

           (catch Exception e
             (println (str "Error extracting " result-type " results: " (.getMessage e)))
             (assoc acc-result (keyword (str (name result-type) "-error")) (.getMessage e)))))

       enhanced-result
       result-specs))))

;;;
;;; Result analysis and post-processing
;;;
(defn summarize-results
  "Create a human-readable summary of execution results."
  [results]
  (let [result-types (:result-types results)
        summary {:execution-summary (:execution-metadata results)
                 :available-results (vec result-types)}]
    
    (cond-> summary
      
      (:measurement-results results)
      (assoc :measurement-summary
             (let [mr (:measurement-results results)]
               {:total-shots (:shot-count mr)
                :unique-outcomes (count (distinct (:measurement-outcomes mr)))
                :most-probable-outcome (when-let [emp-probs (:empirical-probabilities mr)]
                                       (first (sort-by second > emp-probs)))}))
      
      (:expectation-results results)
      (assoc :expectation-summary
             (mapv (fn [er]
                    {:expectation (:expectation-value er)})
                   (:expectation-results results)))
      
      (:variance-results results)
      (assoc :variance-summary
             (mapv (fn [vr]
                    {:variance (:variance-value vr)
                     :uncertainty (:standard-deviation vr)})
                   (:variance-results results)))
      
      (:hamiltonian-result results)
      (assoc :energy-summary
             {:energy (:energy-expectation (:hamiltonian-result results))
              :measurement-groups (count (:measurement-groups (:hamiltonian-result results)))})
      
      (:fidelity-results results)
      (assoc :fidelity-summary
             (let [fidelities (:fidelities (:fidelity-results results))]
               (when (seq fidelities)
                 {:max-fidelity (apply max (vals fidelities))
                  :min-fidelity (apply min (vals fidelities))
                  :average-fidelity (/ (reduce + (vals fidelities)) (count fidelities))}))))))
