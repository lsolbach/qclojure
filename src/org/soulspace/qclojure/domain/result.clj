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
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.domain.math :as qmath]))

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
(s/def ::final-state ::qs/state)
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

;; QAOA-specific result specs
(s/def ::graph (s/coll-of (s/tuple pos-int? pos-int? number?)))  ; [(i j weight), ...]
(s/def ::clauses (s/coll-of (s/coll-of any? :min-count 1)))      ; Boolean clauses
(s/def ::distance-matrix (s/coll-of (s/coll-of number? :min-count 1) :min-count 1))
(s/def ::problem-type #{:max-cut :max-sat :tsp})
(s/def ::cut-edges (s/coll-of (s/tuple pos-int? pos-int? number?)))
(s/def ::cut-weight number?)
(s/def ::partition (s/map-of pos-int? #{0 1}))  ; {vertex-id -> partition}
(s/def ::boolean-assignment (s/map-of pos-int? boolean?))  ; {variable-id -> value}
(s/def ::satisfied-clauses pos-int?)
(s/def ::tour (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::tour-distance number?)
(s/def ::solution-probability number?)
(s/def ::approximation-ratio number?)

(s/def ::max-cut-solution
  (s/keys :req-un [::graph ::problem-type] :opt-un [::shots]))
(s/def ::max-sat-solution 
  (s/keys :req-un [::clauses ::problem-type] :opt-un [::shots]))
(s/def ::tsp-solution
  (s/keys :req-un [::distance-matrix ::problem-type] :opt-un [::shots]))
(s/def ::approximation-ratio-spec
  (s/keys :req-un [::problem-type] :opt-un [::classical-optimum]))
(s/def ::solution-distribution
  (s/keys :req-un [::problem-type] :opt-un [::top-solutions ::shots]))

(s/def ::classical-optimum number?)
(s/def ::top-solutions pos-int?)

(s/def ::result-type #{:measurement :expectation :variance :probability 
                       :amplitude :state-vector :density-matrix :fidelity
                       :sample :adjoint-gradient
                       ;; QAOA-specific result types
                       :max-cut-solution :max-sat-solution :tsp-solution
                       :approximation-ratio :solution-distribution})

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

;; QAOA-specific result types
(s/def ::max-cut-solution-result
  (s/keys :req-un [::cut-edges ::cut-weight ::partition ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::max-sat-solution-result
  (s/keys :req-un [::boolean-assignment ::satisfied-clauses ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::tsp-solution-result
  (s/keys :req-un [::tour ::tour-distance ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::approximation-ratio-result
  (s/keys :req-un [::approximation-ratio ::problem-type]
          :opt-un [::classical-optimum]))

(s/def ::solution-distribution-result
  (s/keys :req-un [::solutions ::total-count]
          :opt-un [::top-solutions]))

(s/def ::solutions (s/coll-of (s/keys :req-un [::solution-string ::probability ::decoded-solution])))
(s/def ::solution-string string?)
(s/def ::decoded-solution any?)  ; Problem-specific decoded solution
(s/def ::total-count pos-int?)

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
   
   Leverages: qs/measure-state, qs/measurement-probabilities
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - measurement-qubits: Qubits that were measured (optional, defaults to all)
   - shots: Number of measurement shots (default 1)
   
   Returns:
   Map with measurement outcomes and probabilities (Braket Sample format)"
  [final-state & {:keys [measurement-qubits shots] :or {shots 1}}]
  {:pre [(s/valid? ::qs/state final-state)]}
  (let [num-qubits (:num-qubits final-state)
        measured-qubits (or measurement-qubits (range num-qubits))
        ;; Perform multiple shots for statistical results
        shot-results (repeatedly shots #(qs/measure-state final-state))
        outcomes (mapv :outcome shot-results)
        theoretical-probs (qs/measurement-probabilities final-state)
        frequencies (frequencies outcomes)
        empirical-probs (into {} (map (fn [[outcome count]]
                                       [outcome (/ count shots)])
                                     frequencies))]
    {:measurement-outcomes outcomes
     :measurement-probabilities theoretical-probs
     :empirical-probabilities empirical-probs
     :shot-count shots
     :measurement-qubits measured-qubits
     :frequencies frequencies}))

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
  {:pre [(s/valid? ::qs/state final-state)
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
  {:pre [(s/valid? ::qs/state final-state)
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
  {:pre [(s/valid? ::qs/state final-state)
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
  {:pre [(s/valid? ::qs/state final-state)]}
  (let [num-qubits (:num-qubits final-state)]
    (if target-states
      ;; Specific target states requested
      (let [probability-outcomes 
            (into {} (map (fn [target]
                           (let [prob (if (vector? target)
                                       ;; Target is bit pattern like [1 0 1]
                                       (qs/probability final-state (qs/bits-to-index target))
                                       ;; Target is basis state index
                                       (qs/probability final-state target))]
                             [target prob]))
                         target-states))]
        {:probability-outcomes probability-outcomes
         :target-states target-states
         :target-qubits target-qubits})
      ;; All probabilities for specified qubits (or all qubits)
      (let [used-qubits (or target-qubits (range num-qubits))
            all-probs (qs/measurement-probabilities final-state)]
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
  {:pre [(s/valid? ::qs/state final-state)
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
  {:pre [(s/valid? ::qs/state final-state)]}
  {:state-vector (:state-vector final-state)
   :num-qubits (:num-qubits final-state)
   :basis-labels (qs/basis-labels (:num-qubits final-state))})

(defn extract-density-matrix-result  
  "Extract density matrix representation (simulation only).
   
   Leverages: qs/density-matrix, qs/trace-one?
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   
   Returns:
   Density matrix representation (Braket style)"
  [final-state]
  {:pre [(s/valid? ::qs/state final-state)]}
  (let [rho (qs/density-matrix final-state)]
    {:density-matrix rho
     :num-qubits (:num-qubits final-state)
     :trace-valid (qs/trace-one? rho)}))

(defn extract-fidelity-result
  "Extract fidelity between final state and reference states.
   
   Leverages: qs/state-fidelity
   
   Parameters:
   - final-state: Final quantum state after circuit execution
   - reference-states: Vector of reference states to compare against
   
   Returns:
   Map with fidelity values for each reference state"
  [final-state reference-states]
  {:pre [(s/valid? ::qs/state final-state)
         (coll? reference-states)]}
  (let [fidelities
        (into {} (map-indexed (fn [idx ref-state]
                               [(str "reference-" idx) 
                                (qs/state-fidelity final-state ref-state)])
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
  {:pre [(s/valid? ::qs/state final-state)
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
;;; QAOA-Specific Helper Functions and Result Extraction
;;;
(defn process-measurement-statistics
  "Extract and process measurement statistics from quantum state.
  
  Common helper function for QAOA algorithms to extract measurement data
  using indices directly (no bitstring conversion).
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - shots: Number of measurement shots
  
  Returns:
  Vector of [index count probability] tuples"
  [final-state shots]
  (let [measurement-stats (qs/measure-state-statistics final-state shots)
        measurement-outcomes (:frequencies measurement-stats)]
    (map (fn [[index count]]
           (let [probability (/ count shots)]
             [index count probability]))
         measurement-outcomes)))

(defn process-qaoa-solutions
  "Process QAOA measurement data into solution analysis.
  
  Common helper that converts measurement statistics into analyzed solutions
  with decoded problem-specific information.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - decode-fn: Function to decode index and num-qubits to problem solution
  - shots: Number of measurement shots
  - objective-key: Key to extract objective value from decoded solution
  
  Returns:
  Vector of solution analyses with :index, :count, :probability, and decoded data"
  [final-state decode-fn shots objective-key]
  (let [measurement-data (process-measurement-statistics final-state shots)
        num-qubits (:num-qubits final-state)]
    (map (fn [[index count probability]]
           (let [decoded (decode-fn index num-qubits)]
             (assoc decoded
                    :index index
                    :count count
                    :probability probability
                    :objective-value (get decoded objective-key))))
         measurement-data)))

(defn decode-max-cut-solution
  "Decode measurement outcomes for MaxCut problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length
  - graph: Collection of edges as [vertex1 vertex2 weight] tuples
  
  Returns:
  Map with decoded MaxCut solution information"
  [index num-qubits graph]
  (let [bits (qs/index-to-bits index num-qubits)
        partition (zipmap (range num-qubits) bits)
        cut-edges (filter (fn [[i j _weight]]
                            (not= (get partition i) (get partition j)))
                          graph)
        cut-weight (reduce + (map #(nth % 2) cut-edges))]
    {:partition partition
     :cut-edges cut-edges
     :cut-weight cut-weight}))

(defn decode-max-sat-solution
  "Decode measurement outcomes for MaxSAT problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length
  - clauses: Collection of Boolean clauses
  
  Returns:
  Map with decoded MaxSAT solution information"
  [index num-qubits clauses]
  (let [bits (qs/index-to-bits index num-qubits)
        assignment (zipmap (range num-qubits) (map #(= % 1) bits))
        
        ;; Function to evaluate a literal given the assignment
        eval-literal (fn [literal]
                       (cond
                         ;; Positive integer literal
                         (and (integer? literal) (>= literal 0))
                         (get assignment literal false)
                         
                         ;; Keyword :not-X format
                         (and (keyword? literal) (.startsWith (name literal) "not-"))
                         (let [var-str (subs (name literal) 4)]
                           (not (get assignment (Integer/parseInt var-str) false)))
                         
                         ;; Map format {:variable X :negated boolean}
                         (map? literal)
                         (let [var-idx (:variable literal)
                               negated? (:negated literal false)
                               var-value (get assignment var-idx false)]
                           (if negated? (not var-value) var-value))
                         
                         :else false))
        
        ;; Evaluate each clause (OR of literals)
        clause-results (map (fn [clause]
                              (some eval-literal clause))
                            clauses)
        
        satisfied-count (count (filter true? clause-results))]
    
    {:boolean-assignment assignment
     :satisfied-clauses satisfied-count
     :total-clauses (count clauses)
     :satisfaction-ratio (/ satisfied-count (count clauses))}))

(defn decode-tsp-solution
  "Decode measurement outcomes for TSP problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length (should be n²)
  - distance-matrix: n×n matrix of distances between cities
  
  Returns:
  Map with decoded TSP solution information"
  [index num-qubits distance-matrix]
  (let [n (count distance-matrix)
        
        ;; Decode index to bit vector
        bits (qs/index-to-bits index num-qubits)
        
        ;; Build assignment matrix: assignment[i][j] = 1 if city i visited at time j
        assignment-matrix (partition n bits)
        
        ;; Extract tour by finding which city is visited at each time step
        tour (vec (for [time-step (range n)]
                    (let [cities-at-time (keep-indexed
                                          (fn [city bit]
                                            (when (= bit 1) city))
                                          (map #(nth % time-step) assignment-matrix))]
                      (if (= (count cities-at-time) 1)
                        (first cities-at-time)
                        -1))))  ; Invalid assignment
        
        ;; Calculate tour distance if valid
        tour-distance (if (and (every? #(>= % 0) tour)
                               (= (count (set tour)) n))  ; All cities visited exactly once
                        (reduce + (map (fn [i]
                                         (let [from-city (nth tour i)
                                               to-city (nth tour (mod (inc i) n))]
                                           (nth (nth distance-matrix from-city) to-city)))
                                       (range n)))
                        Double/POSITIVE_INFINITY)]  ; Invalid tour
    
    {:tour tour
     :tour-distance tour-distance
     :valid-tour? (not= tour-distance Double/POSITIVE_INFINITY)
     :assignment-matrix assignment-matrix}))

(defn extract-max-cut-solution
  "Extract MaxCut solution from quantum state measurements.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - graph: Collection of edges as [vertex1 vertex2 weight] tuples
  - shots: Number of measurement shots (default: 1024)
  
  Returns:
  Map with MaxCut solution analysis"
  [final-state graph & {:keys [shots] :or {shots 1024}}]
  (let [num-qubits (:num-qubits final-state)
        ;; Use the common helper to process solutions
        solution-analyses (process-qaoa-solutions
                          final-state
                          #(decode-max-cut-solution %1 %2 graph)
                          shots
                          :cut-weight)
        
        ;; Find best solution (highest cut weight)
        best-solution (extract-best-solution solution-analyses :max-cut)
        
        ;; Classical optimum (brute force for small instances)
        classical-optimum (if (<= num-qubits 12)
                            (let [all-partitions (range (bit-shift-left 1 num-qubits))
                                  all-cuts (map (fn [partition]
                                                   (:cut-weight (decode-max-cut-solution partition num-qubits graph)))
                                               all-partitions)]
                              (apply max all-cuts))
                            nil)
        
        ;; Calculate approximation ratio using helper
        approx-ratio (calculate-approximation-ratio (:cut-weight best-solution) classical-optimum :max-cut)]
    
    {:cut-edges (:cut-edges best-solution)
     :cut-weight (:cut-weight best-solution)
     :partition (:partition best-solution)
     :solution-probability (:probability best-solution)
     :approximation-ratio approx-ratio
     :classical-optimum classical-optimum
     :all-solutions solution-analyses
     :measurement-distribution (into {} (map (fn [sol] [(:index sol) (:count sol)]) solution-analyses))}))

(defn extract-max-sat-solution
  "Extract MaxSAT solution from quantum state measurements.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - clauses: Collection of Boolean clauses
  - shots: Number of measurement shots (default: 1024)
  
  Returns:
  Map with MaxSAT solution analysis"
  [final-state clauses & {:keys [shots] :or {shots 1024}}]
  (let [num-qubits (:num-qubits final-state)
        ;; Use the common helper to process solutions
        solution-analyses (process-qaoa-solutions
                          final-state
                          #(decode-max-sat-solution %1 %2 clauses)
                          shots
                          :satisfied-clauses)
        
        ;; Find best solution (most satisfied clauses)
        best-solution (extract-best-solution solution-analyses :max-sat)
        
        ;; Classical optimum is all clauses satisfied (optimistic)
        classical-optimum (count clauses)
        
        ;; Calculate approximation ratio using helper
        approx-ratio (calculate-approximation-ratio (:satisfied-clauses best-solution) classical-optimum :max-sat)]
    
    {:boolean-assignment (:boolean-assignment best-solution)
     :satisfied-clauses (:satisfied-clauses best-solution)
     :solution-probability (:probability best-solution)
     :approximation-ratio approx-ratio
     :classical-optimum classical-optimum
     :all-solutions solution-analyses
     :measurement-distribution (into {} (map (fn [sol] [(:index sol) (:count sol)]) solution-analyses))}))

(defn extract-tsp-solution
  "Extract TSP solution from quantum state measurements.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - distance-matrix: n×n matrix of distances between cities
  - shots: Number of measurement shots (default: 1024)
  
  Returns:
  Map with TSP solution analysis"
  [final-state distance-matrix & {:keys [shots] :or {shots 1024}}]
  (let [num-qubits (:num-qubits final-state)
        measurement-data (process-measurement-statistics final-state shots)
        
        ;; Decode each measurement outcome
        solution-analyses (map (fn [[index count probability]]
                                 (let [decoded (decode-tsp-solution index num-qubits distance-matrix)]
                                   (assoc decoded :index index
                                                  :count count
                                                  :probability probability)))
                               measurement-data)
        
        ;; Find best valid solution (shortest distance)
        valid-solutions (filter :valid-tour? solution-analyses)
        best-solution (when (seq valid-solutions)
                        (apply min-key :tour-distance valid-solutions))]
    
    (if best-solution
      {:tour (:tour best-solution)
       :tour-distance (:tour-distance best-solution)
       :solution-probability (:probability best-solution)
       :valid-tour? true
       :all-solutions solution-analyses
       :valid-solutions-count (count valid-solutions)
       :measurement-distribution (into {} (map (fn [[idx c _p]] [idx c]) measurement-data))}
      
      {:tour nil
       :tour-distance Double/POSITIVE_INFINITY
       :solution-probability 0.0
       :valid-tour? false
       :all-solutions solution-analyses
       :valid-solutions-count 0
       :measurement-distribution (into {} (map (fn [[idx c _p]] [idx c]) measurement-data))})))

(defn extract-approximation-ratio
  "Extract approximation ratio for QAOA optimization problems.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - problem-type: Type of optimization problem (:max-cut, :max-sat, :tsp)
  - problem-instance: Problem-specific data (graph, clauses, distance matrix)
  - classical-optimum: Known classical optimum (optional)
  
  Returns:
  Map with approximation ratio analysis"
  [final-state problem-type problem-instance & {:keys [classical-optimum]}]
  (case problem-type
    :max-cut
    (let [result (extract-max-cut-solution final-state problem-instance)]
      {:approximation-ratio (:approximation-ratio result)
       :problem-type :max-cut
       :classical-optimum (:classical-optimum result)
       :qaoa-solution (:cut-weight result)})
    
    :max-sat
    (let [result (extract-max-sat-solution final-state problem-instance)]
      {:approximation-ratio (:approximation-ratio result)
       :problem-type :max-sat
       :classical-optimum (:classical-optimum result)
       :qaoa-solution (:satisfied-clauses result)})
    
    :tsp
    (let [result (extract-tsp-solution final-state problem-instance)]
      (if (:valid-tour? result)
        {:approximation-ratio (when classical-optimum (/ classical-optimum (:tour-distance result)))
         :problem-type :tsp
         :classical-optimum classical-optimum
         :qaoa-solution (:tour-distance result)}
        {:approximation-ratio nil
         :problem-type :tsp
         :classical-optimum classical-optimum
         :qaoa-solution nil
         :error "No valid tour found"}))
    
    {:error (str "Unknown problem type: " problem-type)}))

(defn extract-solution-distribution
  "Extract distribution of solution candidates from quantum measurements.
  
  Parameters:
  - final-state: Final quantum state after QAOA execution
  - problem-type: Type of optimization problem 
  - problem-instance: Problem-specific data
  - top-solutions: Number of top solutions to include (default: 10)
  - shots: Number of measurement shots (default: 1024)
  
  Returns:
  Map with solution distribution analysis"
  [final-state problem-type problem-instance & {:keys [top-solutions shots] :or {top-solutions 10 shots 1024}}]
  (let [num-qubits (:num-qubits final-state)
        measurement-data (process-measurement-statistics final-state shots)
        
        ;; Decode solutions based on problem type
        decoded-solutions 
        (case problem-type
          :max-cut
          (map (fn [[index count probability]]
                 (let [decoded (decode-max-cut-solution index num-qubits problem-instance)]
                   {:index index
                    :probability probability
                    :count count
                    :objective-value (:cut-weight decoded)
                    :decoded-solution decoded}))
               measurement-data)
          
          :max-sat
          (map (fn [[index count probability]]
                 (let [decoded (decode-max-sat-solution index num-qubits problem-instance)]
                   {:index index
                    :probability probability
                    :count count
                    :objective-value (:satisfied-clauses decoded)
                    :decoded-solution decoded}))
               measurement-data)
          
          :tsp
          (map (fn [[index count probability]]
                 (let [decoded (decode-tsp-solution index num-qubits problem-instance)]
                   {:index index
                    :probability probability
                    :count count
                    :objective-value (if (:valid-tour? decoded) (:tour-distance decoded) Double/POSITIVE_INFINITY)
                    :decoded-solution decoded}))
               measurement-data))
        
        ;; Sort by objective value (max for max-cut/max-sat, min for tsp)
        sorted-solutions (if (= problem-type :tsp)
                           (sort-by :objective-value decoded-solutions)
                           (sort-by :objective-value > decoded-solutions))
        
        top-solutions-subset (take top-solutions sorted-solutions)]
    
    {:solutions top-solutions-subset
     :total-count (count decoded-solutions)
     :top-solutions (count top-solutions-subset)
     :best-objective (:objective-value (first sorted-solutions))
     :measurement-entropy (let [probs (map :probability decoded-solutions)
                                non-zero-probs (filter pos? probs)]
                            (- (reduce + (map #(* % (Math/log %)) non-zero-probs))))
     :problem-type problem-type}))

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
  {:pre [(s/valid? ::qs/state (:final-state result))
         (map? result-specs)]}

  (let [final-state (:final-state result)]

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
                :target-qubits (:targets specs))))

      ;; QAOA-specific result extractions
      (:max-cut-solution result-specs)
      (assoc :max-cut-solution-result
             (let [specs (:max-cut-solution result-specs)]
               (extract-max-cut-solution
                final-state
                (:graph specs)
                :shots (or (:shots specs) 1024))))

      (:max-sat-solution result-specs)
      (assoc :max-sat-solution-result
             (let [specs (:max-sat-solution result-specs)]
               (extract-max-sat-solution
                final-state
                (:clauses specs)
                :shots (or (:shots specs) 1024))))

      (:tsp-solution result-specs)
      (assoc :tsp-solution-result
             (let [specs (:tsp-solution result-specs)]
               (extract-tsp-solution
                final-state
                (:distance-matrix specs)
                :shots (or (:shots specs) 1024))))

      (:approximation-ratio result-specs)
      (assoc :approximation-ratio-result
             (let [specs (:approximation-ratio result-specs)]
               (extract-approximation-ratio
                final-state
                (:problem-type specs)
                (:problem-instance specs)
                :classical-optimum (:classical-optimum specs))))

      (:solution-distribution result-specs)
      (assoc :solution-distribution-result
             (let [specs (:solution-distribution result-specs)]
               (extract-solution-distribution
                final-state
                (:problem-type specs)
                (:problem-instance specs)
                :top-solutions (or (:top-solutions specs) 10)
                :shots (or (:shots specs) 1024)))))))

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

      (reduce-kv
       (fn [acc-result result-type spec]
         (try
           (case result-type
             :measurement
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

             ;; QAOA-specific result extractions
             ;; TODO check that noisy desity matrix is used appropriately
             :max-cut-solution
             (let [graph (:graph spec)
                   cut-shots (:shots spec shots)
                   max-cut-data (extract-max-cut-solution final-state graph :shots cut-shots)]
               (assoc acc-result :max-cut-solution-result max-cut-data))

             :max-sat-solution
             (let [clauses (:clauses spec)
                   sat-shots (:shots spec shots)
                   max-sat-data (extract-max-sat-solution final-state clauses :shots sat-shots)]
               (assoc acc-result :max-sat-solution-result max-sat-data))

             :tsp-solution
             (let [distance-matrix (:distance-matrix spec)
                   tsp-shots (:shots spec shots)
                   tsp-data (extract-tsp-solution final-state distance-matrix :shots tsp-shots)]
               (assoc acc-result :tsp-solution-result tsp-data))

             :approximation-ratio
             (let [problem-type (:problem-type spec)
                   problem-instance (:problem-instance spec)
                   classical-optimum (:classical-optimum spec)
                   ratio-data (extract-approximation-ratio final-state problem-type problem-instance
                                                           :classical-optimum classical-optimum)]
               (assoc acc-result :approximation-ratio-result ratio-data))

             :solution-distribution
             (let [problem-type (:problem-type spec)
                   problem-instance (:problem-instance spec)
                   top-solutions (:top-solutions spec 10)
                   dist-shots (:shots spec shots)
                   dist-data (extract-solution-distribution final-state problem-type problem-instance
                                                            :top-solutions top-solutions :shots dist-shots)]
               (assoc acc-result :solution-distribution-result dist-data))

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
