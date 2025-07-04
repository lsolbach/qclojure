(ns org.soulspace.qclojure.application.noise
  "Quantum noise application layer providing high-level noise modeling functions.
  
  This namespace contains advanced noise application functions that bridge the 
  pure quantum mechanics in domain.channel with practical use cases like:
  - Error mitigation algorithms
  - Circuit fidelity estimation  
  - Platform comparison and benchmarking
  - Hardware-aware noise modeling
  
  These functions were moved from the noisy simulator backend to make them
  available for broader use cases throughout the quantum computing stack."
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.channel :as channel]
            [org.soulspace.qclojure.domain.state :as qs]))

;; Specs for noise application functions
(s/def ::noise-type keyword?) ; Allow any keyword, function will handle unknown types gracefully
(s/def ::noise-strength (s/and number? #(<= 0 % 1)))
(s/def ::t1-time pos?) ; T1 relaxation time in microseconds
(s/def ::t2-time pos?) ; T2 dephasing time in microseconds
(s/def ::gate-time pos?) ; Gate operation time in nanoseconds

(s/def ::coherent-error
  (s/keys :req-un [::rotation-angle ::rotation-axis]))

(s/def ::gate-noise-config
  (s/keys :req-un [::noise-type]
          :opt-un [::noise-strength ::t1-time ::t2-time ::gate-time ::coherent-error]))

(s/def ::readout-error-config
  (s/keys :req-un [::prob-0-to-1 ::prob-1-to-0]
          :opt-un [::correlated-errors]))

(s/def ::noise-model
  (s/keys :opt-un [::gate-noise ::readout-error]))

;;
;; Gate Noise Application Functions
;;

(defn apply-gate-noise
  "Apply advanced noise model during gate operation.
  
  This function applies the clean gate operation first, then applies
  the configured noise channel to simulate realistic quantum hardware behavior.
  
  Parameters:
  - state: Current quantum state
  - gate: Gate operation to apply
  - noise-config: Advanced noise configuration map
  
  Returns: State after gate operation and noise application"
  [state gate noise-config]
  {:pre [(s/valid? ::qs/quantum-state state)
         (map? gate)
         (s/valid? ::gate-noise-config noise-config)]}
  (let [clean-state (qc/apply-operation-to-state state gate)
        target-qubit (get-in gate [:operation-params :target] 0)
        {:keys [noise-type t1-time t2-time gate-time]} noise-config
        noise-strength (get noise-config :noise-strength 0.01)] ; Default noise strength
    
    (case noise-type
      :depolarizing
      (let [kraus-ops (channel/depolarizing-kraus-operators noise-strength)]
        (channel/apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :amplitude-damping
      (let [decoherence (if (and t1-time gate-time)
                          (channel/calculate-decoherence-params t1-time (or t2-time t1-time) gate-time)
                          {:gamma-1 noise-strength :gamma-2 0})
            kraus-ops (channel/amplitude-damping-kraus-operators (:gamma-1 decoherence))]
        (channel/apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :phase-damping
      (let [decoherence (if (and t2-time gate-time)
                          (channel/calculate-decoherence-params (or t1-time t2-time) t2-time gate-time)
                          {:gamma-1 0 :gamma-2 noise-strength})
            kraus-ops (channel/phase-damping-kraus-operators (:gamma-2 decoherence))]
        (channel/apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :coherent
      (let [coherent-config (get noise-config :coherent-error {:rotation-angle 0.01 :rotation-axis :z})
            kraus-op (channel/coherent-error-kraus-operator 
                      (:rotation-angle coherent-config)
                      (:rotation-axis coherent-config))]
        (channel/apply-single-qubit-kraus-operator clean-state kraus-op target-qubit))
      
      ;; Default: no noise
      clean-state)))

;;
;; Readout Noise Application Functions
;;

(defn apply-readout-noise
  "Apply advanced readout noise with potential correlations.
  
  This function simulates realistic measurement errors that occur in
  actual quantum hardware, including correlated errors between qubits.
  
  Parameters:
  - results: Clean measurement results (map of bitstring -> count)
  - readout-config: Advanced readout error configuration
  
  Returns: Noisy measurement results with same structure"
  [results readout-config]
  {:pre [(map? results)
         (s/valid? ::readout-error-config readout-config)]}
  (let [{:keys [prob-0-to-1 prob-1-to-0 correlated-errors]} readout-config]
    (into {} 
          (map (fn [[bitstring count]]
                 (let [noisy-bitstring 
                       (apply str 
                              (map-indexed 
                                (fn [qubit-idx bit]
                                  (let [base-flip-prob (case bit
                                                         \0 prob-0-to-1
                                                         \1 prob-1-to-0)
                                        ;; Add correlation effects if configured
                                        correlation-factor (if correlated-errors
                                                             (get correlated-errors qubit-idx 1.0)
                                                             1.0)
                                        final-prob (* base-flip-prob correlation-factor)]
                                    (if (< (rand) final-prob)
                                      (case bit \0 "1" \1 "0" bit)
                                      bit)))
                                bitstring))]
                   [noisy-bitstring count]))
               results))))

;;
;; Circuit Fidelity Analysis Functions
;;

(defn estimate-circuit-fidelity
  "Estimate the overall fidelity of a circuit under given noise model.
  
  This provides a rough estimate based on gate counts and noise strengths,
  useful for circuit optimization and platform comparison.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  - noise-model: Noise model configuration
  
  Returns: Map with fidelity estimates and error analysis"
  [circuit noise-model]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)
         (s/valid? ::noise-model noise-model)]}
  (let [gate-counts (frequencies (map :operation-type (:operations circuit)))
        total-error (reduce-kv 
                     (fn [total-err gate-type count]
                       (let [gate-noise (get-in noise-model [:gate-noise gate-type])
                             noise-strength (get gate-noise :noise-strength 0.0)]
                         (+ total-err (* count noise-strength))))
                     0.0 gate-counts)
        estimated-fidelity (max 0.0 (- 1.0 total-error))]
    
    {:estimated-fidelity estimated-fidelity
     :total-estimated-error total-error
     :gate-counts gate-counts
     :dominant-error-sources (sort-by second > 
                                     (map (fn [[gate-type count]]
                                            [gate-type (* count (get-in noise-model [:gate-noise gate-type :noise-strength] 0.0))])
                                          gate-counts))}))

;;
;; Platform Comparison Functions
;;

(defn compare-hardware-platforms
  "Compare circuit fidelity across different quantum hardware platforms.
  
  This function is valuable for:
  - Selecting the best platform for a given circuit
  - Understanding platform-specific error characteristics
  - Circuit design optimization for specific hardware
  
  Parameters:
  - circuit: Quantum circuit to analyze
  - platform-models: Map of platform names to noise models
  
  Returns: Map of platform comparisons with fidelity estimates and characteristics"
  [circuit platform-models]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)
         (map? platform-models)]}
  (into {} 
        (map (fn [[platform-name noise-model]]
               (let [fidelity-data (estimate-circuit-fidelity circuit noise-model)
                     ;; Extract hardware characteristics
                     sample-gate-config (-> noise-model :gate-noise vals first)
                     coherence-info {:t1-time (get sample-gate-config :t1-time "N/A")
                                     :t2-time (get sample-gate-config :t2-time "N/A")}
                     readout-info (get noise-model :readout-error {})]
                 [platform-name 
                  (merge fidelity-data
                         {:coherence-times coherence-info
                          :readout-fidelity {:prob-0-to-1 (get readout-info :prob-0-to-1 "N/A")
                                             :prob-1-to-0 (get readout-info :prob-1-to-0 "N/A")}
                          :platform-type (cond
                                           (re-find #"ionq" (name platform-name)) :trapped-ion
                                           (re-find #"rigetti|ibm" (name platform-name)) :superconducting  
                                           (re-find #"xanadu" (name platform-name)) :photonic
                                           (re-find #"quera" (name platform-name)) :neutral-atom
                                           :else :unknown)})]))
             platform-models)))

;;
;; Utility Functions for Error Mitigation
;;

(defn noise-aware-circuit-depth
  "Calculate effective circuit depth considering noise accumulation.
  
  This metric accounts for how noise accumulates through circuit layers,
  providing a more realistic assessment than gate count alone.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  - noise-model: Noise model for depth calculation
  
  Returns: Effective depth considering noise accumulation"
  [circuit noise-model]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)
         (s/valid? ::noise-model noise-model)]}
  (let [operations (:operations circuit)
        noise-strengths (map #(get-in noise-model [:gate-noise (:operation-type %) :noise-strength] 0.0) operations)
        max-gate-noise (if (seq noise-strengths) (apply max noise-strengths) 0.0)
        base-depth (count operations)
        noise-penalty (if (pos? max-gate-noise) 
                        (* base-depth max-gate-noise 10) ; Scale factor for noise accumulation
                        0)]
    (+ base-depth noise-penalty)))

(defn recommend-error-mitigation
  "Recommend error mitigation strategies based on circuit and noise analysis.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  - noise-model: Hardware noise model
  - options: Analysis options
  
  Returns: Map with recommended mitigation strategies"
  [circuit noise-model & [options]]
  {:pre [(s/valid? ::qc/quantum-circuit circuit)
         (s/valid? ::noise-model noise-model)]}
  (let [fidelity-data (estimate-circuit-fidelity circuit noise-model)
        circuit-depth (count (:operations circuit))
        dominant-errors (:dominant-error-sources fidelity-data)
        recommendations []]
    
    ;; Analyze error patterns and recommend strategies
    (cond-> recommendations
      (< (:estimated-fidelity fidelity-data) 0.95)
      (conj {:strategy :noise-reduction
             :reason "Low estimated fidelity"
             :suggestion "Consider circuit optimization or shorter depth"})
      
      (> circuit-depth 20)
      (conj {:strategy :circuit-cutting
             :reason "Deep circuit"
             :suggestion "Consider breaking circuit into smaller pieces"})
      
      (some #(> (second %) 0.01) dominant-errors)
      (conj {:strategy :selective-mitigation
             :reason "Specific gates dominate errors"
             :suggestion (str "Focus mitigation on: " (map first dominant-errors))}))))