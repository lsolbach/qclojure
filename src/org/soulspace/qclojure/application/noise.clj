(ns org.soulspace.qclojure.application.noise
  "Quantum noise application layer providing high-level noise modeling functions.
  
   This namespace contains advanced noise application functions that bridge the 
   pure quantum mechanics in domain.channel and domain.noise with practical
   use cases like:
   - Error mitigation algorithms
   - Circuit fidelity estimation  
   - Platform comparison and benchmarking
   - Hardware-aware noise modeling
   
   It builds on the foundational noise models and operations defined in
   org.soulspace.qclojure.domain.noise and org.soulspace.qclojure.domain.channel,
   providing higher-level abstractions for realistic quantum computing scenarios.
   This layer is crucial for developing robust quantum applications that can
   effectively handle and mitigate noise in practical settings."
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc] 
            [org.soulspace.qclojure.domain.noise :as noise]))

;;;
;;; Circuit Fidelity Analysis Functions
;;;
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
         (s/valid? ::noise/noise-model noise-model)]}
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

;;;
;;; Platform Comparison Functions
;;;
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

;;;
;;; Utility Functions for Error Mitigation
;;;
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
         (s/valid? ::noise/noise-model noise-model)]}
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
         (s/valid? ::noise/noise-model noise-model)]}
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
