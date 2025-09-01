(ns org.soulspace.qclojure.domain.noise
  "Specifications and functions for applying quantum noise to quantum states.
   
   This namespace defines specifications for quantum noise models,
   including gate noise and readout errors, as well as functions to
   apply these noise models during gate operations and measurements.

   The noise models are designed to be flexible and extensible,
   allowing users to simulate various types of realistic quantum noise.

   Example noise types include:
   - Depolarizing noise
   - Amplitude damping
   - Phase damping
   - Coherent errors
   "
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.channel :as channel]))

;;;
;;; Specs for quantum noise models
;;;
(s/def ::noise-type keyword?) ; Allow any keyword, function will handle unknown types gracefully
(s/def ::noise-strength (s/and number? #(<= 0 % 1)))
(s/def ::t1-time pos?) ; T1 relaxation time in microseconds
(s/def ::t2-time pos?) ; T2 dephasing time in microseconds
(s/def ::gate-time pos?) ; Gate operation time in nanoseconds
(s/def ::prob-0-to-1 (s/and number? #(<= 0 % 1)))
(s/def ::prob-1-to-0 (s/and number? #(<= 0 % 1)))
(s/def ::correlation-factor pos?)

(s/def ::coherent-error
  (s/keys :req-un [::rotation-angle ::rotation-axis]))

(s/def ::gate-noise-config
  (s/keys :req-un [::noise-type]
          :opt-un [::noise-strength ::t1-time ::t2-time ::gate-time ::coherent-error]))

(s/def ::correlated-errors
  (s/map-of nat-int? (s/map-of nat-int? ::correlation-factor)))

(s/def ::readout-error-config
  (s/keys :req-un [::prob-0-to-1 ::prob-1-to-0]
          :opt-un [::correlated-errors]))

(s/def ::noise-model
  (s/keys :opt-un [::gate-noise ::readout-error]))

;;;
;;; Gate Noise Application Functions
;;;
(defn apply-gate-noise
  "Apply noise model during gate operation.
  
  This function applies applies the configured noise channel to a clean state
  to simulate realistic quantum hardware behavior.
  
  Parameters:
  - clean-state: State after clean gate operation has been applied
  - gate: Gate operation to apply
  - noise-model: noise model to apply
  
  Returns: State after gate operation and noise application"
  [clean-state gate noise-model]
  {:pre [;(s/valid? ::qs/quantum-state clean-state)
         (map? gate)
         (s/valid? ::noise-model noise-model)]}
  (let [gate-type (:operation-type gate)
        target-qubit (get-in gate [:operation-params :target] 0)]
    (if-let [noise-config (get-in noise-model [:gate-noise gate-type])]
      (let [{:keys [noise-type t1-time t2-time gate-time]} noise-config
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
          clean-state))
      ;; no noise for this gate type, return clean state
      clean-state)))

;;;
;;; Readout Noise Application Functions
;;;
(defn- calculate-final-bitstring
  "Helper function to calculate final bitstring after applying readout noise.
  
  This function processes each qubit in order, allowing correlations to influence
  the error probabilities of subsequent qubits based on prior errors.
  
  Parameters:
  - clean-bitstring: Original measurement outcome as a string
  - num-qubits: Number of qubits in the system
  - readout-config: Configuration for readout errors including correlations
  
  Returns: Final bitstring with readout noise applied"
  [clean-bitstring num-qubits readout-config]
  (let [{:keys [prob-0-to-1 prob-1-to-0 correlated-errors]} readout-config
        bit-vector (vec clean-bitstring)]
  
    ;; Single-pass correlated error model
    ;; Process qubits in order, allowing correlations to influence subsequent decisions
    (loop [qubits (range num-qubits)
           current-bits bit-vector
           error-history #{}]
      (if-let [qubit-idx (first qubits)]
        (let [original-bit (nth current-bits qubit-idx)
              base-flip-prob (case original-bit
                               \0 prob-0-to-1
                               \1 prob-1-to-0)
  
              ;; Calculate correlation adjustments from qubits that already had errors
              correlation-factor (if (and correlated-errors (seq error-history))
                                   (reduce (fn [factor source-qubit]
                                             (if-let [qubit-correlations (get correlated-errors source-qubit)]
                                               (if-let [corr-factor (get qubit-correlations qubit-idx)]
                                                 (* factor corr-factor)
                                                 factor)
                                               factor))
                                           1.0
                                           error-history)
                                   1.0)
  
              ;; Apply correlation factor with bounds checking
              effective-prob (min 1.0 (max 0.0 (* base-flip-prob correlation-factor)))
  
              ;; Decide if error occurs
              error-occurs? (< (rand) effective-prob)]
  
          (if error-occurs?
            ;; Error occurred - flip bit and add to error history
            (recur (rest qubits)
                   (assoc current-bits qubit-idx (case original-bit \0 \1 \1 \0))
                   (conj error-history qubit-idx))
            ;; No error - continue
            (recur (rest qubits)
                   current-bits
                   error-history)))
  
        ;; All qubits processed - return final bitstring
        (apply str current-bits)))))

(defn apply-readout-noise
  "Apply advanced readout noise with potential correlations.
  
  This function simulates realistic measurement errors that occur in
  actual quantum hardware, including correlated errors between qubits.
  
  Correlated errors model the physical reality that readout errors on one qubit
  can increase or decrease the probability of errors on nearby qubits due to:
  - Electromagnetic crosstalk during readout
  - Charge noise affecting neighboring qubits  
  - Shared readout circuitry interference
  
  The correlation model works as follows:
  1. For each qubit, calculate effective error probability considering correlations
  2. Apply error decisions in a single pass to avoid double-counting
  3. Correlation factors > 1.0 increase error probability, < 1.0 decrease it
  
  Parameters:
  - state: the state to measure
  - num-qubits: number of qubits in the system
  - noise-model: noise model configuration with optional correlated-errors
  
  Correlated-errors format:
  {:correlated-errors {source-qubit {target-qubit correlation-factor, ...}, ...}}
  where correlation-factor > 1.0 increases error probability, < 1.0 decreases it.
  
  Returns: Bitstring representing the measured outcome with readout noise applied"
  [current-state num-qubits noise-model]
  {:pre [(s/valid? ::noise-model noise-model)]}
  (let [clean-outcome (:outcome (qs/measure-state current-state))
        clean-bitstring (qs/basis-string clean-outcome num-qubits)

        ;; Apply readout noise if configured
        final-bitstring (if-let [readout-config (:readout-error noise-model)]
                          (calculate-final-bitstring clean-bitstring num-qubits readout-config)
                          clean-bitstring)]
    final-bitstring))
