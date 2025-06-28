(ns org.soulspace.qclojure.adapter.backend.noisy-simulator
  "Advanced noisy quantum simulator backend implementing realistic
  quantum computing device simulation with comprehensive noise modeling.

  This backend provides local simulation of quantum devices with noise
  using the domain layer's quantum state and circuit functionality.
  It serves as both a reference implementation and testing backend for
  quantum algorithms under realistic noise conditions.
 
  This simulator models various types of quantum noise including:
  - Depolarizing noise using Kraus operators
  - Amplitude damping (T1 decay) modeling energy dissipation  
  - Phase damping (T2 dephasing) modeling pure dephasing
  - Readout errors with configurable bit-flip probabilities
  - Coherent errors and systematic rotation biases
  - Gate-specific noise parameters based on real device calibration
  - Comprehensive Amazon Braket quantum hardware noise models

  The noise model can be configured with parameters such as T1 and T2
  times, gate operation times, and noise strengths. It supports
  advanced noise configurations including correlated readout errors
  and coherent errors with specific rotation angles and axes.
  The simulator applies noise during gate operations and measurements,
  simulating realistic quantum device behavior.

  The noise model map has the following structure:
  ```clojure
  {:gate-noise {
    :h {:noise-type :depolarizing :noise-strength 0.01}
    :x {:noise-type :amplitude-damping :noise-strength 0.02}
    :cnot {:noise-type :phase-damping :noise-strength 0.03}
    ...}
   :readout-error {:prob-0-to-1 0.05 :prob-1-to-0 0.02}}
  ```
  
  Note: The simulator currently doesn't model crosstalk between qubits.

  The simulator supports asynchronous job management, allowing
  users to submit circuits and retrieve results later. It can be used
  for testing algorithms, circuit designs, and quantum operations
  without requiring access to actual quantum hardware.
   
  It also implements the CloudQuantumBackend protocol for mock cloud
  backend functionality, allowing it to be used in a cloud-like
  environment for testing purposes."
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.gate :as qg]))

;; Noisy simulator state management
(defonce noisy-state (atom {:job-counter 0
                            :active-jobs {}}))

;; Job record for noisy simulations
(defrecord NoisySimulatorJob
  [job-id circuit options noise-model status result created-at completed-at])

;; Helper functions for job management
(defn- generate-noisy-job-id []
  (let [new-counter (:job-counter (swap! noisy-state update :job-counter inc))]
    (str "noisy_job_" new-counter "_" (System/currentTimeMillis))))

;; Advanced noise model specifications
(s/def ::noise-type #{:depolarizing :amplitude-damping :phase-damping :readout :coherent})
(s/def ::noise-strength (s/and number? #(<= 0 % 1)))
(s/def ::t1-time pos?) ; T1 relaxation time in microseconds
(s/def ::t2-time pos?) ; T2 dephasing time in microseconds
(s/def ::gate-time pos?) ; Gate operation time in nanoseconds

(s/def ::coherent-error
  (s/keys :req-un [::rotation-angle ::rotation-axis]))

(s/def ::gate-noise-config
  (s/keys :req-un [::noise-type ::noise-strength]
          :opt-un [::t1-time ::t2-time ::gate-time ::coherent-error]))

(s/def ::readout-error-config
  (s/keys :req-un [::prob-0-to-1 ::prob-1-to-0]
          :opt-un [::correlated-errors]))

(s/def ::advanced-noise-model
  (s/keys :opt-un [::gate-noise-config ::readout-error-config ::crosstalk-matrix]))

;; Kraus operator implementations for quantum channels
(defn depolarizing-kraus-operators
  "Generate Kraus operators for depolarizing noise channel.
  
  The depolarizing channel applies each Pauli operator with probability p/4,
  and leaves the state unchanged with probability 1-3p/4.
  
  For proper normalization: Σᵢ Kᵢ† Kᵢ = I
  
  Parameters:
  - p: Total error probability (0 <= p <= 3/4 for physical channel)
  
  Returns: Vector of Kraus operators with coefficients applied to matrices"
  [p]
  (let [;; Use existing Pauli matrices from gate namespace
        no-error-coeff (m/sqrt (- 1.0 p))
        error-coeff (m/sqrt (/ p 3.0))]
    ;; Apply coefficients directly to matrices for proper quantum channel
    [{:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex no-error-coeff 0) %) row)) qg/pauli-i)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-x)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-y)}
     {:matrix (mapv (fn [row] (mapv #(fc/mult (fc/complex error-coeff 0) %) row)) qg/pauli-z)}]))

(defn amplitude-damping-kraus-operators
  "Generate Kraus operators for amplitude damping (T1 decay).
  
  Models energy dissipation where |1⟩ decays to |0⟩.
  Kraus operators: K₀ = [[1, 0], [0, √(1-γ)]], K₁ = [[0, √γ], [0, 0]]
  
  Parameters:
  - gamma: Damping parameter (0 <= gamma <= 1)
  
  Returns: Vector of Kraus operators using fastmath complex numbers"
  [gamma]
  [{:matrix [[(fc/complex 1.0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (m/sqrt (- 1.0 gamma)) 0)]]}
   {:matrix [[(fc/complex 0 0) (fc/complex (m/sqrt gamma) 0)]
             [(fc/complex 0 0) (fc/complex 0 0)]]}])

(defn phase-damping-kraus-operators
  "Generate Kraus operators for phase damping (T2 dephasing).
  
  Models random phase flips without energy loss.
  Kraus operators: K₀ = [[1, 0], [0, √(1-γ)]], K₁ = [[0, 0], [0, √γ]]
  
  Parameters:
  - gamma: Dephasing parameter (0 <= gamma <= 1)
  
  Returns: Vector of Kraus operators using fastmath complex numbers"
  [gamma]
  [{:matrix [[(fc/complex 1.0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (m/sqrt (- 1.0 gamma)) 0)]]}
   {:matrix [[(fc/complex 0 0) (fc/complex 0 0)]
             [(fc/complex 0 0) (fc/complex (m/sqrt gamma) 0)]]}])

(defn coherent-error-kraus-operator
  "Generate Kraus operator for coherent (systematic) errors.
  
  Parameters:
  - angle: Rotation angle in radians
  - axis: Rotation axis (:x, :y, or :z)
  
  Returns: Single Kraus operator for coherent rotation"
  [angle axis]
  (let [cos-half (m/cos (/ angle 2.0))
        sin-half (m/sin (/ angle 2.0))]
    (case axis
      :x {:matrix [[(fc/complex cos-half 0) (fc/complex (- sin-half) 0)] 
                   [(fc/complex sin-half 0) (fc/complex cos-half 0)]]}
      :y {:matrix [[(fc/complex cos-half 0) (fc/complex sin-half 0)] 
                   [(fc/complex (- sin-half) 0) (fc/complex cos-half 0)]]}
      :z {:matrix [[(fc/complex (m/cos angle) 0) (fc/complex 0 0)] 
                   [(fc/complex 0 0) (fc/complex (m/cos (- angle)) 0)]]})))

;; Advanced noise application functions
(defn apply-matrix-to-amplitude
  "Apply a 2x2 matrix to a single amplitude pair in a quantum state.
  
  Performs proper complex matrix-vector multiplication:
  [new-a0] = [m00 m01] [a0]
  [new-a1]   [m10 m11] [a1]
  
  Parameters:
  - amplitude-pair: [a0 a1] where each is a fastmath Vec2 complex number
  - matrix: 2x2 matrix [[m00 m01] [m10 m11]] with fastmath complex elements
  
  Returns: [new-a0 new-a1] as fastmath complex numbers"
  [amplitude-pair matrix]
  (let [[[m00 m01] [m10 m11]] matrix
        [a0 a1] amplitude-pair]
    [(fc/add (fc/mult m00 a0) (fc/mult m01 a1))
     (fc/add (fc/mult m10 a0) (fc/mult m11 a1))]))

(defn apply-single-qubit-kraus-operator
  "Apply a single Kraus operator to a specific qubit in a multi-qubit state.
  
  This implements the proper quantum mechanics for Kraus operator application:
  |ψ'⟩ = K|ψ⟩ / ||K|ψ⟩||
  
  The Kraus operator matrix should already include any coefficients.
  
  Parameters:
  - state: Quantum state
  - kraus-op: Kraus operator {:matrix matrix}
  - qubit-index: Target qubit (0-indexed)
  
  Returns: New quantum state after applying Kraus operator and normalizing"
  [state kraus-op qubit-index]
  (let [n-qubits (:num-qubits state)
        state-vec (:state-vector state)
        matrix (:matrix kraus-op)
        n-states (count state-vec)]
     
     (if (= n-qubits 1)
       ;; Single qubit case
       (let [[new-amp0 new-amp1] (apply-matrix-to-amplitude 
                                  [(first state-vec) (second state-vec)] 
                                  matrix)
             result-state {:num-qubits 1
                           :state-vector [new-amp0 new-amp1]}]
         ;; Normalize the result using the existing normalize-state function
         (qs/normalize-state result-state))
       
       ;; Multi-qubit case - apply to specific qubit
       (let [new-amplitudes 
             (vec (for [i (range n-states)]
                    (let [qubit-bit (bit-test i (- n-qubits 1 qubit-index))
                          partner-i (if qubit-bit
                                      (bit-clear i (- n-qubits 1 qubit-index))
                                      (bit-set i (- n-qubits 1 qubit-index)))
                          amp-pair (if qubit-bit
                                     [(nth state-vec partner-i) (nth state-vec i)]
                                     [(nth state-vec i) (nth state-vec partner-i)])
                          [new-amp0 new-amp1] (apply-matrix-to-amplitude 
                                                amp-pair
                                                matrix)]
                      (if qubit-bit new-amp1 new-amp0))))
             result-state {:num-qubits n-qubits
                           :state-vector new-amplitudes}]
         
         ;; Normalize the result using the existing normalize-state function
         (qs/normalize-state result-state)))))

(defn apply-quantum-channel
  "Apply a complete quantum channel defined by multiple Kraus operators.
  
  For pure state simulators, implements quantum channels by randomly selecting
  one Kraus operator to apply based on the probabilities encoded in the Kraus
  operator coefficients. This provides correct noise simulation.
  
  Parameters:
  - state: Input quantum state
  - kraus-operators: Vector of Kraus operators with matrices and coefficients
  - qubit-index: Target qubit
  
  Returns: Output quantum state after channel application"
  [state kraus-operators qubit-index]
  (if (= (count kraus-operators) 1)
    ;; Single Kraus operator case - apply directly
    (apply-single-qubit-kraus-operator state (first kraus-operators) qubit-index)
    ;; Multiple Kraus operators - select based on probabilities from coefficients
    (let [;; Calculate probabilities from Kraus operator coefficients (max |coefficient|²)
          probabilities (mapv (fn [kraus-op]
                                (let [matrix (:matrix kraus-op)]
                                  ;; Find maximum coefficient magnitude squared from the matrix
                                  (apply max (map (fn [row]
                                                    (apply max (map (fn [coeff]
                                                                      (+ (* (fc/re coeff) (fc/re coeff)) 
                                                                         (* (fc/im coeff) (fc/im coeff))))
                                                                    row)))
                                                  matrix)))) kraus-operators)
          ;; Generate random number for selection
          rand-val (rand)
          ;; Select Kraus operator based on cumulative probabilities
          selected-operator (loop [cumulative-prob 0.0
                                   idx 0]
                             (let [new-cumulative (+ cumulative-prob (nth probabilities idx))]
                               (if (or (< rand-val new-cumulative) (>= idx (dec (count kraus-operators))))
                                 (nth kraus-operators idx)
                                 (recur new-cumulative (inc idx)))))]
      ;; Apply the selected Kraus operator
      (apply-single-qubit-kraus-operator state selected-operator qubit-index))))

(defn calculate-decoherence-params
  "Calculate decoherence parameters from T1, T2 times and gate duration.
  
  Parameters:
  - t1: T1 relaxation time (microseconds)
  - t2: T2 dephasing time (microseconds) 
  - gate-time: Gate operation time (nanoseconds)
  
  Returns: {:gamma-1 gamma-2} decoherence parameters"
  [t1 t2 gate-time]
  (let [gate-time-us (/ gate-time 1000.0) ; Convert ns to μs
        gamma-1 (- 1.0 (m/exp (- (/ gate-time-us t1))))
        gamma-2 (- 1.0 (m/exp (- (/ gate-time-us t2))))]
    {:gamma-1 gamma-1 :gamma-2 gamma-2}))

;; Advanced noise application during gate operations
(defn apply-advanced-gate-noise
  "Apply advanced noise model during gate operation.
  
  Parameters:
  - state: Current quantum state
  - gate: Gate operation
  - noise-config: Advanced noise configuration
  
  Returns: State after gate operation and noise"
  [state gate noise-config]
  (let [clean-state (qc/apply-operation-to-state state gate)
        target-qubit (get-in gate [:operation-params :target] 0)
        {:keys [noise-type noise-strength t1-time t2-time gate-time]} noise-config]
    
    (case noise-type
      :depolarizing
      (let [kraus-ops (depolarizing-kraus-operators noise-strength)]
        (apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :amplitude-damping
      (let [decoherence (if (and t1-time gate-time)
                          (calculate-decoherence-params t1-time t2-time gate-time)
                          {:gamma-1 noise-strength :gamma-2 0})
            kraus-ops (amplitude-damping-kraus-operators (:gamma-1 decoherence))]
        (apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :phase-damping
      (let [decoherence (if (and t2-time gate-time)
                          (calculate-decoherence-params t1-time t2-time gate-time)
                          {:gamma-1 0 :gamma-2 noise-strength})
            kraus-ops (phase-damping-kraus-operators (:gamma-2 decoherence))]
        (apply-quantum-channel clean-state kraus-ops target-qubit))
      
      :coherent
      (let [coherent-config (get noise-config :coherent-error {:rotation-angle 0.01 :rotation-axis :z})
            kraus-op (coherent-error-kraus-operator 
                      (:rotation-angle coherent-config)
                      (:rotation-axis coherent-config))]
        (apply-single-qubit-kraus-operator clean-state kraus-op target-qubit))
      
      ;; Default: no noise
      clean-state)))

(defn apply-advanced-readout-noise
  "Apply advanced readout noise with potential correlations.
  
  Parameters:
  - results: Clean measurement results
  - readout-config: Advanced readout error configuration
  
  Returns: Noisy measurement results"
  [results readout-config]
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

(defn- execute-single-noisy-shot
  "Execute a single shot of a noisy quantum circuit.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - noise-model: Noise model configuration
  - num-qubits: Number of qubits in the circuit
  
  Returns: Single measurement outcome as a basis string"
  [circuit noise-model num-qubits]
  (loop [current-state (qs/zero-state num-qubits)
         remaining-gates (:operations circuit)]
    
    (if (empty? remaining-gates)
      ;; Final measurement with readout noise  
      (let [clean-outcome (:outcome (qs/measure-state current-state))
            clean-bitstring (qs/basis-string clean-outcome num-qubits)
            
            ;; Apply readout noise if configured
            final-bitstring (if-let [readout-config (:readout-error noise-model)]
                              (let [{:keys [prob-0-to-1 prob-1-to-0]} readout-config]
                                (apply str 
                                       (map-indexed
                                        (fn [qubit-idx bit]
                                          (let [flip-prob (case bit
                                                            \0 prob-0-to-1
                                                            \1 prob-1-to-0)]
                                            (if (< (rand) flip-prob)
                                              (case bit \0 "1" \1 "0" bit)
                                              bit)))
                                        clean-bitstring)))
                              clean-bitstring)]
        final-bitstring)
      
      ;; Apply gate with noise
      (let [gate (first remaining-gates)
            gate-type (:operation-type gate)
            target-qubit (get-in gate [:operation-params :target] 0)
            
            ;; Apply clean gate operation first
            post-gate-state (qc/apply-operation-to-state current-state gate)
            
            ;; Apply noise based on gate-specific configuration
            final-gate-state (if-let [gate-noise-config (get-in noise-model [:gate-noise gate-type])]
                               ;; Apply noise after the gate operation
                               (let [{:keys [noise-type noise-strength t1-time t2-time gate-time]} gate-noise-config]
                                 (case noise-type
                                   :depolarizing
                                   (let [kraus-ops (depolarizing-kraus-operators noise-strength)]
                                     (apply-quantum-channel post-gate-state kraus-ops target-qubit))
                                   
                                   :amplitude-damping
                                   (let [decoherence (if (and t1-time gate-time)
                                                       (calculate-decoherence-params t1-time (or t2-time (* t1-time 2)) gate-time)
                                                       {:gamma-1 noise-strength :gamma-2 0})
                                         kraus-ops (amplitude-damping-kraus-operators (:gamma-1 decoherence))]
                                     (apply-quantum-channel post-gate-state kraus-ops target-qubit))
                                   
                                   :phase-damping
                                   (let [decoherence (if (and t2-time gate-time)
                                                       (calculate-decoherence-params (or t1-time (* t2-time 2)) t2-time gate-time)
                                                       {:gamma-1 0 :gamma-2 noise-strength})
                                         kraus-ops (phase-damping-kraus-operators (:gamma-2 decoherence))]
                                     (apply-quantum-channel post-gate-state kraus-ops target-qubit))
                                   
                                   :coherent
                                   (let [coherent-config (get gate-noise-config :coherent-error {:rotation-angle 0.01 :rotation-axis :z})
                                         kraus-op (coherent-error-kraus-operator 
                                                   (:rotation-angle coherent-config)
                                                   (:rotation-axis coherent-config))]
                                     (apply-single-qubit-kraus-operator post-gate-state kraus-op target-qubit))
                                   
                                   ;; Default: no noise
                                   post-gate-state))
                               ;; No noise configuration for this gate type
                               post-gate-state)]
        
        (recur final-gate-state (rest remaining-gates))))))

(defn- execute-noisy-circuit-simulation
  "Execute a noisy quantum circuit simulation with proper noise independence per shot.
  
  Parameters:
  - circuit: Quantum circuit to simulate
  - options: Execution options including shot count
  - noise-model: Noise model configuration
  
  Returns: Noisy simulation results"
  [circuit options noise-model]
  (try
    (let [start-time (System/currentTimeMillis)
          shots (get options :shots 1024)
          num-qubits (:num-qubits circuit)]
      
      ;; Execute each shot independently with fresh noise
      (loop [shot-count 0
             accumulated-results {}
             last-final-state nil]
        
        (if (>= shot-count shots)
          {:job-status :completed
           :measurement-results accumulated-results
           :final-state last-final-state  ; Return the final state from the last shot
           :noise-applied true
           :shots-executed shots
           :execution-time-ms (- (System/currentTimeMillis) start-time)}
          
          ;; Execute single shot with fresh noise
          (let [outcome-bitstring (execute-single-noisy-shot circuit noise-model num-qubits)
                ;; For the final state, we'll capture it from one of the shots
                ;; (Note: this is somewhat artificial since each shot has different noise)
                updated-results (update accumulated-results outcome-bitstring (fnil inc 0))]
            
            (recur (inc shot-count)
                   updated-results
                   (if (= shot-count (dec shots))
                     ;; Capture final state from last shot for compatibility
                     (loop [state (qs/zero-state num-qubits)
                            gates (:operations circuit)]
                       (if (empty? gates)
                         state
                         (recur (qc/apply-operation-to-state state (first gates))
                                (rest gates))))
                     last-final-state))))))

    (catch Exception e
      {:job-status :failed
       :error-message (.getMessage e)
       :exception-type (.getName (class e))})))

;; Advanced Simulator Implementation  
(deftype LocalNoisyQuantumSimulator [noise-model config]
  qb/QuantumBackend

  (get-backend-info [_this]
    {:backend-type :advanced-noisy-simulator
     :backend-name "Advanced Noisy Quantum Simulator"
     :description "Realistic quantum simulator with comprehensive noise modeling"
     :noise-model noise-model
     :config config
     :capabilities #{:depolarizing-noise :amplitude-damping :phase-damping
                     :coherent-errors :readout-errors :decoherence-modeling}
     :version "1.0.0"})

  (get-supported-gates [_this] #{:x :y :z :h :cnot :rx :ry :rz})
  (is-available? [_this] true)

  (submit-circuit [_this circuit options]
    (let [job-id (generate-noisy-job-id)
          job (->NoisySimulatorJob job-id circuit options noise-model :queued nil
                                   (System/currentTimeMillis) nil)]

      ;; Store job and start execution in background
      (swap! noisy-state assoc-in [:active-jobs job-id] job)

      ;; Execute immediately in future (async execution)
      (future
        (let [result (execute-noisy-circuit-simulation circuit options noise-model)
              completed-job (assoc job
                                   :status (:job-status result)
                                   :result result
                                   :completed-at (System/currentTimeMillis))]
          (swap! noisy-state assoc-in [:active-jobs job-id] completed-job)))

      job-id))

  (get-job-status [_this job-id]
    (if-let [job (get (:active-jobs @noisy-state) job-id)]
      (:status job)
      :not-found))

  (get-job-result [_this job-id]
    (if-let [job (get (:active-jobs @noisy-state) job-id)]
      (if (= (:status job) :completed)
        (assoc (:result job) :job-id job-id)
        {:job-id job-id
         :job-status (:status job)
         :error-message "Job not completed"})
      {:job-id job-id
       :job-status :not-found
       :error-message "Job not found"}))

  (cancel-job [_this job-id]
    (if-let [job (get (:active-jobs @noisy-state) job-id)]
      (if (#{:queued :running} (:status job))
        (do
          (swap! noisy-state assoc-in [:active-jobs job-id]
                 (assoc job :status :cancelled
                        :completed-at (System/currentTimeMillis)))
          :cancelled)
        :already-completed)
      :not-found))

  (get-queue-status [_this]
    (let [jobs (vals (:active-jobs @noisy-state))
          active-count (count (filter #(#{:queued :running} (:status %)) jobs))
          completed-count (count (filter #(= :completed (:status %)) jobs))]
      {:total-jobs (count jobs)
       :active-jobs active-count
       :completed-jobs completed-count}))

  ;; Add CloudQuantumBackend implementation
  qb/CloudQuantumBackend

  (authenticate [_this _credentials]
    {:status :authenticated
     :session-id (str "noisy_sim_session_" (System/currentTimeMillis))
     :expires-at (+ (System/currentTimeMillis) (* 24 60 60 1000))})

  (get-session-info [_this]
    {:status :authenticated
     :backend-type :advanced-noisy-simulator
     :session-id "noisy_sim_session_mock"
     :authenticated-at (System/currentTimeMillis)})

  (list-available-devices [_this]
    [{:device-id "noisy-simulator-1"
      :device-name "Advanced Noisy Quantum Simulator"
      :device-status :online
      :max-qubits (get config :max-qubits 20)
      :backend-type :advanced-noisy-simulator
      :noise-model noise-model}])

  (get-device-topology [_this device-id]
    (let [max-qubits (get config :max-qubits 20)
          coupling-map (for [i (range max-qubits)
                             j (range max-qubits)
                             :when (not= i j)]
                         [i j])]
      {:device-id device-id
       :device-name "Advanced Noisy Quantum Simulator"
       :coupling-map coupling-map
       :max-qubits max-qubits
       :noise-model noise-model}))

  (get-calibration-data [_this device-id]
    {:device-id device-id
     :timestamp (java.time.Instant/now)
     :noise-model noise-model
     :coherence-times (vec (repeat (get config :max-qubits 20) 100000))})

  (estimate-cost [_this _circuit _options]
    {:total-cost 0.0
     :currency "USD"
     :cost-breakdown {:circuit-cost 0.0 :shot-cost 0.0 :device-cost 0.0}
     :estimated-credits 0})

  (batch-submit [this circuits options]
    (let [batch-id (str "noisy_batch_" (System/currentTimeMillis))
          job-ids (mapv #(qb/submit-circuit this % options) circuits)]
      (swap! noisy-state assoc-in [:active-jobs batch-id]
             {:batch-id batch-id
              :job-ids job-ids
              :status :queued
              :created-at (System/currentTimeMillis)})
      batch-id))

  (get-batch-status [this batch-job-id]
    (if-let [batch-info (get (:active-jobs @noisy-state) batch-job-id)]
      (let [job-ids (:job-ids batch-info)
            statuses (map #(qb/get-job-status this %) job-ids)
            status-counts (frequencies statuses)]
        {:batch-id batch-job-id
         :total-jobs (count job-ids)
         :status-counts status-counts
         :overall-status (cond
                           (every? #(= % :completed) statuses) :completed
                           (some #(= % :failed) statuses) :partial-failure
                           (some #(= % :running) statuses) :running
                           :else :queued)})
      {:batch-id batch-job-id :status :not-found}))

  (get-batch-results [this batch-job-id]
    (if-let [batch-info (get (:active-jobs @noisy-state) batch-job-id)]
      (let [job-ids (:job-ids batch-info)
            results (into {} (map (fn [job-id]
                                    [job-id (qb/get-job-result this job-id)])
                                  job-ids))]
        {:batch-id batch-job-id
         :results results
         :completed-jobs (count (filter #(= (:job-status %) :completed)
                                        (vals results)))
         :total-jobs (count job-ids)})
      {:batch-id batch-job-id :error-message "Batch not found"})))

;; Factory functions for advanced simulators
(defn create-noisy-simulator
  "Create a local noisy quantum simulator with comprehensive noise modeling.
  
  Parameters:
  - noise-model: Advanced noise model configuration
  - config: Optional simulator configuration
  
  Returns: LocalNoisyQuantumSimulator instance"
  ([noise-model]
   (create-noisy-simulator noise-model {}))
  ([noise-model config]
   (->LocalNoisyQuantumSimulator noise-model config)))

;; Utility functions for noisy simulator state management
(defn reset-noisy-simulator-state!
  "Reset the noisy simulator state, clearing all jobs and counters.
  
  Returns: Updated state"
  []
  (reset! noisy-state {:job-counter 0 :active-jobs {}}))

(defn get-noisy-simulator-stats
  "Get statistics about the noisy simulator state.
  
  Returns: Map with job counts and statistics"
  []
  (let [current-state @noisy-state
        jobs (vals (:active-jobs current-state))]
    {:total-jobs (count jobs)
     :job-counter (:job-counter current-state)
     :jobs-by-status (->> jobs
                          (group-by :status)
                          (map (fn [[status job-list]] [status (count job-list)]))
                          (into {}))
     :active-jobs-count (count (filter #(#{:queued :running} (:status %)) jobs))
     :oldest-job (when (seq jobs)
                   (apply min (map :created-at jobs)))
     :newest-job (when (seq jobs)
                   (apply max (map :created-at jobs)))}))

(defn cleanup-noisy-completed-jobs!
  "Remove completed jobs older than specified time (in milliseconds).
  
  Parameters:
  - max-age-ms: Maximum age for completed jobs (default: 1 hour)
  
  Returns: Number of jobs cleaned up"
  ([]
   (cleanup-noisy-completed-jobs! (* 60 60 1000))) ; 1 hour default
  ([max-age-ms]
   (let [current-time (System/currentTimeMillis)
         cutoff-time (- current-time max-age-ms)
         current-jobs (:active-jobs @noisy-state)
         jobs-to-keep (into {} 
                            (filter 
                             (fn [[_job-id job]]
                               (or (not= (:status job) :completed)
                                   (> (:completed-at job 0) cutoff-time)))
                             current-jobs))
         removed-count (- (count current-jobs) (count jobs-to-keep))]
     (swap! noisy-state assoc :active-jobs jobs-to-keep)
     removed-count)))

;;;
;;; Noise models
;;;


;; Utility functions for noise analysis
(defn estimate-circuit-fidelity
  "Estimate the overall fidelity of a circuit under given noise model.
  
  This provides a rough estimate based on gate counts and noise strengths."
  [circuit noise-model]
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

(defn compare-hardware-platforms
  "Compare circuit fidelity across different quantum hardware platforms.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  - platform-models: Map of platform names to noise models
  
  Returns: Map of platform comparisons with fidelity estimates and characteristics"
  [circuit platform-models]
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

(defn noise-model-for
  "Get the noise model for a specific platform.
  
  Parameters:
  - platform: Platform name (keyword)
  
  Returns: Noise model map or nil if not found"
  ([platform]
   (noise-model-for qb/devices platform))
  ([devices platform]
   (get-in devices [platform :noise-model])))

(defn all-noise-models
  "Get all available noise models.
  
  Returns: Map of platform names to noise models"
  ([]
   (all-noise-models qb/devices))
  ([devices]
   (into {} (map (fn [[k v]] [k (:noise-model v)]) devices))))

(comment
  (noise-model-for :ibm-lagos)

  ;; Create advanced simulators with different noise characteristics
  (def ibm-sim (create-noisy-simulator (noise-model-for :ibm-lagos-noise)))
  (def rigetti-sim (create-noisy-simulator (noise-model-for :rigetti-aspen-noise)))
  (def ideal-sim (create-noisy-simulator {}))
  
  ;; === Amazon Braket Hardware Models ===
  
  ;; IonQ trapped ion systems (high fidelity, slow gates)
  (def ionq-harmony-sim (create-noisy-simulator (noise-model-for :ionq-harmony-noise)))
  (def ionq-aria-sim (create-noisy-simulator (noise-model-for :ionq-aria-noise)))
  (def ionq-forte-sim (create-noisy-simulator (noise-model-for :ionq-forte-noise)))
  
  ;; Rigetti superconducting systems (fast gates, moderate fidelity)
  (def rigetti-m3-sim (create-noisy-simulator (noise-model-for :rigetti-aspen-m3-noise)))
  
  ;; Photonic and neutral atom systems (unique characteristics)
  (def xanadu-sim (create-noisy-simulator (noise-model-for :xanadu-x-series-noise)))
  (def quera-sim (create-noisy-simulator (noise-model-for :quera-aquila-noise)))
  
  ;; Test Bell circuit across all platforms
  (def bell-circuit 
    (-> (qc/create-circuit 2 "Bell State")
        (qc/h-gate 0)
        (qc/cnot-gate 0 1)))
  
  ;; Compare all Amazon Braket platforms
  (def all-models (all-noise-models))
  (def bell-comparison (compare-hardware-platforms bell-circuit all-models))
  
  (println "=== Amazon Braket Platform Comparison (Bell Circuit) ===")
  (doseq [[platform data] (sort-by (comp :estimated-fidelity second) > bell-comparison)]
    (println (format "%s (% s):" 
                     (name platform) 
                     (name (:platform-type data))))
    (println (format "  Fidelity: %.4f (%.2f%% error)"
                     (:estimated-fidelity data)
                     (* 100 (:total-estimated-error data))))
    (println (format "  T1: %s μs, T2: %s μs"
                     (get-in data [:coherence-times :t1-time])
                     (get-in data [:coherence-times :t2-time])))
    (println (format "  Readout errors: 0→1: %.3f, 1→0: %.3f"
                     (get-in data [:readout-fidelity :prob-0-to-1])
                     (get-in data [:readout-fidelity :prob-1-to-0]))))
  
  ;; Test more complex circuit to see platform differences
  (def grover-circuit
    (-> (qc/create-circuit 3 "3-qubit Grover")
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)  ; Superposition
        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2)      ; Oracle simulation
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)  ; Diffusion
        (qc/z-gate 0) (qc/z-gate 1) (qc/z-gate 2)
        (qc/cnot-gate 0 1) (qc/cnot-gate 1 2)
        (qc/h-gate 0) (qc/h-gate 1) (qc/h-gate 2)))
  
  (def grover-comparison (compare-hardware-platforms grover-circuit all-models))
  
  (println "\n=== Platform Comparison (Complex Grover Circuit) ===")
  (doseq [[platform data] (sort-by (comp :estimated-fidelity second) > grover-comparison)]
    (println (format "%s: %.4f fidelity (%.1f%% total error)"
                     (name platform)
                     (:estimated-fidelity data)
                     (* 100 (:total-estimated-error data)))))
  
  ;; Platform-specific insights
  (println "\n=== Platform Characteristics ===")
  (println "Trapped Ion (IonQ):")
  (println "  - Highest fidelity for complex circuits")
  (println "  - Slower gate times (μs scale)")
  (println "  - All-to-all connectivity")
  (println "  - Best for: Deep circuits, high-fidelity algorithms")
  
  (println "\nSuperconducting (Rigetti, IBM):")
  (println "  - Fast gate times (ns scale)")
  (println "  - Limited connectivity")
  (println "  - Good for: NISQ algorithms, near-term applications")
  
  (println "\nPhotonic (Xanadu):")
  (println "  - Continuous variable operations")
  (println "  - High optical losses")
  (println "  - Good for: Specific algorithms like Gaussian boson sampling")
  
  (println "\nNeutral Atom (QuEra):")
  (println "  - Programmable connectivity")
  (println "  - Unique Rydberg interactions")
  (println "  - Good for: Optimization, many-body physics")
  
  ;; Individual noise channel testing
  (def test-state (qs/plus-state))
  
  ;; Depolarizing noise
  (def depol-ops (depolarizing-kraus-operators 0.1))
  (def noisy-state (apply-quantum-channel test-state depol-ops 0))
  (println "\nDepolarizing noise result:" (:state-vector noisy-state))
  
  ;; Amplitude damping
  (def amp-ops (amplitude-damping-kraus-operators 0.1))
  (def damped-state (apply-quantum-channel test-state amp-ops 0))
  (println "Amplitude damping result:" (:state-vector damped-state))
  
  ;; Decoherence parameters for different platforms
  (def ionq-decoherence (calculate-decoherence-params 15000.0 7000.0 40000.0))
  (def rigetti-decoherence (calculate-decoherence-params 45.0 35.0 200.0))
  
  (println "\nDecoherence comparison:")
  (println "  IonQ Aria (T1=15ms, T2=7ms, gate=40μs):" ionq-decoherence)
  (println "  Rigetti M3 (T1=45μs, T2=35μs, gate=200ns):" rigetti-decoherence))
  
  ;; Advanced noise modeling capabilities:
  ;; 1. Realistic Amazon Braket device parameters
  ;; 2. Platform-specific noise characteristics
  ;; 3. Gate-specific noise strengths and timings
  ;; 4. Multiple noise channels (depolarizing, amplitude/phase damping, coherent errors)
  ;; 5. Readout errors with platform-specific rates
  ;; 6. Circuit fidelity estimation and platform comparison
  ;; 7. Real device calibration data integration
  ;; 8. Cross-platform benchmarking capabilities
  

