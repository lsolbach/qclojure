(ns org.soulspace.qclojure.application.algorithm.quantum-period-finding
  (:require
   [org.soulspace.qclojure.domain.math :as qmath]
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.domain.state :as qs]
   [org.soulspace.qclojure.application.algorithm.modular-arithmetic :as qma]
   [org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft]
   [org.soulspace.qclojure.domain.circuit-transformation :as qct]))

(defn quantum-period-finding
  "Find the period from a phase estimate using improved continued fraction expansion.
  
  This function implements a more robust version of period extraction from
  a phase measurement, which is critical for Shor's algorithm.
  
  Parameters:
  - measured-value: The value from quantum measurement
  - precision: Number of bits used in phase estimation
  - N: Modulus for period finding
  - a: Base for modular exponentiation
  
  Returns:
  Most likely period or nil if no valid period found"
  [measured-value precision N a]
  (let [;; Calculate phase from measurement
        phase (/ measured-value (Math/pow 2 precision))

        ;; Try different depths of continued fraction expansion
        candidates (for [depth [10 20 50 100]
                         :let [cf (qmath/continued-fraction measured-value (Math/pow 2 precision) depth)
                               convs (qmath/convergents cf)]
                         [num den] convs
                         ;; Verify this is actually a period
                         :when (and (pos? den)
                                    (<= den N)
                                    (= 1 (qmath/mod-exp a den N)))]
                     {:period den
                      :fraction [num den]
                      :error (Math/abs (- phase (/ num (Math/pow 2 precision))))})

        ;; Sort by error (lowest first) and then by period (smallest valid first)
        sorted-candidates (sort-by (juxt :error :period) candidates)]

    ;; Return the best candidate's period, or nil if none found
    (when (seq sorted-candidates)
      (:period (first sorted-candidates)))))

(defn enhanced-period-finding
  "Quantum subroutine for finding the period of f(x) = a^x mod N.
  
  This is the quantum heart of Shor's algorithm. It uses quantum phase
  estimation with the QFT to find the period r such that a^r ≡ 1 (mod N).
  
  Parameters:
  - a: Base for the function f(x) = a^x mod N
  - N: Modulus
  - n-qubits: Number of qubits for the quantum register (should be ~2*log₂(N))
  - hardware-compatible: (optional) Use hardware-compatible implementation
  - n-measurements: (optional) Number of measurements to perform for statistical analysis
  
  Returns:
  Map containing:
  - :measured-values - List of measured values from quantum circuit executions
  - :estimated-period - Estimated period based on continued fractions
  - :circuit - The quantum circuit used
  - :success - Whether a valid period was found
  - :confidence - Statistical confidence in the result (only with multiple measurements)"
  ([a N n-qubits]
   (enhanced-period-finding a N n-qubits 1))
  ([a N n-qubits n-measurements]
   {:pre [(pos-int? a) (pos-int? N) (pos-int? n-qubits) (< a N) (pos-int? n-measurements)]}

   ;; Calculate number of qubits needed for target register
   ;; We need enough qubits to represent N
   (let [n-target-qubits (int (Math/ceil (/ (Math/log N) (Math/log 2))))

         ;; Create the complete circuit with both control and target registers
         total-qubits (+ n-qubits n-target-qubits)
         circuit (-> (qc/create-circuit total-qubits
                                        "Period Finding"
                                        "Quantum period finding for Shor's algorithm")

                     ;; Step 1: Put control register in superposition
                     ((fn [c] (reduce #(qc/h-gate %1 %2) c (range n-qubits))))

                     ;; Step 2: Apply controlled modular exponentiation
                     ((fn [c]
                        ;; Create and compose the modular exponentiation circuit
                        (qct/compose-circuits c (qma/controlled-modular-exponentiation-circuit n-qubits n-target-qubits a N))))

                     ;; Step 3: Apply inverse QFT to the control register
                     ((fn [c]
                        ;; Create inverse QFT circuit for the control qubits only
                        (let [iqft-circuit (qft/inverse-quantum-fourier-transform-circuit n-qubits)]
                          ;; Apply the inverse QFT to control qubits only
                          ;; Using the enhanced compose-circuits with control-qubits-only option
                          (qct/compose-circuits c iqft-circuit {:control-qubits-only true})))))

         ;; Execute circuit and perform measurements multiple times for statistical analysis
         measurements (repeatedly n-measurements
                                  (fn []
                                    (let [initial-state (qs/zero-state total-qubits)
                                          final-state (qc/execute-circuit circuit initial-state)
                                          ;; Measure only the control register qubits using measure-subsystem
                                          phase-qubits (range n-qubits)
                                          measurement (qc/measure-subsystem final-state phase-qubits)]
                                      (:outcome measurement))))

         ;; Analyze measurements and find most frequent outcomes
         measurement-freqs (frequencies measurements)
         sorted-measurements (sort-by second > measurement-freqs)
         most-likely-measurements (take (min 5 (count sorted-measurements)) sorted-measurements)

         ;; Find periods from the most likely measurements
         estimated-periods (for [[measured-value _freq] most-likely-measurements
                                 :let [;; Calculate phase
                                       measured-phase (/ measured-value (Math/pow 2 n-qubits))
                                       ;; Use improved continued fraction for better period extraction
                                       cf (qmath/continued-fraction measured-value (Math/pow 2 n-qubits))
                                       convs (qmath/convergents cf)
                                       ;; Find convergent that gives a valid period
                                       period (some (fn [[_num den]]
                                                      (when (and (pos? den)
                                                                 (<= den N)
                                                                 (= 1 (qmath/mod-exp a den N)))
                                                        den))
                                                    convs)]
                                 :when period]
                             {:measured-value measured-value
                              :phase measured-phase
                              :period period
                              :probability (/ (get measurement-freqs measured-value) n-measurements)})

         ;; Sort by probability to get best candidates
         best-periods (sort-by :probability > estimated-periods)
         best-period (first best-periods)]

     {:measured-values measurements
      :measurement-frequencies measurement-freqs
      :estimated-period (when best-period (:period best-period))
      :period-candidates best-periods
      :circuit circuit
      :n-measurements n-measurements
      :success (boolean (seq best-periods))
      :confidence (when (seq best-periods)
                    (/ (reduce + (map :probability best-periods))
                       (count best-periods)))})))

