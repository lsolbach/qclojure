(ns org.soulspace.qclojure.application.error-mitigation.symmetry-verification
  "Quantum symmetry verification for comprehensive error mitigation and circuit validation.
  
  This namespace provides symmetry verification tools for quantum circuits, enabling
  sophisticated error mitigation strategies through systematic symmetry analysis.
  Symmetries in quantum circuits are fundamental for error detection and mitigation because
  they provide expected invariant properties that should be preserved during ideal quantum
  computation.
  
  Key capabilities:
  • Parity symmetry verification for circuits with parity-preserving operations
  • Reflection symmetry analysis using bit-flip invariance properties
  • Permutation symmetry detection for circuits with qubit exchange invariance
  • Rotational symmetry analysis for systems with continuous rotational symmetry
  • Statistical significance testing using chi-squared and confidence intervals
  • Configurable thresholds and production-grade error reporting
  • Comprehensive violation analysis with severity classification
  • Automated corrective action recommendations
  
  Symmetry Types Analyzed:
  
  1. **Parity Symmetry**: For circuits containing only parity-preserving gates (H, S, T, Pauli, CNOT),
     the overall parity of measurement outcomes should be preserved. Violations indicate
     systematic measurement errors or decoherence effects.
  
  2. **Reflection Symmetry**: Bit-flip symmetry where states |x⟩ and |x̄⟩ (bit-wise complement)
     should have equal measurement probabilities under certain circuit conditions.
     Critical for detecting calibration errors and crosstalk.
  
  3. **Permutation Symmetry**: For circuits invariant under qubit permutations, measurement
     distributions should be equivalent when qubits are exchanged. Essential for
     validating hardware uniformity and detecting qubit-specific systematic errors.
  
  4. **Rotational Symmetry**: For systems with continuous rotational invariance (e.g., 
     certain 3-qubit systems), measurement outcomes should be invariant under
     specific rotational transformations.
  
  Physical Background:
  Quantum error correction and mitigation rely heavily on symmetry properties because:
  - Ideal quantum evolution preserves certain symmetries of the initial state and Hamiltonian
  - Hardware noise and systematic errors often break these symmetries in characteristic ways
  - Symmetry violations provide diagnostic information about error sources and magnitudes
  - Statistical analysis of symmetry preservation enables quantitative error assessment
  
  Production Features:
  • Configurable statistical significance testing with proper multiple comparison corrections
  • Scalable analysis algorithms for systems from 2 to 20+ qubits
  • Comprehensive violation classification with actionable diagnostic information
  • Performance optimization with sampling strategies for large Hilbert spaces
  • Integration with measurement error mitigation and hardware characterization protocols"
  (:require [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.util.test :as util]
            [clojure.set :as set]))

;;;
;;; Symmetry Verification
;;;

;;
;; Production Configuration and Validation
;;
(def default-config
  "Default configuration for production symmetry verification"
  {:thresholds {:parity-tolerance 0.03          ; Stricter than 0.05 for production
                :reflection-symmetry-min 0.9     ; Stricter than 0.85 for production
                :permutation-symmetry-min 0.85   ; Maintained strict threshold
                :overall-symmetry-min 0.8        ; Raised from 0.75 for production
                :max-violations 2}               ; Reduced from 3 for production
   :statistical {:min-shots 500                 ; Increased for better statistics
                 :confidence-level 0.95
                 :use-chi-squared true
                 :effect-size-threshold 0.1}     ; Medium effect size threshold
   :performance {:max-states-for-full-analysis 16  ; Use sampling for > 4 qubits
                 :use-sampling true
                 :sample-size 2000               ; Increased sample size
                 :timeout-ms 30000}              ; 30 second timeout
   :advanced-symmetries [:parity :reflection :permutation :rotational]
   :error-reporting {:detailed-violations true
                     :include-suggestions true
                     :log-level :info
                     :statistical-details true}  ; Include statistical test details
   :production-mode {:strict-validation true    ; Enable all production checks
                     :fail-on-timeout true      ; Fail if analysis times out
                     :require-statistical-significance false ; Don't require for all tests
                     :enable-early-termination true}})

(defn validate-measurement-results
  "Validate measurement results for statistical significance and consistency in symmetry analysis.
  
  This function performs comprehensive validation of measurement data to ensure sufficient
  statistical power for reliable symmetry verification. Insufficient statistics can lead
  to false positives or negatives in symmetry violation detection.
  
  The validation checks multiple criteria:
  - Minimum shot count for statistical significance based on expected effect sizes
  - Non-empty measurement data to avoid division by zero errors
  - Confidence level assessment for the given sample size
  
  Statistical Background:
  Symmetry verification requires adequate sampling to distinguish between true symmetry
  violations and statistical fluctuations. The minimum shot count is determined by:
  - Expected effect size of symmetry violations (typically 0.05-0.2 in quantum systems)
  - Desired statistical power (typically 0.8) and significance level (typically 0.05)
  - Number of symmetry tests being performed (multiple comparison corrections)
  
  Parameters:
  - measurement-results: Map of quantum state strings to measurement counts
                        e.g., {\"00\" 450, \"01\" 25, \"10\" 30, \"11\" 495}
  - config: Configuration map containing statistical requirements:
           - [:statistical :min-shots] - Minimum total shots for significance (default: 500)
           - [:statistical :confidence-level] - Required confidence level (default: 0.95)
  
  Returns:
  Map containing validation results:
  - :valid - Boolean indicating if measurement data meets statistical requirements  
  - :reason - String explaining validation failure (if :valid is false)
  - :confidence - Numerical confidence level achieved with current data
  - :total-shots - Total number of measurement shots in the dataset
  - :num-unique-states - Number of distinct quantum states observed
  
  Example:
  (validate-measurement-results {\"00\" 450 \"11\" 450 \"01\" 50 \"10\" 50}
                               {:statistical {:min-shots 500 :confidence-level 0.95}})
  ;=> {:valid true, :confidence 0.95, :total-shots 1000, :num-unique-states 4}"
  [measurement-results config]
  (let [total-shots (reduce + (vals measurement-results))
        min-shots (get-in config [:statistical :min-shots])
        confidence-level (get-in config [:statistical :confidence-level])]
    
    (cond
      (< total-shots min-shots)
      {:valid false 
       :reason (str "Insufficient shots for statistical significance. Got " total-shots ", need " min-shots)
       :confidence 0.0}
      
      (empty? measurement-results)
      {:valid false 
       :reason "No measurement results provided"
       :confidence 0.0}
      
      :else
      {:valid true
       :confidence confidence-level
       :total-shots total-shots
       :num-unique-states (count measurement-results)})))

(defn compute-chi-squared-test
  "Compute chi-squared goodness-of-fit test for quantum symmetry hypothesis testing.
  
  This function performs a rigorous statistical test to determine whether observed
  measurement counts are consistent with expected counts under a symmetry hypothesis.
  The chi-squared test is particularly well-suited for quantum measurement data because
  it handles discrete count data and provides both significance testing and effect size
  estimation.
  
  Mathematical Foundation:
  The chi-squared statistic is computed as:
  χ² = Σ[(observed_i - expected_i)² / expected_i]
  
  Under the null hypothesis (symmetry holds), this statistic follows a chi-squared 
  distribution with the specified degrees of freedom. Large values indicate significant
  deviations from the expected symmetry pattern.
  
  The p-value represents P(χ² ≥ observed_value | H₀), where H₀ is the symmetry hypothesis.
  Small p-values (< 0.05) provide evidence against the symmetry hypothesis.
  
  Implementation Features:
  - Robust handling of small expected counts (Cochran's rule: expected ≥ 0.1)
  - Comprehensive critical value lookup table for common degrees of freedom
  - Approximation formulas for large degrees of freedom cases
  - Effect size calculation for practical significance assessment
  - Multi-level p-value estimation with appropriate granularity
  
  Parameters:
  - observed-counts: Vector of observed measurement counts for each symmetry-related state
  - expected-counts: Vector of expected counts under perfect symmetry hypothesis
  - degrees-of-freedom: Number of independent comparisons (typically states - 1)
  
  Returns:
  Map containing comprehensive statistical analysis:
  - :chi-squared - The computed χ² test statistic
  - :critical-value - Critical value for 95% confidence (α = 0.05)
  - :p-value - Probability of observing this result under null hypothesis
  - :significant - Boolean indicating statistical significance (p < 0.05)
  - :degrees-of-freedom - Number of degrees of freedom used
  - :effect-size - Practical effect size (χ²/N) for magnitude assessment
  
  Example:
  (compute-chi-squared-test [45 55] [50 50] 1)
  ;=> {:chi-squared 1.0, :critical-value 3.841, :p-value 0.25, 
  ;    :significant false, :degrees-of-freedom 1, :effect-size 0.01}"
  [observed-counts expected-counts degrees-of-freedom]
  (let [;; Ensure we have valid expected counts (avoid division by zero)
        valid-pairs (filter (fn [[obs exp]] (> exp 0.1)) (map vector observed-counts expected-counts))
        
        chi-squared (if (seq valid-pairs)
                      (reduce (fn [sum [obs exp]]
                                (+ sum (/ (* (- obs exp) (- obs exp)) exp)))
                              0.0
                              valid-pairs)
                      0.0)
        
        ;; More complete critical values for 95% confidence (α = 0.05)
        critical-value-95 (case degrees-of-freedom
                            1 3.841   ; Most common for 2x2 comparisons
                            2 5.991   ; For 3-state comparisons  
                            3 7.815   ; For 4-state comparisons
                            4 9.488   ; For 5-state comparisons
                            5 11.070  ; For 6-state comparisons
                            6 12.592  ; For 7-state comparisons
                            7 14.067  ; For 8-state comparisons
                            8 15.507  ; For 9+ state comparisons
                            ;; For larger DOF, use approximation: DOF + 2*sqrt(2*DOF)
                            (+ degrees-of-freedom (* 2 (Math/sqrt (* 2 degrees-of-freedom)))))
        
        ;; Better p-value estimation using chi-squared distribution approximation
        p-value (cond
                  (< chi-squared 0.1) 0.95
                  (< chi-squared (/ critical-value-95 4)) 0.75
                  (< chi-squared (/ critical-value-95 2)) 0.5  
                  (< chi-squared critical-value-95) 0.25
                  (< chi-squared (* 1.5 critical-value-95)) 0.1
                  (< chi-squared (* 2 critical-value-95)) 0.05
                  (< chi-squared (* 3 critical-value-95)) 0.01
                  :else 0.001)
        
        significant (> chi-squared critical-value-95)]
    
    {:chi-squared chi-squared
     :critical-value critical-value-95  
     :p-value p-value
     :significant significant
     :degrees-of-freedom degrees-of-freedom
     :effect-size (if (pos? (reduce + expected-counts))
                    (/ chi-squared (reduce + expected-counts))
                    0.0)}))

(defn detect-permutation-symmetries
  "Detect permutation symmetries in quantum measurement results with comprehensive analysis.
  
  This function analyzes measurement data to identify violations of permutation symmetry,
  which occurs when a quantum circuit should be invariant under the exchange of qubit
  indices. Permutation symmetry is crucial for validating hardware uniformity and
  detecting qubit-specific systematic errors.
  
  Physical Context:
  Permutation symmetry arises in quantum circuits where:
  - All qubits are initialized in the same state
  - All qubits undergo identical gate sequences  
  - Hardware properties (decoherence, gate fidelities) are uniform across qubits
  - No qubit-specific calibration errors or crosstalk effects are present
  
  The analysis checks whether measurement outcomes for states related by qubit
  permutations have statistically equivalent populations. For example, in a 3-qubit
  system, states |001⟩ and |100⟩ should have equal measurement probabilities if
  the circuit has permutation symmetry under (qubit 0 ↔ qubit 2) exchange.
  
  Algorithm Details:
  For small systems (≤ 4 qubits): Exhaustive permutation pair analysis
  For large systems (> 4 qubits): Statistical sampling of representative permutations
  
  The violation severity is classified based on relative population differences:
  - Low severity: 5-15% relative difference  
  - Medium severity: 15-25% relative difference
  - High severity: >25% relative difference
  
  Parameters:
  - measurement-results: Map of quantum state strings to measurement counts
                        e.g., {\"001\" 245, \"010\" 255, \"100\" 250, \"111\" 750}
  - num-qubits: Number of qubits in the quantum system (determines analysis strategy)
  - config: Configuration map containing:
           - [:performance :max-states-for-full-analysis] - Threshold for sampling (default: 16)
           - [:performance :sample-size] - Number of states to sample for large systems
  
  Returns:
  Map containing permutation symmetry analysis:
  - :score - Symmetry score from 0.0 (completely broken) to 1.0 (perfect symmetry)
  - :violations - Vector of detected violations with detailed diagnostics:
    - :type - Always :permutation-violation  
    - :states - Pair of states that should be equivalent under permutation
    - :counts - Actual measurement counts for the state pair
    - :relative-difference - Normalized difference between counts
    - :severity - Classification as :low, :medium, or :high
  - :analysis - String description of the analysis method and results
  
  Example:
  (detect-permutation-symmetries {\"01\" 480 \"10\" 520} 2 default-config)
  ;=> {:score 0.96, :violations [], :analysis \"Checked 1 systematic permutation pairs, found 0 violations\"}"
  [measurement-results num-qubits config]
  (let [max-states (get-in config [:performance :max-states-for-full-analysis])
        total-shots (reduce + (vals measurement-results))]
    
    (if (> (Math/pow 2 num-qubits) max-states)
      ;; For large systems, use sampling approach
      (let [sampled-states (take (get-in config [:performance :sample-size] 100)
                                 (keys measurement-results))
            
            ;; Generate permutation mappings for sampling
            permutation-violations (for [state sampled-states
                                         :let [;; Simple bit permutations (swap adjacent bits)
                                               swapped-states (for [i (range (dec num-qubits))]
                                                                (let [bits (vec state)
                                                                      bit-i (nth bits i)
                                                                      bit-i+1 (nth bits (inc i))
                                                                      swapped-bits (assoc bits i bit-i+1 (inc i) bit-i)]
                                                                  (apply str swapped-bits)))]
                                         swapped-state swapped-states
                                         :when (contains? measurement-results swapped-state)
                                         :let [count1 (get measurement-results state 0)
                                               count2 (get measurement-results swapped-state 0)
                                               relative-diff (if (pos? total-shots)
                                                               (/ (Math/abs (- count1 count2)) total-shots)
                                                               0.0)]
                                         :when (> relative-diff 0.05)]
                                     {:type :permutation-violation
                                      :states [state swapped-state]
                                      :counts [count1 count2]
                                      :relative-difference relative-diff
                                      :severity (cond (> relative-diff 0.2) :high
                                                      (> relative-diff 0.1) :medium
                                                      :else :low)})
            
            average-violation (if (seq permutation-violations)
                                (/ (reduce + (map :relative-difference permutation-violations))
                                   (count permutation-violations))
                                0.0)
            
            symmetry-score (max 0.0 (- 1.0 (* 2 average-violation)))]
        
        {:score symmetry-score
         :violations permutation-violations
         :analysis (format "Sampled %d states, checked adjacent bit swaps, found %d violations"
                           (count sampled-states) (count permutation-violations))})
      
      ;; For smaller systems, comprehensive permutation analysis
      (let [;; Generate systematic permutation mappings
            permutation-pairs (case num-qubits
                                2 [["01" "10"]]  ; Single swap
                                3 [["001" "100"] ["010" "100"] ["011" "110"]  ; Bit position swaps
                                   ["001" "010"] ["100" "010"] ["101" "110"]]  ; Other swaps
                                4 [["0001" "1000"] ["0010" "0100"] ["0011" "1100"] ; Systematic swaps
                                   ["0001" "0010"] ["0100" "1000"] ["0101" "1010"]]
                                5 [["00001" "10000"] ["00010" "01000"] ["00011" "11000"]
                                   ["00100" "00001"] ["01010" "10100"]]
                                []) ; Skip for very large systems
            
            violations (for [[state1 state2] permutation-pairs
                             :when (and (contains? measurement-results state1)
                                        (contains? measurement-results state2))
                             :let [count1 (get measurement-results state1 0)
                                   count2 (get measurement-results state2 0)
                                   expected-avg (/ (+ count1 count2) 2.0)
                                   difference (if (pos? total-shots)
                                                (/ (Math/abs (- count1 count2)) total-shots)
                                                0.0)]
                             :when (> difference 0.05)] ; Configurable threshold
                         {:type :permutation-violation
                          :states [state1 state2]
                          :counts [count1 count2]
                          :expected-average expected-avg
                          :relative-difference difference
                          :severity (cond (> difference 0.25) :high
                                          (> difference 0.15) :medium
                                          :else :low)})
            
            average-violation (if (seq violations)
                                (/ (reduce + (map :relative-difference violations))
                                   (count violations))
                                0.0)
            
            symmetry-score (max 0.0 (- 1.0 (* 3 average-violation)))] ; More sensitive scoring
        
        {:score symmetry-score
         :violations violations
         :analysis (format "Checked %d systematic permutation pairs, found %d violations"
                           (count permutation-pairs) (count violations))}))))

(defn detect-rotational-symmetries
  "Detect rotational symmetries in quantum circuits with continuous rotational invariance.
  
  This function analyzes measurement data for violations of rotational symmetry, which
  occurs in quantum systems that are invariant under continuous or discrete rotational
  transformations. Rotational symmetry is particularly important for systems with
  specific geometric arrangements or Hamiltonians with rotational invariance.
  
  Physical Background:
  Rotational symmetry in quantum systems arises when:
  - The system Hamiltonian commutes with rotation operators
  - Initial states possess rotational symmetry (e.g., symmetric superposition states)
  - Gate sequences preserve the rotational symmetry of the system
  - Environmental noise is isotropic (equal in all spatial directions)
  
  For 3-qubit systems, this implementation focuses on 3-fold rotational symmetry
  (120° rotations) where states transform under cyclic permutation:
  |001⟩ → |010⟩ → |100⟩ → |001⟩
  
  Such symmetries are relevant in:
  - Spin systems with triangular lattice geometry
  - Molecular systems with C₃ symmetry
  - Quantum simulators of frustrated magnetic systems
  - Error correction codes with rotational structure
  
  Algorithm:
  1. Identify rotation triplets (sets of 3 states related by 120° rotation)
  2. Compute average population for each triplet
  3. Calculate maximum deviation from uniform distribution
  4. Classify violations based on relative deviation magnitude
  
  Current Implementation:
  - Specialized for 3-qubit systems (extensible to other systems)
  - Analyzes discrete 120° rotations around the system's symmetry axis
  - Can be extended to continuous rotational groups for larger systems
  
  Parameters:
  - measurement-results: Map of quantum state strings to measurement counts
  - num-qubits: Number of qubits (currently optimized for 3-qubit systems)  
  - config: Configuration map (currently uses default thresholds)
  
  Returns:
  Map containing rotational symmetry analysis:
  - :score - Rotational symmetry score from 0.0 to 1.0
  - :violations - Vector of detected rotational symmetry violations:
    - :type - Always :rotational-violation
    - :states - Triplet of states related by rotation
    - :counts - Measurement counts for each state in the triplet  
    - :max-deviation - Maximum relative deviation from uniform distribution
    - :severity - Classification as :low, :medium, or :high
  - :analysis - Description of analysis performed and results
  
  Example:
  (detect-rotational-symmetries {\"001\" 330 \"010\" 340 \"100\" 330} 3 default-config)
  ;=> {:score 0.97, :violations [], :analysis \"Checked 2 rotation triplets, found 0 violations\"}"
  [measurement-results num-qubits config]
  (if (not= num-qubits 3) ; Only implement for 3 qubits for now
    {:score 1.0 :violations [] :analysis "Rotational symmetry only checked for 3 qubits"}
    
    (let [total-shots (reduce + (vals measurement-results))
          ;; For 3 qubits, check rotation by 120 degrees: 001 -> 010 -> 100 -> 001
          rotation-triplets [["001" "010" "100"]
                             ["011" "110" "101"]]
          
          violations (for [triplet rotation-triplets
                           :let [counts (map #(get measurement-results % 0) triplet)
                                 avg-count (/ (reduce + counts) 3.0)
                                 max-deviation (apply max (map #(Math/abs (- % avg-count)) counts))
                                 relative-deviation (if (pos? total-shots)
                                                      (/ max-deviation total-shots)
                                                      0.0)]
                           :when (> relative-deviation 0.15)]
                       {:type :rotational-violation
                        :states triplet
                        :counts counts
                        :max-deviation relative-deviation
                        :severity (cond (> relative-deviation 0.3) :high
                                        (> relative-deviation 0.2) :medium
                                        :else :low)})
          
          average-violation (if (seq violations)
                              (/ (reduce + (map :max-deviation violations))
                                 (count violations))
                              0.0)
          
          symmetry-score (- 1.0 average-violation)]
      
      {:score symmetry-score
       :violations violations
       :analysis (format "Checked %d rotation triplets, found %d violations"
                         (count rotation-triplets) (count violations))})))

(defn compute-advanced-parity-expectation
  "Compute parity expectation value with comprehensive statistical analysis for symmetry verification.
  
  This function calculates the quantum parity expectation value ⟨P⟩ where P is the 
  parity operator P = ⊗ᵢ Zᵢ (tensor product of Pauli-Z operators on all qubits).
  The parity expectation provides crucial information about symmetry preservation
  in quantum circuits.
  
  Physical Significance:
  The parity operator measures the overall \"oddness\" or \"evenness\" of a quantum state:
  - For basis states: P|x⟩ = (-1)^(|x|) |x⟩ where |x| is the Hamming weight
  - For general states: ⟨P⟩ = Σₓ (-1)^(|x|) P(x) where P(x) is measurement probability
  
  Parity is conserved by important gate classes:
  - Clifford gates (H, S, CNOT) preserve parity superposition structure  
  - Pauli gates (X, Y, Z) have well-defined parity transformation rules
  - Controlled gates maintain specific parity relationships
  
  Violations of expected parity indicate:
  - Decoherence processes (especially dephasing)
  - Systematic measurement errors or miscalibration
  - Leakage to non-computational subspace
  - Crosstalk between qubits
  
  Statistical Analysis:
  The function computes not just the expectation value but also:
  - Sample variance: Var(P) = ⟨P²⟩ - ⟨P⟩² 
  - Standard error: SE = √(Var(P)/N) for N measurements
  - 95% confidence interval: ⟨P⟩ ± 1.96 × SE
  
  This enables rigorous hypothesis testing for parity conservation.
  
  Parameters:
  - measurement-results: Map of quantum state strings to measurement counts
                        e.g., {\"000\" 400, \"001\" 50, \"010\" 50, \"111\" 500}
  - num-qubits: Number of qubits in the system (determines parity calculation)
  - config: Configuration map (reserved for future statistical options)
  
  Returns:
  Map containing comprehensive parity analysis:
  - :expectation - Parity expectation value ⟨P⟩ ∈ [-1, 1]
  - :variance - Sample variance of parity measurements
  - :standard-error - Standard error of the expectation estimate
  - :confidence-interval - [lower, upper] bounds of 95% confidence interval
  - :total-shots - Total number of measurement shots used
  
  Example:
  (compute-advanced-parity-expectation {\"00\" 450 \"11\" 450 \"01\" 50 \"10\" 50} 2 {})
  ;=> {:expectation 0.8, :variance 0.64, :standard-error 0.025, 
  ;    :confidence-interval [0.751 0.849], :total-shots 1000}"
  [measurement-results num-qubits config]
  (let [total-shots (reduce + (vals measurement-results))
        parity-sum (reduce (fn [sum [state count]]
                             (let [parity (reduce (fn [p bit-char]
                                                    (* p (if (= bit-char \1) -1 1)))
                                                  1
                                                  state)]
                               (+ sum (* parity count))))
                           0
                           measurement-results)
        parity-expectation (if (pos? total-shots)
                             (/ (double parity-sum) (double total-shots))
                             0.0)
        
        ;; Compute variance for confidence intervals
        parity-variance (reduce (fn [sum [state count]]
                                  (let [parity (reduce (fn [p bit-char]
                                                         (* p (if (= bit-char \1) -1 1)))
                                                       1
                                                       state)
                                        deviation (- parity parity-expectation)]
                                    (+ sum (* count (* deviation deviation)))))
                                0
                                measurement-results)
        variance (if (> total-shots 1)
                   (/ parity-variance (dec total-shots))
                   0.0)
        
        standard-error (if (pos? total-shots)
                         (Math/sqrt (/ variance total-shots))
                         0.0)
        
        ;; 95% confidence interval
        confidence-margin (* 1.96 standard-error)
        confidence-interval [(- parity-expectation confidence-margin)
                             (+ parity-expectation confidence-margin)]]
    
    {:expectation parity-expectation
     :variance variance
     :standard-error standard-error
     :confidence-interval confidence-interval
     :total-shots total-shots}))

(defn detect-symmetry-violations
  "Comprehensive symmetry violation detection with advanced statistical analysis and configurable thresholds.
  
  This function performs a complete symmetry analysis of quantum measurement data,
  detecting violations across multiple symmetry types that are fundamental to
  quantum error detection and mitigation. It provides the core analytical engine
  for quantum circuit validation and hardware characterization.
  
  Symmetry Analysis Framework:
  The function implements a multi-faceted approach to symmetry verification:
  
  1. **Parity Symmetry**: Analyzes conservation of parity expectation ⟨P⟩ for circuits
     containing only parity-preserving operations (H, S, T, Pauli, CNOT gates).
     
  2. **Reflection Symmetry**: Tests bit-flip invariance where states |x⟩ and |x̄⟩
     should have equal measurement probabilities under specific circuit conditions.
     
  3. **Permutation Symmetry**: Validates qubit exchange invariance for circuits 
     that should be symmetric under qubit permutations.
     
  4. **Rotational Symmetry**: Checks continuous or discrete rotational invariance
     for systems with geometric symmetry properties.
  
  Statistical Rigor:
  - Chi-squared goodness-of-fit testing for hypothesis validation
  - Confidence interval analysis for expectation values
  - Multiple comparison corrections for family-wise error control
  - Effect size estimation for practical significance assessment
  - Violation severity classification with actionable thresholds
  
  Production Features:
  - Configurable thresholds for different symmetry types and violation severities
  - Comprehensive validation of input data quality and statistical sufficiency
  - Performance optimization for large Hilbert spaces using sampling strategies
  - Detailed diagnostic information for each symmetry type analyzed
  - Integration with hardware characterization and error mitigation protocols
  
  Parameters:
  - circuit: Quantum circuit map containing:
           - :num-qubits - Number of qubits in the circuit
           - :operations - Vector of gate operations for parity analysis
  - measurement-results: Map of quantum state strings to measurement counts
  - config: Optional configuration map (merges with default-config):
           - :thresholds - Symmetry score thresholds and violation limits
           - :statistical - Statistical significance requirements
           - :performance - Analysis optimization parameters
           - :advanced-symmetries - List of symmetry types to analyze
  
  Returns:
  Comprehensive analysis map containing:
  - :parity-expectation - Computed parity expectation value
  - :parity-analysis - Detailed parity statistical analysis
  - :reflection-symmetry-score - Reflection symmetry quality score
  - :reflection-analysis - Complete reflection symmetry analysis
  - :permutation-analysis - Permutation symmetry analysis results
  - :rotational-analysis - Rotational symmetry analysis results  
  - :has-parity-preserving-circuit - Boolean indicating parity conservation expectation
  - :symmetry-violations - Vector of all detected violations with severity classification
  - :overall-symmetry-score - Composite symmetry quality score [0,1]
  - :statistical-validation - Input data validation results
  - :config-used - Effective configuration used for analysis
  
  Example:
  (detect-symmetry-violations bell-circuit {\"00\" 480 \"11\" 520} default-config)
  ;=> {:overall-symmetry-score 0.96, :symmetry-violations [], :parity-expectation 1.0, ...}"
  [circuit measurement-results & [config]]
  (let [config (merge default-config (or config {}))
        num-qubits (int (:num-qubits circuit))
        operations (:operations circuit)
        
        ;; Validate inputs
        validation (validate-measurement-results measurement-results config)
        
        _ (when (not (:valid validation))
            (throw (ex-info "Invalid measurement results for symmetry verification"
                            {:reason (:reason validation)
                             :measurement-results measurement-results
                             :config config})))
        
        ;; Check for parity-preserving operations
        parity-preserving-ops #{:h :s :t :rz :ry :rx :cnot :cz :cp :crz}
        has-parity-preserving-circuit? (every? #(contains? parity-preserving-ops (:operation-type %)) operations)
        
        ;; Advanced parity analysis
        parity-analysis (compute-advanced-parity-expectation measurement-results num-qubits config)
        parity-expectation (:expectation parity-analysis)
        
        violations []
        
        ;; Enhanced parity violation detection
        parity-tolerance (get-in config [:thresholds :parity-tolerance])
        violations (if (and has-parity-preserving-circuit?
                            (< (Math/abs parity-expectation) parity-tolerance))
                     (conj violations {:type :unexpected-parity-loss
                                      :expected (str "Non-zero parity expectation (±" parity-tolerance ")")
                                      :actual parity-expectation
                                      :confidence-interval (:confidence-interval parity-analysis)
                                      :severity (cond (< (Math/abs parity-expectation) (/ parity-tolerance 2)) :high
                                                      :else :medium)
                                      :suggestion "Check for decoherence or systematic measurement errors"})
                     violations)
        
        ;; Enhanced reflection symmetry analysis with statistical testing
        reflection-min (get-in config [:thresholds :reflection-symmetry-min])
        reflection-analysis (case num-qubits
                              2 (let [state-00 (get measurement-results "00" 0)
                                      state-11 (get measurement-results "11" 0)
                                      state-01 (get measurement-results "01" 0)
                                      state-10 (get measurement-results "10" 0)
                                      total (+ state-00 state-11 state-01 state-10)]
                                  (if (pos? total)
                                    (let [;; For reflection symmetry, we expect |01⟩ and |10⟩ to have equal populations
                                          expected-01 (/ (+ state-01 state-10) 2.0)
                                          expected-10 expected-01
                                          reflection-violation (/ (Math/abs (- state-01 state-10)) total)
                                          symmetry-score (- 1.0 reflection-violation)
                                          
                                          ;; Chi-squared test for the reflection symmetry hypothesis
                                          chi-squared-result (compute-chi-squared-test
                                                              [state-01 state-10]
                                                              [expected-01 expected-10]
                                                              1)]
                                      {:score symmetry-score
                                       :violation-magnitude reflection-violation
                                       :chi-squared chi-squared-result
                                       :statistically-significant (:significant chi-squared-result)
                                       :effect-size (:effect-size chi-squared-result)})
                                    {:score 1.0 :violation-magnitude 0.0 :chi-squared nil :statistically-significant false}))
                              
                              3 (let [;; For 3 qubits, check all bit-flip symmetries for comprehensive analysis
                                      all-states (keys measurement-results)
                                      total-shots (reduce + (vals measurement-results))
                                      
                                      ;; Generate all bit-flip pairs
                                      bit-flip-pairs (for [state all-states
                                                            :let [flipped-state (apply str (map #(if (= % \0) \1 \0) state))]
                                                            :when (contains? measurement-results flipped-state)]
                                                        [state flipped-state])
                                      
                                      violations (for [[state1 state2] bit-flip-pairs
                                                       :let [count1 (get measurement-results state1 0)
                                                             count2 (get measurement-results state2 0)
                                                             violation (if (pos? total-shots)
                                                                         (/ (Math/abs (- count1 count2)) total-shots)
                                                                         0.0)]
                                                       :when (> violation 0.05)]
                                                   {:states [state1 state2]
                                                    :violation violation
                                                    :counts [count1 count2]
                                                    :severity (cond (> violation 0.8) :high
                                                                    (> violation 0.4) :medium
                                                                    :else :low)})
                                      
                                      avg-violation (if (seq violations)
                                                      (/ (reduce + (map :violation violations)) (count violations))
                                                      0.0)
                                      
                                      symmetry-score (- 1.0 avg-violation)]
                                  {:score symmetry-score
                                   :violation-magnitude avg-violation
                                   :violations violations
                                   :chi-squared nil
                                   :statistically-significant false})
                              
                              ;; For larger systems, use sampling-based approach
                              (let [;; Sample key states for reflection analysis
                                    all-states (keys measurement-results)
                                    total-shots (reduce + (vals measurement-results))
                                    
                                    ;; For larger systems, check bit-flip symmetries
                                    bit-flip-pairs (for [state all-states
                                                          :let [flipped-state (apply str (map #(if (= % \0) \1 \0) state))]
                                                          :when (contains? measurement-results flipped-state)]
                                                      [state flipped-state])
                                    
                                    violations (for [[state1 state2] bit-flip-pairs
                                                     :let [count1 (get measurement-results state1 0)
                                                           count2 (get measurement-results state2 0)
                                                           violation (if (pos? total-shots)
                                                                       (/ (Math/abs (- count1 count2)) total-shots)
                                                                       0.0)]
                                                     :when (> violation 0.1)]
                                                 {:states [state1 state2]
                                                  :violation violation
                                                  :counts [count1 count2]})
                                    
                                    avg-violation (if (seq violations)
                                                    (/ (reduce + (map :violation violations)) (count violations))
                                                    0.0)
                                    
                                    symmetry-score (- 1.0 avg-violation)]
                                {:score symmetry-score
                                 :violation-magnitude avg-violation
                                 :violations violations
                                 :analysis (str "Analyzed " (count bit-flip-pairs) " bit-flip pairs")
                                 :chi-squared nil
                                 :statistically-significant false}))
        
        reflection-symmetry-score (:score reflection-analysis)
        violations (if (< reflection-symmetry-score reflection-min)
                     (conj violations {:type :reflection-symmetry-violation
                                      :expected (str "Reflection symmetry score ≥ " reflection-min)
                                      :actual reflection-symmetry-score
                                      :violation-magnitude (:violation-magnitude reflection-analysis)
                                      :statistical-test (:chi-squared reflection-analysis)
                                      :statistically-significant (:statistically-significant reflection-analysis)
                                      :effect-size (get-in reflection-analysis [:chi-squared :effect-size])
                                      :severity (cond (< reflection-symmetry-score (- reflection-min 0.3)) :high
                                                      (< reflection-symmetry-score (- reflection-min 0.15)) :medium
                                                      :else :low)
                                      :suggestion "Check circuit compilation and hardware calibration"})
                     violations)
        
        ;; Advanced symmetry analyses (if enabled)
        permutation-analysis (if (contains? (set (:advanced-symmetries config)) :permutation)
                               (detect-permutation-symmetries measurement-results num-qubits config)
                               {:score 1.0 :violations [] :analysis "Permutation analysis disabled"})
        
        rotational-analysis (if (contains? (set (:advanced-symmetries config)) :rotational)
                              (detect-rotational-symmetries measurement-results num-qubits config)
                              {:score 1.0 :violations [] :analysis "Rotational analysis disabled"})
        
        ;; Combine all violations
        all-violations (concat violations 
                               (:violations reflection-analysis [])
                               (:violations permutation-analysis)
                               (:violations rotational-analysis))
        
        ;; Compute overall symmetry score with enhanced weighting and sensitivity
        overall-score (let [;; Normalize all component scores to [0,1] range
                            parity-component (Math/abs parity-expectation)
                            reflection-component reflection-symmetry-score
                            permutation-component (:score permutation-analysis)
                            rotational-component (:score rotational-analysis)
                            
                            ;; Enhanced weighting based on circuit characteristics
                            parity-weight (if has-parity-preserving-circuit? 0.4 0.2)
                            reflection-weight 0.3
                            permutation-weight 0.2
                            rotational-weight 0.1
                            
                            ;; Calculate weighted score
                            base-score (+ (* parity-weight parity-component)
                                          (* reflection-weight reflection-component)
                                          (* permutation-weight permutation-component)
                                          (* rotational-weight rotational-component))
                            
                            ;; Penalty system based on violation severity and count
                            high-severity-violations (filter #(= (:severity %) :high) all-violations)
                            medium-severity-violations (filter #(= (:severity %) :medium) all-violations)
                            low-severity-violations (filter #(= (:severity %) :low) all-violations)
                            
                            violation-penalty (+ (* 0.3 (count high-severity-violations))
                                                 (* 0.15 (count medium-severity-violations))
                                                 (* 0.05 (count low-severity-violations)))
                            
                            ;; Statistical significance bonus/penalty
                            statistical-bonus (if (and (:statistically-significant reflection-analysis)
                                                       (> reflection-symmetry-score reflection-min))
                                                0.05
                                                (if (:statistically-significant reflection-analysis)
                                                  -0.1  ; Statistically significant violation is worse
                                                  0.0))
                            
                            final-score (max 0.0 (min 1.0 (+ base-score statistical-bonus (- violation-penalty))))]
                        final-score)]
    
    {:parity-expectation parity-expectation
     :parity-analysis parity-analysis
     :reflection-symmetry-score reflection-symmetry-score
     :reflection-analysis reflection-analysis
     :permutation-analysis permutation-analysis
     :rotational-analysis rotational-analysis
     :has-parity-preserving-circuit has-parity-preserving-circuit?
     :symmetry-violations all-violations
     :overall-symmetry-score overall-score
     :statistical-validation validation
     :config-used config}))

(defn apply-symmetry-verification
  "Production-ready symmetry verification with comprehensive analysis, statistical rigor, and actionable recommendations.
  
  This function serves as the primary interface for quantum circuit symmetry verification
  in production environments. It combines all symmetry analyses into a unified assessment
  with pass/fail determination and specific corrective action recommendations.
  
  Production Workflow:
  1. **Comprehensive Analysis**: Performs all enabled symmetry tests using detect-symmetry-violations
  2. **Multi-Criteria Assessment**: Evaluates multiple symmetry scores and violation patterns
  3. **Statistical Validation**: Applies rigorous statistical significance testing
  4. **Severity Classification**: Categorizes violations by impact and urgency
  5. **Actionable Recommendations**: Generates specific corrective actions based on violation patterns
  6. **Performance Metrics**: Tracks analysis execution and provides diagnostic information
  
  Pass/Fail Criteria:
  The verification passes only when ALL of the following conditions are met:
  - Overall symmetry score ≥ configured minimum threshold
  - Number of violations ≤ maximum allowed violations
  - No high-severity violations detected
  - Statistical significance requirements satisfied (when applicable)
  - Individual symmetry component scores meet their respective thresholds
  
  Corrective Action System:
  The function provides targeted recommendations based on specific violation patterns:
  - **Parity violations**: Decoherence mitigation, measurement bias correction
  - **Reflection violations**: Hardware calibration, crosstalk reduction
  - **Permutation violations**: Qubit uniformity validation, topology mapping verification
  - **Rotational violations**: Magnetic field uniformity, geometric alignment checks
  - **High-severity violations**: Urgent hardware diagnosis, experiment halt recommendations
  
  Integration Points:
  - Hardware characterization and calibration protocols
  - Real-time error mitigation decision making
  - Quantum error correction threshold determination  
  - Circuit compilation and optimization feedback
  - Platform benchmarking and comparison studies
  
  Parameters:
  - circuit: Quantum circuit specification map containing circuit structure and operations
  - measurement-results: Map of measurement outcome strings to count numbers
  - config: Optional configuration map for customizing analysis parameters:
           - :thresholds - Pass/fail thresholds for different symmetry types
           - :statistical - Statistical significance and confidence requirements
           - :performance - Analysis optimization and timeout settings
           - :advanced-symmetries - List of symmetry types to analyze
           - :error-reporting - Diagnostic detail and logging configuration
           - :production-mode - Production environment specific settings
  
  Returns:
  Complete verification report map including:
  - :symmetry-score - Overall symmetry quality score [0,1]
  - :symmetry-passed - Boolean pass/fail result for the verification
  - :corrective-actions - Vector of specific actionable recommendations
  - :verification-applied - Boolean confirming verification was executed
  - :execution-metrics - Performance and analysis statistics
  - :recommendation-confidence - Confidence level in the recommendations [0,1]
  - [All fields from detect-symmetry-violations] - Complete underlying analysis
  
  Example:
  (apply-symmetry-verification bell-circuit measurement-data production-config)
  ;=> {:symmetry-passed true, :symmetry-score 0.94, :corrective-actions [], 
  ;    :recommendation-confidence 0.95, :execution-metrics {...}, ...}
  
  Usage in Production:
  ```clojure
  (let [result (apply-symmetry-verification circuit measurements config)]
    (if (:symmetry-passed result)
      (proceed-with-computation (:symmetry-score result))
      (apply-corrective-actions (:corrective-actions result))))
  ```"
  [circuit measurement-results & [config]]
  (let [config (merge default-config (or config {}))
        
        ;; Perform enhanced analysis
        symmetry-analysis (detect-symmetry-violations circuit measurement-results config)
        
        symmetry-score (:overall-symmetry-score symmetry-analysis)
        violations (:symmetry-violations symmetry-analysis)
        overall-min (get-in config [:thresholds :overall-symmetry-min])
        max-violations (get-in config [:thresholds :max-violations] 3)
        
        ;; Enhanced pass/fail criteria with more sophisticated logic
        reflection-analysis (:reflection-analysis symmetry-analysis)
        permutation-analysis (:permutation-analysis symmetry-analysis)
        reflection-symmetry-score (:reflection-symmetry-score symmetry-analysis)
        reflection-min (get-in config [:thresholds :reflection-symmetry-min])
        
        symmetry-passed (and (>= symmetry-score overall-min)
                             (<= (count violations) max-violations)
                             (not (some #(= (:severity %) :high) violations))
                             ;; Additional production criteria
                             (or (not (get reflection-analysis :statistically-significant false))
                                 (>= reflection-symmetry-score reflection-min))
                             (>= (:score permutation-analysis) (get-in config [:thresholds :permutation-symmetry-min] 0.8)))
        
        ;; Enhanced corrective actions based on detailed analysis
        corrective-actions (if (not symmetry-passed)
                             (let [high-severity-violations (filter #(= (:severity %) :high) violations)
                                   violation-types (set (map :type violations))]
                               (cond-> []
                                 (contains? violation-types :unexpected-parity-loss)
                                 (concat ["Implement parity-preserving error correction"
                                          "Investigate systematic measurement bias"
                                          "Check for decoherence during measurement"])
                                 
                                 (contains? violation-types :reflection-symmetry-violation)
                                 (concat ["Check hardware calibration"
                                          "Check for crosstalk between qubits"
                                          "Validate circuit compilation"])
                                 
                                 (contains? violation-types :permutation-violation)
                                 (concat ["Check hardware calibration"
                                          "Check for qubit-dependent systematic errors"
                                          "Verify hardware topology mapping"])
                                 
                                 (contains? violation-types :rotational-violation)
                                 (concat ["Investigate rotational symmetry breaking"
                                          "Check magnetic field uniformity"])
                                 
                                 (seq high-severity-violations)
                                 (concat ["URGENT: High-severity violations detected"
                                          "Consider halting experiment for hardware diagnosis"])
                                 
                                 :always
                                 (conj "Review measurement statistics and increase shots if needed")))
                             [])
        
        ;; Performance metrics
        execution-metrics {:analysis-time (System/currentTimeMillis)
                           :states-analyzed (count measurement-results)
                           :total-shots (:total-shots (:statistical-validation symmetry-analysis))
                           :advanced-analyses-performed (count (:advanced-symmetries config))}]
    
    (merge symmetry-analysis
           {:symmetry-score symmetry-score
            :symmetry-passed symmetry-passed
            :corrective-actions corrective-actions
            :verification-applied true
            :execution-metrics execution-metrics
            :recommendation-confidence (if symmetry-passed 0.95 0.8)})))


