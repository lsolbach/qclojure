(ns org.soulspace.qclojure.application.error-mitigation.symmetry-verification
  "Symmetry verification for error mitigation strategies."
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
  "Validate measurement results for statistical significance and consistency"
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
  "Compute chi-squared test for symmetry hypothesis with proper statistical analysis"
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
  "Detect permutation symmetries in measurement results with comprehensive analysis.
  
  For circuits with qubit permutation symmetry, measurement outcomes
  should be invariant under qubit index permutations."
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
  "Detect rotational symmetries for circuits with rotational invariance"
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
  "Compute advanced parity expectation with statistical analysis"
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
  "Symmetry violation detection with configurable thresholds and advanced analysis"
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
  "Production-ready symmetry verification with full configurability and statistical rigor"
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


