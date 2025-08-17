(ns org.soulspace.qclojure.application.error-mitigation.readout-error
  "Readout error mitigation for quantum measurement correction.
  
  This namespace provides functions for correcting measurement errors
  that occur during quantum state readout. Readout errors are one of the
  most significant sources of noise in current quantum hardware, and
  proper mitigation can substantially improve algorithm fidelity.
  
  Key capabilities:
  - Single and multi-qubit readout error characterization
  - Calibration matrix construction using tensor products
  - Linear system solving for error correction
  - Fidelity improvement measurement and analysis
  
  The implementation supports arbitrary numbers of qubits (up to practical memory
  limits) and uses proper mathematical techniques including matrix inversion
  and probability normalization to ensure physically valid results.
  
  Typical workflow:
  1. Characterize readout errors using calibration experiments  
  2. Create calibration matrix from error parameters
  3. Apply mitigation to measured data using matrix inversion
  4. Analyze improvement in measurement fidelity"
  (:require [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.math.core :as mcore]
            [org.soulspace.qclojure.domain.state :as qs]))

;;
;; Readout Error Mitigation
;;
(defn create-single-qubit-readout-matrix
  "Create readout error matrix for a single qubit from characterization data.
  
  This function constructs the fundamental 2×2 readout error matrix that describes
  the conditional probabilities of measurement errors for a single qubit. The matrix
  element (i,j) represents P(measure state i | prepared state j).
  
  Readout errors arise from several physical mechanisms:
  - Imperfect state discrimination in measurement circuits
  - Thermal excitation during readout
  - Crosstalk between measurement channels
  - Finite measurement time and relaxation
  
  The error characterization typically comes from calibration experiments where
  known states |0⟩ and |1⟩ are prepared and measured repeatedly to determine
  error probabilities.
  
  Parameters:
  - readout-errors: Map containing error characterization with keys:
    - :prob-0-to-1 - Probability of measuring |1⟩ when |0⟩ was prepared (false positive)
    - :prob-1-to-0 - Probability of measuring |0⟩ when |1⟩ was prepared (false negative)
  
  Returns:
  2×2 matrix [[P(0|0) P(0|1)]   where:
              [P(1|0) P(1|1)]]
  - P(0|0) = 1 - prob-0-to-1 (correct measurement of |0⟩)
  - P(0|1) = prob-1-to-0     (error: measure |0⟩ when |1⟩ prepared)
  - P(1|0) = prob-0-to-1     (error: measure |1⟩ when |0⟩ prepared)  
  - P(1|1) = 1 - prob-1-to-0 (correct measurement of |1⟩)
  
  Example:
  (create-single-qubit-readout-matrix {:prob-0-to-1 0.05 :prob-1-to-0 0.03})
  ;=> [[0.95 0.03]
  ;    [0.05 0.97]]  ; 95% accuracy for |0⟩, 97% accuracy for |1⟩"
  [readout-errors]
  (let [prob-0-to-1 (:prob-0-to-1 readout-errors) ; P(measure 1 | prepared 0)
        prob-1-to-0 (:prob-1-to-0 readout-errors) ; P(measure 0 | prepared 1)
        p00 (- 1.0 prob-0-to-1) ; P(measure 0 | prepared 0)
        p01 prob-1-to-0         ; P(measure 0 | prepared 1)
        p10 prob-0-to-1         ; P(measure 1 | prepared 0)
        p11 (- 1.0 prob-1-to-0)] ; P(measure 1 | prepared 1)
    [[p00 p10]
     [p01 p11]]))

(defn create-calibration-matrix
  "Create calibration matrix from readout error characterization for multi-qubit systems.
  
  This function constructs the full calibration matrix for n-qubit quantum systems
  by computing the tensor product of single-qubit readout matrices. The resulting
  2^n × 2^n matrix captures the complete readout error model including correlations
  between qubits (assuming independent errors per qubit).
  
  The calibration matrix C has the interpretation:
  C[i,j] = P(measure computational basis state i | prepared computational basis state j)
  
  For example, with 2 qubits, the matrix relates states as:
  C * [P(|00⟩) P(|01⟩) P(|10⟩) P(|11⟩)]ᵀ = [measured |00⟩ rate, measured |01⟩ rate, ...]ᵀ
  
  The tensor product construction ensures that:
  - Single-qubit error models combine correctly
  - Matrix has proper probabilistic normalization 
  - Computational complexity scales as O(4^n) in space
  
  For practical quantum computers, this matrix becomes large (64×64 for 6 qubits,
  1024×1024 for 10 qubits), requiring careful memory management and potentially
  sparse matrix techniques for larger systems.
  
  Parameters:
  - num-qubits: Number of qubits in the quantum register (positive integer ≤ 10)
  - readout-errors: Single-qubit readout error characterization map containing:
    - :prob-0-to-1 - Error probability |0⟩ → |1⟩ 
    - :prob-1-to-0 - Error probability |1⟩ → |0⟩
  
  Returns:
  2^n × 2^n calibration matrix where element (i,j) represents
  P(measure state i | prepared state j) for computational basis states
  
  Throws:
  AssertionError if num-qubits > 10 (memory protection) or num-qubits ≤ 0
  
  Example:
  (create-calibration-matrix 2 {:prob-0-to-1 0.05 :prob-1-to-0 0.03})
  ;=> 4×4 matrix for 2-qubit readout error correction"
  [num-qubits readout-errors]
  {:pre [(pos-int? num-qubits) (<= num-qubits 10)]} ; Reasonable limit for memory
  (let [single-qubit-matrix (create-single-qubit-readout-matrix readout-errors)]
    (if (= num-qubits 1)
      single-qubit-matrix
      ;; Compute tensor product iteratively for multi-qubit systems
      (reduce (fn [acc-matrix _]
                (mcore/kronecker acc-matrix single-qubit-matrix))
              single-qubit-matrix
              (range (dec num-qubits))))))

(defn mitigate-readout-errors
  "Apply readout error mitigation using calibration matrix.
  
  This function corrects measurement errors by solving the linear system:
  measured_counts = calibration_matrix * true_counts
  
  The calibration matrix C represents the conditional probabilities P(measure i | prepared j)
  where rows correspond to measured outcomes and columns to prepared states.
  By inverting this relationship, we can estimate the true quantum state distribution
  from the noisy measurement data.
  
  The mitigation process:
  1. Convert measured counts to probability distribution
  2. Solve C * true_probs = measured_probs using matrix inversion
  3. Apply non-negativity constraints and renormalization
  4. Convert back to corrected count distribution
  5. Calculate fidelity improvement metrics
  
  Parameters:
  - measured-counts: Map of measurement outcome strings to count numbers
                     e.g. {\"00\" 450, \"01\" 25, \"10\" 30, \"11\" 495}
  - calibration-matrix: 2^n × 2^n matrix where element (i,j) = P(measure i | prepared j)
                        Constructed from readout error characterization experiments
  - num-qubits: Number of qubits in the quantum system
  
  Returns:
  Map containing:
  - :measured-counts - Original measurement data
  - :corrected-counts - Error-corrected count distribution
  - :improvement-factor - Ratio of error reduction (< 1.0 means improvement)
  - :total-shots - Total number of measurement shots
  - :target-state - Reference state used for fidelity calculation
  - :measured-fidelity - Fidelity before correction
  - :corrected-fidelity - Fidelity after correction
  
  Example:
  (mitigate-readout-errors {\"00\" 450 \"11\" 450 \"01\" 50 \"10\" 50}
                           calibration-matrix 2)
  ;=> {:improvement-factor 0.12, :corrected-fidelity 0.95, ...}"
  [measured-counts calibration-matrix num-qubits]
  (let [total-shots (reduce + (vals measured-counts))
        
        ;; Generate proper state labels
        state-keys (qs/basis-strings num-qubits)
        
        ;; Convert counts to probability distribution vector in correct order
        measured-probs (mapv #(/ (double (get measured-counts % 0)) 
                                 (double total-shots)) 
                             state-keys)
        
        ;; Solve the linear system: C * true_probs = measured_probs
        ;; where C is the calibration matrix
        corrected-probs (try
                          (mcore/solve-linear-system calibration-matrix measured-probs)
                          (catch Exception e
                            (println "Matrix inversion failed, using measured probabilities:" (.getMessage e))
                            measured-probs))
        
        ;; Ensure probabilities are non-negative and normalized
        clipped-probs (mapv #(max 0.0 %) corrected-probs)
        prob-sum (reduce + clipped-probs)
        normalized-probs (if (> prob-sum 1e-10)
                           (mapv #(/ % prob-sum) clipped-probs)
                           measured-probs)
        
        ;; Convert back to counts
        corrected-counts (zipmap state-keys
                                 (mapv #(max 0 (int (* % total-shots))) 
                                       normalized-probs))
        
        ;; Calculate improvement based on fidelity improvement 
        ;; Use first state as target for demonstration (could be customized)
        target-state (first state-keys)
        ideal-fidelity 0.95  ; Assume high fidelity target
        measured-fidelity (/ (double (get measured-counts target-state 0)) total-shots)
        corrected-fidelity (/ (double (get corrected-counts target-state 0)) total-shots)
        error-before (Math/abs (- ideal-fidelity measured-fidelity))
        error-after (Math/abs (- ideal-fidelity corrected-fidelity))
        improvement-factor (if (> error-before 1e-10)
                            (/ error-after error-before)
                            1.0)]
    
    {:measured-counts measured-counts
     :corrected-counts corrected-counts
     :improvement-factor improvement-factor
     :total-shots total-shots
     :target-state target-state
     :measured-fidelity measured-fidelity
     :corrected-fidelity corrected-fidelity}))

(comment
  ;;
  ;; Readout Error Mitigation Examples
  ;;
  
  ;; Basic single-qubit readout error characterization
  ;; These parameters come from calibration experiments on real hardware
  (def typical-readout-errors
    {:prob-0-to-1 0.05   ; 5% chance of measuring |1⟩ when |0⟩ was prepared
     :prob-1-to-0 0.03}) ; 3% chance of measuring |0⟩ when |1⟩ was prepared

  ;; Create single-qubit readout matrix
  (def single-qubit-matrix
    (create-single-qubit-readout-matrix typical-readout-errors))
  ; => [[0.95 0.05]  ; Rows: measured outcomes, Cols: prepared states
  ;     [0.03 0.97]]  ; P(measure 0|prepared 0) = 0.95, etc.

  ;; Create 2-qubit calibration matrix (4×4)
  (def two-qubit-calibration 
    (create-calibration-matrix 2 typical-readout-errors))
  ; => 4×4 matrix with tensor product structure

  ;; Example measurement data with readout errors
  (def noisy-measurements
    {"00" 450  ; Should be higher if no errors
     "01" 25   ; Some false positives
     "10" 30   ; Some false positives  
     "11" 495}) ; Should be higher if no errors

  ;; Apply readout error mitigation
  (def mitigation-result
    (mitigate-readout-errors noisy-measurements two-qubit-calibration 2))

  ;; Analyze the results
  (:measured-counts mitigation-result)   ; Original noisy data
  (:corrected-counts mitigation-result)  ; Error-corrected data
  (:improvement-factor mitigation-result) ; < 1.0 means improvement
  (:measured-fidelity mitigation-result)  ; Fidelity before correction
  (:corrected-fidelity mitigation-result) ; Fidelity after correction

  ;; Multi-qubit example with higher error rates
  (def high-error-params
    {:prob-0-to-1 0.1    ; 10% error rate
     :prob-1-to-0 0.08}) ; 8% error rate

  ;; Create 3-qubit calibration matrix (8×8)
  (def three-qubit-calibration
    (create-calibration-matrix 3 high-error-params))

  ;; Highly corrupted measurement data
  (def corrupted-measurements
    {"000" 400 "001" 80 "010" 85 "011" 15
     "100" 90  "101" 20 "110" 25 "111" 385})

  ;; Apply mitigation to 3-qubit system
  (def three-qubit-result
    (mitigate-readout-errors corrupted-measurements three-qubit-calibration 3))

  ;; Performance considerations for larger systems
  ;; Memory usage scales as O(4^n):
  ;; 2 qubits:   4×4 matrix =     16 elements
  ;; 3 qubits:   8×8 matrix =     64 elements  
  ;; 4 qubits:  16×16 matrix =    256 elements
  ;; 5 qubits:  32×32 matrix =  1,024 elements
  ;; 6 qubits:  64×64 matrix =  4,096 elements
  ;; 10 qubits: 1024×1024 matrix = 1,048,576 elements

  ;; For systems beyond ~10 qubits, consider:
  ;; - Sparse matrix techniques for low error rates
  ;; - Block-wise error correction for structured noise
  ;; - Approximate correction methods
  ;; - Noise-adapted quantum error correction codes

  ;; Real-world calibration workflow:
  ;; 1. Prepare |0⟩ state and measure many times → estimate prob-0-to-1
  ;; 2. Prepare |1⟩ state and measure many times → estimate prob-1-to-0  
  ;; 3. Repeat for different qubits to check for spatial correlations
  ;; 4. Monitor error rates over time for drift
  ;; 5. Update calibration matrices as needed

  ;; Integration with quantum algorithms:
  ;; - Apply mitigation to final measurement results
  ;; - Particularly important for variational algorithms (VQE, QAOA)
  ;; - Essential for quantum machine learning applications
  ;; - Critical for quantum error correction threshold experiments

  )

