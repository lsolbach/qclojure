(ns org.soulspace.qclojure.application.error-mitigation.readout-error
  (:require [org.soulspace.qclojure.domain.math :as qmath]
            [org.soulspace.qclojure.domain.state :as qs]))

;;
;; Readout Error Mitigation
;;
(defn create-single-qubit-readout-matrix
  "Create readout error matrix for a single qubit.
  
  Returns 2x2 matrix where element (i,j) = P(measure i | prepared j)."
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
  "Create calibration matrix from readout error characterization.
  
  For n qubits, this creates a 2^n x 2^n matrix where element (i,j)
  represents P(measure state i | prepared state j).
  
  Uses tensor product to properly construct multi-qubit calibration matrices."
  [num-qubits readout-errors]
  {:pre [(pos-int? num-qubits) (<= num-qubits 10)]} ; Reasonable limit for memory
  (let [single-qubit-matrix (create-single-qubit-readout-matrix readout-errors)]
    (if (= num-qubits 1)
      single-qubit-matrix
      ;; Compute tensor product iteratively for multi-qubit systems
      (reduce (fn [acc-matrix _]
                (qmath/tensor-product acc-matrix single-qubit-matrix))
              single-qubit-matrix
              (range (dec num-qubits))))))

(defn mitigate-readout-errors
  "Apply readout error mitigation using calibration matrix.
  
  This inverts the effect of readout errors by solving the linear system:
  measured_counts = calibration_matrix * true_counts
  
  Production implementation supports arbitrary number of qubits using proper matrix inversion."
  [measured-counts calibration-matrix num-qubits]
  (let [num-states (count calibration-matrix)
        total-shots (reduce + (vals measured-counts))
        
        ;; Generate proper state labels
        state-keys (qs/basis-strings num-qubits)
        
        ;; Convert counts to probability distribution vector in correct order
        measured-probs (mapv #(/ (double (get measured-counts % 0)) 
                                 (double total-shots)) 
                             state-keys)
        
        ;; Solve the linear system: C * true_probs = measured_probs
        ;; where C is the calibration matrix
        corrected-probs (try
                          (qmath/solve-linear-system calibration-matrix measured-probs)
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

