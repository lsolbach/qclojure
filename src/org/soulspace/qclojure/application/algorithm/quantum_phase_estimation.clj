(ns org.soulspace.qclojure.application.algorithm.quantum-phase-estimation
  "Quantum Phase Estimation (QPE) algorithm implementation.
  
  The Quantum Phase Estimation algorithm is a fundamental quantum algorithm that estimates
  the eigenvalue of a unitary operator. Given a unitary operator U and one of its 
  eigenstates |ψ⟩ such that U|ψ⟩ = e^(iφ)|ψ⟩, QPE estimates the phase φ.
  
  Algorithm Overview:
  1. Initialize precision qubits in superposition (|+⟩ states)
  2. Prepare eigenstate qubit in a known eigenstate of U
  3. Apply controlled-U^(2^k) operations for k = 0 to n-1
  4. Apply inverse Quantum Fourier Transform to precision qubits
  5. Measure precision qubits to extract phase estimate
  
  The precision of the phase estimate depends on the number of precision qubits used.
  With n precision qubits, the phase can be estimated to within 2π/2^n.
  
  Key Functions:
  - quantum-phase-estimation-circuit: Build QPE circuit
  - quantum-phase-estimation: Execute complete QPE algorithm
  - parse-measurement-to-phase: Convert measurement results to phase estimates
  - analyze-qpe-results: Analyze QPE measurement statistics
  
  Example Usage:
  (def simulator (create-simulator))
  (def result (quantum-phase-estimation simulator (/ Math/PI 4) 3 :plus))
  (:estimated-phase (:result result)) ; => ~0.7854 (π/4)"
  (:require 
            [fastmath.core :as fm]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft]
            [org.soulspace.qclojure.application.backend :as qb]
            [org.soulspace.qclojure.domain.state :as qs]))

(defn quantum-phase-estimation-circuit
  "Build a quantum phase estimation circuit.
   
   Parameters:
   - precision-qubits: Number of qubits for phase precision
   - eigenstate-type: Type of eigenstate preparation (:default, :plus, :one)
   - phase: The phase to estimate (for simulation purposes)
   
   Returns: Quantum circuit implementing QPE"
  [precision-qubits eigenstate-type phase]
  (let [total-qubits (+ precision-qubits 1) ; +1 for the eigenstate qubit
        eigenstate-qubit precision-qubits   ; Last qubit is the eigenstate
        circuit (qc/create-circuit total-qubits "Quantum Phase Estimation")]
    
    (-> circuit
        ;; Step 1: Prepare eigenstate qubit
        (as-> c
          (case eigenstate-type
            :plus (qc/h-gate c eigenstate-qubit)
            :one  (qc/x-gate c eigenstate-qubit)
            :default c)) ; |0⟩ state
        
        ;; Step 2: Initialize precision qubits in superposition
        (as-> c 
          (reduce (fn [circuit qubit]
                    (qc/h-gate circuit qubit))
                  c
                  (range precision-qubits)))
        
        ;; Step 3: Apply controlled-U^(2^k) operations
        ;; Using controlled-RZ as our unitary operation
        (as-> c
          (reduce (fn [circuit k]
                    ;; Apply controlled-U^(2^k) with control qubit k
                    (let [power-phase (* phase (Math/pow 2 k))]
                      (qc/crz-gate circuit k eigenstate-qubit power-phase)))
                  c
                  (range precision-qubits)))
        
        ;; Step 4: Apply inverse QFT to precision qubits
        ;; Apply proper inverse QFT circuit composition
        (as-> c
          (let [iqft-circuit (qft/inverse-quantum-fourier-transform-circuit precision-qubits)]
            ;; Compose the main circuit with the IQFT circuit using control-qubits-only option
            ;; This ensures IQFT is applied only to the first precision-qubits qubits
            (cc/compose-circuits c iqft-circuit {:control-qubits-only true}))))))

(defn quantum-phase-estimation-circuit-with-custom-unitary
  "Create a quantum circuit for phase estimation with specified precision and eigenstate qubits.
   
   This function builds the quantum circuit for phase estimation, including:
   1. Preparing the eigenstate qubits
   2. Initializing precision qubits in superposition
   3. Applying controlled unitary operations
   4. Applying inverse quantum Fourier transform (QFT)
   
   Parameters:
   - precision-qubits: Number of qubits for phase precision
   - eigenstate-qubits: Number of qubits for eigenstate preparation
   
   Returns:
   Quantum circuit ready for execution"
  [precision-qubits eigenstate-qubits eigenstate-prep-fn controlled-unitary-fn]
  {:pre [(pos-int? precision-qubits)
         (pos-int? eigenstate-qubits)
         (fn? eigenstate-prep-fn)
         (fn? controlled-unitary-fn)]}
  (let [total-qubits (+ precision-qubits eigenstate-qubits)
        eigenstate-qubit-range (range precision-qubits (+ precision-qubits eigenstate-qubits))]
    (-> (qc/create-circuit total-qubits "Generalized Quantum Phase Estimation")

        ;; Step 1: Prepare eigenstate qubits
        (eigenstate-prep-fn eigenstate-qubit-range)

        ;; Step 2: Initialize precision qubits in superposition
        (as-> c
              (reduce (fn [circuit qubit]
                        (qc/h-gate circuit qubit))
                      c
                      (range precision-qubits)))

        ;; Step 3: Apply controlled-U^(2^k) operations
        (as-> c
              (reduce (fn [circuit k]
                        (controlled-unitary-fn circuit k (int (Math/pow 2 k)) eigenstate-qubit-range))
                      c
                      (range precision-qubits)))

        ;; Step 4: Apply inverse QFT to precision qubits
        (as-> c
              (let [iqft-circuit (qft/inverse-quantum-fourier-transform-circuit precision-qubits)]
                (cc/compose-circuits c iqft-circuit {:control-qubits-only true}))))))


(defn bitstring-to-phase
  "Convert bitstring to phase estimate.

   Parameters:
   - measurement: Measured index (integer)
   - num-qubits: Number of qubits used
   - precision-qubits: Number of precision qubits

   Returns: Map with binary value and estimated phase (wrapped to [0, 2π))"
  [bits precision-qubits]
  (let [precision-bits (take precision-qubits bits)
        binary-val (qs/bits-to-index precision-bits)
        raw-phase (* 2 Math/PI binary-val (/ 1 (Math/pow 2 precision-qubits)))
        estimated-phase (mod raw-phase (* 2 Math/PI))]
    {:binary-value binary-val
     :estimated-phase estimated-phase}))

(defn index-to-phase
  "Convert measurement index to phase estimate.

   Parameters:
   - measurement: Measured index (integer)
   - num-qubits: Number of qubits used
   - precision-qubits: Number of precision qubits

   Returns: Map with binary value and estimated phase (wrapped to [0, 2π))"
  [index num-qubits precision-qubits]
  (let [bits (qs/index-to-bits index num-qubits)]
    (bitstring-to-phase bits precision-qubits)))

(defn analyze-qpe-results
  "Analyze QPE measurement results to extract phase estimate.
   Parameters:
   - measurements: Map of measurement outcomes to counts (indices)
   - num-qubits: Number of qubits used
   - precision-qubits: Number of precision qubits used
   - actual-phase: Actual phase value (for comparison)
   Returns: Map with analysis results"
  [measurements num-qubits precision-qubits actual-phase]
  (let [phase-estimates (for [[measurement count] measurements]
                          (let [parsed (index-to-phase measurement num-qubits precision-qubits)]
                            (assoc parsed 
                                   :measurement measurement
                                   :count count
                                   :probability (/ count (reduce + (vals measurements))))))
        best-estimate (apply max-key :count phase-estimates)
        total-shots (reduce + (vals measurements))
        weighted-phase (/ (reduce + (map #(* (:estimated-phase %) (:count %)) phase-estimates))
                          total-shots)]
    {:phase-estimates phase-estimates
     :best-estimate best-estimate
     :weighted-average-phase weighted-phase
     :actual-phase actual-phase
     :best-error (fm/abs (- (:estimated-phase best-estimate) actual-phase))
     :weighted-error (fm/abs (- weighted-phase actual-phase))
     :total-shots total-shots}))

(defn quantum-phase-estimation
  "Execute quantum phase estimation to estimate the phase of a unitary operator.
   
   The quantum phase estimation algorithm estimates the eigenvalue of a unitary
   operator U when given an eigenstate. For a unitary U with eigenvalue e^(iφ),
   QPE estimates the phase φ.
   
   Algorithm steps:
   1. Prepare eigenstate qubit (|+⟩, |1⟩, or |0⟩)
   2. Initialize precision qubits in superposition with Hadamard gates
   3. Apply controlled-U^(2^k) operations for k = 0 to precision-qubits-1
   4. Apply inverse quantum Fourier transform to precision qubits
   5. Measure precision qubits to extract phase estimate
   
   Parameters:
   - backend: Quantum backend to execute the circuit
   - phase: Phase to estimate (for simulation purposes)
   - precision-qubits: Number of qubits for phase precision (affects accuracy)
   - eigenstate-type: Type of eigenstate preparation (:default, :plus, :one)
   - options: Execution options (shots, etc.)
   
   Returns: Map with comprehensive phase estimation results"
  ([backend phase precision-qubits]
   (quantum-phase-estimation backend phase precision-qubits :default))
  ([backend phase precision-qubits eigenstate-type]
   (quantum-phase-estimation backend phase precision-qubits eigenstate-type {:shots 1000}))
  ([backend phase precision-qubits eigenstate-type options]
   {:pre [(satisfies? qb/QuantumBackend backend)
          (pos-int? precision-qubits)
          (keyword? eigenstate-type)]}
   
   (let [;; Build the QPE circuit
         circuit (quantum-phase-estimation-circuit precision-qubits eigenstate-type phase)
         num-qubits (:num-qubits circuit)
         options (merge options {:result-specs {:measurements {:shots (:shots options 1000)}}})         
         ;; Execute the circuit
         execution-result (qb/execute-circuit backend circuit options)
         results (:results execution-result)
         ;; Extract measurement results
         measurements (:measurement-results results)
         freqs (:frequencies measurements)

         ;; Analyze results
         analysis (analyze-qpe-results freqs num-qubits precision-qubits phase)]

     {:result {:estimated-phase (:estimated-phase (:best-estimate analysis))
               :actual-phase phase
               :phase-error (:best-error analysis)
               :precision-qubits precision-qubits
               :success-probability (:probability (:best-estimate analysis))}
      :analysis analysis
      :measurement-results measurements
      :circuit circuit
      :execution-result execution-result})))

(defn quantum-phase-estimation-with-custom-unitary
  "Perform generalized quantum phase estimation with custom unitary operations.
   
   This function implements a generalized version of quantum phase estimation that
   allows for custom controlled unitary operations, enabling it to work with any
   unitary operator, not just phase rotations.
   
   Parameters:
   - backend: Quantum backend implementing the QuantumBackend protocol
   - precision-qubits: Number of qubits for phase precision (affects accuracy)
   - eigenstate-qubits: Number of qubits for eigenstate preparation
   - eigenstate-prep-fn: Function to prepare the eigenstate (receives circuit and eigenstate qubit range)
   - controlled-unitary-fn: Function to apply controlled U^(2^k) operations
                            (receives circuit, control qubit, power, and eigenstate qubit range)
   - options: Map containing additional backend options (e.g., :shots, :n-measurements)
   
   Returns:
   Map containing:
   - :measurements - Combined measurement results from all executions
   - :circuit - The quantum circuit used for QPE
   - :execution-results - Results from all circuit executions
   - :precision-qubits - Number of precision qubits used
   - :eigenstate-qubits - Number of eigenstate qubits used
   - :n-measurements - Number of measurements performed"
  [backend precision-qubits eigenstate-qubits eigenstate-prep-fn controlled-unitary-fn options]
  {:pre [(satisfies? qb/QuantumBackend backend)
         (pos-int? precision-qubits)
         (pos-int? eigenstate-qubits)
         (fn? eigenstate-prep-fn)
         (fn? controlled-unitary-fn)]}

  (let [n-measurements (get options :n-measurements 1)

        ;; Build the generalized QPE circuit once
        circuit (quantum-phase-estimation-circuit-with-custom-unitary
                 precision-qubits
                 eigenstate-qubits
                 eigenstate-prep-fn
                 controlled-unitary-fn)
        options (merge options {:measurements {:shots (:shots options 1)}})
        ;; Execute the circuit n-measurements times
        ;; FIXME result extraction
        execution-results (repeatedly n-measurements
                                      #(qb/execute-circuit backend circuit options))

        ;; Combine all measurement results
        all-measurements (reduce (fn [combined-measurements execution-result]
                                   (let [measurements (get-in execution-result [:results :measurement-results :frequencies])]
                                     (merge-with + combined-measurements measurements)))
                                 {}
                                 execution-results)]

    {:measurements all-measurements
     :circuit circuit
     :execution-results execution-results
     :precision-qubits precision-qubits
     :eigenstate-qubits eigenstate-qubits
     :n-measurements n-measurements}))


(comment
  ;; Example usage and testing of the quantum phase estimation algorithm
  
  ;; Create a simulator
  (require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
  (def simulator (sim/create-simulator {:max-qubits 10}))
  
  ;; Basic usage - estimate phase π/4
  (def result (quantum-phase-estimation simulator (/ Math/PI 4) 3 :plus {:shots 1000}))
  
  ;; Extract results
  (get-in result [:result :estimated-phase])  ; => ~0.7854 (π/4)
  (get-in result [:result :phase-error])      ; => error from actual phase
  (get-in result [:result :success-probability]) ; => probability of best estimate
  
  ;; Test with various phases
  (def test-phases [(/ Math/PI 8) (/ Math/PI 4) (/ Math/PI 2) Math/PI])
  
  (doseq [phase test-phases]
    (let [result (quantum-phase-estimation simulator phase 3 :plus {:shots 1000})]
      (println (format "Testing phase %.4f:" phase))
      (println (format "  Estimated: %.4f" (get-in result [:result :estimated-phase])))
      (println (format "  Error: %.4f" (get-in result [:result :phase-error])))
      (println (format "  Success prob: %.3f" (get-in result [:result :success-probability])))
      (println)))
  
  ;; Test different precision levels (more qubits = higher precision)
  (let [test-phase (/ Math/PI 4)]
    (doseq [precision [2 3 4 5]]
      (let [result (quantum-phase-estimation simulator test-phase precision :plus {:shots 1000})]
        (println (format "Precision %d qubits -> Error: %.4f" 
                         precision 
                         (get-in result [:result :phase-error]))))))
  
  ;; Analyze the circuit structure
  (def sample-circuit (quantum-phase-estimation-circuit 3 :plus (/ Math/PI 4)))
  (println "Circuit has" (count (:operations sample-circuit)) "operations")
  (println "Operations:")
  (doseq [op (:operations sample-circuit)]
    (println "  " (:operation-type op) (:operation-params op)))
  
  ;; Compare different eigenstate preparations
  (let [test-phase (/ Math/PI 4)]
    (doseq [eigenstate [:default :plus :one]]
      (let [result (quantum-phase-estimation simulator test-phase 3 eigenstate {:shots 1000})]
        (println (format "Eigenstate %s -> Error: %.4f" 
                         (name eigenstate)
                         (get-in result [:result :phase-error]))))))
  
  ;; Advanced: Analyze detailed measurement statistics
  (let [result (quantum-phase-estimation simulator (/ Math/PI 4) 4 :plus {:shots 2000})
        analysis (:analysis result)]
    (println "Top 5 measurement outcomes:")
    (->> (:phase-estimates analysis)
         (sort-by :count >)
         (take 5)
         (map #(println (format "  %s: count=%d, phase=%.4f, prob=%.3f"
                               (:measurement %)
                               (:count %)
                               (:estimated-phase %)
                               (:probability %))))))
  
  ;; Performance testing
  (time (quantum-phase-estimation simulator (/ Math/PI 4) 3 :plus {:shots 1000}))
  )

