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
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as fm]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft]
            [org.soulspace.qclojure.application.backend :as qb]))

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

(defn parse-measurement-to-phase
  "Convert measurement string to phase estimate.
   
   Parameters:
   - measurement: Binary measurement string (e.g., '001')
   - precision-qubits: Number of precision qubits
   
   Returns: Map with binary value and estimated phase"
  [measurement precision-qubits]
  (let [;; Extract precision bits (first precision-qubits bits) 
        precision-bits (take precision-qubits measurement)
        ;; Convert binary string to decimal
        binary-val (reduce +
                          (map-indexed 
                           (fn [i bit-char]
                             (let [bit (Character/digit (char bit-char) 10)]
                               (* bit (Math/pow 2 (- precision-qubits 1 i)))))
                           precision-bits))
        ;; Convert to phase estimate: φ = 2π * (binary_val / 2^n)
        estimated-phase (* 2 Math/PI binary-val (/ 1 (Math/pow 2 precision-qubits)))]
    {:binary-value binary-val
     :estimated-phase estimated-phase}))

(defn analyze-qpe-results
  "Analyze QPE measurement results to extract phase estimate.
   
   Parameters:
   - measurements: Map of measurement outcomes to counts
   - precision-qubits: Number of precision qubits used
   - actual-phase: Actual phase value (for comparison)
   
   Returns: Map with analysis results"
  [measurements precision-qubits actual-phase]
  (let [phase-estimates (for [[measurement count] measurements]
                         (let [parsed (parse-measurement-to-phase measurement precision-qubits)]
                           (assoc parsed 
                                  :measurement measurement
                                  :count count
                                  :probability (/ count (reduce + (vals measurements))))))
        
        ;; Find most likely estimate
        best-estimate (apply max-key :count phase-estimates)
        
        ;; Calculate statistics
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
   (quantum-phase-estimation backend phase precision-qubits eigenstate-type {:shots 1024}))
  ([backend phase precision-qubits eigenstate-type options]
   {:pre [(satisfies? qb/QuantumBackend backend)
          (pos-int? precision-qubits)
          (keyword? eigenstate-type)]}
   
   (let [;; Build the QPE circuit
         circuit (quantum-phase-estimation-circuit precision-qubits eigenstate-type phase)
         
         ;; Execute the circuit
         execution-result (qb/execute-circuit backend circuit options)
         
         ;; Extract measurement results
         measurements (:measurement-results execution-result)
         
         ;; Analyze results
         analysis (analyze-qpe-results measurements precision-qubits phase)]
     
     {:result {:estimated-phase (:estimated-phase (:best-estimate analysis))
               :actual-phase phase
               :phase-error (:best-error analysis)
               :precision-qubits precision-qubits
               :success-probability (:probability (:best-estimate analysis))}
      :analysis analysis
      :measurement-results measurements
      :circuit circuit
      :execution-result execution-result})))

;; Specs for quantum phase estimation
(s/def ::precision-qubits pos-int?)
(s/def ::eigenstate-type #{:default :plus :one})
(s/def ::phase number?)
(s/def ::backend #(satisfies? qb/QuantumBackend %))

(s/def ::qpe-result 
  (s/keys :req-un [::result ::analysis ::measurement-results ::circuit ::execution-result]))

(s/def ::phase-estimate-result
  (s/keys :req-un [::estimated-phase ::actual-phase ::phase-error 
                   ::precision-qubits ::success-probability]))

(s/fdef quantum-phase-estimation-circuit
  :args (s/cat :precision-qubits ::precision-qubits
               :eigenstate-type ::eigenstate-type  
               :phase ::phase)
  :ret map?)

(s/fdef quantum-phase-estimation
  :args (s/alt :three-args (s/cat :backend ::backend 
                                  :phase ::phase
                                  :precision-qubits ::precision-qubits)
               :four-args (s/cat :backend ::backend
                                :phase ::phase  
                                :precision-qubits ::precision-qubits
                                :eigenstate-type ::eigenstate-type)
               :five-args (s/cat :backend ::backend
                                :phase ::phase
                                :precision-qubits ::precision-qubits  
                                :eigenstate-type ::eigenstate-type
                                :options map?))
  :ret ::qpe-result)

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

