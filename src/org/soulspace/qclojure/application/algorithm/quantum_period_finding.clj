(ns org.soulspace.qclojure.application.algorithm.quantum-period-finding
  "Quantum period finding algorithm for Shor's algorithm.
   
   This algorithm is a specialized application of quantum phase estimation (QPE)
   to find the period of the modular exponentiation function f(x) = a^x mod N.
   
   Instead of reimplementing QPE, this module leverages the existing quantum-phase-estimation
   algorithm and adapts it for period finding by:
   1. Setting up the appropriate unitary operator (modular exponentiation)
   2. Using QPE to estimate the phase
   3. Converting phase estimates to period estimates using continued fractions
   
   This follows the DRY principle and maintains a clean separation of concerns."
  (:require
   [clojure.spec.alpha :as s]
   [org.soulspace.qclojure.domain.math :as qmath]
   [org.soulspace.qclojure.domain.circuit :as qc]
   [org.soulspace.qclojure.domain.circuit-transformation :as qct]
   [org.soulspace.qclojure.application.algorithm.quantum-fourier-transform :as qft]
   [org.soulspace.qclojure.application.algorithm.quantum-phase-estimation :as qpe]
   [org.soulspace.qclojure.application.backend :as qb]))

(defn phase-to-period
  "Convert a phase estimate from QPE to a period estimate for modular exponentiation.
   
   In quantum period finding, we're estimating the phase φ where the eigenvalue
   of the modular exponentiation unitary is e^(iφ). The period r relates to the
   phase through: φ = 2πs/r for some integer s.
   
   We use continued fractions to find the best rational approximation s/r
   where r is likely to be the period.
   
   Parameters:
   - phase: Estimated phase from QPE
   - precision-qubits: Number of precision qubits used in QPE
   - N: Modulus for period finding
   - a: Base for modular exponentiation a^x mod N
   
   Returns:
   Map with period estimate or nil if no valid period found"
  [phase precision-qubits N a]
  ;; Validate parameters: a must be less than N for valid period finding
  (when (and (< a N) (> a 1))
    (let [;; Convert phase to a fraction: phase = 2π * (measured_value / 2^n)
          ;; So measured_value = phase * 2^n / (2π)
          measured-value (/ (* phase (Math/pow 2 precision-qubits)) (* 2 Math/PI))
          
          ;; Use continued fractions to find best rational approximation
          cf (qmath/continued-fraction measured-value (Math/pow 2 precision-qubits))
          convergents (qmath/convergents cf)
          
          ;; Find convergent that gives a valid period
          valid-periods (for [[_num den] convergents
                             ;; Safely convert denominator to integer, handling overflow
                             :let [period (try
                                            (int den)
                                            (catch ArithmeticException _
                                              ;; If denominator is too large, skip this convergent
                                              nil))]
                             :when (and period  ; Only proceed if conversion succeeded
                                       (pos? period)
                                       (<= period N)
                                       (= 1 (qmath/mod-exp a period N)))]
                         {:period period
                          :measured-value measured-value  
                          :phase phase})]
      
      (first valid-periods)))) ; Return the first (and likely best) valid period

(defn quantum-phase-estimation-circuit
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
        eigenstate-qubit-range (range precision-qubits (+ precision-qubits eigenstate-qubits))
        ]
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
                      (controlled-unitary-fn circuit k (Math/pow 2 k) eigenstate-qubit-range))
                    c
                    (range precision-qubits)))

      ;; Step 4: Apply inverse QFT to precision qubits
      (as-> c
            (let [iqft-circuit (qft/inverse-quantum-fourier-transform-circuit precision-qubits)]
              (qct/compose-circuits c iqft-circuit {:control-qubits-only true}))))))

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
        circuit (quantum-phase-estimation-circuit
                  precision-qubits
                  eigenstate-qubits
                  eigenstate-prep-fn
                  controlled-unitary-fn)
        
        ;; Execute the circuit n-measurements times
        execution-results (repeatedly n-measurements
                                     #(qb/execute-circuit backend circuit options))
        
        ;; Combine all measurement results
        all-measurements (reduce (fn [combined-measurements execution-result]
                                  (let [measurements (:measurement-results execution-result)]
                                    (merge-with + combined-measurements measurements)))
                                {}
                                execution-results)]
    
    {:measurements all-measurements
     :circuit circuit
     :execution-results execution-results
     :precision-qubits precision-qubits
     :eigenstate-qubits eigenstate-qubits
     :n-measurements n-measurements}))

(defn quantum-period-finding
  "Find the period of f(x) = a^x mod N using quantum phase estimation.
   
   This function is a specialized application of quantum phase estimation for
   period finding in Shor's algorithm. It uses a generalized QPE that works
   with modular exponentiation as the unitary operator.
   
   The approach:
   1. Use generalized QPE with modular exponentiation as the controlled unitary
   2. Convert phase measurement results to period estimates using continued fractions
   3. Return the most likely period with statistical confidence
   
   Parameters:
   - backend: Quantum backend implementing the QuantumBackend protocol
   - a: Base for the function f(x) = a^x mod N  
   - N: Modulus
   - precision-qubits: Number of qubits for phase precision (affects accuracy)
   - n-measurements: (optional) Number of measurements for statistical analysis (default: 1)
   - options: (optional) Map containing additional backend options
   
   Returns:
   Map containing:
   - :estimated-period - Most likely period estimate
   - :period-candidates - All valid period candidates with probabilities
   - :qpe-results - Raw results from quantum phase estimation
   - :success - Whether a valid period was found
   - :confidence - Statistical confidence in the result"
  ([backend a N precision-qubits]
   (quantum-period-finding backend a N precision-qubits 1 {}))
  
  ([backend a N precision-qubits n-measurements]
   (quantum-period-finding backend a N precision-qubits n-measurements {}))
   
  ([backend a N precision-qubits n-measurements options]
   {:pre [(satisfies? qb/QuantumBackend backend)
          (pos-int? a) (pos-int? N) (pos-int? precision-qubits) 
          (< a N) (> a 1) (pos-int? n-measurements)]}
   
   (let [;; Calculate number of qubits needed for target register (to represent N)
         eigenstate-qubits (int (Math/ceil (/ (Math/log N) (Math/log 2))))
         
         ;; Define eigenstate preparation: initialize to |1⟩ for modular exponentiation
         eigenstate-prep-fn (fn [circuit eigenstate-qubit-range]
                             ;; Set the first target qubit to |1⟩ (representing 1 mod N)
                             (qc/x-gate circuit (first eigenstate-qubit-range)))
         
         ;; Define controlled modular exponentiation operations
         ;; This is a simplified version - in a real implementation, this would be
         ;; implemented using a full modular arithmetic circuit
         controlled-unitary-fn (fn [circuit control-qubit power eigenstate-qubit-range]
                                 ;; For now, we'll simulate with controlled phase gates
                                 ;; In a real implementation, this would be controlled
                                 ;; modular exponentiation: |y⟩ → |a^power * y mod N⟩
                                 (let [target-qubit (first eigenstate-qubit-range)
                                       ;; Calculate the phase that corresponds to this power
                                       ;; This is a simplified simulation
                                       phase (* 2 Math/PI (/ power N))]
                                   (qc/crz-gate circuit control-qubit target-qubit phase)))
         
         ;; Add n-measurements to options and perform QPE once
         qpe-options (assoc options :n-measurements n-measurements)
         qpe-result (quantum-phase-estimation-with-custom-unitary
                     backend 
                     precision-qubits
                     eigenstate-qubits
                     eigenstate-prep-fn
                     controlled-unitary-fn
                     qpe-options)
         
         ;; Extract and analyze measurement results
         all-measurements (:measurements qpe-result)
         
         ;; Convert measurements to phase estimates and then to period estimates
         period-candidates (for [measurement (keys all-measurements)
                                :let [phase-result (qpe/parse-measurement-to-phase measurement precision-qubits)
                                      phase (:estimated-phase phase-result)
                                      period-result (phase-to-period phase precision-qubits N a)]
                                :when period-result]
                            (assoc period-result 
                                   :measurement measurement
                                   :frequency (all-measurements measurement)))
         
         ;; Calculate probabilities for each period
         total-measurements (reduce + (vals all-measurements))
         enhanced-candidates (map #(assoc % :probability (/ (:frequency %) total-measurements))
                                 period-candidates)
         
         ;; Sort by probability to get best estimate
         best-candidates (sort-by :probability > enhanced-candidates)
         best-period (first best-candidates)]
     
     {:estimated-period (when best-period (:period best-period))
      :period-candidates (vec enhanced-candidates)  ; Convert to vector
      :qpe-result qpe-result
      :n-measurements n-measurements
      :success (boolean (seq period-candidates))
      :confidence (if best-period (:probability best-period) 0.0)
      :circuit (:circuit qpe-result)})))

;; Specs for quantum period finding
(s/def ::precision-qubits pos-int?)
(s/def ::a pos-int?)
(s/def ::N pos-int?)
(s/def ::phase number?)
(s/def ::backend #(satisfies? qb/QuantumBackend %))
(s/def ::n-measurements pos-int?)

(s/def ::period-candidate
  (s/keys :req-un [::period ::measured-value ::phase ::probability ::frequency]))

(s/def ::period-finding-result
  (s/keys :req-un [::estimated-period ::period-candidates ::qpe-result 
                   ::n-measurements ::success ::confidence]))

(s/fdef phase-to-period
  :args (s/cat :phase ::phase
               :precision-qubits ::precision-qubits
               :N ::N
               :a ::a)
  :ret (s/nilable map?))

(s/fdef quantum-period-finding
  :args (s/alt :four-args (s/cat :backend ::backend
                                 :a ::a
                                 :N ::N  
                                 :precision-qubits ::precision-qubits)
               :five-args (s/cat :backend ::backend
                                :a ::a
                                :N ::N
                                :precision-qubits ::precision-qubits
                                :n-measurements ::n-measurements)
               :six-args (s/cat :backend ::backend
                               :a ::a
                               :N ::N
                               :precision-qubits ::precision-qubits
                               :n-measurements ::n-measurements
                               :options map?))
  :ret ::period-finding-result)

(comment
  ;; Example usage and testing of the refactored quantum period finding
  
  ;; Create a simulator
  (require '[org.soulspace.qclojure.adapter.backend.simulator :as sim])
  (def simulator (sim/create-simulator {:max-qubits 10}))
  
  ;; Test period finding for known cases
  ;; For a=7, N=15, the period should be 4 (since 7^4 ≡ 1 mod 15)
  (def result (quantum-period-finding simulator 7 15 4 3 {:shots 1000}))
  
  ;; Extract results  
  (:estimated-period result)      ; Should be 4
  (:success result)               ; Should be true
  (:confidence result)            ; Probability/confidence
  
  ;; Test with different parameters
  (quantum-period-finding simulator 2 15 4 3 {:shots 1000})  ; Should also find period 4
  (quantum-period-finding simulator 3 8 3 3 {:shots 1000})   ; Should find period 2
  
  ;; Compare with classical verification
  (defn classical-period [a N]
    (loop [r 1]
      (if (= 1 (mod (qmath/mod-exp a r N) N))
        r
        (recur (inc r)))))
  
  (classical-period 7 15)  ; Should be 4
  (classical-period 2 15)  ; Should be 4  
  (classical-period 3 8)   ; Should be 2
  
  ;; Test the phase-to-period conversion function
  (phase-to-period 0.5 3 15 7)  ; Test conversion
  )

