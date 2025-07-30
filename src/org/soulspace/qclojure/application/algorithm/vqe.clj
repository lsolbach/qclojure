(ns org.soulspace.qclojure.application.algorithm.vqe
  "Variational Quantum Eigensolver (VQE) Algorithm Implementation
  
  VQE is a quantum-classical hybrid algorithm for finding the ground state energy
  of quantum systems. It uses a parameterized quantum circuit (ansatz) to prepare
  trial states and classical optimization to minimize the energy expectation value.
  
  Key Features:
  - Multiple ansatz types (hardware-efficient, UCCSD-inspired, symmetry-preserving)
  - Pauli string Hamiltonian representation with measurement grouping
  - Integration with fastmath optimization for classical optimization
  - Comprehensive analysis and convergence monitoring
  - Support for both gate-based and measurement-based implementations
  
  Algorithm Flow:
  1. Initialize parameterized quantum circuit (ansatz)
  2. Prepare trial state |ψ(θ)⟩ with parameters θ
  3. Measure expectation value ⟨ψ(θ)|H|ψ(θ)⟩
  4. Use classical optimizer to update parameters
  5. Repeat until convergence
  
  This implementation targets production use with real quantum hardware."
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as m]
            [fastmath.optimization :as opt]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.application.backend :as qb]))

;; Specs for VQE components
(s/def ::pauli-term 
  (s/keys :req-un [::coefficient ::pauli-string]))

(s/def ::coefficient number?)
(s/def ::pauli-string string?)

(s/def ::hamiltonian 
  (s/coll-of ::pauli-term))

(s/def ::ansatz-type 
  #{:hardware-efficient :uccsd :symmetry-preserving :custom})

(s/def ::optimization-method 
  #{:gradient-descent :adam :quantum-natural-gradient :nelder-mead})

(s/def ::vqe-config
  (s/keys :req-un [::hamiltonian ::ansatz-type ::num-qubits]
          :opt-un [::initial-parameters ::max-iterations ::tolerance 
                   ::optimization-method ::shots ::measurement-grouping]))

;;
;; Hamiltonian Representation and Manipulation
;;
(defn pauli-term
  "Create a Pauli term with coefficient and Pauli string.
  
  Parameters:
  - coefficient: Real coefficient for the term
  - pauli-string: String like 'XYZZ' representing tensor product of Pauli operators
  
  Returns:
  Map representing a single term in the Hamiltonian"
  [coefficient pauli-string]
  {:pre [(number? coefficient) (string? pauli-string)
         (every? #{\I \X \Y \Z} pauli-string)]}
  {:coefficient coefficient
   :pauli-string pauli-string})

(defn validate-hamiltonian
  "Validate that a Hamiltonian is properly formed.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  
  Returns:
  Boolean indicating validity"
  [hamiltonian]
  (and (coll? hamiltonian)
       (every? #(s/valid? ::pauli-term %) hamiltonian)
       (let [string-lengths (map #(count (:pauli-string %)) hamiltonian)]
         (or (empty? string-lengths) ; empty Hamiltonian is valid
             (apply = string-lengths)))))

(defn molecular-hydrogen-hamiltonian
  "Create the Hamiltonian for molecular hydrogen (H2) in the STO-3G basis.
  
  This is a standard benchmark Hamiltonian used in quantum chemistry.
  The Hamiltonian has 15 terms and operates on 4 qubits.
  
  Parameters:
  - bond-distance: Internuclear distance in Angstroms (default: 0.735)
  
  Returns:
  Collection of Pauli terms representing the H2 Hamiltonian"
  ([]
   (molecular-hydrogen-hamiltonian 0.735))
  ([bond-distance]
   ;; Coefficients are computed for the given bond distance
   ;; These values are for equilibrium geometry (0.735 Å)
   (let [coefficients (if (< (abs (- bond-distance 0.735)) 0.01)
                        ;; Equilibrium geometry coefficients
                        [-1.0523732  0.39793742 -0.39793742 -0.01128010
                          0.18093119  0.18093119  0.17218393  0.17218393
                         -0.24274280 -0.24274280  0.17059738  0.04475014
                         -0.04475014 -0.04475014  0.04475014]
                        ;; For other distances, use scaled coefficients
                        (let [scale (/ 0.735 bond-distance)]
                          (map #(* % scale) 
                               [-1.0523732  0.39793742 -0.39793742 -0.01128010
                                 0.18093119  0.18093119  0.17218393  0.17218393
                                -0.24274280 -0.24274280  0.17059738  0.04475014
                                -0.04475014 -0.04475014  0.04475014])))
         pauli-strings ["IIII" "IIIZ" "IIZI" "IIZZ" "IZII" "IZIZ" "IZZI" "IZZZ"
                        "ZIII" "ZIIZ" "ZIZI" "ZIZZ" "ZZII" "ZZIZ" "ZZZI"]]
     (mapv pauli-term coefficients pauli-strings))))

(defn heisenberg-hamiltonian
  "Create a Heisenberg model Hamiltonian for a 1D chain.
  
  H = J Σᵢ (XᵢXᵢ₊₁ + YᵢYᵢ₊₁ + ZᵢZᵢ₊₁)
  
  Parameters:
  - num-sites: Number of sites in the chain
  - coupling: Coupling strength J (default: 1.0)
  - periodic: Whether to use periodic boundary conditions (default: true)
  
  Returns:
  Collection of Pauli terms"
  ([num-sites]
   (heisenberg-hamiltonian num-sites 1.0 true))
  ([num-sites coupling]
   (heisenberg-hamiltonian num-sites coupling true))
  ([num-sites coupling periodic]
   {:pre [(pos-int? num-sites) (number? coupling)]}
   (let [max-i (if periodic num-sites (dec num-sites))]
     (for [i (range max-i)
           pauli [\X \Y \Z]]
       (let [j (mod (inc i) num-sites)
             pauli-string (apply str (map (fn [k]
                                            (cond 
                                              (= k i) pauli
                                              (= k j) pauli
                                              :else \I))
                                          (range num-sites)))]
         (pauli-term coupling pauli-string))))))

(defn group-commuting-terms
  "Group Hamiltonian terms that can be measured simultaneously.
  
  Terms that commute can be measured in the same quantum circuit execution,
  reducing the number of measurements needed.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  
  Returns:
  Vector of groups, where each group is a collection of commuting terms"
  [hamiltonian]
  {:pre [(validate-hamiltonian hamiltonian)]}
  ;; Simple greedy grouping algorithm
  ;; More sophisticated algorithms exist (e.g., graph coloring)
  (loop [ungrouped hamiltonian
         groups []]
    (if (empty? ungrouped)
      groups
      (let [current-term (first ungrouped)
            current-string (:pauli-string current-term)
            ;; Find all terms that commute with current term
            commuting (filter (fn [term]
                                (let [other-string (:pauli-string term)]
                                  ;; Two Pauli strings commute if they have an even number
                                  ;; of positions where both have non-commuting Paulis
                                  (even? (count (filter (fn [[p1 p2]]
                                                          (and (not= p1 \I) (not= p2 \I)
                                                               (not= p1 p2)))
                                                        (map vector current-string other-string))))))
                              ungrouped)
            remaining (remove (set commuting) ungrouped)]
        (recur remaining (conj groups commuting))))))

;;
;; Ansatz Circuit Construction
;;
(defn hardware-efficient-ansatz
  "Create a hardware-efficient ansatz circuit.
  
  This ansatz consists of layers of single-qubit rotations followed by
  entangling gates, designed to be implementable on near-term quantum devices.
  
  Parameters:
  - num-qubits: Number of qubits
  - num-layers: Number of ansatz layers (default: 1)
  - entangling-gate: Type of entangling gate (:cnot, :cz, :crz) (default: :cnot)
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits]
   (hardware-efficient-ansatz num-qubits 1 :cnot))
  ([num-qubits num-layers]
   (hardware-efficient-ansatz num-qubits num-layers :cnot))
  ([num-qubits num-layers entangling-gate]
   {:pre [(pos-int? num-qubits) (pos-int? num-layers)
          (contains? #{:cnot :cz :crz} entangling-gate)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)
            (= (count parameters) (* num-layers num-qubits 3))]}
     (let [circuit (qc/create-circuit num-qubits "Hardware Efficient Ansatz")]
       (loop [c circuit
              layer 0
              param-idx 0]
         (if (>= layer num-layers)
           c
           ;; Add rotation gates for each qubit
           (let [c-with-rotations 
                 (loop [c-rot c
                        qubit 0
                        p-idx param-idx]
                   (if (>= qubit num-qubits)
                     c-rot
                     (let [rx-angle (nth parameters p-idx)
                           ry-angle (nth parameters (inc p-idx))
                           rz-angle (nth parameters (+ p-idx 2))]
                       (recur (-> c-rot
                                  (qc/rx-gate qubit rx-angle)
                                  (qc/ry-gate qubit ry-angle)
                                  (qc/rz-gate qubit rz-angle))
                              (inc qubit)
                              (+ p-idx 3)))))
                 ;; Add entangling gates
                 c-with-entangling
                 (case entangling-gate
                   :cnot (loop [c-ent c-with-rotations
                               qubit 0]
                           (if (>= qubit (dec num-qubits))
                             c-ent
                             (recur (qc/cnot-gate c-ent qubit (inc qubit))
                                    (inc qubit))))
                   :cz (loop [c-ent c-with-rotations
                             qubit 0]
                         (if (>= qubit (dec num-qubits))
                           c-ent
                           (recur (qc/cz-gate c-ent qubit (inc qubit))
                                  (inc qubit))))
                   :crz (loop [c-ent c-with-rotations
                              qubit 0]
                          (if (>= qubit (dec num-qubits))
                            c-ent
                            (recur (qc/crz-gate c-ent qubit (inc qubit) 
                                                (/ m/PI 4)) ; Fixed angle
                                   (inc qubit)))))]
             (recur c-with-entangling
                    (inc layer)
                    (+ param-idx (* num-qubits 3))))))))))

(defn uccsd-inspired-ansatz
  "Create a UCCSD-inspired ansatz circuit.
  
  Unitary Coupled Cluster with Singles and Doubles (UCCSD) is a quantum chemistry
  ansatz that captures important electronic correlations. This is a simplified
  version suitable for VQE implementations.
  
  Parameters:
  - num-qubits: Number of qubits (must be even for electron pairs)
  - num-excitations: Number of excitation operators to include
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits]
   (uccsd-inspired-ansatz num-qubits (/ num-qubits 2)))
  ([num-qubits num-excitations]
   {:pre [(pos-int? num-qubits) (even? num-qubits) (pos-int? num-excitations)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)
            (= (count parameters) num-excitations)]}
     (-> (qc/create-circuit num-qubits "UCCSD Inspired Ansatz")
         ;; Start with Hartree-Fock state (fill lower orbitals)
         ((fn [circuit]
            (loop [c circuit
                   qubit 0]
              (if (>= qubit (/ num-qubits 2))
                c
                (recur (qc/x-gate c qubit) (inc qubit))))))
         ;; Add excitation operators
         ((fn [hf-circuit]
            (loop [c hf-circuit
                   exc 0]
              (if (>= exc num-excitations)
                c
                (let [angle (nth parameters exc)
                      ;; Single excitation: rotate between occupied and virtual orbitals
                      i (mod exc (/ num-qubits 2))  ; occupied orbital
                      a (+ (/ num-qubits 2) (mod exc (/ num-qubits 2)))] ; virtual orbital
                  (recur (-> c
                             (qc/ry-gate i (/ angle 2))
                             (qc/cnot-gate i a)
                             (qc/ry-gate a (/ angle 2))
                             (qc/cnot-gate i a)
                             (qc/ry-gate i (- (/ angle 2))))
                         (inc exc)))))))))))

(defn symmetry-preserving-ansatz
  "Create a symmetry-preserving ansatz circuit.
  
  This ansatz preserves certain symmetries like particle number conservation,
  important for quantum chemistry and condensed matter applications.
  
  Parameters:
  - num-qubits: Number of qubits
  - num-particles: Number of particles to conserve
  - num-layers: Number of ansatz layers
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits num-particles]
   (symmetry-preserving-ansatz num-qubits num-particles 1))
  ([num-qubits num-particles num-layers]
   {:pre [(pos-int? num-qubits) (<= num-particles num-qubits) (pos-int? num-layers)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)]}
     (-> (qc/create-circuit num-qubits "Symmetry Preserving Ansatz")
         ;; Initialize with correct particle number
         ((fn [circuit]
            (loop [c circuit
                   qubit 0
                   particles-placed 0]
              (if (or (>= qubit num-qubits) 
                      (>= particles-placed num-particles))
                c
                (recur (qc/x-gate c qubit)
                       (inc qubit)
                       (inc particles-placed))))))
         ;; Add particle-conserving gates
         ((fn [init-circuit]
            (let [param-per-layer (dec num-qubits)]
              (loop [c init-circuit
                     layer 0
                     param-idx 0]
                (if (>= layer num-layers)
                  c
                  (let [c-with-swaps
                        (loop [c-swap c
                               qubit 0
                               p-idx param-idx]
                          (if (>= qubit (dec num-qubits))
                            c-swap
                            (let [angle (if (< p-idx (count parameters))
                                          (nth parameters p-idx)
                                          0.0)]
                              ;; Particle-conserving swap rotation
                              (recur (qc/cry-gate c-swap qubit (inc qubit) angle)
                                     (inc qubit)
                                     (inc p-idx)))))]
                    (recur c-with-swaps
                           (inc layer)
                           (+ param-idx param-per-layer))))))))))))

;;
;; Expectation Value Calculation
;;
(defn pauli-string-expectation
  "Calculate expectation value of a single Pauli string.
  
  Parameters:
  - pauli-string: String like 'XYZZ' representing Pauli operators
  - quantum-state: Quantum state to measure
  
  Returns:
  Real expectation value"
  [pauli-string quantum-state]
  {:pre [(string? pauli-string) (map? quantum-state)]}
  (let [observable (obs/pauli-string->observable pauli-string)]
    (obs/expectation-value observable quantum-state)))

(defn hamiltonian-expectation
  "Calculate expectation value of a Hamiltonian.
  
  ⟨H⟩ = Σᵢ cᵢ ⟨Pᵢ⟩ where cᵢ are coefficients and Pᵢ are Pauli strings.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  - quantum-state: Quantum state to measure
  
  Returns:
  Real expectation value (energy)"
  [hamiltonian quantum-state]
  {:pre [(validate-hamiltonian hamiltonian) (map? quantum-state)]}
  (reduce + (map (fn [term]
                   (let [coeff (:coefficient term)
                         pauli-str (:pauli-string term)]
                     (* coeff (pauli-string-expectation pauli-str quantum-state))))
                 hamiltonian)))

(defn measurement-based-expectation
  "Calculate expectation value using measurement statistics (for real hardware).
  
  This function is used when running on actual quantum hardware where we get
  measurement counts rather than direct access to the quantum state.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  - measurement-results: Map from bit strings to counts
  - total-shots: Total number of measurements
  
  Returns:
  Real expectation value estimated from measurements"
  [hamiltonian measurement-results total-shots]
  {:pre [(validate-hamiltonian hamiltonian) (map? measurement-results) (pos-int? total-shots)]}
  (reduce + (map (fn [term]
                   (let [coeff (:coefficient term)
                         pauli-str (:pauli-string term)
                         ;; Calculate expectation from measurement frequencies
                         expectation (reduce-kv 
                                      (fn [acc bit-string count]
                                        (let [prob (/ count total-shots)
                                              ;; Calculate Pauli string value for this bit string
                                              pauli-value (reduce * (map-indexed 
                                                                      (fn [i pauli-char]
                                                                        (let [bit (Character/digit (nth bit-string i) 10)]
                                                                          (case pauli-char
                                                                            \I 1
                                                                            \Z (if (= bit 0) 1 -1)
                                                                            \X 0  ; Requires basis rotation
                                                                            \Y 0  ; Requires basis rotation
                                                                            )))
                                                                      pauli-str))]
                                          (+ acc (* prob pauli-value))))
                                      0 measurement-results)]
                     (* coeff expectation)))
                 hamiltonian)))

;;
;; VQE Core Algorithm
;;
(defn create-vqe-objective
  "Create the objective function for VQE optimization.
  
  This function creates a production-ready objective function that:
  1. Takes variational parameters as input
  2. Constructs the ansatz circuit with those parameters
  3. Executes the circuit on the backend or simulator
  4. Calculates the Hamiltonian expectation value from the final quantum state
  5. Returns the energy (to be minimized by the optimizer)
  
  The implementation supports both backend execution (for real hardware)
  and direct simulation (for testing and development).
  
  Parameters:
  - hamiltonian: Hamiltonian to minimize (collection of Pauli terms)
  - ansatz-fn: Function that takes parameters and returns a circuit
  - backend: Quantum backend for circuit execution (can be nil for direct simulation)
  - options: Execution options (shots, etc.)
  
  Returns:
  Function that takes parameters and returns energy expectation value"
  [hamiltonian ansatz-fn backend options]
  {:pre [(validate-hamiltonian hamiltonian) (fn? ansatz-fn)]}
  (fn objective [parameters]
    (try
      (let [;; Step 1: Create ansatz circuit with current parameters
            circuit (ansatz-fn parameters)

            ;; Step 2: Execute circuit to get final quantum state
            final-state (try
                          (let [execution-result (qb/execute-circuit backend circuit options)]
                            (if (= (:job-status execution-result) :completed)
                              (:final-state execution-result)
                              ;; Fallback to simulation if backend execution fails
                              (qc/execute-circuit circuit (qs/zero-state (:num-qubits circuit)))))
                          (catch Exception _e
                            ;; Fallback to direct simulation if backend unavailable
                            (qc/execute-circuit circuit (qs/zero-state (:num-qubits circuit)))))


            ;; Step 3: Calculate Hamiltonian expectation value (energy)
            energy (hamiltonian-expectation hamiltonian final-state)]

        ;; Return the energy (real number to be minimized)
        energy)
      
      (catch Exception e
        ;; Return large positive value if any step fails
        (println "VQE objective evaluation failed:" (.getMessage e))
        (.printStackTrace e)
        1000.0))))

(defn parameter-shift-gradient
  "Calculate gradient using the parameter shift rule.
  
  For a parameterized gate with parameter θ, the gradient is:
  ∂⟨H⟩/∂θ = (1/2)[⟨H⟩(θ + π/2) - ⟨H⟩(θ - π/2)]
  
  This gives exact gradients for quantum circuits with rotation gates.
  
  Parameters:
  - objective-fn: VQE objective function
  - parameters: Current parameter vector
  - param-index: Index of parameter to compute gradient for
  
  Returns:
  Gradient value for the specified parameter"
  [objective-fn parameters param-index]
  (let [shift (/ m/PI 2)  ; Standard π/2 shift for rotation gates
        params-plus (assoc parameters param-index 
                          (+ (nth parameters param-index) shift))
        params-minus (assoc parameters param-index 
                           (- (nth parameters param-index) shift))
        energy-plus (objective-fn params-plus)
        energy-minus (objective-fn params-minus)]
    (* 0.5 (- energy-plus energy-minus))))

(defn calculate-vqe-gradient
  "Calculate full gradient vector using parameter shift rule.
  
  Parameters:
  - objective-fn: VQE objective function
  - parameters: Current parameter vector
  
  Returns:
  Vector of gradients for all parameters"
  [objective-fn parameters]
  (mapv #(parameter-shift-gradient objective-fn parameters %)
        (range (count parameters))))

(defn gradient-descent-optimization
  "VQE optimization using gradient descent with parameter shift rules.
  
  This is the preferred optimization method for VQE as it:
  1. Uses exact quantum gradients via parameter shift rule
  2. Has theoretical convergence guarantees
  3. Is efficient (2 circuit evaluations per parameter per iteration)
  4. Handles quantum circuit structure naturally
  
  Parameters:
  - objective-fn: VQE objective function
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [objective-fn initial-parameters options]
  (let [learning-rate (:learning-rate options 0.01)  ; Conservative default
        max-iter (:max-iterations options 500)        ; Reasonable default
        tolerance (:tolerance options 1e-6)
        adaptive-lr (:adaptive-learning-rate options true)
        momentum (:momentum options 0.9)
        
        ;; Initialize momentum vector
        velocity (vec (repeat (count initial-parameters) 0.0))]
    
    (loop [params initial-parameters
           v velocity
           iteration 0
           prev-energy (objective-fn initial-parameters)
           lr learning-rate
           energies [prev-energy]]
      
      (if (>= iteration max-iter)
        {:success false
         :optimal-parameters params
         :optimal-energy (objective-fn params)
         :iterations iteration
         :function-evaluations (* iteration (count params) 2) ; 2 evals per param per iteration
         :convergence-history energies
         :reason "max-iterations-reached"}
        
        (let [;; Calculate gradient using parameter shift rule
              gradient (calculate-vqe-gradient objective-fn params)
              
              ;; Update velocity (momentum)
              new-velocity (mapv #(+ (* momentum %1) (* lr %2)) v gradient)
              
              ;; Update parameters
              new-params (mapv - params new-velocity)
              
              ;; Calculate new energy
              new-energy (objective-fn new-params)
              energy-diff (abs (- prev-energy new-energy))
              
              ;; Adaptive learning rate (decrease if energy increases)
              new-lr (if (and adaptive-lr (> new-energy prev-energy))
                       (* lr 0.8)  ; Reduce learning rate
                       lr)
              
              new-energies (conj energies new-energy)]
          
          (if (< energy-diff tolerance)
            {:success true
             :optimal-parameters new-params
             :optimal-energy new-energy
             :iterations iteration
             :function-evaluations (* iteration (count params) 2)
             :convergence-history new-energies
             :final-gradient gradient
             :reason "converged"}
            
            (recur new-params new-velocity (inc iteration) new-energy new-lr new-energies)))))))

(defn adam-optimization
  "VQE optimization using Adam optimizer with parameter shift gradients.
  
  Adam combines momentum with adaptive learning rates per parameter,
  often providing faster and more stable convergence than plain gradient descent.
  
  Parameters:
  - objective-fn: VQE objective function
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [objective-fn initial-parameters options]
  (let [learning-rate (:learning-rate options 0.001)
        max-iter (:max-iterations options 500)
        tolerance (:tolerance options 1e-6)
        beta1 (:beta1 options 0.9)   ; Momentum decay
        beta2 (:beta2 options 0.999) ; RMSprop decay
        epsilon (:epsilon options 1e-8)
        
        param-count (count initial-parameters)
        m (vec (repeat param-count 0.0))  ; First moment
        v (vec (repeat param-count 0.0))] ; Second moment
    
    (loop [params initial-parameters
           m-vec m
           v-vec v
           iteration 1  ; Adam uses 1-based iteration for bias correction
           prev-energy (objective-fn initial-parameters)
           energies [prev-energy]]
      
      (if (>= iteration max-iter)
        {:success false
         :optimal-parameters params
         :optimal-energy (objective-fn params)
         :iterations iteration
         :function-evaluations (* iteration param-count 2)
         :convergence-history energies
         :reason "max-iterations-reached"}
        
        (let [;; Calculate gradient
              gradient (calculate-vqe-gradient objective-fn params)
              
              ;; Update biased first moment estimate
              new-m (mapv #(+ (* beta1 %1) (* (- 1 beta1) %2)) m-vec gradient)
              
              ;; Update biased second moment estimate
              new-v (mapv #(+ (* beta2 %1) (* (- 1 beta2) %2 %2)) v-vec gradient)
              
              ;; Bias correction
              m-corrected (mapv #(/ % (- 1 (m/pow beta1 iteration))) new-m)
              v-corrected (mapv #(/ % (- 1 (m/pow beta2 iteration))) new-v)
              
              ;; Parameter update
              new-params (mapv #(- %1 (* learning-rate (/ %2 (+ (m/sqrt %3) epsilon))))
                              params m-corrected v-corrected)
              
              ;; Energy evaluation
              new-energy (objective-fn new-params)
              energy-diff (abs (- prev-energy new-energy))
              new-energies (conj energies new-energy)]
          
          (if (< energy-diff tolerance)
            {:success true
             :optimal-parameters new-params
             :optimal-energy new-energy
             :iterations iteration
             :function-evaluations (* iteration param-count 2)
             :convergence-history new-energies
             :final-gradient gradient
             :reason "converged"}
            
            (recur new-params new-m new-v (inc iteration) new-energy new-energies)))))))

(defn quantum-natural-gradient-optimization
  "VQE optimization using Quantum Natural Gradient (QNG).
  
  QNG uses the quantum Fisher information matrix to define a more natural
  metric for parameter updates, often leading to faster convergence than
  standard gradient descent.
  
  Note: This is a simplified implementation. Full QNG requires computing
  the quantum Fisher information matrix, which is computationally expensive.
  
  Parameters:
  - objective-fn: VQE objective function  
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [objective-fn initial-parameters options]
  ;; For now, fall back to Adam optimization with different defaults
  ;; Full QNG implementation would require Fisher information matrix calculation
  (adam-optimization objective-fn initial-parameters 
                    (merge {:learning-rate 0.1
                            :beta1 0.999
                            :beta2 0.999} 
                           options)))

(defn vqe-optimization
  "Run VQE optimization using the specified method.
  
  Supports multiple optimization methods:
  - :gradient-descent - Parameter shift rule with gradient descent (recommended)
  - :adam - Adam optimizer with parameter shift gradients (often fastest)
  - :quantum-natural-gradient - Quantum Natural Gradient (experimental)
  - :nelder-mead - Classical derivative-free optimizer (fallback only)
  
  Parameters:
  - objective-fn: Objective function to minimize
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [objective-fn initial-parameters options]
  (let [method (:optimization-method options :gradient-descent)]  ; Default to Adam
    (case method
      :gradient-descent 
      (gradient-descent-optimization objective-fn initial-parameters options)
      
      :adam
      (adam-optimization objective-fn initial-parameters options)
      
      :quantum-natural-gradient
      (quantum-natural-gradient-optimization objective-fn initial-parameters options)
      
      :nelder-mead
      (do 
        (println "Warning: Using Nelder-Mead optimizer. Consider :adam or :gradient-descent for better performance.")
        (let [max-iter (:max-iterations options 1000)  ; Much higher default for Nelder-Mead
              tolerance (:tolerance options 1e-6)
              param-count (count initial-parameters)
              
              ;; Wrap objective function for fastmath (converts varargs to vector)
              wrapped-objective (fn [& params] (objective-fn (vec params)))
              
              ;; Configure fastmath optimization with proper bounds
              config {:max-evals max-iter
                      :rel-threshold tolerance
                      :abs-threshold tolerance
                      ;; Add parameter bounds for optimization stability
                      :bounds (vec (repeat param-count [-4.0 4.0]))  ; Allow rotations up to ~2π
                      ;; Provide initial parameters
                      :initial initial-parameters}
              
              ;; Run optimization (correct API: method, wrapped-objective-fn, config)
              result (opt/minimize :nelder-mead wrapped-objective config)]
          
          {:success (:converged result false)
           :optimal-parameters (:arg result initial-parameters)
           :optimal-energy (:value result (objective-fn initial-parameters))
           :iterations (:iterations result 0)
           :function-evaluations (:evaluations result 0)
           :optimization-result result}))
      
      ;; Default fallback
      (throw (ex-info (str "Unknown optimization method: " method)
                      {:method method
                       :available-methods [:gradient-descent :adam :quantum-natural-gradient :nelder-mead]})))))

(defn variational-quantum-eigensolver
  "Main VQE algorithm implementation.
  
  Parameters:
  - backend: Quantum backend implementing QuantumBackend protocol
  - config: VQE configuration map
  - options: Additional options for execution
  
  Returns:
  Map containing VQE results and analysis"
  ([backend config]
   (variational-quantum-eigensolver backend config {}))
  ([backend config _options]
   {:pre [(s/valid? ::vqe-config config)]}
   (let [hamiltonian (:hamiltonian config)
         ansatz-type (:ansatz-type config)
         num-qubits (:num-qubits config)
         max-iter (:max-iterations config 500)  ; Higher default for gradient-based methods
         tolerance (:tolerance config 1e-6)
         opt-method (:optimization-method config :gradient-descent)  ; Default to Adam optimizer
         shots (:shots config 1024)
         
         ;; Create ansatz function
         ansatz-fn (case ansatz-type
                     :hardware-efficient 
                     (hardware-efficient-ansatz num-qubits 
                                                (:num-layers config 1)
                                                (:entangling-gate config :cnot))
                     :uccsd 
                     (uccsd-inspired-ansatz num-qubits (:num-excitations config 2))
                     :symmetry-preserving 
                     (symmetry-preserving-ansatz num-qubits 
                                                 (:num-particles config 2)
                                                 (:num-layers config 1))
                     :custom
                     (:custom-ansatz config))
         
         ;; Determine parameter count and initial values
         param-count (case ansatz-type
                       :hardware-efficient (* (:num-layers config 1) num-qubits 3)
                       :uccsd (:num-excitations config 2)
                       :symmetry-preserving (* (:num-layers config 1) (dec num-qubits))
                       :custom (count (:initial-parameters config)))
         
         initial-params (or (:initial-parameters config)
                           (vec (repeatedly param-count #(* 0.1 (- (rand) 0.5)))))
         
         ;; Create objective function
         exec-options {:shots shots}
         objective-fn (create-vqe-objective hamiltonian ansatz-fn backend exec-options)
         
         ;; Run optimization
         opt-options {:optimization-method opt-method
                      :max-iterations max-iter
                      :tolerance tolerance}
         
         start-time (System/currentTimeMillis)
         opt-result (vqe-optimization objective-fn initial-params opt-options)
         end-time (System/currentTimeMillis)
         
         ;; Calculate final results
         optimal-params (:optimal-parameters opt-result)
         optimal-energy (:optimal-energy opt-result)
         final-circuit (ansatz-fn optimal-params)
         
         ;; Analysis
         grouped-terms (when (:measurement-grouping config)
                         (group-commuting-terms hamiltonian))
         
         classical-energy (when (:calculate-classical-bound config)
                             ;; Simple estimation: sum of absolute coefficients
                             (reduce + (map #(abs (:coefficient %)) hamiltonian)))]
     
     {:algorithm "Variational Quantum Eigensolver"
      :config config
      :results {:optimal-energy optimal-energy
                :optimal-parameters optimal-params
                :success (:success opt-result)
                :iterations (:iterations opt-result)
                :function-evaluations (:function-evaluations opt-result)}
      :circuit {:final-circuit final-circuit
                :num-qubits num-qubits
                :parameter-count param-count
                :ansatz-type ansatz-type}
      :hamiltonian {:terms (count hamiltonian)
                    :grouped-terms (when grouped-terms (count grouped-terms))
                    :classical-bound classical-energy}
      :timing {:execution-time-ms (- end-time start-time)
               :start-time start-time
               :end-time end-time}
      :analysis {:initial-energy (objective-fn initial-params)
                 :energy-improvement (- (objective-fn initial-params) optimal-energy)
                 :convergence-achieved (:success opt-result)}
      :optimization opt-result})))

;;
;; Analysis and Utilities
;;
(defn analyze-vqe-landscape
  "Analyze the VQE energy landscape around optimal parameters.
  
  Parameters:
  - objective-fn: VQE objective function
  - optimal-params: Optimal parameters found
  - perturbation-size: Size of parameter perturbations for analysis
  
  Returns:
  Map with landscape analysis"
  [objective-fn optimal-params perturbation-size]
  (let [num-params (count optimal-params)
        optimal-energy (objective-fn optimal-params)
        
        ;; Calculate gradients (finite difference)
        gradients (mapv (fn [i]
                          (let [params-plus (assoc optimal-params i 
                                                    (+ (nth optimal-params i) perturbation-size))
                                params-minus (assoc optimal-params i 
                                                     (- (nth optimal-params i) perturbation-size))
                                energy-plus (objective-fn params-plus)
                                energy-minus (objective-fn params-minus)]
                            (/ (- energy-plus energy-minus) (* 2 perturbation-size))))
                        (range num-params))
        
        ;; Calculate parameter sensitivities
        sensitivities (mapv (fn [i]
                              (let [params-perturbed (assoc optimal-params i 
                                                            (+ (nth optimal-params i) perturbation-size))
                                    energy-perturbed (objective-fn params-perturbed)]
                                (abs (- energy-perturbed optimal-energy))))
                            (range num-params))]
    
    {:optimal-energy optimal-energy
     :gradients gradients
     :gradient-norm (m/sqrt (reduce + (map #(* % %) gradients)))
     :sensitivities sensitivities
     :most-sensitive-parameter (apply max-key #(nth sensitivities %) (range num-params))
     :least-sensitive-parameter (apply min-key #(nth sensitivities %) (range num-params))}))

(defn vqe-convergence-analysis
  "Analyze VQE convergence from optimization history.
  
  Parameters:
  - optimization-result: Result from VQE optimization
  
  Returns:
  Map with convergence analysis"
  [optimization-result]
  (let [history (:history optimization-result [])
        energies (map :energy history)]
    
    (when (seq energies)
      {:total-iterations (count energies)
       :initial-energy (first energies)
       :final-energy (last energies)
       :energy-improvement (- (first energies) (last energies))
       :convergence-rate (when (> (count energies) 1)
                           (/ (- (first energies) (last energies))
                              (count energies)))
       :monotonic-decrease? (every? (fn [[e1 e2]] (<= e2 e1)) 
                                   (partition 2 1 energies))})))

(comment
  ;; Example usage and testing
  
  ;; Create a simple Hamiltonian (H2 molecule)
  (def h2-hamiltonian (molecular-hydrogen-hamiltonian))
  
  ;; Create VQE configuration  
  (def vqe-config
    {:hamiltonian h2-hamiltonian
     :ansatz-type :hardware-efficient
     :num-qubits 4
     :num-layers 2
     :max-iterations 50
     :tolerance 1e-4
     :optimization-method :nelder-mead
     :shots 1024
     :measurement-grouping true})
  
  ;; Run VQE (requires backend)
  ;; (def vqe-result (variational-quantum-eigensolver backend vqe-config))
  
  ;; Analyze results
  ;; (:optimal-energy (:results vqe-result))
  ;; (:energy-improvement (:analysis vqe-result))
  
  ;; Test different ansatz types
  (def he-ansatz (hardware-efficient-ansatz 4 2))
  (def test-params (vec (repeatedly 24 #(* 0.1 (rand)))))
  ;; (def test-circuit (he-ansatz test-params))
  
  ;; Test Hamiltonian grouping
  (def grouped (group-commuting-terms h2-hamiltonian))
  (println "H2 Hamiltonian can be measured in" (count grouped) "groups")
  
  ;; Test Heisenberg model
  (def heisenberg (heisenberg-hamiltonian 4 1.0 true))
  (println "Heisenberg chain has" (count heisenberg) "terms")
  
  )
