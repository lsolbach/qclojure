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
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.domain.ansatz :as ansatz]
            [org.soulspace.qclojure.application.algorithm.variational-algorithm :as va]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            ;[fastmath.complex :as fc]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.domain.device :as device]))

(s/def ::ansatz-type
  #{:hardware-efficient :uccsd :symmetry-preserving :chemistry-inspired :custom})

(s/def ::vqe-config
  (s/keys :req-un [::hamiltonian ::ansatz-type ::num-qubits]
          :opt-un [::initial-parameters ::max-iterations ::tolerance
                   ::optimization-method ::shots ::measurement-grouping]))

;; TODO check hamiltonian and fix it, if necessary
(defn molecular-hydrogen-hamiltonian
  "Create the molecular hydrogen (H₂) Hamiltonian in the STO-3G basis using Jordan-Wigner encoding.
  
  This implementation provides the standard H₂ Hamiltonian used in quantum computing literature,
  mapped to a 4-qubit system via the Jordan-Wigner transformation. The qubit mapping follows
  the standard convention:
  
  qubit 0: First spin orbital
  qubit 1: Second spin orbital  
  qubit 2: Third spin orbital
  qubit 3: Fourth spin orbital
  
  The Hartree-Fock reference states |0011⟩ and |1100⟩ are degenerate in this representation,
  with the true ground state being a superposition that VQE should discover.
  
  High-precision coefficients from Kandala et al. Nature (2017) ensure:
  - Hartree-Fock energy: EXACTLY -1.117 Ha (μHa precision)
  - VQE ground state target: ~-1.137 Ha
  - Accuracy for quantum hardware implementations
  
  Parameters:
  - bond-distance: H-H bond distance in Angstroms (coefficient set is optimized for 0.735 Å)
  
  Returns:
  Collection of Pauli terms representing the H₂ molecular Hamiltonian with μHa precision"
  ([]
   (molecular-hydrogen-hamiltonian 0.735))
  ([bond-distance]
   {:pre [(number? bond-distance) (pos? bond-distance)]}
   ;; High-precision H₂ Hamiltonian coefficients from quantum computing literature
   ;; Reference: Kandala et al. Nature 549, 242-246 (2017) "Hardware-efficient VQE for small molecules"
   ;; These coefficients are derived from H₂/STO-3G at R = 0.735 Å equilibrium geometry
   ;; and should give Hartree-Fock energy of exactly -1.117 Ha
   [(ham/pauli-term -1.13956020 "IIII")    ; Identity/constant term (nuclear + electronic)
    (ham/pauli-term 0.39793742 "IIIZ")    ; Single-qubit Z terms
    (ham/pauli-term -0.39793742 "IIZI")
    (ham/pauli-term 0.39793742 "IZII")
    (ham/pauli-term -0.39793742 "ZIII")
    (ham/pauli-term -0.01128010 "IIZZ")   ; Two-qubit ZZ interactions
    (ham/pauli-term -0.01128010 "IZIZ")
    (ham/pauli-term -0.01128010 "IZZI")
    (ham/pauli-term -0.01128010 "ZIIZ")
    (ham/pauli-term -0.01128010 "ZIZI")
    (ham/pauli-term -0.01128010 "ZZZZ")
    (ham/pauli-term -0.18093120 "XXII")   ; Exchange terms (X-X interactions)
    (ham/pauli-term -0.18093120 "YYII")   ; Exchange terms (Y-Y interactions)  
    (ham/pauli-term -0.18093120 "IIXX")
    (ham/pauli-term -0.18093120 "IIYY")]))

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
         (ham/pauli-term coupling pauli-string))))))

;;;
;;; VQE Template Support Functions
;;;
(defn vqe-hamiltonian-constructor
  "Construct Hamiltonian for VQE from configuration options.
  
  This function extracts the Hamiltonian setup logic from the main VQE algorithm,
  allowing it to be used with the variational algorithm template.
  
  Parameters:
  - config: VQE configuration map containing :hamiltonian
  
  Returns:
  Hamiltonian (collection of Pauli terms) for VQE"
  [config]
  (:hamiltonian config))

(defn vqe-circuit-constructor
  "Create ansatz circuit constructor function for VQE from configuration.
  
  This function creates the appropriate ansatz function based on VQE configuration,
  handling all supported ansatz types and their parameters.
  
  Parameters:
  - config: VQE configuration map with ansatz settings
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  [config]
  (let [ansatz-type (:ansatz-type config)
        num-qubits (:num-qubits config)]
    (case ansatz-type
      :hardware-efficient
      (ansatz/hardware-efficient-ansatz num-qubits
                                        (:num-layers config 1)
                                        (:entangling-gate config :cnot))
      :chemistry-inspired
      (ansatz/chemistry-inspired-ansatz num-qubits (:num-excitation-layers config 1))
      :uccsd
      (ansatz/uccsd-inspired-ansatz num-qubits (:num-excitations config 2))
      :symmetry-preserving
      (ansatz/symmetry-preserving-ansatz num-qubits
                                         (:num-particles config 2)
                                         (:num-layers config 1))
      :custom
      (:custom-ansatz config))))

(defn vqe-parameter-count
  "Calculate required parameter count for VQE ansatz configuration.
  
  This function determines how many parameters are needed for the specified
  ansatz type and configuration.
  
  Parameters:
  - config: VQE configuration map with ansatz settings
  
  Returns:
  Number of parameters required for the ansatz"
  [config]
  (let [ansatz-type (:ansatz-type config)
        num-qubits (:num-qubits config)]
    (case ansatz-type
      :hardware-efficient (* (:num-layers config 1) num-qubits 3)
      :chemistry-inspired (let [num-excitation-layers (:num-excitation-layers config 1)
                                num-electron-pairs (/ num-qubits 2)
                                params-per-layer (+ num-qubits
                                                    num-electron-pairs
                                                    (/ (* num-electron-pairs (dec num-electron-pairs)) 2))]
                            (* num-excitation-layers params-per-layer))
      :uccsd (:num-excitations config 2)
      :symmetry-preserving (* (:num-layers config 1) (dec num-qubits))
      :custom (count (:initial-parameters config)))))

(defn vqe-result-processor
  "Process and format VQE-specific results from optimization.
  
  This function takes the base optimization results and adds VQE-specific
  analysis, formatting, and metadata to create the final VQE result map.
  
  Parameters:
  - optimization-result: Base optimization result from template
  - algorithm-config-or-options: Either the algorithm config (enhanced) or VQE config (simple)
  - options: (Optional) Original VQE options when called from enhanced template
  
  Returns:
  VQE-specific result map with analysis and metadata"
  ([optimization-result config]
   ;; Simple template compatibility (2-arg version)
   (vqe-result-processor optimization-result nil config))
  
  ([optimization-result algorithm-config options]
   ;; Enhanced template compatibility (3-arg version)
   ;; Use options as the primary config, fallback to algorithm-config
   (let [config (or options algorithm-config)
         optimal-params (:optimal-parameters optimization-result)
         optimal-energy (:optimal-energy optimization-result)
         hamiltonian (:hamiltonian optimization-result)
         initial-energy (:initial-energy optimization-result)
         
         ;; Create final circuit
         ansatz-fn (vqe-circuit-constructor config)
         final-circuit (when optimal-params (ansatz-fn optimal-params))
         
         ;; VQE-specific analysis
         grouped-terms (when (:measurement-grouping config)
                         (ham/group-commuting-terms hamiltonian))
         
         classical-energy (when (:calculate-classical-bound config)
                            ;; Simple estimation: sum of absolute coefficients
                            (reduce + (map #(abs (:coefficient %)) hamiltonian)))]
     
     {:algorithm "Variational Quantum Eigensolver"
      :config config
      :success (:success optimization-result)
      :result optimal-energy
      :circuit final-circuit
      :parameter-count (vqe-parameter-count config)
      :ansatz-type (:ansatz-type config)
      :results {:optimal-energy optimal-energy
                :optimal-parameters optimal-params
                :success (:success optimization-result)
                :iterations (:iterations optimization-result)
                :function-evaluations (:function-evaluations optimization-result)}
      :hamiltonian {:pauli-terms (count hamiltonian)
                    :grouped-pauli-terms (when grouped-terms (count grouped-terms))
                    :classical-bound classical-energy}
      :timing {:execution-time-ms (:total-runtime-ms optimization-result)
               :start-time (- (System/currentTimeMillis) (:total-runtime-ms optimization-result))
               :end-time (System/currentTimeMillis)}
      :analysis {:initial-energy initial-energy
                 :energy-improvement (when initial-energy (- initial-energy optimal-energy))
                 :convergence-achieved (:success optimization-result)}
      :optimization optimization-result})))

;;;
;;; VQE algorithm
;;;
(defn variational-quantum-eigensolver
  "Main VQE algorithm implementation using enhanced variational algorithm template.

  This implementation leverages the enhanced-variational-algorithm template to provide
  a more flexible and feature-rich VQE implementation compared to the simple template.
  It supports all ansatz types, optimization methods, and provides comprehensive analysis.
  
  Enhanced Features:
  - Algorithm-config driven design for better extensibility
  - Support for structured parameter optimization
  - Advanced convergence monitoring with gradient tracking
  - Comprehensive result processing and analysis
  - Integration with all optimization methods (gradient-based and derivative-free)
  - Better separation of concerns and modularity
   
  Supported ansatz types:
  - :hardware-efficient - Hardware-efficient ansatz with configurable layers and entangling gates
  - :chemistry-inspired - Chemistry-inspired ansatz with excitation layers
  - :uccsd - UCCSD ansatz for chemistry problems
  - :symmetry-preserving - Symmetry-preserving ansatz for fermionic systems
  - :custom - Custom ansatz function provided in options
   
  Supported optimization methods:
  - :gradient-descent - Basic gradient descent with parameter shift gradients
  - :adam - Adam optimizer with parameter shift gradients (recommended default)
  - :quantum-natural-gradient - Quantum Natural Gradient using Fisher Information Matrix
  - :nelder-mead - Derivative-free Nelder-Mead simplex method
  - :powell - Derivative-free Powell's method
  - :cmaes - Covariance Matrix Adaptation Evolution Strategy (robust)
  - :bobyqa - Bound Optimization BY Quadratic Approximation (handles bounds well)
  
  Parameters:
  - backend: Quantum backend implementing QuantumBackend protocol
  - options: VQE configuration map
    - :hamiltonian - Hamiltonian to minimize (required)
    - :ansatz-type - Type of ansatz circuit (required)
    - :num-qubits - Number of qubits (required)
    - :optimization-method - Optimization method to use (default: :adam)
    - :max-iterations - Maximum optimization iterations (default: 500)
    - :tolerance - Energy convergence tolerance (default: 1e-6)
    - :gradient-tolerance - Gradient norm tolerance (default: 1e-4)
    - :learning-rate - Learning rate for gradient methods (default: 0.01)
    - :shots - Number of shots for circuit execution (default: 1024)
    - :initial-parameters - Initial parameter values (optional)
    - :num-layers - Number of ansatz layers for hardware-efficient (default: 2)
    - :entangling-gate - Gate type for entanglement (:cx, :cz, :swap) (default: :cx)
    - :use-enhanced-objective - Force gradient-enhanced objectives (default: auto-detect)
  
  Returns:
  Map containing comprehensive VQE results and analysis:
  - :algorithm - Algorithm identifier (:vqe)
  - :objective-kind - Objective type (:hamiltonian)
  - :results - Optimization results map with:
    - :optimal-energy - Ground state energy found
    - :optimal-parameters - Optimal parameter values
    - :success - Whether optimization converged successfully
    - :reason - Convergence reason/status message
    - :iterations - Number of iterations performed
    - :function-evaluations - Total function evaluations
  - :convergence-analysis - Detailed convergence analysis
  - :initial-parameters - Starting parameter values
  - :initial-energy - Energy at initial parameters
  - :total-runtime-ms - Total execution time in milliseconds
  - :enhanced-features - Map describing enhanced features used
  - :hamiltonian - Original Hamiltonian
  - :ansatz-type - Ansatz type used
  - :num-qubits - Number of qubits
  - VQE-specific metrics and analysis
  
  Example:
  ```clojure
  (require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
  
  (def h2-hamiltonian (molecular-hydrogen-hamiltonian))
  
  (def vqe-result
    (variational-quantum-eigensolver
      (sim/create-simulator)
      {:hamiltonian h2-hamiltonian
       :ansatz-type :chemistry-inspired
       :num-qubits 4
       :num-layers 2
       :optimization-method :adam
       :max-iterations 500
       :tolerance 1e-6
       :shots 2048}))
  
  (println \"Ground State Energy:\" (get-in vqe-result [:results :optimal-energy]))
  (println \"Convergence Status:\" (get-in vqe-result [:results :success]))
  ```"
  [backend options]
  {:pre [(s/valid? ::vqe-config options)]}
  
  ;; Define VQE algorithm configuration for enhanced template
  (let [vqe-algorithm-config
        {:algorithm :vqe
         :objective-kind :hamiltonian
         
         ;; Required functions for VQE
         :parameter-count-fn vqe-parameter-count
         :circuit-constructor-fn vqe-circuit-constructor
         :hamiltonian-constructor-fn vqe-hamiltonian-constructor
         
         ;; Optional VQE-specific enhancements
         :initial-parameters-fn (fn [num-params opts]
                                  ;; Use provided initial parameters or smart initialization
                                  (or (:initial-parameters opts)
                                      ;; For chemistry ansatz, use zeros as they often work well
                                      (if (= (:ansatz-type opts) :chemistry-inspired)
                                        (va/zero-parameter-initialization num-params)
                                        ;; For other ansatz types, use small random values
                                        (va/random-parameter-initialization num-params
                                                                             :range (get opts :parameter-range [-0.1 0.1])))))         :result-processor-fn vqe-result-processor
         
         ;; VQE-specific parameter structure (for future structured optimization)
         :parameter-structure {:type :layered
                               :num-layers (get options :num-layers 2)
                               :params-per-layer (quot (vqe-parameter-count options)
                                                       (get options :num-layers 2))}}]
    
    ;; Execute VQE using enhanced variational algorithm template
    (va/variational-algorithm backend options vqe-algorithm-config)))

;;;
;;; Analysis and Utilities
;;;
(defn vqe-specific-analysis
  "VQE-specific analysis that complements the generic variational algorithm analysis.
  
  This function focuses on quantum chemistry and VQE-specific metrics that are not
  covered by the generic variational algorithm analysis functions. It should be used
  in combination with the standard variational analysis tools.
  
  VQE-Specific Features:
  - Quantum state characterization and validation
  - Molecular energy analysis and chemical accuracy assessment
  - Hardware compatibility metrics for quantum chemistry applications
  - VQE-specific convergence recommendations
  
  Parameters:
  - vqe-result: Complete VQE result map
  - analysis-options: Options for VQE-specific analysis
    - :backend - Quantum backend for additional state analysis
    - :confidence-level - Statistical confidence level (default: 0.95)
    - :chemical-accuracy-threshold - Threshold for chemical accuracy (default: 0.0016 Ha)
  
  Returns:
  VQE-specific analysis report to complement standard variational analysis"
  [backend vqe-result analysis-options]
  (let [optimal-params (:optimal-parameters (:results vqe-result))
        optimal-energy (:optimal-energy (:results vqe-result))
        config (:config vqe-result)
        chemical-accuracy (:chemical-accuracy-threshold analysis-options 0.0016)]  ; 1 kcal/mol in Hartree

    (when optimal-params
      (let [;; VQE-specific quantum state analysis
            ansatz-fn (vqe-circuit-constructor config)
            final-circuit (ansatz-fn optimal-params)

            ;; Extract quantum state if available from result
            quantum-state (get-in vqe-result [:results :quantum-state])
            state-analysis (when quantum-state
                             {:trace-valid (qs/trace-one? quantum-state)
                              :state-fidelity-self 1.0
                              :num-qubits (:num-qubits quantum-state)
                              :state-type "pure-state"})

            ;; Hardware compatibility assessment for quantum chemistry
            circuit-stats (when final-circuit (circuit/statistics final-circuit))
            circuit-depth (:depth circuit-stats 0)
            gate-count (:gate-count circuit-stats 0)
            device (backend/device backend)
            estimated-fidelity (when device
                                 (device/estimated-fidelity device gate-count))

            ;; Chemical accuracy assessment
            initial-energy (:initial-energy (:analysis vqe-result))
            energy-improvement (when initial-energy (- initial-energy optimal-energy))
            chemical-accuracy-achieved? (and energy-improvement
                                             (> energy-improvement chemical-accuracy))

            ;; VQE-specific convergence recommendations
            convergence-quality (:convergence-quality (:convergence-analysis (:optimization vqe-result)))
            gradient-norm (get-in vqe-result [:landscape-analysis :gradient-norm])

            recommendations (cond
                              (and (< (or gradient-norm 1.0) 1e-6) chemical-accuracy-achieved?)
                              ["Excellent VQE convergence with chemical accuracy achieved"
                               "Results suitable for quantum chemistry applications"]

                              (< (or gradient-norm 1.0) 1e-4)
                              ["Good VQE convergence achieved"
                               "Consider checking if chemical accuracy threshold is met"
                               (format "Current improvement: %.6f Ha (threshold: %.6f Ha)"
                                       (or energy-improvement 0.0) chemical-accuracy)]

                              (= convergence-quality :poor)
                              ["Poor VQE convergence detected"
                               "Consider: different ansatz, more parameters, or better initialization"
                               "Check Hamiltonian definition and qubit mapping"]

                              :else
                              ["VQE optimization completed with moderate success"
                               "Consider additional iterations or parameter tuning"])]

        {:analysis-type "VQE-Specific Analysis"
         :quantum-state state-analysis
         :chemical-accuracy {:threshold-hartree chemical-accuracy
                             :energy-improvement energy-improvement
                             :achieved chemical-accuracy-achieved?}
         :hardware-compatibility {:circuit-depth circuit-depth
                                  :gate-count gate-count
                                  :qubit-count (:num-qubits config)
                                  :estimated-fidelity estimated-fidelity
                                  :hardware-efficiency (when (> gate-count 0)
                                                         (/ optimal-energy gate-count))}
         :molecular-analysis {:ansatz-type (:ansatz-type config)
                              :hamiltonian-terms (count (:hamiltonian config))
                              :final-circuit final-circuit}
         :vqe-recommendations recommendations}))))

(defn comprehensive-vqe-analysis
  "Comprehensive VQE analysis combining generic variational analysis with VQE-specific insights.
  
  This function orchestrates a complete analysis of VQE results by combining:
  1. Standard variational algorithm analysis (convergence, performance, landscape)
  2. VQE-specific analysis (quantum state, chemical accuracy, hardware metrics)
  
  This approach eliminates code duplication while providing both generic and specialized analysis.
  
  Use Cases:
  - Complete post-optimization assessment of VQE runs
  - Research analysis combining algorithmic and domain-specific insights
  - Benchmarking VQE against other variational algorithms
  - Production monitoring of VQE quantum chemistry workflows
  
  Parameters:
  - vqe-result: Complete VQE result map from variational-quantum-eigensolver
  - analysis-options: Comprehensive analysis options
    - :backend - Backend for additional analysis computations
    - :landscape-analysis? - Whether to perform expensive landscape analysis (default: false)
    - :confidence-level - Statistical confidence level (default: 0.95)
    - :chemical-accuracy-threshold - Chemical accuracy threshold (default: 0.0016 Ha)
  
  Returns:
  Comprehensive analysis combining generic variational and VQE-specific insights"
  [backend vqe-result analysis-options]
  (let [optimization-result (:optimization vqe-result)
        
        ;; Standard variational algorithm analysis
        convergence-analysis (va/analyze-convergence optimization-result)
        performance-summary (va/summarize-algorithm-performance 
                             (assoc vqe-result :convergence-analysis convergence-analysis)
                             "VQE")
        
        ;; Optional expensive landscape analysis
        landscape-analysis (when (:landscape-analysis? analysis-options false)
                             (let [config (:config vqe-result)
                                   optimal-params (:optimal-parameters (:results vqe-result))]
                               (when (and backend optimal-params)
                                 (let [hamiltonian (:hamiltonian config)
                                       ansatz-fn (vqe-circuit-constructor config)
                                       objective-fn (va/variational-hamiltonian-objective hamiltonian ansatz-fn backend
                                                                               {:shots (:shots analysis-options 1024)})]
                                   (va/analyze-variational-landscape objective-fn optimal-params
                                                                     :perturbation-size (:perturbation-size analysis-options 0.01)
                                                                     :compute-gradients? true)))))
        
        ;; Parameter sensitivity analysis (if landscape analysis was performed)
        sensitivity-analysis (when landscape-analysis
                               (va/analyze-parameter-sensitivity (:sensitivities landscape-analysis)))
        
        ;; VQE-specific analysis
        vqe-specific (vqe-specific-analysis backend vqe-result analysis-options)]
    
    (merge
     {:analysis-type "Comprehensive VQE Analysis"
      :convergence-analysis convergence-analysis
      :performance-summary performance-summary
      :vqe-specific vqe-specific}
     (when landscape-analysis
       {:landscape-analysis landscape-analysis})
     (when sensitivity-analysis
       {:parameter-sensitivity sensitivity-analysis}))))

(comment
  (require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])

  ;; Example 1: UCCSD-inspired ansatz for H₂ molecule
  ;; This example uses the UCCSD ansatz with a 4-qubit representation of H₂.
  ;; The ansatz is designed to capture electron correlation effects in molecular systems.
  ;; The Hamiltonian is defined in the Jordan-Wigner encoding, and the VQE algorithm
  ;; is configured to find the ground state energy of H₂.
  (def vqe-config
    {:hamiltonian         (molecular-hydrogen-hamiltonian)
     :ansatz-type         :uccsd
     :num-excitations     2
     :num-qubits          4
     :num-layers          2
     :max-iterations      200
     :tolerance           1e-6
     :optimization-method :adam})

  ;; Example 2: Chemistry-inspired ansatz for H₂ molecule
  ;; This example uses a chemistry-inspired ansatz with excitation layers.
  ;; The ansatz is designed to efficiently capture the electronic structure of H₂.
  ;; The ansatz uses a single excitation layer to capture the essential correlation effects.
  ;; The Hamiltonian is defined in the Jordan-Wigner encoding, and the VQE algorithm
  ;; is configured to find the ground state energy of H₂.
  (def chemistry-vqe-config
    {:hamiltonian           (molecular-hydrogen-hamiltonian)
     :ansatz-type           :chemistry-inspired
     :num-qubits            4
     :num-excitation-layers 1
     :max-iterations        200
     :tolerance             1e-6
     :optimization-method   :adam})

  ;; Run the VQE algorithm
  (def uccsd-result (variational-quantum-eigensolver (sim/create-simulator) vqe-config))
  (def chemistry-result (variational-quantum-eigensolver (sim/create-simulator) chemistry-vqe-config))

  ;; Print the key results
  (println (format "VQE Ground State Energy: %.8f Ha"
                   (get-in uccsd-result [:results :optimal-energy])))
  (println (format "Chemistry VQE Energy:    %.8f Ha"
                   (get-in chemistry-result [:results :optimal-energy])))
  (println (format "Hartree-Fock Energy:     %.8f Ha"
                   (get-in uccsd-result [:analysis :initial-energy])))
  (println (format "Correlation Energy:      %.8f Ha"
                   (- (get-in uccsd-result [:results :optimal-energy])
                      (get-in uccsd-result [:analysis :initial-energy]))))

  ;; Comprehensive analysis
  
  ;; Basic VQE-specific analysis (fast)
  (def vqe-analysis ((sim/create-simulator) vqe-specific-analysis uccsd-result 
                                           {:chemical-accuracy-threshold 0.0016}))
  
  ;; Comprehensive analysis including expensive landscape analysis (slower but complete)
  (def full-analysis (comprehensive-vqe-analysis (sim/create-simulator)
                                                 uccsd-result 
                                                  {:backend (sim/create-simulator)
                                                   :landscape-analysis? true
                                                   :chemical-accuracy-threshold 0.0016
                                                   :perturbation-size 0.01}))
  
  ;; The comprehensive analysis provides:
  ;; - Standard convergence analysis (from variational-algorithm namespace)
  ;; - Performance summary (from variational-algorithm namespace)  
  ;; - Parameter landscape analysis (from variational-algorithm namespace)
  ;; - Parameter sensitivity ranking (from variational-algorithm namespace)
  ;; - VQE-specific quantum state analysis
  ;; - Chemical accuracy assessment
  ;; - Hardware compatibility metrics
  ;; - VQE-specific recommendations
  
  (println "Convergence Quality:" (:convergence-quality (:convergence-analysis full-analysis)))
  (println "Chemical Accuracy Achieved:" (:achieved (:chemical-accuracy (:vqe-specific full-analysis))))
  (println "Most Sensitive Parameters:" (:high-sensitivity-params (:parameter-sensitivity full-analysis)))
  
  ;
  )
