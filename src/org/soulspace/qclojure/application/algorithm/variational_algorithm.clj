(ns org.soulspace.qclojure.application.algorithm.variational-algorithm
  "Common infrastructure for variational quantum algorithms (VQE, QAOA, etc.).
  
  This namespace provides reusable components that are shared between different
  variational quantum algorithms, reducing code duplication and ensuring
  consistent behavior across algorithms.
  
  Key Features:
  - Generic objective function creation
  - Common optimization method dispatching
  - Shared algorithm structure templates
  - Common result analysis and processing
  - Parameter initialization strategies
  
  Design Principles:
  - Algorithm-agnostic: Works with any parameterized quantum circuit
  - Composable: Functions can be mixed and matched as needed
  - Consistent: Uniform interfaces and error handling
  - Extensible: Easy to add new optimization methods or analysis functions"
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.application.algorithm.optimization :as qopt]
            [org.soulspace.qclojure.application.backend :as backend]))

;;;
;;; Specs for common variational algorithm components
;;;
(s/def ::objective-kind #{:hamiltonian :classification :regression
                          :combinatorial :custom})
(s/def ::circuit-constructor-fn fn?)
(s/def ::parameter-count-fn fn?)
(s/def ::initial-parameters-fn fn?)
(s/def ::execution-plan-fn fn?)
(s/def ::loss-fn fn?)
(s/def ::result-processor-fn fn?)
(s/def ::hamiltonian-constructor-fn fn?)
(s/def ::dataset-fn fn?)
(s/def ::batch-sampler-fn fn?)
(s/def ::prediction-extractor-fn fn?)
(s/def ::gradient-fn fn?)
(s/def ::early-stopping-fn fn?)
(s/def ::regularization-fn fn?)
(s/def ::constraints-fn fn?)
(s/def ::parameter-structure map?)

;; Enhanced algorithm config spec
(s/def ::algorithm keyword?)
(s/def ::enhanced-algorithm-config
  (s/keys :req-un [::algorithm ::objective-kind
                   ::parameter-count-fn ::circuit-constructor-fn]
          :opt-un [::initial-parameters-fn ::execution-plan-fn
                   ::loss-fn ::result-processor-fn
                   ::hamiltonian-constructor-fn ::dataset-fn
                   ::batch-sampler-fn ::prediction-extractor-fn
                   ::gradient-fn ::early-stopping-fn
                   ::regularization-fn ::constraints-fn
                   ::parameter-structure]))

;;;
;;; Common Parameter Initialization Strategies
;;;
(defn random-parameter-initialization
  "Generate random initial parameters for variational algorithms.
  
  Parameters:
  - num-parameters: Number of parameters to initialize
  - range: Parameter range as [min max] (default: [-0.1 0.1])
  
  Returns:
  Vector of random initial parameters"
  [num-parameters & {:keys [range] :or {range [-0.1 0.1]}}]
  {:pre [(pos-int? num-parameters) (= 2 (count range))]}
  (let [[min-val max-val] range
        param-range (- max-val min-val)]
    (vec (repeatedly num-parameters
                     #(+ min-val (* (rand) param-range))))))

(defn zero-parameter-initialization
  "Generate zero initial parameters for variational algorithms.
  
  Parameters:
  - num-parameters: Number of parameters to initialize
  
  Returns:
  Vector of zero initial parameters"
  [num-parameters]
  {:pre [(pos-int? num-parameters)]}
  (vec (repeat num-parameters 0.0)))

;;;
;;; Common Result Analysis Functions
;;;
(defn analyze-convergence
  "Comprehensive convergence analysis for variational quantum algorithms.
  
  This function provides unified convergence analysis by examining both the 
  optimization result metadata and the detailed optimization history. It serves
  as the primary convergence analysis tool for all variational algorithms.
  
  Use Cases:
  - Post-optimization assessment of convergence quality
  - Debugging optimization problems and parameter tuning
  - Comparing different optimization methods or hyperparameters
  - Research analysis of algorithm behavior across different problems
  
  Parameters:
  - optimization-result: Complete result map from optimization containing:
    - :success, :reason, :iterations, :function-evaluations (metadata)
    - :convergence-history or :history (energy trajectory data)
    - :optimal-energy, :optimal-parameters (final results)
  
  Returns:
  Comprehensive map with convergence analysis including:
  - Basic convergence status and metadata
  - Energy improvement metrics and statistics
  - Convergence rate and trajectory analysis
  - Gradient-based convergence indicators (when available)"
  [optimization-result]
  (let [;; Extract metadata
        success (:success optimization-result false)
        reason (:reason optimization-result)
        iterations (:iterations optimization-result 0)
        function-evals (:function-evaluations optimization-result 0)
        
        ;; Extract energy history from multiple possible sources
        convergence-history (:convergence-history optimization-result [])
        detailed-history (:history optimization-result [])
        energy-history (if (seq convergence-history)
                         convergence-history
                         (map :energy detailed-history))
        
        ;; Basic convergence info
        base-analysis {:converged success
                       :final-reason reason
                       :total-iterations iterations
                       :function-evaluations function-evals}]
    
    (if (seq energy-history)
      ;; Enhanced analysis with energy trajectory
      (let [initial-energy (first energy-history)
            final-energy (last energy-history)
            energy-improvement (- initial-energy final-energy)
            relative-improvement (when (not= initial-energy 0.0)
                                   (/ energy-improvement (abs initial-energy)))
            num-energy-points (count energy-history)
            
            ;; Statistical analysis
            energy-mean (/ (reduce + energy-history) num-energy-points)
            energy-std (when (> num-energy-points 1)
                         (let [variances (map #(* (- % energy-mean) (- % energy-mean)) energy-history)]
                           (Math/sqrt (/ (reduce + variances) num-energy-points))))
            
            ;; Trajectory analysis
            monotonic-decrease? (every? (fn [[e1 e2]] (<= e2 e1))
                                        (partition 2 1 energy-history))
            convergence-rate (when (> num-energy-points 1)
                               (/ energy-improvement num-energy-points))
            
            ;; Gradient analysis (from detailed history if available)
            final-gradient-norm (when-let [last-step (last detailed-history)]
                                  (when-let [gradients (:gradients last-step)]
                                    (Math/sqrt (reduce + (map #(* % %) gradients)))))
            
            ;; Quality assessment
            convergence-quality (cond
                                  (< energy-improvement 1e-8) :poor
                                  (< energy-improvement 1e-6) :fair  
                                  (< energy-improvement 1e-4) :good
                                  :else :excellent)]
        
        (merge base-analysis
               {:initial-energy initial-energy
                :final-energy final-energy
                :energy-improvement energy-improvement
                :relative-improvement relative-improvement
                :energy-std energy-std
                :monotonic-decrease? monotonic-decrease?
                :convergence-rate convergence-rate
                :final-gradient-norm final-gradient-norm
                :convergence-quality convergence-quality
                :energy-trajectory-length num-energy-points}))
      
      ;; Basic analysis without energy history
      base-analysis)))

(defn summarize-algorithm-performance
  "Create a high-level performance summary for variational quantum algorithms.
  
  This function provides a concise, standardized summary focused on practical
  performance metrics and overall algorithm assessment. It's designed for
  benchmarking, reporting, and quick performance comparison across runs.
  
  Use Cases:
  - Benchmarking different algorithms (VQE vs QAOA) or configurations
  - Performance reporting for research papers or technical documentation
  - Quick assessment of whether an optimization run was successful
  - Comparative analysis across different quantum backends or hardware
  - Automated performance monitoring in production quantum workflows
  
  Parameters:
  - algorithm-result: Complete result map from algorithm execution containing:
    - :convergence-analysis (from analyze-convergence)
    - :optimal-energy, :success, :iterations (optimization results)
    - :total-runtime-ms (timing information)
  - algorithm-name: Name of the algorithm (e.g., 'VQE', 'QAOA', 'QAOA-MaxCut')
  
  Returns:
  Standardized performance summary with:
  - Algorithm identification and success status
  - Key performance metrics (energy, runtime, efficiency)
  - Qualitative assessments (convergence quality, efficiency score)"
  [algorithm-result algorithm-name]
  (let [convergence (:convergence-analysis algorithm-result)
        timing (:total-runtime-ms algorithm-result 0)
        iterations (:iterations algorithm-result 0)]

    {:algorithm algorithm-name
     :success (:success algorithm-result false)
     :final-energy (:optimal-energy algorithm-result)
     :iterations iterations
     :runtime-seconds (/ timing 1000.0)
     :convergence-quality (or (:convergence-quality convergence)
                              ;; Fallback quality assessment
                              (cond
                                (:converged convergence) :excellent
                                (and (> iterations 50)
                                     (< (:final-energy algorithm-result Double/POSITIVE_INFINITY) 1e-3)) :good
                                (> iterations 10) :fair
                                :else :poor))
     :efficiency-score (when (and (> timing 0) (> iterations 0))
                         (/ 1000.0 (/ timing iterations)))  ; iterations per second
     :energy-improvement (:energy-improvement convergence)
     :function-evaluations (:function-evaluations convergence 0)}))

;;;
;;; Convergence Monitoring
;;;
(defn convergence-monitor
  "Monitor variational algorithm convergence with sophisticated stopping criteria.
  
  This function tracks optimization progress and implements intelligent
  stopping criteria based on energy convergence, gradient norms, and 
  parameter stability. Works with any variational quantum algorithm.
  
  Parameters:
  - history: Vector of optimization steps {:iteration :energy :gradients :parameters}
  - options: Convergence options map
    - :tolerance - Energy convergence tolerance (default: 1e-6)
    - :gradient-tolerance - Gradient norm tolerance (default: 1e-4)
    - :min-iterations - Minimum iterations before convergence checking (default: 10)
    - :patience - Window size for convergence analysis (default: 20)
  
  Returns:
  Map with convergence analysis and recommendations"
  [history options]
  (let [tolerance (:tolerance options 1e-6)
        gradient-tolerance (:gradient-tolerance options 1e-4)
        min-iterations (:min-iterations options 10)
        patience (:patience options 20)

        current-iteration (count history)
        recent-history (take-last (min patience current-iteration) history)]

    (when (seq history)
      (let [current-step (last history)
            current-energy (:energy current-step)
            current-gradients (:gradients current-step)

            ;; Energy-based convergence criteria
            energy-converged? (and (>= current-iteration min-iterations)
                                   (>= (count recent-history) 2)
                                   (let [energy-changes (map (fn [[step1 step2]]
                                                               (abs (- (:energy step2) (:energy step1))))
                                                             (partition 2 1 recent-history))]
                                     (every? #(< % tolerance) energy-changes)))

            ;; Gradient-based convergence criteria  
            gradient-norm (when current-gradients
                            (Math/sqrt (reduce + (map #(* % %) current-gradients))))
            gradient-converged? (and gradient-norm (< gradient-norm gradient-tolerance))

            ;; Parameter stability analysis
            parameter-stable? (and (>= (count recent-history) 3)
                                   (let [param-changes (map (fn [[step1 step2]]
                                                              (let [params1 (:parameters step1)
                                                                    params2 (:parameters step2)]
                                                                (when (and params1 params2)
                                                                  (Math/sqrt (reduce + (map #(* % %)
                                                                                            (map - params2 params1)))))))
                                                            (partition 2 1 recent-history))]
                                     (every? #(and % (< % tolerance)) param-changes)))

            ;; Overall convergence assessment
            converged? (or energy-converged? gradient-converged? parameter-stable?)]

        {:converged converged?
         :energy-converged energy-converged?
         :gradient-converged gradient-converged?
         :parameter-stable parameter-stable?
         :current-energy current-energy
         :gradient-norm gradient-norm
         :iterations current-iteration
         :recommendation (cond
                           converged? :stop-converged
                           (> current-iteration (* 2 patience)) :stop-slow-progress
                           :else :continue)}))))

;;;
;;; Objective Function Creation
;;;
(defn variational-hamiltonian-objective
  "Create a generic objective function for variational quantum algorithms.
  
  This function provides a common interface for creating objective functions
  that work with both VQE and QAOA (and future variational algorithms). It
  abstracts the common pattern of:
  1. Convert parameters to quantum circuit
  2. Execute circuit on backend
  3. Extract Hamiltonian expectation value using result-specs
  4. Return energy for optimization
  
  The result extraction infrastructure handles backend capabilities transparently,
  so this always uses result-specs for consistent and efficient operation.
  
  Parameters:
  - hamiltonian: Hamiltonian to minimize (collection of Pauli terms)
  - circuit-construction-fn: Function that takes parameters and returns a circuit
  - backend: Quantum backend for circuit execution
  - execution-options: Options for circuit execution (shots, etc.)
  
  Returns:
  Function that takes parameters and returns energy expectation value
  
  Examples:
  ;; For VQE:
  (create-variational-objective h2-hamiltonian ansatz-fn backend options)
  
  ;; For QAOA:
  (create-variational-objective problem-hamiltonian 
                                (partial qaoa-ansatz-circuit problem-h mixer-h num-qubits)
                                backend options)"
  [hamiltonian circuit-construction-fn backend execution-options]
  {:pre [(ham/validate-hamiltonian hamiltonian)
         (fn? circuit-construction-fn)
         (map? execution-options)]}
  (let [result-specs {:result-specs {:hamiltonian hamiltonian}}
        run-options (merge execution-options result-specs)]
    (fn objective [parameters]
      (try
        (let [;; Ensure parameters is a vector (fastmath optimizers may pass ArraySeq)
              params-vec (if (vector? parameters) parameters (vec parameters))
              ;; Create circuit with current parameters
              circuit (circuit-construction-fn params-vec)
              ;; Execute circuit and extract Hamiltonian expectation via result-specs
              execution-result (backend/execute-circuit backend circuit run-options)
              results (:results execution-result)
              hamiltonian-result (:hamiltonian-result results)
              ;; Extract energy from hamiltonian result
              energy (:energy-expectation hamiltonian-result)]
          ;; Return the energy (real number to be minimized)
          energy)
        (catch Exception e
          ;; Consistent error handling: return high energy for failed evaluations
          (println "Variational objective evaluation failed:" (.getMessage e))
          Double/POSITIVE_INFINITY)))))

(defn gradient-based-variational-hamiltonian-objective
  "Create a variational objective function that provides gradients.
  
  This function creates an objective that computes both energy and gradients
  efficiently using the parameter shift rule. The gradient computation is integrated 
  with the result framework to enable sophisticated gradient-based optimization methods.
  
  Parameters:
  - hamiltonian: Hamiltonian to minimize  
  - circuit-construction-fn: Function that takes parameters and returns a circuit
  - backend: Quantum backend for circuit execution
  - execution-options: Execution options (can include :parallel? for gradient computation)
  
  Returns:
  Function that takes parameters and returns {:energy value :gradients [...] :quantum-state state}"
  [hamiltonian circuit-construction-fn backend execution-options]
  {:pre [(ham/validate-hamiltonian hamiltonian) (fn? circuit-construction-fn)]}
  (let [base-result-specs {:result-specs {:hamiltonian hamiltonian
                                          :state-vector true}}
        run-options (merge execution-options base-result-specs)

        ;; Create standard objective function for gradient computation
        standard-objective (variational-hamiltonian-objective hamiltonian circuit-construction-fn backend run-options)]

    (fn enhanced-objective [parameters]
      (try
        (let [params-vec (if (vector? parameters) parameters (vec parameters))

              ;; Compute energy at current parameters using result framework
              base-circuit (circuit-construction-fn params-vec)
              base-result (backend/execute-circuit backend base-circuit run-options)
              base-energy (:energy-expectation (:hamiltonian-result (:results base-result)))
              base-state (:final-state (:results base-result))

              ;; Compute gradients using existing parameter shift implementation
              gradients (qopt/calculate-parameter-shift-gradient standard-objective params-vec
                                                                 {:parallel? (:parallel? execution-options true)})]

          {:energy base-energy
           :gradients gradients
           :quantum-state base-state})

        (catch Exception e
          (println "Gradient variational objective evaluation failed:" (.getMessage e))
          {:energy 1000.0
           :gradients (vec (repeat (count parameters) 0.0))})))))

;;;
;;; Generic Optimization Method Dispatching
;;;
(defn variational-optimization
  "Run optimization for variational quantum algorithms using specified method.
  
  This function provides a common interface for optimization that works with
  both VQE and QAOA. It handles the method dispatching and delegates to the
  appropriate optimization functions from the qopt namespace.
  
  Supported optimization methods:
  - :gradient-descent - Basic gradient descent with parameter shift gradients
  - :adam - Adam optimizer with parameter shift gradients (recommended default)
  - :quantum-natural-gradient - Quantum Natural Gradient using Fisher Information Matrix
  - :nelder-mead - Derivative-free Nelder-Mead simplex method
  - :powell - Derivative-free Powell's method
  - :cmaes - Covariance Matrix Adaptation Evolution Strategy (robust)
  - :bobyqa - Bound Optimization BY Quadratic Approximation (handles bounds well)
  - :gradient - Fastmath gradient-based optimizers
  - :lbfgsb - L-BFGS-B optimization
  
  Parameters:
  - objective-fn: Objective function to minimize
  - initial-parameters: Starting parameter values
  - options: Optimization options map
  
  Returns:
  Map with optimization results including convergence information"
  [objective-fn initial-parameters options]
  {:pre [(fn? objective-fn) (vector? initial-parameters) (map? options)]}
  (let [method (:optimization-method options :adam)]  ; Default to Adam
    (case method
      ;; Custom implementations with parameter shift rules
      :gradient-descent
      (qopt/gradient-descent-optimization objective-fn initial-parameters options)

      :adam
      (qopt/adam-optimization objective-fn initial-parameters options)

      :quantum-natural-gradient
      (qopt/quantum-natural-gradient-optimization objective-fn initial-parameters options)

      ;; Fastmath derivative-free optimizers
      (:nelder-mead :powell :cmaes :bobyqa)
      (qopt/fastmath-derivative-free-optimization method objective-fn initial-parameters options)

      ;; Fastmath gradient-based optimizers
      (:gradient :lbfgsb)
      (qopt/fastmath-gradient-based-optimization method objective-fn initial-parameters options)

      ;; Default fallback with helpful error message
      (throw (ex-info "Unknown optimization method for variational algorithm"
                      {:method method
                       :available-methods [:gradient-descent :adam :quantum-natural-gradient
                                           :nelder-mead :powell :cmaes :bobyqa :gradient :lbfgsb]
                       :algorithm-type :variational})))))

;;;
;;; Enhanced Optimization with Convergence Monitoring
;;;
(defn enhanced-variational-optimization
  "Run variational algorithm optimization with integrated convergence monitoring.
  
  This function wraps optimization methods with intelligent convergence monitoring,
  allowing for early stopping based on energy changes, gradient norms, and parameter
  stability. It tracks the full optimization history and provides detailed convergence
  analysis.
  
  Supports enhanced objectives that provide gradients, falling back to standard
  optimization for regular objective functions.
  
  Parameters:
  - objective-fn: Objective function to minimize (can be enhanced or standard)
  - initial-parameters: Starting parameter values  
  - options: Optimization options including convergence monitoring parameters
    - :optimization-method - Method to use (default: :adam)
    - :max-iterations - Maximum iterations (default: 500)
    - :tolerance - Energy convergence tolerance (default: 1e-6)
    - :gradient-tolerance - Gradient norm tolerance (default: 1e-4)
    - :min-iterations - Minimum iterations before convergence (default: 10)
    - :patience - Convergence analysis window (default: 20)
    - :learning-rate - Learning rate for gradient descent (default: 0.01)
  
  Returns:
  Map with optimization results and convergence analysis"
  [objective-fn initial-parameters options]
  (let [tolerance (:tolerance options 1e-6)
        gradient-tolerance (:gradient-tolerance options 1e-4)
        min-iterations (:min-iterations options 10)
        patience (:patience options 20)
        max-iterations (:max-iterations options 500)

        ;; Check if we have gradient-enhanced objective
        enhanced? (try
                    (let [test-result (objective-fn initial-parameters)]
                      (and (map? test-result) (:gradients test-result)))
                    (catch Exception _ false))

        convergence-options {:tolerance tolerance
                             :gradient-tolerance gradient-tolerance
                             :min-iterations min-iterations
                             :patience patience}]

    (if enhanced?
      ;; Use convergence monitoring with gradient-enhanced objective
      (loop [iteration 0
             current-params initial-parameters
             history []
             best-energy Double/POSITIVE_INFINITY
             best-params initial-parameters]

        (if (>= iteration max-iterations)
          {:success false
           :reason :max-iterations
           :optimal-energy best-energy
           :optimal-parameters best-params
           :iterations iteration
           :history history
           :convergence-analysis (convergence-monitor history convergence-options)}

          ;; Evaluate current point
          (let [current-result (objective-fn current-params)
                current-energy (:energy current-result)
                current-gradients (:gradients current-result)

                ;; Update history
                step-info {:iteration iteration
                           :energy current-energy
                           :gradients current-gradients
                           :parameters current-params}
                updated-history (conj history step-info)

                ;; Check convergence
                convergence-result (convergence-monitor updated-history convergence-options)
                converged? (:converged convergence-result)]

            (cond
              ;; Convergence achieved
              converged?
              {:success true
               :reason :converged
               :optimal-energy (if (< current-energy best-energy) current-energy best-energy)
               :optimal-parameters (if (< current-energy best-energy) current-params best-params)
               :iterations iteration
               :history updated-history
               :convergence-analysis convergence-result}

              ;; Slow progress detected
              (= (:recommendation convergence-result) :stop-slow-progress)
              {:success false
               :reason :slow-progress
               :optimal-energy best-energy
               :optimal-parameters best-params
               :iterations iteration
               :history updated-history
               :convergence-analysis convergence-result}

              ;; Continue optimization with simple gradient descent
              :else
              (let [learning-rate (:learning-rate options 0.01)
                    gradient-step (mapv #(* learning-rate %) current-gradients)
                    next-params (mapv - current-params gradient-step)

                    ;; Track best result
                    new-best-energy (if (< current-energy best-energy) current-energy best-energy)
                    new-best-params (if (< current-energy best-energy) current-params best-params)]

                (recur (inc iteration)
                       next-params
                       updated-history
                       new-best-energy
                       new-best-params))))))

      ;; Fallback to standard optimization without convergence monitoring for non-enhanced objectives
      (variational-optimization objective-fn initial-parameters options))))

;;;
;;; Algorithm Structure Template
;;;
(defn simple-variational-algorithm
  "Template for variational quantum algorithms like QAOA and VQE.
  
  This version supports gradient-enhanced objectives, advanced convergence
  monitoring, and optimization strategies required by algorithms like VQE.
  
  Parameters:
  - backend: Quantum backend for circuit execution
  - options: Algorithm options map including advanced optimization settings
    - :optimization-method - Optimization method (default: :adam)
    - :max-iterations - Maximum iterations (default: 500)
    - :tolerance - Convergence tolerance (default: 1e-6)
    - :gradient-tolerance - Gradient norm tolerance (default: 1e-4)
    - :use-enhanced-objective - Whether to use gradient-enhanced objectives (default: auto-detect)
    - :shots - Number of shots for execution (default: 1024)
    - Other algorithm-specific options
  - algorithm-fns: Map of algorithm-specific functions:
    - :hamiltonian-constructor - (fn [config] -> hamiltonian)
    - :circuit-constructor - (fn [config] -> circuit-construction-fn)
    - :parameter-count - (fn [config] -> number)
    - :result-processor - (fn [optimization-result config] -> final-result)
  
  Returns:
  Complete algorithm result map with enhanced analysis"
  [backend options algorithm-fns]
  {:pre [(map? options) (map? algorithm-fns)]}
  (let [{:keys [hamiltonian-constructor circuit-constructor
                parameter-count result-processor]} algorithm-fns

        ;; Timing
        start-time (System/currentTimeMillis)

        ;; Algorithm-specific construction
        hamiltonian (hamiltonian-constructor options)
        circuit-construction-fn (circuit-constructor options)
        num-params (parameter-count options)

        ;; Parameter initialization
        initial-parameters (or (:initial-parameters options)
                               (random-parameter-initialization num-params))

        ;; Objective creation with auto-detection of gradient support
        execution-options {:shots (:shots options 1024)}
        opt-method (:optimization-method options :adam)
        use-gradients? (or (:use-enhanced-objective options)
                           (contains? #{:gradient-descent :adam :quantum-natural-gradient} opt-method))

        objective-fn (if use-gradients?
                       (gradient-based-variational-hamiltonian-objective hamiltonian circuit-construction-fn backend execution-options)
                       (variational-hamiltonian-objective hamiltonian circuit-construction-fn backend execution-options))

        ;; Optimization with convergence monitoring
        optimization-options (merge options {:gradient-method :parameter-shift
                                             :ansatz-fn circuit-construction-fn
                                             :backend backend
                                             :exec-options execution-options})

        optimization-result (if use-gradients?
                              (enhanced-variational-optimization objective-fn initial-parameters optimization-options)
                              (variational-optimization objective-fn initial-parameters optimization-options))

        end-time (System/currentTimeMillis)

        ;; Analysis
        convergence-analysis (analyze-convergence optimization-result)

        ;; Calculate initial energy for analysis
        initial-result (objective-fn initial-parameters)
        initial-energy (if (map? initial-result) (:energy initial-result) initial-result)

        base-result (merge optimization-result
                           {:hamiltonian hamiltonian
                            :initial-parameters initial-parameters
                            :convergence-analysis convergence-analysis
                            :initial-energy initial-energy
                            :total-runtime-ms (- end-time start-time)
                            :enhanced-features {:gradient-enhanced use-gradients?
                                                :convergence-monitored true}})]

    ;; Algorithm-specific result processing
    (result-processor base-result options)))

;; TODO Enhanced implementation suitable for QAOA, VQE, VQC/QNN (non-hamiltonian optimization, ...)
;;      and future variational algorithms with advanced features.
;;      We leave the basic version above until this is fully implemented and tested.
;;      This is a template function that can be specialized via the algorithm-config map.
(defn enhanced-variational-algorithm
  "Enhanced template for variational quantum algorithms.
   
  This function provides a flexible and extensible framework for implementing
  various variational quantum algorithms, including VQE, QAOA, and VQC/QNN. It
  supports advanced features such as gradient-enhanced objectives, sophisticated
  convergence monitoring, and customizable optimization strategies.
  
  The algorithm-config map specifies how to construct and execute the algorithm:
  
  Required keys:
  - :algorithm - Algorithm identifier keyword (e.g., :vqe, :qaoa, :vqc, :qnn)
  - :objective-kind - Type of objective (:hamiltonian, :classification, :regression, :combinatorial, :custom)
  - :parameter-count-fn - (fn [options] -> int) Returns number of parameters needed
  - :circuit-constructor-fn - (fn [options] -> circuit-constructor) Creates circuit builder
  
  Optional keys for all algorithms:
  - :initial-parameters-fn - (fn [num-params options] -> params) Custom parameter initialization
  - :result-processor-fn - (fn [result config options] -> enhanced-result) Post-process results
  - :parameter-structure - Map describing parameter organization (for structured optimization)
  
  Optional keys for Hamiltonian-based algorithms (:hamiltonian, :combinatorial):
  - :hamiltonian-constructor-fn - (fn [options] -> hamiltonian) Creates problem Hamiltonian
  - :gradient-fn - (fn [objective params] -> gradients) Custom gradient computation
  
  Optional keys for ML algorithms (:classification, :regression):
  - :loss-fn - (fn [predictions labels] -> loss) Computes prediction loss
  - :dataset-fn - (fn [options] -> dataset) Extracts training/test data
  - :batch-sampler-fn - (fn [dataset batch-size] -> batches) Creates data batches
  - :prediction-extractor-fn - (fn [measurement-result] -> predictions) Extracts predictions
  - :early-stopping-fn - (fn [history options] -> should-stop?) Custom early stopping
  - :regularization-fn - (fn [parameters] -> penalty) Adds regularization penalty
  
  Optional keys for constrained optimization:
  - :constraints-fn - (fn [parameters] -> feasible?) Validates parameter constraints
  - :execution-plan-fn - (fn [circuit params backend] -> result) Custom execution strategy
  
  Parameters:
  - backend: Quantum backend for circuit execution
  - options: Algorithm execution options (optimization config, shots, etc.)
  - algorithm-config: Algorithm-specific configuration map (see above)
  
  Returns:
  Comprehensive result map with optimization results, convergence analysis, and algorithm-specific metadata"
  [backend options algorithm-config]
  {:pre [(s/valid? ::enhanced-algorithm-config algorithm-config)]}
  
  (let [;; Extract configuration
        objective-kind (:objective-kind algorithm-config)
        algorithm-name (:algorithm algorithm-config)
        
        ;; Timing
        start-time (System/currentTimeMillis)
        
        ;; Step 1: Parameter Initialization
        num-params ((:parameter-count-fn algorithm-config) options)
        initial-parameters (if-let [init-fn (:initial-parameters-fn algorithm-config)]
                             (init-fn num-params options)
                             (or (:initial-parameters options)
                                 (random-parameter-initialization num-params
                                                                   :range (get options :parameter-range [-0.1 0.1]))))
        
        ;; Step 2: Objective Function Construction (branching on objective-kind)
        objective-fn (case objective-kind
                       ;; Hamiltonian-based objectives (VQE, QAOA)
                       (:hamiltonian :combinatorial)
                       (let [hamiltonian ((:hamiltonian-constructor-fn algorithm-config) options)
                             circuit-constructor ((:circuit-constructor-fn algorithm-config) options)
                             execution-options {:shots (:shots options 1024)}
                             opt-method (:optimization-method options :adam)
                             use-gradients? (or (:use-enhanced-objective options)
                                                (contains? #{:gradient-descent :adam :quantum-natural-gradient} opt-method))]
                         (if use-gradients?
                           (gradient-based-variational-hamiltonian-objective hamiltonian circuit-constructor backend execution-options)
                           (variational-hamiltonian-objective hamiltonian circuit-constructor backend execution-options)))
                       
                       ;; ML-based objectives (VQC, QNN)
                       (:classification :regression)
                       (let [loss-fn (or (:loss-fn algorithm-config)
                                         (throw (ex-info "loss-fn required for ML objectives" {:objective-kind objective-kind})))
                             dataset (if-let [dataset-fn (:dataset-fn algorithm-config)]
                                       (dataset-fn options)
                                       (:training-data options))
                             circuit-constructor ((:circuit-constructor-fn algorithm-config) options)
                             execution-options {:shots (:shots options 1024)}
                             prediction-extractor (or (:prediction-extractor-fn algorithm-config)
                                                      ;; Default: extract from measurement frequencies
                                                      (fn [result] (:frequencies result)))]
                         
                         ;; Create ML objective function
                         (fn [parameters]
                           (let [;; Handle batching if batch-sampler provided
                                 batches (if-let [batch-sampler (:batch-sampler-fn algorithm-config)]
                                           (batch-sampler dataset (:batch-size options 32))
                                           [dataset]) ; Single batch = full dataset
                                 
                                 ;; Compute loss across batches
                                 batch-losses (mapv (fn [batch]
                                                      (let [features (:features batch)
                                                            labels (:labels batch)
                                                            ;; Execute circuits for all samples in batch
                                                            predictions (mapv (fn [feature]
                                                                                (let [circuit (circuit-constructor parameters feature)
                                                                                      result (backend/execute-circuit backend circuit execution-options)
                                                                                      measurement-result (get-in result [:results :measurement-results])]
                                                                                  (prediction-extractor measurement-result)))
                                                                              features)]
                                                        (loss-fn predictions labels)))
                                                    batches)
                                 
                                 ;; Average loss across batches
                                 avg-loss (/ (reduce + batch-losses) (count batches))
                                 
                                 ;; Add regularization if provided
                                 regularization-penalty (if-let [reg-fn (:regularization-fn algorithm-config)]
                                                          (reg-fn parameters)
                                                          0.0)]
                             (+ avg-loss regularization-penalty))))
                       
                       ;; Custom objective (user provides complete objective function in options)
                       :custom
                       (or (:objective-function options)
                           (throw (ex-info "objective-function required for custom objective-kind" {:objective-kind objective-kind}))))
        
        ;; Step 3: Optimization Execution
        optimization-method (:optimization-method options :adam)
        optimization-options (merge {:learning-rate 0.01
                                     :max-iterations 500
                                     :tolerance 1e-6
                                     :gradient-tolerance 1e-4
                                     :shots 1024}
                                    options)
        
        ;; Determine if we're using gradient-based optimization for Hamiltonian objectives
        use-enhanced-optimization? (and (= objective-kind :hamiltonian)
                                        (contains? #{:gradient-descent :adam :quantum-natural-gradient} optimization-method))
        
        ;; Add additional options for enhanced optimization
        enhanced-options (if use-enhanced-optimization?
                           (merge optimization-options
                                  {:gradient-method :parameter-shift
                                   :ansatz-fn (:circuit-constructor-fn algorithm-config)
                                   :backend backend
                                   :exec-options {:shots (:shots options 1024)}})
                           optimization-options)
        
        ;; Run optimization with appropriate method
        optimization-result (if use-enhanced-optimization?
                              (enhanced-variational-optimization objective-fn initial-parameters enhanced-options)
                              (variational-optimization objective-fn initial-parameters optimization-options))
        
        ;; Calculate initial energy/loss for comparison (handle both map and number returns)
        initial-result (objective-fn initial-parameters)
        initial-energy (if (map? initial-result) (:energy initial-result) initial-result)
        
        ;; Timing
        end-time (System/currentTimeMillis)
        
        ;; Step 4: Convergence Analysis
        convergence-analysis (analyze-convergence optimization-result)
        
        ;; Step 5: Build base result map
        base-result (merge optimization-result
                           {:algorithm algorithm-name
                            :objective-kind objective-kind
                            :initial-parameters initial-parameters
                            :convergence-analysis convergence-analysis
                            :initial-energy initial-energy
                            :total-runtime-ms (- end-time start-time)
                            :enhanced-features {:optimization-method optimization-method
                                                :convergence-monitored true
                                                :algorithm-config-driven true}})]
    
    ;; Step 6: Algorithm-specific result processing
    (if-let [result-processor (:result-processor-fn algorithm-config)]
      (result-processor base-result algorithm-config options)
      base-result)))


;;;
;;; Parameter Landscape Analysis Functions
;;;
(defn analyze-variational-landscape
  "Analyze the energy landscape around optimal parameters for variational algorithms.
  
  This function performs computational analysis of the parameter space by evaluating
  the objective function at perturbed parameter values. It provides insights into
  the local structure of the energy landscape and parameter sensitivity.
  
  Use Cases:
  - Understanding which parameters most affect the objective function
  - Identifying optimization challenges (flat vs steep landscapes)
  - Validating that optimization found a reasonable local minimum
  - Research into ansatz design and parameter initialization strategies
  - Debugging optimization convergence issues
  
  Computational Cost: Medium to High - requires n additional circuit evaluations for
  finite difference sensitivities, plus optionally 2×n evaluations for gradients.
  
  Parameters:
  - objective-fn: Objective function to analyze (typically the same used in optimization)
  - optimal-params: Optimal parameters found by optimization
  - perturbation-size: Size of parameter perturbations for analysis (default: 0.01)
  - compute-gradients?: Whether to compute gradients via parameter shift (default: true)
  
  Returns:
  Map with comprehensive landscape analysis:
  - :optimal-energy - Energy at optimal parameters
  - :sensitivities - Finite difference sensitivities for each parameter
  - :most/least-sensitive-parameter - Indices of extreme sensitivity parameters
  - :gradients - Parameter shift gradients (if compute-gradients? true)
  - :gradient-norm - L2 norm of gradient vector (if gradients computed)
  - Metadata about analysis parameters and parameter count"
  [objective-fn optimal-params & {:keys [perturbation-size compute-gradients?] 
                                   :or {perturbation-size 0.01 compute-gradients? true}}]
  (let [num-params (count optimal-params)
        optimal-energy (objective-fn optimal-params)

        ;; Calculate gradients using parameter shift rule (optional for performance)
        gradients (when compute-gradients?
                    (qopt/calculate-parameter-shift-gradient objective-fn optimal-params))

        ;; Calculate parameter sensitivities via finite differences
        sensitivities (mapv (fn [i]
                              (let [params-perturbed (assoc optimal-params i
                                                            (+ (nth optimal-params i) perturbation-size))
                                    energy-perturbed (objective-fn params-perturbed)]
                                (abs (- energy-perturbed optimal-energy))))
                            (range num-params))

        ;; Compute gradient norm (only if gradients computed)
        gradient-norm (when gradients
                        (Math/sqrt (reduce + (map #(* % %) gradients))))]

    (merge
     {:optimal-energy optimal-energy
      :sensitivities sensitivities
      :most-sensitive-parameter (apply max-key #(nth sensitivities %) (range num-params))
      :least-sensitive-parameter (apply min-key #(nth sensitivities %) (range num-params))
      :perturbation-size perturbation-size
      :parameter-count num-params}
     (when gradients
       {:gradients gradients
        :gradient-norm gradient-norm}))))

(defn analyze-parameter-sensitivity
  "Process and rank parameter sensitivities from landscape analysis.
  
  This function takes raw sensitivity data (typically from analyze-variational-landscape)
  and provides normalized analysis, ranking, and categorization of parameter importance.
  It's designed to be used as a post-processing step after landscape analysis.
  
  Use Cases:
  - Identifying the most important parameters for optimization focus
  - Reducing parameter space dimension by eliminating low-sensitivity parameters
  - Ansatz design guidance - understanding which parameter placements matter most
  - Adaptive optimization strategies based on parameter importance
  - Research into parameter efficiency and circuit expressivity
  
  Computational Cost: Low - pure data processing, no additional circuit evaluations.
  
  Parameters:
  - sensitivities: Vector of parameter sensitivities (from analyze-variational-landscape)
  
  Returns:
  Map with processed sensitivity analysis:
  - :sensitivities - Original sensitivity values
  - :normalized-sensitivities - Normalized to [0,1] range
  - :sensitivity-range - Range between max and min sensitivities
  - :ranked-parameters - Parameters sorted by sensitivity (index, value pairs)
  - :high/low-sensitivity-params - Top/bottom 3 most important parameters"
  [sensitivities]
  (let [max-sensitivity (apply max sensitivities)
        min-sensitivity (apply min sensitivities)
        sensitivity-range (- max-sensitivity min-sensitivity)
        
        ;; Normalize sensitivities to [0, 1] range
        normalized-sensitivities (if (> sensitivity-range 0)
                                   (mapv #(/ (- % min-sensitivity) sensitivity-range) sensitivities)
                                   (vec (repeat (count sensitivities) 0.0)))
        
        ;; Create parameter ranking by sensitivity
        indexed-sensitivities (map-indexed vector sensitivities)
        ranked-parameters (sort-by second > indexed-sensitivities)]
    
    {:sensitivities sensitivities
     :normalized-sensitivities normalized-sensitivities
     :max-sensitivity max-sensitivity
     :min-sensitivity min-sensitivity
     :sensitivity-range sensitivity-range
     :ranked-parameters ranked-parameters
     :high-sensitivity-params (map first (take 3 ranked-parameters))
     :low-sensitivity-params (map first (take-last 3 ranked-parameters))}))


