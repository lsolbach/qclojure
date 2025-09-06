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
            [org.soulspace.qclojure.application.backend :as qb]))

;;;
;;; Specs for common variational algorithm components
;;;
(s/def ::circuit-construction-fn fn?)
(s/def ::hamiltonian ::ham/hamiltonian)
(s/def ::backend any?) ; Backend satisfying QuantumBackend protocol
(s/def ::execution-options map?)
(s/def ::optimization-method
  #{:gradient-descent :adam :quantum-natural-gradient
    :nelder-mead :powell :cmaes :bobyqa :gradient :lbfgsb})

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
(defn analyze-optimization-convergence
  "Analyze convergence properties of optimization results.
  
  Parameters:
  - optimization-result: Result map from optimization
  
  Returns:
  Map with convergence analysis"
  [optimization-result]
  (let [history (:convergence-history optimization-result [])
        success (:success optimization-result false)
        reason (:reason optimization-result)
        iterations (:iterations optimization-result 0)]
    
    (merge
     {:converged success
      :final-reason reason
      :total-iterations iterations
      :function-evaluations (:function-evaluations optimization-result 0)}
     
     (when (seq history)
       (let [initial-energy (first history)
             final-energy (last history)
             energy-improvement (- initial-energy final-energy)
             relative-improvement (when (not= initial-energy 0.0)
                                    (/ energy-improvement (abs initial-energy)))]
         {:initial-energy initial-energy
          :final-energy final-energy
          :energy-improvement energy-improvement
          :relative-improvement relative-improvement
          :energy-std (when (> (count history) 1)
                        (let [mean (/ (reduce + history) (count history))
                              variances (map #(* (- % mean) (- % mean)) history)]
                          (Math/sqrt (/ (reduce + variances) (count history)))))})))))

(defn summarize-algorithm-performance
  "Create a summary of variational algorithm performance.
  
  Parameters:
  - algorithm-result: Complete result map from algorithm execution
  - algorithm-name: Name of the algorithm (e.g., 'VQE', 'QAOA')
  
  Returns:
  Map with performance summary"
  [algorithm-result algorithm-name]
  (let [convergence (:convergence-analysis algorithm-result)
        timing (:total-runtime-ms algorithm-result 0)]
    
    {:algorithm algorithm-name
     :success (:success algorithm-result false)
     :final-energy (:optimal-energy algorithm-result)
     :iterations (:iterations algorithm-result 0)
     :runtime-seconds (/ timing 1000.0)
     :convergence-quality (cond
                            (:converged convergence) :excellent
                            (and (> (:iterations algorithm-result 0) 50)
                                 (< (:final-energy algorithm-result Double/POSITIVE_INFINITY) 1e-3)) :good
                            (> (:iterations algorithm-result 0) 10) :fair
                            :else :poor)
     :efficiency-score (when (and (> timing 0) (> (:iterations algorithm-result 0) 0))
                         (/ 1000.0 (/ timing (:iterations algorithm-result))))}))  ; iterations per second

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
;;; Generic Objective Function Creation
;;;
(defn variational-objective
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
              execution-result (qb/execute-circuit backend circuit run-options)
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

;;;
;;; Enhanced Objective Functions
;;;
(defn enhanced-variational-objective
  "Create enhanced variational objective function that provides gradients.
  
  This function creates an enhanced objective that computes both energy and gradients
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
        standard-objective (variational-objective hamiltonian circuit-construction-fn backend run-options)]

    (fn enhanced-objective [parameters]
      (try
        (let [params-vec (if (vector? parameters) parameters (vec parameters))

              ;; Compute energy at current parameters using result framework
              base-circuit (circuit-construction-fn params-vec)
              base-result (qb/execute-circuit backend base-circuit run-options)
              base-energy (:energy-expectation (:hamiltonian-result (:results base-result)))
              base-state (:final-state (:results base-result))

              ;; Compute gradients using existing parameter shift implementation
              gradients (qopt/calculate-parameter-shift-gradient standard-objective params-vec
                                                                 {:parallel? (:parallel? execution-options true)})]

          {:energy base-energy
           :gradients gradients
           :quantum-state base-state})

        (catch Exception e
          (println "Enhanced variational objective evaluation failed:" (.getMessage e))
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
;;; Analysis Functions for Variational Algorithms
;;;
(defn analyze-variational-landscape
  "Analyze the energy landscape around optimal parameters for any variational algorithm.
  
  This function performs parameter sensitivity analysis by perturbing each parameter
  and measuring the energy change. It provides insights into which parameters have
  the most impact on the objective function.
  
  Parameters:
  - objective-fn: Objective function to analyze
  - optimal-params: Optimal parameters found by optimization
  - perturbation-size: Size of parameter perturbations for analysis (default: 0.01)
  
  Returns:
  Map with landscape analysis including gradient norms and parameter sensitivities"
  [objective-fn optimal-params & {:keys [perturbation-size] :or {perturbation-size 0.01}}]
  (let [num-params (count optimal-params)
        optimal-energy (objective-fn optimal-params)

        ;; Calculate gradients using parameter shift rule
        gradients (qopt/calculate-parameter-shift-gradient objective-fn optimal-params)

        ;; Calculate parameter sensitivities via finite differences
        sensitivities (mapv (fn [i]
                              (let [params-perturbed (assoc optimal-params i
                                                            (+ (nth optimal-params i) perturbation-size))
                                    energy-perturbed (objective-fn params-perturbed)]
                                (abs (- energy-perturbed optimal-energy))))
                            (range num-params))

        ;; Compute gradient norm
        gradient-norm (Math/sqrt (reduce + (map #(* % %) gradients)))]

    {:optimal-energy optimal-energy
     :gradients gradients
     :gradient-norm gradient-norm
     :sensitivities sensitivities
     :most-sensitive-parameter (apply max-key #(nth sensitivities %) (range num-params))
     :least-sensitive-parameter (apply min-key #(nth sensitivities %) (range num-params))
     :perturbation-size perturbation-size
     :parameter-count num-params}))

(defn analyze-convergence-history
  "Analyze convergence from optimization history for any variational algorithm.
  
  This function analyzes the optimization trajectory to provide insights into
  convergence behavior, energy improvement, and optimization quality.
  
  Parameters:
  - optimization-result: Result map from optimization containing :history
  
  Returns:
  Map with convergence analysis"
  [optimization-result]
  (let [history (:history optimization-result [])
        energies (map :energy history)]

    (when (seq energies)
      (let [initial-energy (first energies)
            final-energy (last energies)
            energy-improvement (- initial-energy final-energy)
            num-iterations (count energies)]
        
        {:total-iterations num-iterations
         :initial-energy initial-energy
         :final-energy final-energy
         :energy-improvement energy-improvement
         :relative-improvement (when (not= initial-energy 0.0)
                                 (/ energy-improvement (abs initial-energy)))
         :convergence-rate (when (> num-iterations 1)
                             (/ energy-improvement num-iterations))
         :monotonic-decrease? (every? (fn [[e1 e2]] (<= e2 e1))
                                      (partition 2 1 energies))
         :final-gradient-norm (when-let [last-step (last history)]
                                (when-let [gradients (:gradients last-step)]
                                  (Math/sqrt (reduce + (map #(* % %) gradients)))))
         :convergence-quality (cond
                                (< energy-improvement 1e-8) :poor
                                (< energy-improvement 1e-6) :fair  
                                (< energy-improvement 1e-4) :good
                                :else :excellent)}))))

(defn analyze-parameter-sensitivity
  "Analyze parameter sensitivity for variational algorithms.
  
  This function identifies which parameters have the most impact on the objective
  function by computing normalized sensitivities and providing ranking.
  
  Parameters:
  - sensitivities: Vector of parameter sensitivities from landscape analysis
  
  Returns:
  Map with sensitivity analysis"
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

;;;
;;; Algorithm Structure Template
;;;
(defn variational-algorithm-template
  "Template for implementing variational quantum algorithms.
  
  This function provides a common structure that can be used to implement
  new variational algorithms or refactor existing ones. It handles the
  common workflow and delegates algorithm-specific tasks to provided functions.
  
  Parameters:
  - config: Algorithm configuration map
  - algorithm-fns: Map of algorithm-specific functions:
    - :hamiltonian-constructor - (fn [config] -> hamiltonian)
    - :circuit-constructor - (fn [config] -> circuit-construction-fn)
    - :parameter-count - (fn [config] -> number)
    - :result-processor - (fn [optimization-result config] -> final-result)
  
  Returns:
  Complete algorithm result map"
  [config algorithm-fns]
  {:pre [(map? config) (map? algorithm-fns)]}
  (let [{:keys [hamiltonian-constructor circuit-constructor 
                parameter-count result-processor]} algorithm-fns
        
        ;; Timing
        start-time (System/currentTimeMillis)
        
        ;; Algorithm-specific construction
        hamiltonian (hamiltonian-constructor config)
        circuit-construction-fn (circuit-constructor config)
        num-params (parameter-count config)
        
        ;; Common parameter initialization
        initial-parameters (or (:initial-parameters config)
                               (random-parameter-initialization num-params))
        
        ;; Common objective creation and optimization
        backend (:backend config)
        execution-options {:shots (:shots config 1024)}
        objective-fn (variational-objective hamiltonian circuit-construction-fn 
                                                    backend execution-options)
        
        optimization-options (merge config {:gradient-method :parameter-shift})
        optimization-result (variational-optimization objective-fn initial-parameters optimization-options)
        
        end-time (System/currentTimeMillis)
        
        ;; Common analysis
        convergence-analysis (analyze-optimization-convergence optimization-result)
        base-result (merge optimization-result
                           {:hamiltonian hamiltonian
                            :initial-parameters initial-parameters
                            :convergence-analysis convergence-analysis
                            :total-runtime-ms (- end-time start-time)})]
    
    ;; Algorithm-specific result processing
    (result-processor base-result config)))

#_
(comment
  ;; Example usage for VQE:
  (defn vqe-using-template [backend config]
    (variational-algorithm-template
     (assoc config :backend backend)
     {:hamiltonian-constructor (fn [cfg] (:hamiltonian cfg))
      :circuit-constructor (fn [cfg] (create-vqe-ansatz cfg))
      :parameter-count (fn [cfg] (count-vqe-parameters cfg))
      :result-processor (fn [result cfg] (add-vqe-specific-analysis result cfg))}))
  
  ;; Example usage for QAOA:
  (defn qaoa-using-template [backend config]
    (variational-algorithm-template
     (assoc config :backend backend)
     {:hamiltonian-constructor (fn [cfg] (create-qaoa-hamiltonian cfg))
      :circuit-constructor (fn [cfg] (create-qaoa-circuit-fn cfg))
      :parameter-count (fn [cfg] (* 2 (:num-layers cfg)))
      :result-processor (fn [result cfg] (add-qaoa-solution-analysis result cfg))}))
  )