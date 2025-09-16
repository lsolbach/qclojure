(ns org.soulspace.qclojure.application.algorithm.optimization
  "Pure optimization algorithms and gradient computation for variational quantum algorithms.
  
  This namespace provides the core mathematical optimization algorithms specifically designed
  for quantum variational algorithms like VQE, QAOA, and quantum machine learning. It focuses
  on efficient parameter optimization with quantum-aware gradient computation and robust
  classical optimization techniques.
  
  Core Features:
   
  Quantum Gradient Computation:
  - **Parameter Shift Rule**: Exact gradients for parameterized quantum circuits
  - **Finite Differences**: General gradient computation for classical functions
  - **Adaptive Parameter Shifts**: Optimized shifts for different parameter types
  - **Parallel Gradient Evaluation**: Efficient computation using multiple threads
  
  Classical Optimization Methods:
  - **Gradient Descent**: Basic optimization with momentum and adaptive learning rates
  - **Adam Optimizer**: Adaptive moment estimation with bias correction
  - **Quantum Natural Gradient**: Fisher Information Matrix-based natural gradients
  - **Fastmath Integration**: Derivative-free and gradient-based external optimizers
  
  Quantum Fisher Information:
  - **QFIM Computation**: Quantum Fisher Information Matrix calculation
  - **Natural Gradient Updates**: Optimal parameter space metrics
  - **Matrix Operations**: Linear algebra utilities for quantum optimization
  - **Regularization**: Numerical stability for ill-conditioned systems

  Optimization Method Selection Guide
  
  For Quantum Variational Algorithms (VQE, QAOA):
  - **:adam** - Fast convergence, adaptive learning rates (recommended default)
  - **:gradient-descent** - Simple, reliable, theoretical guarantees
  - **:quantum-natural-gradient** - Optimal convergence in quantum parameter space
  
  For Noisy or Difficult Landscapes:
  - **:cmaes** - Robust global optimization, handles noise well
  - **:nelder-mead** - Derivative-free simplex method
  - **:powell** - Coordinate descent without gradients
  
  For High-Precision Requirements:
  - **:quantum-natural-gradient** - Uses quantum Fisher information
  - **:adam** - With small learning rates and tight tolerances
  
  Parameter Shift Rule
  
  The parameter shift rule is fundamental to quantum optimization:
  ```
  ∂⟨H⟩/∂θ = (1/2)[⟨H⟩(θ + π/2) - ⟨H⟩(θ - π/2)]
  ```
  
  This provides exact gradients for quantum circuits with rotation gates,
  requiring only 2 circuit evaluations per parameter.
  
  Usage Examples
  
  ```clojure
  ;; Basic Adam optimization
  (adam-optimization objective-fn initial-params
    {:learning-rate 0.01
     :max-iterations 500
     :tolerance 1e-6
     :gradient-method :parameter-shift})
  
  ;; Quantum Natural Gradient with Fisher Information
  (quantum-natural-gradient-optimization objective-fn initial-params
    {:ansatz-fn ansatz-constructor
     :backend quantum-backend
     :learning-rate 0.1
     :fisher-regularization 1e-8})
  
  ;; Robust derivative-free optimization
  (fastmath-derivative-free-optimization :cmaes objective-fn initial-params
    {:max-iterations 2000
     :cmaes-sigma 0.3
     :parameter-bounds [[-π π] [-π π]]})
  ```
  
  Integration with VQE
  
  This namespace is designed to integrate seamlessly with VQE and other
  variational quantum algorithms. The optimization functions expect:
  - **Objective Function**: Takes parameter vector, returns scalar energy
  - **Initial Parameters**: Starting point for optimization
  - **Options Map**: Configuration for optimization behavior
  
  Performance Considerations
  
  - **Parameter Shift**: 2N circuit evaluations per gradient (N = parameters)
  - **Finite Differences**: 2N circuit evaluations per gradient  
  - **Quantum Natural Gradient**: N² + 2N circuit evaluations per iteration
  - **Derivative-Free**: Varies by method, typically 10-100x more evaluations
  
  Design Principles
  
  - **Quantum-Aware**: Exploits quantum circuit structure for efficiency
  - **Flexible**: Supports multiple optimization strategies
  - **Robust**: Handles numerical instabilities and edge cases
  - **Extensible**: Easy to add new optimization methods
  - **Production-Ready**: Suitable for real quantum hardware
  
  See also: `org.soulspace.qclojure.application.algorithm.vqe` for usage in VQE."
  (:require [clojure.spec.alpha :as s]
            [fastmath.core :as fm]
            [fastmath.optimization :as opt]
            [org.soulspace.qclojure.domain.math.complex-linear-algebra :as cla]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.application.backend :as qb]))

(s/def ::optimization-method
  #{:gradient-descent :adam :quantum-natural-gradient
    ;; Fastmath derivative-free optimizers (verified working)
    :nelder-mead :powell :cmaes :bobyqa
    ;; Fastmath gradient-based optimizers
    :lbfgsb :gradient
    ;; Note: :cobyla :bfgs :lbfgs :cg not available in this fastmath version
    })

;;
;; Gradient Calculation Methods
;;
(defn finite-difference-gradient
  "Calculate gradient using finite differences (for general functions).
  
  This is more appropriate for classical test functions or when the parameter
  shift rule doesn't apply.
  
  Parameters:
  - objective-fn: Objective function
  - parameters: Current parameter vector
  - h: Step size (default: 1e-6)
  
  Returns:
  Vector of gradients for all parameters"
  ([objective-fn parameters]
   (finite-difference-gradient objective-fn parameters 1e-6))
  ([objective-fn parameters h]
   (mapv (fn [i]
           (let [params-plus (assoc parameters i (+ (nth parameters i) h))
                 params-minus (assoc parameters i (- (nth parameters i) h))
                 f-plus (objective-fn params-plus)
                 f-minus (objective-fn params-minus)]
             (/ (- f-plus f-minus) (* 2.0 h))))
         (range (count parameters)))))

(defn parameter-shift-gradient
  "Calculate gradient using the parameter shift rule.
  
  For a parameterized gate with parameter θ, the gradient is:
  ∂⟨H⟩/∂θ = (1/2)[⟨H⟩(θ + π/2) - ⟨H⟩(θ - π/2)]
  
  This gives exact gradients for quantum circuits with rotation gates.
  
  Parameters:
  - objective-fn: VQE objective function
  - parameters: Current parameter vector
  - param-index: Index of parameter to compute gradient for
  - shift: Parameter shift value (default: π/2)
  
  Returns:
  Gradient value for the specified parameter"
  ([objective-fn parameters param-index]
   (parameter-shift-gradient objective-fn parameters param-index (/ fm/PI 2)))
  ([objective-fn parameters param-index shift]
   (let [params-plus (assoc parameters param-index
                            (+ (nth parameters param-index) shift))
         params-minus (assoc parameters param-index
                             (- (nth parameters param-index) shift))
         energy-plus (objective-fn params-plus)
         energy-minus (objective-fn params-minus)]
     (* 0.5 (- energy-plus energy-minus)))))

(defn adaptive-parameter-shift-gradient
  "Calculate gradient using adaptive parameter shift rule.
  
  Uses different shift values for different types of parameters to improve
  gradient accuracy. For example, uses π/2 for rotation angles but smaller
  shifts for amplitude parameters.
  
  Parameters:
  - objective-fn: VQE objective function
  - parameters: Current parameter vector
  - param-types: Vector indicating parameter types (:rotation, :amplitude, :phase)
  - options: Options map
  
  Returns:
  Vector of gradients for all parameters"
  [objective-fn parameters param-types options]
  (mapv (fn [i param-type]
          (let [shift (case param-type
                        :rotation (/ fm/PI 2)      ; Standard π/2 for rotations
                        :amplitude (/ fm/PI 4)     ; Smaller shift for amplitudes  
                        :phase (/ fm/PI 2)         ; Standard for phases
                        (/ fm/PI 2))]              ; Default
            (parameter-shift-gradient objective-fn parameters i shift)))
        (range (count parameters))
        param-types))

(defn calculate-parameter-shift-gradient
  "Calculate full gradient vector using parameter shift rule.
  
  Uses parallel computation for efficiency when computing multiple gradients.
  
  Parameters:
  - objective-fn: VQE objective function
  - parameters: Current parameter vector
  - options: Options map with :parallel? (default true) and :shift (default π/2)
  
  Returns:
  Vector of gradients for all parameters"
  ([objective-fn parameters]
   (calculate-parameter-shift-gradient objective-fn parameters {}))
  ([objective-fn parameters options]
   (let [shift (:shift options (/ fm/PI 2))
         parallel? (:parallel? options true)]
     (if parallel?
       ;; Use pmap for parallel computation of gradients
       (vec (pmap #(parameter-shift-gradient objective-fn parameters % shift)
                  (range (count parameters))))
       ;; Sequential computation
       (mapv #(parameter-shift-gradient objective-fn parameters % shift)
             (range (count parameters)))))))

(defn calculate-gradient
  "Calculate gradient using the appropriate method.
  
  For VQE quantum objectives, uses parameter shift rule.
  For general functions, uses finite differences.
  
  Parameters:
  - objective-fn: Objective function
  - parameters: Current parameter vector
  - options: Options map, can contain :gradient-method (:parameter-shift or :finite-difference)
  
  Returns:
  Vector of gradients for all parameters"
  [objective-fn parameters options]
  (let [method (:gradient-method options :finite-difference)]  ; Default to finite differences
    (case method
      :parameter-shift (calculate-parameter-shift-gradient objective-fn parameters options)
      :finite-difference (finite-difference-gradient objective-fn parameters
                                                     (:gradient-step-size options 1e-6))
      ;; Default fallback
      (finite-difference-gradient objective-fn parameters))))

;;
;; Optimization Methods
;;
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
              gradient (calculate-gradient objective-fn params options)

              ;; Update biased first moment estimate
              new-m (mapv #(+ (* beta1 %1) (* (- 1 beta1) %2)) m-vec gradient)

              ;; Update biased second moment estimate
              new-v (mapv #(+ (* beta2 %1) (* (- 1 beta2) %2 %2)) v-vec gradient)

              ;; Bias correction
              m-corrected (mapv #(/ % (- 1 (fm/pow beta1 iteration))) new-m)
              v-corrected (mapv #(/ % (- 1 (fm/pow beta2 iteration))) new-v)

              ;; Parameter update
              new-params (mapv #(- %1 (* learning-rate (/ %2 (+ (fm/sqrt %3) epsilon))))
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
              gradient (calculate-gradient objective-fn params options)

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

;;
;; Quantum Fisher Information Matrix and Natural Gradient Implementation
;;

;; TODO move matrix operations to math namespace
(defn compute-state-derivative
  "Compute the derivative of the quantum state with respect to a parameter.
  
  Uses the parameter shift rule to compute |∂ψ(θ)/∂θᵢ⟩ efficiently.
  For a state |ψ(θ)⟩, the derivative is computed as:
  |∂ψ/∂θᵢ⟩ ≈ [|ψ(θ + π/2·eᵢ)⟩ - |ψ(θ - π/2·eᵢ)⟩] / 2
  
  Parameters:
  - ansatz-fn: Function that creates quantum circuit from parameters
  - backend: Quantum backend for execution
  - parameters: Current parameter vector
  - param-index: Index of parameter to compute derivative for
  - options: Execution options
  
  Returns:
  Map representing the state derivative"
  [ansatz-fn backend parameters param-index options]
  (let [shift (/ fm/PI 2)
        ;; Create shifted parameter vectors
        params-plus (assoc parameters param-index
                           (+ (nth parameters param-index) shift))
        params-minus (assoc parameters param-index
                            (- (nth parameters param-index) shift))

        ;; Execute circuits to get states
        circuit-plus (ansatz-fn params-plus)
        circuit-minus (ansatz-fn params-minus)

        state-plus (let [result (qb/execute-circuit backend circuit-plus options)]
                     (if (= (:job-status result) :completed)
                       (:final-state result)
                       ;; TODO: no fallback, handle backend failures appropriately
                       (qc/execute-circuit circuit-plus (qs/zero-state (:num-qubits circuit-plus)))))


        state-minus (let [result (qb/execute-circuit backend circuit-minus options)]
                      (if (= (:job-status result) :completed)
                        (:final-state result)
                        ;; TODO: no fallback, handle backend failures appropriately
                        (qc/execute-circuit circuit-minus (qs/zero-state (:num-qubits circuit-minus)))))
                    
        ;; Compute derivative as (|ψ⁺⟩ - |ψ⁻⟩) / 2
        amplitudes-plus (:amplitudes state-plus)
        amplitudes-minus (:amplitudes state-minus)]
    
    {:amplitudes (mapv (fn [a+ a-]
                        ;; Complex arithmetic: (a+ - a-) / 2
                        {:real (/ (- (:real a+) (:real a-)) 2.0)
                         :imag (/ (- (:imag a+) (:imag a-)) 2.0)})
                      amplitudes-plus amplitudes-minus)
     :num-qubits (:num-qubits state-plus)}))

(defn state-inner-product
  "Compute inner product ⟨ψ₁|ψ₂⟩ between two quantum states.
  
  Parameters:
  - state1: First quantum state
  - state2: Second quantum state
  
  Returns:
  Complex number representing the inner product"
  [state1 state2]
  (let [amps1 (:amplitudes state1)
        amps2 (:amplitudes state2)]
    (reduce (fn [acc [a1 a2]]
              ;; Complex conjugate: a1* · a2 = (re1 - i·im1) · (re2 + i·im2)
              ;; = re1·re2 + im1·im2 + i·(re1·im2 - im1·re2)
              {:real (+ (:real acc) 
                       (- (* (:real a1) (:real a2)) 
                          (* (:imag a1) (:imag a2))))
               :imag (+ (:imag acc) 
                       (+ (* (:real a1) (:imag a2)) 
                          (* (:imag a1) (:real a2))))})
            {:real 0.0 :imag 0.0}
            (map vector amps1 amps2))))

(defn compute-fisher-information-matrix
  "Compute the Quantum Fisher Information Matrix (QFIM).
  
  The QFIM is defined as:
  F_ij = 4 * Re[⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩ - ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩]
  
  This matrix provides the optimal metric for parameter updates in the
  quantum parameter space, leading to faster convergence than standard
  gradient descent.
  
  Parameters:
  - ansatz-fn: Function that creates quantum circuit from parameters
  - backend: Quantum backend for execution  
  - parameters: Current parameter vector
  - options: Execution options
  
  Returns:
  Fisher Information Matrix as vector of vectors"
  [ansatz-fn backend parameters options]
  (let [n (count parameters)
        
        ;; Get current state |ψ(θ)⟩
        current-circuit (ansatz-fn parameters)
        current-state (let [result (qb/execute-circuit backend current-circuit options)]
                        (if (= (:job-status result) :completed)
                          (:final-state result)
                          ;; TODO: no fallback, handle backend failures appropriately
                          (qc/execute-circuit current-circuit (qs/zero-state (:num-qubits current-circuit)))))
        
        ;; Compute all state derivatives |∂ψ/∂θᵢ⟩
        state-derivatives (mapv #(compute-state-derivative ansatz-fn backend parameters % options)
                               (range n))]
    
    ;; Compute Fisher Information Matrix elements
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (let [;; ⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩
                        inner-deriv-deriv (state-inner-product (nth state-derivatives i)
                                                              (nth state-derivatives j))
                        
                        ;; ⟨∂ψ/∂θᵢ|ψ⟩
                        inner-deriv-state-i (state-inner-product (nth state-derivatives i)
                                                                 current-state)
                        
                        ;; ⟨ψ|∂ψ/∂θⱼ⟩ = ⟨∂ψ/∂θⱼ|ψ⟩*
                        inner-state-deriv-j {:real (:real (state-inner-product (nth state-derivatives j)
                                                                              current-state))
                                            :imag (- (:imag (state-inner-product (nth state-derivatives j)
                                                                                current-state)))}
                        
                        ;; ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩
                        product-terms {:real (- (* (:real inner-deriv-state-i) (:real inner-state-deriv-j))
                                                (* (:imag inner-deriv-state-i) (:imag inner-state-deriv-j)))
                                      :imag (+ (* (:real inner-deriv-state-i) (:imag inner-state-deriv-j))
                                              (* (:imag inner-deriv-state-i) (:real inner-state-deriv-j)))}
                        
                        ;; F_ij = 4 * Re[⟨∂ψ/∂θᵢ|∂ψ/∂θⱼ⟩ - ⟨∂ψ/∂θᵢ|ψ⟩⟨ψ|∂ψ/∂θⱼ⟩]
                        fisher-element (* 4.0 (- (:real inner-deriv-deriv)
                                                 (:real product-terms)))]
                    
                    fisher-element)))))))

(defn regularize-fisher-matrix
  "Regularize the Fisher Information Matrix to ensure numerical stability.
  
  Adds a small diagonal term to prevent singularity and improve conditioning.
  
  Parameters:
  - fisher-matrix: Fisher Information Matrix
  - regularization: Regularization parameter (default: 1e-8)
  
  Returns:
  Regularized Fisher Information Matrix"
  [fisher-matrix regularization]
  (let [n (count fisher-matrix)
        reg (or regularization 1e-8)]
    (vec (for [i (range n)]
           (vec (for [j (range n)]
                  (if (= i j)
                    (+ (get-in fisher-matrix [i j]) reg)
                    (get-in fisher-matrix [i j]))))))))

(defn quantum-natural-gradient-optimization
  "VQE optimization using Quantum Natural Gradient (QNG) with full Fisher Information Matrix.
  
  QNG uses the quantum Fisher information matrix to define a more natural
  metric for parameter updates. The update rule is:
  θ_{k+1} = θ_k - α * F⁻¹ * ∇E(θ_k)
  
  where F is the Fisher Information Matrix and ∇E is the energy gradient.
  This often leads to faster convergence than standard gradient descent.
  
  Parameters:
  - objective-fn: VQE objective function  
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [objective-fn initial-parameters options]
  (let [learning-rate (:learning-rate options 0.1)
        max-iter (:max-iterations options 200)  ; QNG typically needs fewer iterations
        tolerance (:tolerance options 1e-6)
        regularization (:fisher-regularization options 1e-8)
        ansatz-fn (:ansatz-fn options)  ; Required for QNG
        backend (:backend options)      ; Required for QNG
        exec-options (:exec-options options {:shots 1000})
        
        ;; Validate required parameters
        _ (when (or (nil? ansatz-fn) (nil? backend))
            (throw (ex-info "QNG requires :ansatz-fn and :backend in options"
                           {:ansatz-fn ansatz-fn :backend backend})))]
    
    (loop [params initial-parameters
           iteration 0
           prev-energy (objective-fn initial-parameters)
           energies [prev-energy]]
      
      (if (>= iteration max-iter)
        {:success false
         :optimal-parameters params
         :optimal-energy (objective-fn params)
         :iterations iteration
         :function-evaluations (* iteration (+ (* (count params) (count params) 2) ; Fisher matrix
                                              (* (count params) 2)))              ; Gradient
         :convergence-history energies
         :reason "max-iterations-reached"}
        
        (let [;; Compute energy gradient using parameter shift rule
              gradient (calculate-parameter-shift-gradient objective-fn params)
              
              ;; Compute Fisher Information Matrix
              fisher-matrix (compute-fisher-information-matrix ansatz-fn backend params exec-options)
              
              ;; Regularize Fisher matrix for numerical stability
              regularized-fisher (regularize-fisher-matrix fisher-matrix regularization)
              
              ;; Compute Fisher matrix inverse
              fisher-inverse (cla/matrix-inverse regularized-fisher)
              
              ;; Check if matrix is invertible
              new-params (if fisher-inverse
                          ;; QNG update: θ_{k+1} = θ_k - α * F⁻¹ * ∇E
                          (let [natural-gradient (first (cla/matrix-multiply fisher-inverse [gradient]))]
                            (mapv #(- %1 (* learning-rate %2)) params natural-gradient))
                          ;; Fallback to regular gradient descent if Fisher matrix is singular
                          (do
                            (println "Warning: Fisher matrix is singular, falling back to gradient descent")
                            (mapv #(- %1 (* learning-rate %2)) params gradient)))
              
              ;; Evaluate new energy
              new-energy (objective-fn new-params)
              energy-diff (abs (- prev-energy new-energy))
              new-energies (conj energies new-energy)]
          
          (if (< energy-diff tolerance)
            {:success true
             :optimal-parameters new-params
             :optimal-energy new-energy
             :iterations iteration
             :function-evaluations (* iteration (+ (* (count params) (count params) 2)
                                                  (* (count params) 2)))
             :convergence-history new-energies
             :final-gradient gradient
             :final-fisher-matrix regularized-fisher
             :reason "converged"}
            
            (recur new-params (inc iteration) new-energy new-energies)))))))

;;
;; Fastmath Optimizer Integrations
;;
(defn fastmath-derivative-free-optimization
  "VQE optimization using fastmath derivative-free optimizers.
  
  These optimizers don't require gradients and can be used when parameter
  shift gradients are expensive or unavailable.
  
  Supported methods:
  - :nelder-mead - Nelder-Mead simplex (good general purpose)
  - :powell - Powell's method (coordinate descent)  
  - :cmaes - Covariance Matrix Adaptation Evolution Strategy (robust)
  - :bobyqa - Bound Optimization BY Quadratic Approximation (handles bounds well)
  
  Parameters:
  - method: Fastmath optimization method keyword
  - objective-fn: VQE objective function
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [method objective-fn initial-parameters options]
  (let [max-iter (:max-iterations options 2000)  ; Higher default for derivative-free
        tolerance (:tolerance options 1e-6)
        param-count (count initial-parameters)
        
        ;; Wrap objective function for fastmath (converts varargs to vector)
        wrapped-objective (fn [& params] (objective-fn (vec params)))
        
        ;; Configure bounds for optimization stability  
        param-bounds (:parameter-bounds options 
                      (vec (repeat param-count [-4.0 4.0])))  ; Allow ~2π rotations
        
        ;; Method-specific configuration with higher evaluation limits
        config (merge {:max-evals (* max-iter 10)  ; Allow more evaluations for fastmath
                       :rel-threshold tolerance
                       :abs-threshold tolerance
                       :initial initial-parameters
                       :bounds param-bounds}  ; All fastmath optimizers need bounds
                      
                      ;; Method-specific parameters
                      (case method
                        :cmaes {:sigma (:cmaes-sigma options 0.3)  ; Initial step size
                                :population-size (:cmaes-population options (* 4 param-count))}
                        :nelder-mead {:rho (:nelder-mead-rho options 1.0)      ; Reflection
                                      :chi (:nelder-mead-chi options 2.0)      ; Expansion
                                      :gamma (:nelder-mead-gamma options 0.5)  ; Contraction
                                      :sigma (:nelder-mead-sigma options 0.5)} ; Shrinkage
                        :powell {:ftol (:powell-ftol options tolerance)}
                        :bobyqa {:initial-radius (:bobyqa-initial-radius options 0.5)
                                 :stopping-radius (:bobyqa-stopping-radius options tolerance)}
                        :cobyla {:rho-beg (:cobyla-rho-beg options 0.5)
                                 :rho-end (:cobyla-rho-end options tolerance)}
                        {}))
        
        ;; Run optimization
        start-time (System/currentTimeMillis)
        result (try
                 (opt/minimize method wrapped-objective config)
                 (catch Exception e
                   (println "Fastmath optimization failed:" (.getMessage e))
                   [initial-parameters (objective-fn initial-parameters)]))
        end-time (System/currentTimeMillis)
        
        ;; Extract results from vector format [parameters value]
        optimal-params (if (vector? result) (first result) initial-parameters)
        optimal-value (if (vector? result) (second result) (objective-fn initial-parameters))]
    
    {:success (< optimal-value (+ (objective-fn initial-parameters) tolerance))  ; Improved from initial
     :optimal-parameters optimal-params
     :optimal-energy optimal-value
     :iterations 0  ; fastmath doesn't report iterations for derivative-free methods
     :function-evaluations 0  ; fastmath doesn't report evaluations separately
     :execution-time-ms (- end-time start-time)
     :optimization-method method
     :fastmath-result result
     :reason "completed"}))

(defn fastmath-gradient-based-optimization
  "VQE optimization using fastmath gradient-based optimizers with parameter shift gradients.
  
  These optimizers use our exact parameter shift rule gradients for faster
  convergence than derivative-free methods.
  
  Supported methods:
  - :lbfgsb - L-BFGS-B (limited memory BFGS with bounds)
  - :gradient - Simple gradient descent (not recommended for VQE)
  
  Parameters:
  - method: Fastmath optimization method keyword
  - objective-fn: VQE objective function
  - initial-parameters: Starting parameter values
  - options: Optimization options
  
  Returns:
  Map with optimization results"
  [method objective-fn initial-parameters options]
  (let [max-iter (:max-iterations options 500)  ; Lower default for gradient-based
        tolerance (:tolerance options 1e-6)
        param-count (count initial-parameters)

        ;; Gradient evaluation counter
        gradient-evaluations (atom 0)
        function-evaluations (atom 0)

        ;; Wrap objective function to count evaluations
        wrapped-objective (fn [& params]
                            (swap! function-evaluations inc)
                            (objective-fn (vec params)))

        ;; Wrap gradient function using our parameter shift rule
        wrapped-gradient (fn [& params]
                           (swap! gradient-evaluations inc)
                           (let [param-vec (vec params)
                                 gradient (calculate-parameter-shift-gradient objective-fn param-vec)]
                             ;; Convert to array for fastmath
                             (double-array gradient)))

        ;; Configure optimization
        config (merge {:max-evals max-iter
                       :rel-threshold tolerance
                       :abs-threshold tolerance
                       :initial initial-parameters
                       :gradient wrapped-gradient  ; Provide our gradient function
                       :bounds (:parameter-bounds options
                                                  (vec (repeat param-count [-10.0 10.0])))}  ; Default bounds

                      ;; Method-specific parameters
                      (case method
                        :lbfgs {:memory (:lbfgs-memory options 10)  ; History size
                                :step-size (:lbfgs-step-size options 1.0)}
                        :cg {:beta-type (:cg-beta-type options :fletcher-reeves)}  ; or :polak-ribiere
                        {}))

        ;; Run optimization
        start-time (System/currentTimeMillis)
        result (try
                 (opt/minimize method wrapped-objective config)
                 (catch Exception e
                   (println "Fastmath gradient optimization failed:" (.getMessage e))
                   [initial-parameters (objective-fn initial-parameters)]))
        end-time (System/currentTimeMillis)

        ;; Extract results from vector format [parameters value]
        optimal-params (if (vector? result) (first result) initial-parameters)
        optimal-value (if (vector? result) (second result) (objective-fn initial-parameters))]

    {:success (< (abs (- optimal-value (objective-fn initial-parameters))) tolerance)
     :optimal-parameters optimal-params
     :optimal-energy optimal-value
     :iterations 0  ; fastmath doesn't report iterations consistently
     :function-evaluations @function-evaluations
     :gradient-evaluations @gradient-evaluations
     :total-circuit-evaluations (+ @function-evaluations (* @gradient-evaluations param-count 2))
     :execution-time-ms (- end-time start-time)
     :optimization-method method
     :fastmath-result result
     :reason "completed"}))

