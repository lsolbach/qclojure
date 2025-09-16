(ns org.soulspace.qclojure.application.hardware-optimization
  "Hardware-specific optimization and topology management for quantum circuits.
   
   This namespace provides the full optimization pipeline that integrates
   gate cancellation, qubit optimization, topology-aware transformation with
   decomposition-aware routing, and final gate decomposition to ensure that
   quantum circuits are optimized for execution on specific hardware topologies."
  (:require [clojure.set :as set]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.gate-optimization :as go]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]
            [org.soulspace.qclojure.domain.topology :as topo]))

(defn validate-result-context
  "Validate the optimization result context to ensure all gates are supported.
   
   Parameters:
   - ctx: Optimization context containing:
       :circuit - The optimized circuit
       :supported-operations - Set of natively supported operations
   
   Returns:
   Updated context with:
   - :all-gates-supported? - Boolean indicating if all gates are supported
   - :final-unsupported-gates - List of unsupported gate types (if any)"
  [ctx]
  (let [circuit (:circuit ctx)
        supported-operations (:supported-operations ctx)
        gate-types (set (map :operation-type (:operations circuit)))
        unsupported-gates (remove #(contains? supported-operations %) gate-types)]
    (if (empty? unsupported-gates)
      (assoc ctx :all-gates-supported? true :final-unsupported-gates [])
      (assoc ctx :all-gates-supported? false :final-unsupported-gates (vec unsupported-gates)))))


;;;
;;; Full Optimization Pipeline
;;;
(defn optimize
  "Optimization pipeline that handles gate decomposition properly.
    
    The order is:
    1. Gate cancellation optimization (remove redundant gates)
    2. Qubit optimization (minimize qubits before topology constraints)
    3. Topology optimization (with decomposition-aware routing)  
    4. Final gate decomposition (handle any remaining virtual gates)
    5. Validation and cleanup
    
    Parameters:
    - circuit: Quantum circuit to optimize
    - supported-operations: Set of natively supported operations
    - coupling: Qubit coupling for hardware topology (optional)
    - options: optional Optimization options (defaults shown)
      - :optimize-gates? (default true) - Enable gate cancellation optimization
      - :optimize-qubits? (default true) - Enable qubit usage optimization
      - :optimize-topology? (default true) - Enable topology-aware optimization
      - :transform-operations? (default true) - Enable final gate decomposition
      - Additional options passed to sub-functions
    
    Returns:
    Complete optimization result with corrected pipeline"
  ([ctx]
   (-> ctx
       ;; STEP 1: Gate cancellation optimization (after qubit optimization)
       (go/optimize-gates)

       ;; STEP 2: Qubit optimization FIRST (before topology constraints)
       (qo/optimize-qubit-usage)

       ;; STEP 3: Topology optimization with decomposition-aware routing
       (topo/topology-aware-transform)

       ;; STEP 4: Final gate decomposition (including any remaining virtual gates)
       (ct/transform-circuit)

       ;; STEP 5: Final validation
       (validate-result-context)

       (assoc :pipeline-order
              [:gate-cancellation
               :qubit-optimization
               :topology-optimization
               :gate-decomposition
               :validation])))
  ([circuit device]
   (optimize circuit device {:optimize-gates? true
                             :optimize-qubits? true
                             :optimize-topology? true
                             :transform-operations? true}))
  ([circuit device options]
   (optimize {:circuit circuit
              :device device
              :options options})))

(defn optimization-statistics
  "Analyze original and optimized circuits and provide
   comprehensive report of the optimizations.
   
   Parameters:
    - original-circuit: The circuit before optimization
    - optimized-circuit: The circuit after optimization
   
    Returns:
    Map containing analysis data including qubit/gate counts,
    circuit depth, and operation type differences."
  [original-circuit optimized-circuit]
  (let [original-stats (circuit/statistics original-circuit)
        optimized-stats (circuit/statistics optimized-circuit)

        orig-qubits (:num-qubits original-stats 0)
        opt-qubits (:num-qubits optimized-stats 0)
        qubits-delta (- opt-qubits orig-qubits)
        qubits-reduction-percent (if (zero? orig-qubits) 0
                                     (double (* 100 (/ qubits-delta orig-qubits))))

        orig-gates (:operation-count original-stats 0)
        opt-gates (:operation-count optimized-stats 0)
        gates-delta (- opt-gates orig-gates)
        gates-reduction-percent (if (zero? orig-gates) 0
                                    (double (* 100 (/ gates-delta orig-gates))))

        orig-swaps (:swap-count original-stats 0)
        opt-swaps (:swap-count optimized-stats 0)
        swaps-delta (- opt-swaps orig-swaps)
        swaps-reduction-percent (if (zero? orig-swaps) 0
                                    (double (* 100 (/ swaps-delta orig-swaps))))

        orig-circuit-depth (:depth original-stats 0)
        opt-circuit-depth (:depth optimized-stats 0)
        depth-delta (- opt-circuit-depth orig-circuit-depth)
        depth-reduction-percent (if (zero? orig-circuit-depth) 0
                                    (double (* 100 (/ depth-delta orig-circuit-depth))))

        orig-circuit-operation-types (set (map :operation-type (:operations original-circuit)))
        opt-circuit-operation-types (set (map :operation-type (:operations optimized-circuit)))
        operation-type-difference (set/difference orig-circuit-operation-types opt-circuit-operation-types)]
    {:original-qubits orig-qubits
     :optimized-qubits opt-qubits
     :original-gates orig-gates
     :optimized-gates opt-gates
     :original-swaps orig-swaps
     :optimized-swaps opt-swaps
     :original-circuit-depth orig-circuit-depth
     :optimized-circuit-depth opt-circuit-depth
     :original-operation-types orig-circuit-operation-types
     :optimized-operation-types opt-circuit-operation-types
     :qubits-delta qubits-delta
     :gates-delta gates-delta
     :depth-delta depth-delta
     :swaps-delta swaps-delta
     :qubits-delta-percent qubits-reduction-percent
     :gates-delta-percent gates-reduction-percent
     :depth-delta-percent depth-reduction-percent
     :swaps-delta-percent swaps-reduction-percent
     :operation-type-difference operation-type-difference
     ;
     }))

(comment
  ;; Test optimization pipeline with real use case
  (def test-circuit
    {:num-qubits 3
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :t :operation-params {:target 0}}  ; Virtual gate
                  {:operation-type :cnot :operation-params {:control 0 :target 2}}]}) ; Non-adjacent

  (def supported-gates #{:h :x :z :rz :cnot})  ; SWAP is NOT supported
  (def linear-coupling [[1] [0 2] [1]])        ; Linear: 0-1-2

  ;; Test the corrected pipeline
  (optimize test-circuit
            {:native-gates supported-gates
             :coupling linear-coupling}
            {:optimize-gates? true
             :optimize-qubits? true
             :optimize-topology? true
             :transform-operations? true
             :max-iterations 50})

  (optimization-statistics
   test-circuit
   (:circuit (optimize test-circuit 
                       {:native-gates supported-gates
                        :coupling linear-coupling}
                       {:optimize-gates? true
                        :optimize-qubits? true
                        :optimize-topology? true
                        :transform-operations? true
                        :max-iterations 50})))

  ;
  )
