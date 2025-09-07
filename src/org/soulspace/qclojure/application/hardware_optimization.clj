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
            [org.soulspace.qclojure.application.topology :as topo]))

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
    - options: Optimization options
      - :optimize-gates? (default true) - Enable gate cancellation optimization
      - :optimize-qubits? (default true) - Enable qubit usage optimization
      - :optimize-topology? (default false) - Enable topology-aware optimization
      - :transform-operations? (default true) - Enable final gate decomposition
      - Additional options passed to sub-functions
    
    Returns:
    Complete optimization result with corrected pipeline"
  [circuit supported-operations & [coupling options]]

  (let [opts (or options {})
        optimize-gates? (get opts :optimize-gates? true)
        optimize-qubits? (get opts :optimize-qubits? true)
        optimize-topology? (and coupling (get opts :optimize-topology? false))
        transform-operations? (get opts :transform-operations? true)

        ;; STEP 1: Gate cancellation optimization (after qubit optimization)
        gate-optimization-result (if optimize-gates?
                                   (let [optimized-circuit (go/optimize-gates circuit)
                                         gates-removed (- (count (:operations circuit))
                                                          (count (:operations optimized-circuit)))]
                                     {:circuit optimized-circuit
                                      :gates-removed gates-removed
                                      :original-gate-count (count (:operations circuit))
                                      :optimized-gate-count (count (:operations optimized-circuit))})
                                   {:circuit circuit
                                    :gates-removed 0
                                    :original-gate-count (count (:operations circuit))
                                    :optimized-gate-count (count (:operations circuit))})

        step1-circuit (:circuit gate-optimization-result)
        
        ;; STEP 2: Qubit optimization FIRST (before topology constraints)
        qubit-result (if optimize-qubits?
                       (qo/optimize-qubit-usage step1-circuit)
                       {:circuit step1-circuit
                        :qubits-saved 0
                        :original-qubits (:num-qubits step1-circuit)
                        :optimized-qubits (:num-qubits step1-circuit)})

        step2-circuit (:circuit qubit-result)

        ;; STEP 3: Topology optimization with decomposition-aware routing
        topo-result (if optimize-topology?
                      (topo/topology-aware-transform step2-circuit coupling supported-operations opts)
                      {:circuit step2-circuit
                       :swap-count 0
                       :total-cost 0})

        step3-circuit (:circuit topo-result)

        ;; STEP 4: Final gate decomposition (including any remaining virtual gates)
        final-transform-result (if transform-operations?
                                 (ct/transform-circuit step3-circuit supported-operations opts)
                                 {:circuit step3-circuit
                                  :transformed-operation-count 0
                                  :unsupported-operations []})

        final-circuit (:circuit final-transform-result)

        ;; STEP 5: Final validation
        final-gate-types (map :operation-type (:operations final-circuit))
        unsupported-final (remove #(contains? supported-operations %) final-gate-types)
        all-supported? (empty? unsupported-final)]

    ;; Return comprehensive result
    {:circuit final-circuit
     :pipeline-order [:gate-cancellation :qubit-optimization :topology-optimization :gate-decomposition :validation]
     :qubit-optimization-result qubit-result
     :gate-optimization-result gate-optimization-result
     :topology-optimization-result topo-result
     :gate-decomposition-result final-transform-result
     :all-gates-supported? all-supported?
     :final-unsupported-gates (vec (distinct unsupported-final))
     :optimization-summary (str "Circuit optimization:\n"
                                "- Original qubits: " (:num-qubits circuit) "\n"
                                "- Final qubits: " (:num-qubits final-circuit) "\n"
                                "- Original operations: " (count (:operations circuit)) "\n"
                                "- Final operations: " (count (:operations final-circuit)) "\n"
                                "- Gates removed by cancellation: " (:gates-removed gate-optimization-result) "\n"
                                "- All gates supported: " all-supported?)}))

(defn optimization-statistics
  "Analyze original and optimized circuits and provide
   comprehensive report of the optimizations.
   
   Parameters:
    - original-circuit: The circuit before optimization
    - optimized-circuit: The circuit after optimization
   
    Returns:
    Map containing analysis report"
  [original-circuit optimized-circuit]
  ; TODO implement/use circuit/statistics
  (let [original-stats (circuit/statistics original-circuit)
        optimized-stats (circuit/statistics optimized-circuit)

        orig-qubits (:num-qubits original-stats)
        opt-qubits (:num-qubits optimized-stats)
        qubits-delta (- orig-qubits opt-qubits)

        orig-gates (:operation-count original-stats)
        opt-gates (:operation-count optimized-stats)
        gates-delta (- orig-gates opt-gates)

        orig-swaps (:swap-count original-stats)
        opt-swaps (:swap-count optimized-stats)
        swaps-delta (- orig-swaps opt-swaps)

        orig-circuit-depth (:circuit-depth original-stats)
        opt-circuit-depth (:circuit-depth optimized-stats)
        depth-delta (- orig-circuit-depth opt-circuit-depth)

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
  (def linear-topology [[1] [0 2] [1]])        ; Linear: 0-1-2

  ;; Test the topology-aware transformation
  (topo/topology-aware-transform test-circuit linear-topology supported-gates {})

  ;; Test the corrected pipeline
  (optimize test-circuit supported-gates linear-topology {:optimize-topology? true})

  ;
  )
