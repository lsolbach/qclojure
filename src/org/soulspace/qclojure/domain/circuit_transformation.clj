(ns org.soulspace.qclojure.domain.circuit-transformation
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming operations not supported
   by the backend into equivalent sequences of supported operations.
   
   Circuit Transformation:
   - Transforms entire quantum circuits to use only supported operations
   - Preserves quantum circuit semantics while changing implementation
   - Provides iterative decomposition with cycle detection"
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.gate-decomposition :as gd]))

;; Specs
(s/def ::transformation-result
  (s/keys :req-un [::qc/circuit]
          :opt-un [::transformed-operation-count ::unsupported-operations]))

(defn transform-circuit
  "Transform a quantum circuit to use only operations supported by a given backend.

  This function takes a quantum circuit and the supported operations, and returns a new circuit
  where all operations have been decomposed into the supported operations.
  
  Parameters:
  - circuit: Quantum circuit to transform
  - supported-operations: Set of operation types supported
  - options: Optional map with transformation options:
      :max-iterations - Maximum number of decomposition iterations (default: 100)
      :transform-unsupported? - Whether to transform unsupported operations (default: true)
  
  Returns:
  A map containing:
  - :circuit - The transformed circuit
  - :transformed-operation-count - Count of operations that were transformed
  - :unsupported-operations - List of operation types that couldn't be transformed
  
  Example:
  (transform-circuit my-circuit #{:h :x :cnot} {:max-iterations 50})
  ;=> {:circuit <transformed-circuit>, :transformed-operation-count 3, :unsupported-operations []}"
  ([ctx]
   {:pre [(s/valid? ::qc/circuit (:circuit ctx))]}

   (if-not (get-in ctx [:options :transform-operations?])
     ;; No transformation requested, return original context
     ctx
     ;; Perform transformation and return updated context
     (let [circuit (:circuit ctx)
           device (:device ctx)
           supported-operations (:supported-operations device)
           options (:options ctx {})
           max-iterations (get options :max-iterations 100)
           transform-unsupported? (get options :transform-unsupported? true)

           original-operations (:operations circuit)
           original-operation-count (count original-operations)

           ;; Apply transformation
           transformed-operations (if transform-unsupported?
                                    (gd/transform-operations original-operations supported-operations max-iterations)
                                    original-operations)

           ;; Create new circuit with transformed operations
           transformed-circuit (assoc circuit :operations transformed-operations)

           ;; Calculate stats for return value
           new-types (frequencies (map :operation-type transformed-operations))
           remaining-unsupported (into []
                                       (filter #(not (contains? supported-operations %))
                                               (keys new-types)))]

       (assoc ctx
              :circuit transformed-circuit
              :transformed-operation-count (- (count transformed-operations) original-operation-count)
              :unsupported-operations remaining-unsupported)))))

(comment
  ;; Example usage of circuit transformation

  ;; Create a test circuit with unsupported operations
  (def test-circuit
    {:num-qubits 3
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :t :operation-params {:target 1}}
                  {:operation-type :cnot :operation-params {:control 0 :target 2}}]})

  ;; Transform for a backend that only supports H, X, Z, and CNOT
  (def backend-gates #{:h :x :z :cnot})
  (def result (transform-circuit {:circuit test-circuit 
                                  :supported-gates backend-gates
                                  :options {:transform-operations? true
                                            :max-iterations 50}}))

  (println "Original operations:" (count (:operations test-circuit)))
  (println "Transformed operations:" (count (:operations (:circuit result))))
  (println "Transformation count:" (:transformed-operation-count result))
  (println "Unsupported operations:" (:unsupported-operations result))

  ;; The T gate should be decomposed into supported operations
  ;
  )
