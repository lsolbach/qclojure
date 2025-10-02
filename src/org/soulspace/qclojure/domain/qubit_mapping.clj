(ns org.soulspace.qclojure.domain.qubit-mapping
  "Qubit mapping utilities for the optimization pipeline.
   
   This namespace provides functions for creating, inverting, and composing
   qubit mappings throughout the optimization pipeline. These mappings are
   essential for tracing measurement results back to the original circuit.
   
   Mapping Chain in Pipeline:
   1. Qubit Optimization: original-logical → compacted-logical
   2. Error Correction: compacted-logical → physical+ancillas
   3. Topology Optimization: logical → hardware-physical
   
   Reverse Mapping Chain (for results):
   hardware-physical → logical → compacted-logical → original-logical")

;;;
;;; Forward Mapping Utilities
;;;
(defn compose-mappings
  "Compose two qubit mappings sequentially.
   
   Given mapping1: A → B and mapping2: B → C,
   returns composed mapping: A → C
   
   Parameters:
   - mapping1: First mapping {A → B}
   - mapping2: Second mapping {B → C}
   
   Returns:
   Composed mapping {A → C}
   
   Example:
   (compose-mappings {0 10, 1 20} {10 100, 20 200})
   ;=> {0 100, 1 200}"
  [mapping1 mapping2]
  (when (and mapping1 mapping2)
    (into {}
          (map (fn [[k v]]
                 [k (get mapping2 v v)])
               mapping1))))

;;;
;;; Reverse Mapping Utilities
;;;
(defn invert-simple-mapping
  "Invert a simple one-to-one qubit mapping.
   
   Parameters:
   - mapping: Map from source qubits to target qubits {source → target}
   
   Returns:
   Inverted mapping {target → source}
   
   Example:
   (invert-simple-mapping {0 0, 2 1, 4 2})
   ;=> {0 0, 1 2, 2 4}"
  [mapping]
  (when mapping
    (into {} (map (fn [[k v]] [v k]) mapping))))

(defn invert-vector-mapping
  "Invert a one-to-many mapping (logical to physical/ancilla).
   
   Parameters:
   - vector-mapping: Map from logical qubits to qubit vectors {log → [phys...]}
   
   Returns:
   Inverted mapping {phys → log}
   
   Example:
   (invert-vector-mapping {0 [0 1 2], 1 [3 4 5]})
   ;=> {0 0, 1 0, 2 0, 3 1, 4 1, 5 1}"
  [vector-mapping]
  (when vector-mapping
    (into {}
          (mapcat (fn [[logical qubit-vec]]
                    (map (fn [phys] [phys logical]) qubit-vec))
                  vector-mapping))))

;;;
;;; Pipeline-Specific Reverse Mappings
;;;
(defn create-reverse-qubit-mapping
  "Create reverse mapping from qubit optimization result.
   
   This inverts the qubit-mapping created by qubit optimization,
   allowing tracing from compacted logical qubits back to original logical qubits.
   
   Parameters:
   - ctx: Context containing :qubit-mapping {original → compacted}
   
   Returns:
   Context with added :inverse-qubit-mapping {compacted → original}"
  [ctx]
  (if-let [qubit-mapping (:qubit-mapping ctx)]
    (assoc ctx :inverse-qubit-mapping (invert-simple-mapping qubit-mapping))
    ctx))

(defn create-reverse-ec-mappings
  "Create reverse mappings from error correction result.
   
   This inverts the logical-to-physical and logical-to-ancillas mappings
   created by error correction, allowing tracing from physical qubits back
   to compacted logical qubits.
   
   Parameters:
   - ctx: Context containing :logical-to-physical and :logical-to-ancillas
   
   Returns:
   Context with added :physical-to-logical and :ancilla-to-logical"
  [ctx]
  (cond-> ctx
    (:logical-to-physical ctx)
    (assoc :physical-to-logical (invert-vector-mapping (:logical-to-physical ctx)))
    
    (:logical-to-ancillas ctx)
    (assoc :ancilla-to-logical (invert-vector-mapping (:logical-to-ancillas ctx)))))

(defn create-all-reverse-mappings
  "Create all reverse mappings in the context.
   
   This is a convenience function that creates all reverse mappings
   from qubit optimization and error correction.
   
   Parameters:
   - ctx: Context from optimization pipeline
   
   Returns:
   Context with all reverse mappings added"
  [ctx]
  (-> ctx
      create-reverse-qubit-mapping
      create-reverse-ec-mappings))

;;;
;;; Result Mapping Utilities
;;;
(defn map-qubit-to-original
  "Map a final qubit index back to original logical qubit.
   
   This composes the full reverse mapping chain:
   final-qubit → logical → compacted → original
   
   Parameters:
   - qubit-id: Final qubit index from optimized circuit
   - ctx: Context containing all reverse mappings
   
   Returns:
   Original logical qubit index, or nil if not mappable
   
   Example:
   Given mappings: physical-to-logical {7 2}, inverse-qubit-mapping {2 4}
   (map-qubit-to-original 7 ctx) ;=> 4"
  [qubit-id ctx]
  (let [;; Step 1: Check if it's a physical qubit (from error correction)
        physical-to-logical (:physical-to-logical ctx)
        compacted-logical (when physical-to-logical
                            (get physical-to-logical qubit-id))
        
        ;; If not physical, check if it's hardware-mapped (from topology)
        topo-physical-to-logical (:physical-to-logical ctx) ; topology may update this
        compacted-logical (or compacted-logical
                             (when topo-physical-to-logical
                               (get topo-physical-to-logical qubit-id)))
        
        ;; Step 2: Map from compacted to original logical (from qubit opt)
        inverse-qubit-mapping (:inverse-qubit-mapping ctx)
        original-logical (when (and compacted-logical inverse-qubit-mapping)
                          (get inverse-qubit-mapping compacted-logical compacted-logical))]
    
    original-logical))

(defn map-measurements-to-original
  "Map all measurement results back to original logical qubits.
   
   Parameters:
   - measurements: Map of qubit measurements {qubit-id → value}
   - ctx: Context containing all reverse mappings
   
   Returns:
   Map of measurements mapped to original logical qubits
   
   Example:
   (map-measurements-to-original {0 1, 1 0, 7 1} ctx)
   ;=> {0 1, 2 0, 4 1}  ; mapped to original qubits"
  [measurements ctx]
  (into {}
        (keep (fn [[qubit-id value]]
                (when-let [original-qubit (map-qubit-to-original qubit-id ctx)]
                  [original-qubit value]))
              measurements)))

(defn get-physical-qubits-for-logical
  "Get all physical qubit indices for an original logical qubit.
   
   This composes forward mappings to find which physical qubits
   correspond to a logical qubit from the original circuit.
   
   Parameters:
   - logical-qubit: Original logical qubit index
   - ctx: Context containing all forward mappings
   
   Returns:
   Vector of physical qubit indices, or empty vector if not found
   
   Example:
   (get-physical-qubits-for-logical 4 ctx)
   ;=> [6 7 8]  ; physical qubits for original logical qubit 4"
  [logical-qubit ctx]
  (let [;; Step 1: Map original to compacted (qubit optimization)
        qubit-mapping (:qubit-mapping ctx)
        compacted-logical (if qubit-mapping
                           (get qubit-mapping logical-qubit logical-qubit)
                           logical-qubit)
        
        ;; Step 2: Map compacted to physical (error correction)
        logical-to-physical (:logical-to-physical ctx)
        physical-qubits (if logical-to-physical
                         (get logical-to-physical compacted-logical)
                         [compacted-logical])]
    
    (or physical-qubits [])))

(defn get-ancilla-qubits-for-logical
  "Get all ancilla qubit indices for an original logical qubit.
   
   Parameters:
   - logical-qubit: Original logical qubit index
   - ctx: Context containing all forward mappings
   
   Returns:
   Vector of ancilla qubit indices, or empty vector if not found"
  [logical-qubit ctx]
  (let [;; Step 1: Map original to compacted
        qubit-mapping (:qubit-mapping ctx)
        compacted-logical (if qubit-mapping
                           (get qubit-mapping logical-qubit logical-qubit)
                           logical-qubit)
        
        ;; Step 2: Get ancillas for compacted logical
        logical-to-ancillas (:logical-to-ancillas ctx)
        ancilla-qubits (if logical-to-ancillas
                        (get logical-to-ancillas compacted-logical)
                        [])]
    
    (or ancilla-qubits [])))

;;;
;;; Mapping Summary and Debugging
;;;
(defn summarize-mappings
  "Create a human-readable summary of all mappings in the context.
   
   Parameters:
   - ctx: Context from optimization pipeline
   
   Returns:
   String with formatted mapping information"
  [ctx]
  (str "Qubit Mapping Summary:\n"
       "---------------------\n"
       "Forward Mappings:\n"
       "  qubit-mapping (orig→compact): " (:qubit-mapping ctx) "\n"
       "  logical-to-physical (compact→phys): " (:logical-to-physical ctx) "\n"
       "  logical-to-ancillas (compact→anc): " (:logical-to-ancillas ctx) "\n"
       "\nReverse Mappings:\n"
       "  inverse-qubit-mapping (compact→orig): " (:inverse-qubit-mapping ctx) "\n"
       "  physical-to-logical (phys→compact): " (:physical-to-logical ctx) "\n"
       "  ancilla-to-logical (anc→compact): " (:ancilla-to-logical ctx) "\n"))

(comment
  ;; Test mapping utilities
  (require '[org.soulspace.qclojure.domain.qubit-mapping :as mapping])

  ;; Test simple mapping inversion
  (mapping/invert-simple-mapping {0 0, 2 1, 4 2})
  ;=> {0 0, 1 2, 2 4}

  ;; Test vector mapping inversion
  (mapping/invert-vector-mapping {0 [0 1 2], 1 [3 4 5], 2 [6 7 8]})
  ;=> {0 0, 1 0, 2 0, 3 1, 4 1, 5 1, 6 2, 7 2, 8 2}

  ;; Test mapping composition
  (mapping/compose-mappings {0 10, 1 20} {10 100, 20 200})
  ;=> {0 100, 1 200}

  ;
  )