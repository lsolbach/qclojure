(ns org.soulspace.qclojure.domain.error-correction
  "Quantum error correction codes based on the stabilizer formalism.
   
   This namespace provides a flexible framework for implementing quantum error
   correction (QEC) codes using stabilizers. The stabilizer formalism is a
   powerful framework for describing and analyzing quantum error correction codes.
   
   Core Concepts:
   - Stabilizer: A Pauli operator that leaves the code space invariant
   - Stabilizer Generator: A minimal set of stabilizers that generate the full stabilizer group
   - Syndrome: Measurement outcomes that indicate which error occurred
   - Logical Operators: Pauli operators that act on the logical qubit
   
   Supported Codes:
   - Bit-flip code (3-qubit repetition)
   - Phase-flip code (3-qubit in Hadamard basis)
   - Shor code (9-qubit)
   - Steane code (7-qubit)
   - Five-qubit code (5-qubit perfect code)
   
   References:
   - Gottesman, D. (1997). Stabilizer Codes and Quantum Error Correction
   - Nielsen & Chuang, Quantum Computation and Quantum Information, Chapter 10"
  (:require [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.domain.observables :as obs]
            [org.soulspace.qclojure.domain.qubit-mapping :as mapping]))

;;;
;;; Stabilizer Code Definition
;;;
(s/def ::num-physical-qubits pos-int?)
(s/def ::num-logical-qubits pos-int?)
(s/def ::stabilizer-generators (s/coll-of ::obs/pauli-string :kind vector?))
(s/def ::logical-operators (s/keys :req-un [::X ::Z]))
(s/def ::distance pos-int?)

(s/def ::stabilizer-code
  (s/keys :req-un [::name
                   ::num-physical-qubits
                   ::num-logical-qubits
                   ::stabilizer-generators]
          :opt-un [::logical-operators
                   ::distance
                   ::description]))

;;;
;;; Syndrome Measurement
;;;
(s/def ::syndrome
  (s/coll-of #{0 1} :kind vector?))

(defn syndrome-weight
  "Calculate the weight of a syndrome (number of non-zero measurements).
   
   Parameters:
   - syndrome: Vector of 0s and 1s
   
   Returns:
   Integer count of 1s in syndrome"
  [syndrome]
  {:pre [(s/valid? ::syndrome syndrome)]}
  (count (filter #(= 1 %) syndrome)))

(defn trivial-syndrome?
  "Check if a syndrome indicates no errors (all zeros).
   
   Parameters:
   - syndrome: Vector of 0s and 1s
   
   Returns:
   Boolean indicating if syndrome is all zeros"
  [syndrome]
  {:pre [(s/valid? ::syndrome syndrome)]}
  (every? zero? syndrome))

;;;
;;; Error Correction Lookup Tables
;;;
(defn build-syndrome-table
  "Build a lookup table mapping syndromes to correctable errors.
   
   For a given stabilizer code, this generates all correctable single-qubit
   errors and their corresponding syndromes.
   
   Parameters:
   - code: Stabilizer code definition
   
   Returns:
   Map from syndrome vector to Pauli error string"
  [code]
  {:pre [(s/valid? ::stabilizer-code code)]}
  (let [n (:num-physical-qubits code)
        generators (:stabilizer-generators code)
        
        ;; Generate all single-qubit Pauli errors
        single-errors (for [i (range n)
                           p [\X \Y \Z]]
                       (apply str (concat (repeat i \I)
                                         [p]
                                         (repeat (- n i 1) \I))))]
    
    ;; For each error, calculate its syndrome
    (into {}
          (map (fn [error]
                 (let [syndrome (mapv (fn [gen]
                                       (if (obs/pauli-commute? error gen) 0 1))
                                     generators)]
                   [syndrome error]))
               single-errors))))

;;;
;;; Predefined Stabilizer Codes
;;;
(def bit-flip-code
  "The 3-qubit bit-flip code protects against single bit-flip (X) errors.
   
   Encoding:
   - |0⟩ → |000⟩
   - |1⟩ → |111⟩
   
   Stabilizer generators: Z₀Z₁, Z₁Z₂
   
   This code can detect and correct a single bit-flip error on any qubit."
  {:name "Bit-flip code"
   :num-physical-qubits 3
   :num-logical-qubits 1
   :stabilizer-generators ["ZZI" "IZZ"]
   :logical-operators {:X "XXX" :Z "ZII"}
   :distance 3
   :description "Three-qubit repetition code protecting against bit-flip errors"})

(def phase-flip-code
  "The 3-qubit phase-flip code protects against single phase-flip (Z) errors.
   
   Encoding:
   - |0⟩ → |+++⟩ = (|000⟩+|111⟩)/√2
   - |1⟩ → |---⟩ = (|000⟩-|111⟩)/√2
   
   Stabilizer generators: X₀X₁, X₁X₂
   
   This code can detect and correct a single phase-flip error on any qubit."
  {:name "Phase-flip code"
   :num-physical-qubits 3
   :num-logical-qubits 1
   :stabilizer-generators ["XXI" "IXX"]
   :logical-operators {:X "XII" :Z "ZZZ"}
   :distance 3
   :description "Three-qubit code protecting against phase-flip errors"})

(def shor-code
  "The 9-qubit Shor code protects against arbitrary single-qubit errors.
   
   This code combines the bit-flip and phase-flip codes to protect against
   both X and Z errors (and thus arbitrary errors by linearity).
   
   Stabilizer generators:
   - 6 Z-type stabilizers (for bit-flip detection in 3 blocks)
   - 2 X-type stabilizers (for phase-flip detection between blocks)
   
   Distance: 3 (can correct any single-qubit error)"
  {:name "Shor code"
   :num-physical-qubits 9
   :num-logical-qubits 1
   :stabilizer-generators ["ZZIIIIIII"  ; Z₀Z₁ for block 1
                           "IZZIIIIII"  ; Z₁Z₂ for block 1
                           "IIIIZZIII"  ; Z₃Z₄ for block 2
                           "IIIIIZZII"  ; Z₄Z₅ for block 2
                           "IIIIIIZZI"  ; Z₆Z₇ for block 3
                           "IIIIIIIZZ"  ; Z₇Z₈ for block 3
                           "XXXXXXIII"  ; X₀...X₂ X₃...X₅ for blocks 1 and 2
                           "IIIXXXXXX"]  ; X₃...X₅ X₆...X₈ for blocks 2 and 3
   :logical-operators {:X "XXXXXXXXX" :Z "ZIIIIIII"}
   :distance 3
   :description "Nine-qubit code protecting against arbitrary single-qubit errors"})

(def steane-code
  "The 7-qubit Steane code is a CSS code derived from the classical [7,4] Hamming code.
   
   This is one of the most efficient codes for correcting arbitrary single-qubit errors,
   using only 7 physical qubits to encode 1 logical qubit.
   
   Distance: 3 (can correct any single-qubit error)"
  {:name "Steane code"
   :num-physical-qubits 7
   :num-logical-qubits 1
   :stabilizer-generators ["IIIXXXX"
                           "IXXIIXX"
                           "XIXIXIX"
                           "IIIZZZZ"
                           "IZZIIZZ"
                           "ZIZIZIZ"]
   :logical-operators {:X "XXXXXXX" :Z "ZZZZZZZ"}
   :distance 3
   :description "Seven-qubit CSS code with efficient encoding"})

(def five-qubit-code
  "The 5-qubit perfect code is the smallest code that can correct arbitrary single-qubit errors.
   
   This code is optimal in the sense that it uses the minimum number of physical qubits
   (5) required to encode 1 logical qubit with distance 3.
   
   Distance: 3 (can correct any single-qubit error)"
  {:name "Five-qubit code"
   :num-physical-qubits 5
   :num-logical-qubits 1
   :stabilizer-generators ["XZZXI"
                           "IXZZX"
                           "XIXZZ"
                           "ZXIXZ"]
   :logical-operators {:X "XXXXX" :Z "ZZZZZ"}
   :distance 3
   :description "Smallest perfect code correcting arbitrary single-qubit errors"})

;;;
;;; Code Registry
;;;
(def available-codes
  "Registry of available quantum error correction codes."
  {:bit-flip bit-flip-code
   :phase-flip phase-flip-code
   :shor shor-code
   :steane steane-code
   :five-qubit five-qubit-code})

(defn get-code
  "Retrieve a predefined error correction code by keyword.
   
   Parameters:
   - code-key: Keyword identifying the code (e.g., :bit-flip, :shor)
   
   Returns:
   Stabilizer code definition, or nil if not found"
  [code-key]
  (get available-codes code-key))

(defn list-available-codes
  "List all available error correction codes.
   
   Returns:
   Vector of maps with :key, :name, and :description for each code"
  []
  (mapv (fn [[k code]]
          {:key k
           :name (:name code)
           :description (:description code)
           :physical-qubits (:num-physical-qubits code)
           :logical-qubits (:num-logical-qubits code)
           :distance (:distance code)})
        available-codes))

;;;
;;; Forward declarations for encoding and syndrome measurement functions
;;;
(declare encode-bit-flip encode-shor encode-steane encode-five-qubit)
(declare measure-bit-flip-syndrome measure-shor-syndrome 
         measure-steane-syndrome measure-five-qubit-syndrome)

;;;
;;; Error Correction Application
;;;

(defn- get-encoding-function
  "Get the encoding function for a specific error correction code.
   
   Parameters:
   - code-key: Keyword identifying the code (:bit-flip, :shor, etc.)
   
   Returns:
   Function that takes [circuit logical-qubit-index] and returns encoded circuit"
  [code-key]
  (case code-key
    :bit-flip encode-bit-flip
    :shor encode-shor
    :steane encode-steane
    :five-qubit encode-five-qubit
    ;; Phase-flip code: bit-flip in Hadamard basis
    :phase-flip (fn [circuit q] 
                  (-> circuit
                      (circuit/h-gate q)
                      (encode-bit-flip q)
                      (circuit/h-gate q)
                      (circuit/h-gate (+ q 1))
                      (circuit/h-gate (+ q 2))))
    (throw (ex-info "No encoding function for code"
                    {:code-key code-key}))))

(defn- translate-operation-to-physical
  "Translate a logical operation to physical qubit operations.
   
   For error-corrected qubits, operations must be applied to all physical
   qubits that encode the logical qubit.
   
   Parameters:
   - operation: Original operation on logical qubit
   - logical-to-physical: Map from logical qubit index to vector of physical indices
   
   Returns:
   Vector of operations on physical qubits"
  [operation logical-to-physical]
  (let [op-type (:operation-type operation)]
    (case op-type
      ;; Single-qubit gates: apply to all physical qubits in the block
      (:x :y :z :h :s :t :sdg :tdg)
      (let [logical-qubit (get-in operation [:operation-params :target])
            physical-qubits (get logical-to-physical logical-qubit)]
        (if physical-qubits
          (mapv (fn [phys-q]
                  (assoc-in operation [:operation-params :target] phys-q))
                physical-qubits)
          [operation]))
      
      ;; Two-qubit gates: apply between corresponding physical qubits
      (:cnot :cz :swap)
      (let [control (get-in operation [:operation-params :control])
            target (get-in operation [:operation-params :target])
            control-physical (get logical-to-physical control)
            target-physical (get logical-to-physical target)]
        (if (and control-physical target-physical)
          ;; Apply gate between each pair of physical qubits
          (vec (for [c control-physical
                     t target-physical]
                 (-> operation
                     (assoc-in [:operation-params :control] c)
                     (assoc-in [:operation-params :target] t))))
          [operation]))
      
      ;; Controlled gates with multiple controls
      (:ccnot :toffoli)
      (let [controls (get-in operation [:operation-params :controls])
            target (get-in operation [:operation-params :target])
            target-physical (get logical-to-physical target)]
        (if (and controls target-physical)
          ;; For now, apply to first physical qubit of each control and all target qubits
          ;; This is simplified - full implementation would be more complex
          (mapv (fn [t]
                  (assoc-in operation [:operation-params :target] t))
                target-physical)
          [operation]))
      
      ;; Measurement: measure all physical qubits
      :measure
      (let [qubits (or (get-in operation [:operation-params :measurement-qubits])
                       (get-in operation [:operation-params :qubits]))]
        (if (vector? qubits)
          ;; Multiple qubits to measure - expand each to physical qubits
          (let [all-physical (vec (mapcat #(get logical-to-physical % [%]) qubits))]
            [(assoc-in operation [:operation-params :measurement-qubits] all-physical)])
          ;; Single qubit (old format with :target)
          (let [logical-qubit (get-in operation [:operation-params :target])
                physical-qubits (get logical-to-physical logical-qubit)]
            (if physical-qubits
              [(assoc operation :operation-params {:measurement-qubits physical-qubits})]
              [operation]))))
      
      ;; Unknown operation type: pass through
      [operation])))

(defn inject-syndrome-measurements
  "Add syndrome measurement operations to an error-corrected circuit.
   
   This function adds the syndrome measurement gates and operations that detect
   errors in the encoded qubits. The syndrome measurements are non-destructive:
   they detect errors without collapsing the quantum state of the logical qubits.
   
   For bit-flip code:
   - Adds CNOT gates to measure Z₀Z₁ and Z₁Z₂ stabilizers
   - Measures 2 ancilla qubits per logical qubit
   
   For Shor code:
   - Adds bit-flip syndrome measurements (6 ancillas per logical qubit)
   - Adds phase-flip syndrome measurements (2 ancillas per logical qubit)
   - Total: 8 ancilla measurements per logical qubit
   
   Parameters:
   - circuit: Circuit with encoded qubits
   - code-key: Error correction code (:bit-flip or :shor)
   - logical-to-physical: Map from logical qubit indices to physical qubit vectors
   - logical-to-ancillas: Map from logical qubit indices to ancilla qubit vectors
   
   Returns:
   Updated circuit with syndrome measurement operations added"
  [circuit code-key logical-to-physical logical-to-ancillas]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (map? logical-to-physical)
         (map? logical-to-ancillas)]}
  (let [logical-qubits (keys logical-to-physical)]
    (reduce (fn [circ logical-q]
              (let [physical-qubits (get logical-to-physical logical-q)
                    ancilla-qubits (get logical-to-ancillas logical-q)]
                (case code-key
                  :bit-flip
                  (measure-bit-flip-syndrome circ physical-qubits ancilla-qubits)
                  
                  :shor
                  (let [bit-flip-ancillas (subvec ancilla-qubits 0 6)
                        phase-flip-ancillas (subvec ancilla-qubits 6 8)]
                    (measure-shor-syndrome circ
                                           physical-qubits
                                           bit-flip-ancillas
                                           phase-flip-ancillas))
                  
                  :steane
                  (measure-steane-syndrome circ physical-qubits ancilla-qubits)
                  
                  :five-qubit
                  (measure-five-qubit-syndrome circ physical-qubits ancilla-qubits)
                  
                  ;; Unknown code - should not happen due to validation earlier
                  (throw (ex-info "Unsupported error correction code for syndrome measurement"
                                  {:code-key code-key
                                   :logical-qubit logical-q})))))
            circuit
            logical-qubits)))

(defn apply-error-correction
  "Apply error correction encoding to a circuit context.
   
   This function integrates error correction into the optimization pipeline,
   transforming the circuit to use physical qubits with error correction.
   
   Process:
   1. Encode ALL logical qubits in the circuit
   2. Create new circuit with enough physical qubits for encoding + ancillas
   3. Apply encoding gates for each logical qubit
   4. Translate all existing operations to work on physical qubits
   5. Add syndrome measurement operations (NEW in Phase 1)
   
   Parameters:
   - ctx: Optimization context containing:
       :circuit - The quantum circuit (with logical qubits)
       :options - Map with:
         :error-correction-code - Keyword for the code to use (e.g., :bit-flip)
         :apply-error-correction? - Whether to apply error correction (default: false)
         :include-syndrome-measurement? - Whether to add syndrome measurements (default: true)
   
   Returns:
   Updated context with:
   - :circuit - Expanded circuit with error correction encoding + syndrome measurement
   - :error-correction-code - The code definition
   - :logical-to-physical - Mapping from logical qubit indices to physical qubit vectors
   - :syndrome-table - Syndrome lookup table
   - :error-correction-applied? - true
   - :syndrome-measurement-included? - true if syndrome measurements were added"
  [ctx]
  (if (get-in ctx [:options :apply-error-correction?] false)
    (let [code-key (get-in ctx [:options :error-correction-code] :bit-flip)
          code (get-code code-key)
          _ (when-not code
              (throw (ex-info "Unknown error correction code"
                              {:code-key code-key
                               :available-codes (keys available-codes)})))
          
          original-circuit (:circuit ctx)
          num-logical-qubits (:num-qubits original-circuit)
          
          ;; Encode ALL logical qubits
          qubits-to-encode (vec (range num-logical-qubits))
          
          ;; Calculate physical qubit requirements
          physical-per-logical (:num-physical-qubits code)
          num-physical-qubits (* num-logical-qubits physical-per-logical)
          
          ;; Calculate ancilla requirements for syndrome measurement
          include-syndrome? (get-in ctx [:options :include-syndrome-measurement?] true)
          ancillas-per-logical (if include-syndrome?
                                 (case code-key
                                   :bit-flip 2      ; 2 ancillas for bit-flip syndrome
                                   :shor 8          ; 8 ancillas for Shor syndrome (6 bit-flip + 2 phase-flip)
                                   :steane 6        ; 6 ancillas for Steane syndrome (3 X-type + 3 Z-type)
                                   :five-qubit 4    ; 4 ancillas for 5-qubit code syndrome
                                   0)
                                 0)
          num-ancilla-qubits (* num-logical-qubits ancillas-per-logical)
          total-qubits (+ num-physical-qubits num-ancilla-qubits)
          
          ;; Create mapping from logical to physical qubits
          logical-to-physical (into {}
                                    (map-indexed
                                     (fn [idx logical-q]
                                       [logical-q
                                        (vec (range (* idx physical-per-logical)
                                                    (* (inc idx) physical-per-logical)))])
                                     qubits-to-encode))
          
          ;; Create mapping from logical qubits to ancilla qubits
          logical-to-ancillas (when include-syndrome?
                                (into {}
                                      (map-indexed
                                       (fn [idx logical-q]
                                         [logical-q
                                          (vec (range (+ num-physical-qubits (* idx ancillas-per-logical))
                                                      (+ num-physical-qubits (* (inc idx) ancillas-per-logical))))])
                                       qubits-to-encode)))
          
          ;; Get encoding function
          encode-fn (get-encoding-function code-key)
          
          ;; Create new circuit with physical qubits + ancillas
          physical-circuit (circuit/create-circuit total-qubits)
          
          ;; Apply encoding for each logical qubit
          encoded-circuit (reduce (fn [circ logical-q]
                                    (let [physical-start (* logical-q physical-per-logical)]
                                      (encode-fn circ physical-start)))
                                  physical-circuit
                                  qubits-to-encode)
          
          ;; Translate all original operations to physical qubits
          translated-ops (vec (mapcat #(translate-operation-to-physical % logical-to-physical)
                                      (:operations original-circuit)))
          
          ;; Combine encoding operations with translated operations
          circuit-with-ops (update encoded-circuit :operations
                                   (fn [encoding-ops]
                                     (vec (concat encoding-ops translated-ops))))
          
          ;; Add syndrome measurement operations (Phase 1 enhancement)
          final-circuit (if include-syndrome?
                          (inject-syndrome-measurements circuit-with-ops
                                                        code-key
                                                        logical-to-physical
                                                        logical-to-ancillas)
                          circuit-with-ops)]
      
      (assoc ctx
             :circuit final-circuit
             :error-correction-code code
             :logical-to-physical logical-to-physical
             :logical-to-ancillas logical-to-ancillas
             :syndrome-table (build-syndrome-table code)
             :error-correction-applied? true
             :syndrome-measurement-included? include-syndrome?))
    ctx))

(comment
  ;; Test Pauli string operations
  (obs/pauli-string? "XYZII")
  ;=> true
  
  (obs/pauli-weight "XYZII")
  ;=> 3
  
  (obs/pauli-commute? "XII" "IXI")
  ;=> true
  
  (obs/pauli-commute? "XII" "ZII")
  ;=> false

  ;; Test stabilizer codes
  (list-available-codes)
  
  (get-code :bit-flip)
  
  (get-code :shor)

  ;; Test syndrome table generation
  (build-syndrome-table bit-flip-code)
  
  (build-syndrome-table shor-code)

  ;;
  ;; Circuit Transformation with Error Correction
  ;;
  
  ;; Test error correction application - transforms the circuit!
  (def simple-ctx
    {:circuit (-> (circuit/create-circuit 2)
                  (circuit/h-gate 0)
                  (circuit/cnot-gate 0 1))
     :options {:apply-error-correction? true
               :error-correction-code :bit-flip}})
  
  (def transformed-ctx (apply-error-correction simple-ctx))
  
  ;; Observe the transformation
  {:original-qubits 2
   :physical-qubits (get-in transformed-ctx [:circuit :num-qubits])
   :logical-to-physical (:logical-to-physical transformed-ctx)
   :operations-before 2
   :operations-after (count (get-in transformed-ctx [:circuit :operations]))}
  ;=> {:original-qubits 2, :physical-qubits 6, 
  ;    :logical-to-physical {0 [0 1 2], 1 [3 4 5]},
  ;    :operations-before 2, :operations-after 16}
  
  ;; With Shor code (9x expansion)
  (def shor-ctx
    {:circuit (circuit/create-circuit 1)
     :options {:apply-error-correction? true
               :error-correction-code :shor}})
  
  (def shor-transformed (apply-error-correction shor-ctx))
  
  {:original-qubits 1
   :physical-qubits (get-in shor-transformed [:circuit :num-qubits])
   :logical-to-physical (:logical-to-physical shor-transformed)}
  ;=> {:original-qubits 1, :physical-qubits 9,
  ;    :logical-to-physical {0 [0 1 2 3 4 5 6 7 8]}}
  
  ;;
  ;; Integration with Optimization Pipeline
  ;;
  
  (require '[org.soulspace.qclojure.application.hardware-optimization :as hw])
  
  ;; Create a Bell state circuit
  (def bell-circuit
    (-> (circuit/create-circuit 2)
        (circuit/h-gate 0)
        (circuit/cnot-gate 0 1)))
  
  ;; Run through full pipeline with error correction
  (def optimized
    (hw/optimize
     {:circuit bell-circuit
      :supported-operations #{:h :cnot :x :y :z :measure}
      :options {:apply-error-correction? true
                :error-correction-code :bit-flip}}))
  
  ;; Check the results
  {:ec-applied? (:error-correction-applied? optimized)
   :physical-qubits (get-in optimized [:circuit :num-qubits])
   :logical-to-physical (:logical-to-physical optimized)
   :pipeline (:pipeline-order optimized)}
  ;=> {:ec-applied? true, :physical-qubits 6,
  ;    :logical-to-physical {0 [0 1 2], 1 [3 4 5]},
  ;    :pipeline [:gate-cancellation :qubit-optimization :error-correction 
  ;               :topology-optimization :gate-decomposition :validation]}
  
  ;;
  ;; Complete Example: Circuit Transformation Details
  ;;
  
  (def example-circuit
    (-> (circuit/create-circuit 3)
        (circuit/h-gate 0)
        (circuit/cnot-gate 0 1)
        (circuit/cnot-gate 1 2)
        (circuit/measure-operation [0 1 2])))
  
  (def example-result
    (apply-error-correction
     {:circuit example-circuit
      :options {:apply-error-correction? true
                :error-correction-code :bit-flip}}))
  
  ;; Original: 3 logical qubits, 4 operations
  ;; Encoded: 9 physical qubits, 28 operations
  ;; - 6 encoding CNOTs (2 per logical qubit)
  ;; - 3 translated H gates (q0 → [0,1,2])
  ;; - 9 translated CNOTs (q0→q1: [0,1,2]→[3,4,5])
  ;; - 9 translated CNOTs (q1→q2: [3,4,5]→[6,7,8])
  ;; - 1 measurement on all 9 physical qubits
  
  {:original {:qubits 3, :ops 4}
   :encoded {:qubits (get-in example-result [:circuit :num-qubits])
             :ops (count (get-in example-result [:circuit :operations]))
             :op-types (frequencies (map :operation-type 
                                         (get-in example-result [:circuit :operations])))}}
  ;=> {:original {:qubits 3, :ops 4},
  ;    :encoded {:qubits 9, :ops 28, 
  ;              :op-types {:cnot 24, :h 3, :measure 1}}}
  
  )

;;;
;;; Bit-Flip Code Implementation
;;;
(defn encode-bit-flip
  "Encode a logical qubit into the 3-qubit bit-flip code.
   
   The encoding circuit applies two CNOT gates to create the encoded state:
   - |0⟩ → |000⟩
   - |1⟩ → |111⟩
   
   Parameters:
   - circuit: Quantum circuit
   - logical-qubit: Index of the logical qubit to encode (will use qubits logical-qubit, logical-qubit+1, logical-qubit+2)
   
   Returns:
   Updated circuit with encoding operations added"
  [circuit logical-qubit]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (>= logical-qubit 0)
         (< (+ logical-qubit 2) (:num-qubits circuit))]}
  (-> circuit
      (circuit/cnot-gate logical-qubit (+ logical-qubit 1))
      (circuit/cnot-gate logical-qubit (+ logical-qubit 2))))

(defn measure-bit-flip-syndrome
  "Measure the syndrome for the 3-qubit bit-flip code.
   
   This adds ancilla qubits and measurement operations to detect errors
   without destroying the encoded quantum information.
   
   The syndrome measurements check:
   - Z₀Z₁ (compares first two qubits)
   - Z₁Z₂ (compares last two qubits)
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 3 qubit indices [q0 q1 q2] containing the encoded logical qubit
   - ancilla-qubits: Vector of 2 ancilla qubit indices for syndrome measurement
   
   Returns:
   Updated circuit with syndrome measurement operations"
  [circuit encoded-qubits ancilla-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 3 (count encoded-qubits))
         (= 2 (count ancilla-qubits))]}
  (let [[q0 q1 q2] encoded-qubits
        [a0 a1] ancilla-qubits]
    (-> circuit
        ;; Measure Z₀Z₁ using first ancilla
        (circuit/cnot-gate q0 a0)
        (circuit/cnot-gate q1 a0)
        
        ;; Measure Z₁Z₂ using second ancilla
        (circuit/cnot-gate q1 a1)
        (circuit/cnot-gate q2 a1)
        
        ;; Measure ancilla qubits to get syndrome
        (circuit/measure-operation [a0 a1]))))

(defn correct-bit-flip-error
  "Apply correction based on bit-flip syndrome.
   
   Syndrome interpretation:
   - [0 0] -> No error
   - [1 0] -> Error on first qubit
   - [1 1] -> Error on second qubit
   - [0 1] -> Error on third qubit
   
   Parameters:
   - circuit: Quantum circuit
   - encoded-qubits: Vector of 3 qubit indices containing the encoded logical qubit
   - syndrome: Vector of 2 measurement outcomes [s0 s1]
   
   Returns:
   Updated circuit with correction applied"
  [circuit encoded-qubits syndrome]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 3 (count encoded-qubits))
         (= 2 (count syndrome))]}
  (let [[q0 q1 q2] encoded-qubits
        [_s0 _s1] syndrome]
    (cond
      ;; No error
      (= [0 0] syndrome)
      circuit
      
      ;; Error on first qubit
      (= [1 0] syndrome)
      (circuit/x-gate circuit q0)
      
      ;; Error on second qubit
      (= [1 1] syndrome)
      (circuit/x-gate circuit q1)
      
      ;; Error on third qubit
      (= [0 1] syndrome)
      (circuit/x-gate circuit q2)
      
      ;; Unknown syndrome (should not happen for single-qubit errors)
      :else
      (throw (ex-info "Unknown syndrome for bit-flip code"
                      {:syndrome syndrome
                       :encoded-qubits encoded-qubits})))))

(defn decode-bit-flip
  "Decode a 3-qubit bit-flip code back to a logical qubit.
   
   The decoding circuit is the inverse of the encoding:
   applies two CNOT gates in reverse order.
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 3 qubit indices [q0 q1 q2]
   
   Returns:
   Updated circuit with decoding operations, logical qubit is in q0"
  [circuit encoded-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 3 (count encoded-qubits))]}
  (let [[q0 q1 q2] encoded-qubits]
    (-> circuit
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q1))))

;;;
;;; Shor Code Implementation (9-qubit code)
;;;
(defn encode-shor
  "Encode a logical qubit into the 9-qubit Shor code.
   
   The Shor code combines bit-flip and phase-flip protection:
   1. First, apply phase-flip encoding (creates 3 qubits in superposition)
   2. Then, apply bit-flip encoding to each of the 3 blocks
   
   Encoding:
   - |0⟩ → 1/2√2 [(|000⟩+|111⟩) ⊗ (|000⟩+|111⟩) ⊗ (|000⟩+|111⟩)]
   - |1⟩ → 1/2√2 [(|000⟩-|111⟩) ⊗ (|000⟩-|111⟩) ⊗ (|000⟩-|111⟩)]
   
   Parameters:
   - circuit: Quantum circuit
   - logical-qubit: Index of the first qubit (will use qubits 0-8)
   
   Returns:
   Updated circuit with Shor encoding operations"
  [circuit logical-qubit]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (>= logical-qubit 0)
         (< (+ logical-qubit 8) (:num-qubits circuit))]}
  (let [q0 logical-qubit
        q1 (+ logical-qubit 1)
        q2 (+ logical-qubit 2)
        q3 (+ logical-qubit 3)
        q4 (+ logical-qubit 4)
        q5 (+ logical-qubit 5)
        q6 (+ logical-qubit 6)
        q7 (+ logical-qubit 7)
        q8 (+ logical-qubit 8)]
    (-> circuit
        ;; Phase-flip encoding (create 3-qubit superposition)
        (circuit/cnot-gate q0 q3)
        (circuit/cnot-gate q0 q6)
        
        ;; Apply Hadamard to create superposition states
        (circuit/h-gate q0)
        (circuit/h-gate q3)
        (circuit/h-gate q6)
        
        ;; Bit-flip encoding for first block (qubits 0, 1, 2)
        (circuit/cnot-gate q0 q1)
        (circuit/cnot-gate q0 q2)
        
        ;; Bit-flip encoding for second block (qubits 3, 4, 5)
        (circuit/cnot-gate q3 q4)
        (circuit/cnot-gate q3 q5)
        
        ;; Bit-flip encoding for third block (qubits 6, 7, 8)
        (circuit/cnot-gate q6 q7)
        (circuit/cnot-gate q6 q8))))

(defn measure-shor-syndrome
  "Measure the syndrome for the 9-qubit Shor code.
   
   The Shor code requires:
   - 6 ancilla qubits for bit-flip syndrome (2 per block)
   - 2 ancilla qubits for phase-flip syndrome
   Total: 8 ancilla qubits
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 9 qubit indices for the encoded logical qubit
   - bit-flip-ancillas: Vector of 6 ancilla qubit indices for bit-flip detection
   - phase-flip-ancillas: Vector of 2 ancilla qubit indices for phase-flip detection
   
   Returns:
   Updated circuit with syndrome measurement operations"
  [circuit encoded-qubits bit-flip-ancillas phase-flip-ancillas]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 9 (count encoded-qubits))
         (= 6 (count bit-flip-ancillas))
         (= 2 (count phase-flip-ancillas))]}
  (let [[q0 q1 q2 q3 q4 q5 q6 q7 q8] encoded-qubits
        [a0 a1 a2 a3 a4 a5] bit-flip-ancillas
        [p0 p1] phase-flip-ancillas]
    (-> circuit
        ;; Bit-flip syndrome for block 1 (qubits 0, 1, 2)
        (circuit/cnot-gate q0 a0)
        (circuit/cnot-gate q1 a0)
        (circuit/cnot-gate q1 a1)
        (circuit/cnot-gate q2 a1)
        
        ;; Bit-flip syndrome for block 2 (qubits 3, 4, 5)
        (circuit/cnot-gate q3 a2)
        (circuit/cnot-gate q4 a2)
        (circuit/cnot-gate q4 a3)
        (circuit/cnot-gate q5 a3)
        
        ;; Bit-flip syndrome for block 3 (qubits 6, 7, 8)
        (circuit/cnot-gate q6 a4)
        (circuit/cnot-gate q7 a4)
        (circuit/cnot-gate q7 a5)
        (circuit/cnot-gate q8 a5)
        
        ;; Phase-flip syndrome (compare blocks using Hadamard basis)
        ;; Measure X₀X₁X₂ X₃X₄X₅ and X₃X₄X₅ X₆X₇X₈
        (circuit/h-gate q0) (circuit/h-gate q1) (circuit/h-gate q2)
        (circuit/h-gate q3) (circuit/h-gate q4) (circuit/h-gate q5)
        (circuit/h-gate q6) (circuit/h-gate q7) (circuit/h-gate q8)
        
        ;; Compare first two blocks
        (circuit/cnot-gate q0 p0)
        (circuit/cnot-gate q1 p0)
        (circuit/cnot-gate q2 p0)
        (circuit/cnot-gate q3 p0)
        (circuit/cnot-gate q4 p0)
        (circuit/cnot-gate q5 p0)
        
        ;; Compare last two blocks
        (circuit/cnot-gate q3 p1)
        (circuit/cnot-gate q4 p1)
        (circuit/cnot-gate q5 p1)
        (circuit/cnot-gate q6 p1)
        (circuit/cnot-gate q7 p1)
        (circuit/cnot-gate q8 p1)
        
        ;; Undo Hadamards
        (circuit/h-gate q0) (circuit/h-gate q1) (circuit/h-gate q2)
        (circuit/h-gate q3) (circuit/h-gate q4) (circuit/h-gate q5)
        (circuit/h-gate q6) (circuit/h-gate q7) (circuit/h-gate q8)
        
        ;; Measure all ancilla qubits
        (circuit/measure-operation (vec (concat bit-flip-ancillas phase-flip-ancillas))))))

(defn correct-shor-error
  "Apply correction based on Shor code syndrome.
   
   The syndrome has two parts:
   - Bit-flip syndromes: 6 measurements (2 per block) indicating X errors
   - Phase-flip syndromes: 2 measurements indicating Z errors between blocks
   
   Parameters:
   - circuit: Quantum circuit
   - encoded-qubits: Vector of 9 qubit indices
   - bit-flip-syndrome: Vector of 6 measurement outcomes for bit-flip errors
   - phase-flip-syndrome: Vector of 2 measurement outcomes for phase-flip errors
   
   Returns:
   Updated circuit with correction applied"
  [circuit encoded-qubits bit-flip-syndrome phase-flip-syndrome]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 9 (count encoded-qubits))
         (= 6 (count bit-flip-syndrome))
         (= 2 (count phase-flip-syndrome))]}
  (let [[q0 q1 q2 q3 q4 q5 q6 q7 q8] encoded-qubits
        ;; Parse bit-flip syndromes for each block
        [s0 s1 s2 s3 s4 s5] bit-flip-syndrome
        [p0 p1] phase-flip-syndrome
        
        ;; Determine which qubit in each block has bit-flip error
        block1-error (cond
                       (= [0 0] [s0 s1]) nil
                       (= [1 0] [s0 s1]) q0
                       (= [1 1] [s0 s1]) q1
                       (= [0 1] [s0 s1]) q2)
        block2-error (cond
                       (= [0 0] [s2 s3]) nil
                       (= [1 0] [s2 s3]) q3
                       (= [1 1] [s2 s3]) q4
                       (= [0 1] [s2 s3]) q5)
        block3-error (cond
                       (= [0 0] [s4 s5]) nil
                       (= [1 0] [s4 s5]) q6
                       (= [1 1] [s4 s5]) q7
                       (= [0 1] [s4 s5]) q8)
        
        ;; Determine which block has phase-flip error
        phase-error-block (cond
                            (= [0 0] [p0 p1]) nil    ; No phase error
                            (= [1 0] [p0 p1]) 1      ; Error in block 1
                            (= [1 1] [p0 p1]) 2      ; Error in block 2
                            (= [0 1] [p0 p1]) 3)]    ; Error in block 3
    
    (cond-> circuit
      ;; Correct bit-flip errors
      block1-error (circuit/x-gate block1-error)
      block2-error (circuit/x-gate block2-error)
      block3-error (circuit/x-gate block3-error)
      
      ;; Correct phase-flip errors (apply Z to all qubits in the block)
      (= phase-error-block 1) (-> (circuit/z-gate q0)
                                   (circuit/z-gate q1)
                                   (circuit/z-gate q2))
      (= phase-error-block 2) (-> (circuit/z-gate q3)
                                   (circuit/z-gate q4)
                                   (circuit/z-gate q5))
      (= phase-error-block 3) (-> (circuit/z-gate q6)
                                   (circuit/z-gate q7)
                                   (circuit/z-gate q8)))))

(defn decode-shor
  "Decode a 9-qubit Shor code back to a logical qubit.
   
   The decoding reverses the encoding operations:
   1. Undo bit-flip encoding in each block
   2. Undo Hadamards
   3. Undo phase-flip encoding
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 9 qubit indices
   
   Returns:
   Updated circuit with decoding operations, logical qubit is in first position"
  [circuit encoded-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 9 (count encoded-qubits))]}
  (let [[q0 q1 q2 q3 q4 q5 q6 q7 q8] encoded-qubits]
    (-> circuit
        ;; Undo bit-flip encoding for all blocks
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q1)
        (circuit/cnot-gate q3 q5)
        (circuit/cnot-gate q3 q4)
        (circuit/cnot-gate q6 q8)
        (circuit/cnot-gate q6 q7)
        
        ;; Undo Hadamards
        (circuit/h-gate q0)
        (circuit/h-gate q3)
        (circuit/h-gate q6)
        
        ;; Undo phase-flip encoding
        (circuit/cnot-gate q0 q6)
        (circuit/cnot-gate q0 q3))))

(comment
  ;;
  ;; Stabilizer Framework Tests
  ;;
  
  ;; Test Pauli string operations
  (obs/pauli-string? "XYZII")
  ;=> true
  
  (obs/pauli-weight "XYZII")
  ;=> 3
  
  (obs/pauli-commute? "XII" "IXI")
  ;=> true
  
  (obs/pauli-commute? "XII" "ZII")
  ;=> false

  ;; Test stabilizer codes
  (list-available-codes)
  ;=> [{:key :bit-flip, :name "Bit-flip code", ...}
  ;    {:key :shor, :name "Shor code", ...}
  ;    ...]
  
  (get-code :shor)
  ;=> {:name "Shor code", :num-physical-qubits 9, ...}

  ;; Test syndrome table generation
  (def shor-table (build-syndrome-table shor-code))
  (count shor-table)
  ;=> 20+ syndromes (errors detectable by the code)
  
  ;;
  ;; Shor Code Tests (9-qubit arbitrary error protection)
  ;;
  
  ;; Create circuit with 9 data qubits
  (def shor-test-circuit (circuit/create-circuit 9))
  
  ;; Prepare state and encode
  (def shor-encoded
    (-> shor-test-circuit
        (circuit/h-gate 0)      ; Prepare superposition
        (encode-shor 0)))       ; Encode with Shor code
  
  ;; Count operations (should be 11: H + 2 CNOTs + 3 Hs + 6 CNOTs)
  (count (:operations shor-encoded))
  ;=> 11
  
  ;; Simulate bit-flip error on qubit 4 (middle of block 2)
  (def shor-with-bit-error
    (circuit/x-gate shor-encoded 4))
  
  ;; Simulate phase-flip error on block 1 (apply Z to qubits 0, 1, 2)
  (def shor-with-phase-error
    (-> shor-encoded
        (circuit/z-gate 0)
        (circuit/z-gate 1)
        (circuit/z-gate 2)))
  
  ;; Measure syndrome with ancilla qubits (requires 8 ancillas total)
  (def shor-full-circuit (circuit/create-circuit 17))  ; 9 data + 8 ancilla
  (def shor-prepared
    (-> shor-full-circuit
        (circuit/h-gate 0)
        (encode-shor 0)))
  
  ;; Introduce error and measure
  (def shor-with-error
    (circuit/x-gate shor-prepared 4))
  
  (def shor-measured
    (measure-shor-syndrome shor-with-error
                           [0 1 2 3 4 5 6 7 8]        ; data qubits
                           [9 10 11 12 13 14]         ; bit-flip ancillas
                           [15 16]))                   ; phase-flip ancillas
  
  ;; Apply correction based on syndrome
  (def shor-corrected
    (correct-shor-error shor-with-error
                        [0 1 2 3 4 5 6 7 8]
                        [0 0 1 1 0 0]  ; bit-flip syndrome: error in block 2, qubit 1
                        [0 0]))         ; no phase error
  
  ;; Verify correction applied X gate to qubit 4
  (last (:operations shor-corrected))
  ;=> {:operation-type :x, :operation-params {:target 4}}
  
  ;; Decode Shor code
  (def shor-decoded
    (decode-shor (circuit/create-circuit 9) [0 1 2 3 4 5 6 7 8]))
  
  ;; Verify decoding operations
  (count (:operations shor-decoded))
  ;=> 11 operations (inverse of encoding)

  ;; Test stabilizer codes
  (list-available-codes)
  
  (get-code :bit-flip)
  
  (get-code :shor)

  ;; Test syndrome table generation
  (def bit-flip-table (build-syndrome-table bit-flip-code))
  bit-flip-table
  ;=> {[1 0] "XII", [1 1] "IXI", [0 1] "IIX", ...}
  
  (def shor-table (build-syndrome-table shor-code))
  (count shor-table)  ; Should have 27 entries (9 qubits * 3 errors)
  
  ;;
  ;; Bit-Flip Code Tests
  ;;
  
  ;; Create a simple circuit with 3 qubits
  (def test-circuit (circuit/create-circuit 3))
  
  ;; Prepare a superposition state on the first qubit
  (def prepared-circuit
    (-> test-circuit
        (circuit/h-gate 0)))  ; Put qubit 0 in |+⟩ state
  
  ;; Encode this logical qubit using bit-flip code
  (def encoded-circuit
    (encode-bit-flip prepared-circuit 0))
  
  ;; Show the encoding operations
  (:operations encoded-circuit)
  ;=> [{:operation-type :h, :operation-params {:target 0}}
  ;    {:operation-type :cnot, :operation-params {:control 0, :target 1}}
  ;    {:operation-type :cnot, :operation-params {:control 0, :target 2}}]
  
  ;; Simulate an error (bit flip on qubit 1)
  (def error-circuit
    (-> encoded-circuit
        (circuit/x-gate 1)))  ; Introduce bit-flip error
  
  ;; Now we would measure the syndrome (needs ancilla qubits)
  (def circuit-with-ancilla (circuit/create-circuit 5))  ; 3 data + 2 ancilla
  (def prepared-with-ancilla
    (-> circuit-with-ancilla
        (circuit/h-gate 0)
        (encode-bit-flip 0)))
  
  ;; Introduce error on qubit 1
  (def error-with-ancilla
    (circuit/x-gate prepared-with-ancilla 1))
  
  ;; Measure syndrome
  (def with-syndrome
    (measure-bit-flip-syndrome error-with-ancilla [0 1 2] [3 4]))
  
  ;; Show syndrome measurement operations
  (:operations with-syndrome)
  
  ;; Apply correction based on syndrome [1 1] (error on qubit 1)
  (def corrected-circuit
    (correct-bit-flip-error error-with-ancilla [0 1 2] [1 1]))
  
  ;; Show the correction operation
  (last (:operations corrected-circuit))
  ;=> {:operation-type :x, :operation-params {:target 1}}
  
  ;; Decode the corrected circuit
  (def decoded-circuit
    (decode-bit-flip (circuit/create-circuit 3) [0 1 2]))
  
  (:operations decoded-circuit)
  ;=> CNOT operations in reverse order
  
  ;;
  ;; Pipeline Integration Test
  ;;
  
  ;; Test error correction in optimization context
  (def ctx
    {:circuit (circuit/create-circuit 3)
     :options {:apply-error-correction? true
               :error-correction-code :bit-flip}})
  
  (def ctx-with-ec
    (apply-error-correction ctx))
  
  (:error-correction-applied? ctx-with-ec)
  ;=> true
  
  (:error-correction-code ctx-with-ec)
  ;=> bit-flip-code definition
  
  (keys (:syndrome-table ctx-with-ec))
  ;=> All possible syndromes for bit-flip code
  
  ;; Test with disabled error correction
  (def ctx-no-ec
    {:circuit (circuit/create-circuit 3)
     :options {:apply-error-correction? false}})
  
  (def result-no-ec
    (apply-error-correction ctx-no-ec))
  
  (:error-correction-applied? result-no-ec))
  ;=> nil or false
  
  ;

;;;
;;; Phase 2: Post-Processing and Classical Correction
;;;

(defn extract-syndrome-bits
  "Extract syndrome measurement bits from quantum circuit results.
   Maps ancilla measurements to original logical qubits using reverse mappings.
   
   The syndrome bits are stored in the measurement results of the ancilla qubits.
   This function identifies which measurement results correspond to syndrome
   measurements and maps them back to the original circuit's logical qubits.
   
   Parameters:
   - results: Map containing measurement results from circuit execution
     {:measurements {qubit-idx value, ...}, :measurement-counts {...}, ...}
   - ctx: Optimization context with error correction metadata and reverse mappings
     {:logical-to-ancillas {compacted-logical [ancilla...], ...}
      :inverse-qubit-mapping {compacted-logical original-logical, ...}}
   
   Returns:
   Map from ORIGINAL logical qubit index to syndrome bits
   {original-logical-0 [1 0], original-logical-2 [0 1], ...}
   
   Example:
   Original circuit has qubits 0, 2 (1 unused). After optimization:
   - Compacted 0 → Original 0, ancillas [6 7]
   - Compacted 1 → Original 2, ancillas [8 9]
   - Measurements {6 1, 7 0, 8 0, 9 1}
   - Result: {0 [1 0], 2 [0 1]}"
  [results ctx]
  {:pre [(map? results)
         (map? ctx)]}
  (let [logical-to-ancillas (:logical-to-ancillas ctx)
        inverse-qubit-mapping (:inverse-qubit-mapping ctx)
        measurements (:measurements results)]
    (when (and measurements logical-to-ancillas)
      (into {}
            (map (fn [[compacted-logical ancilla-qubits]]
                   ;; Map compacted logical qubit to original logical qubit
                   (let [original-logical (get inverse-qubit-mapping compacted-logical compacted-logical)
                         syndrome-bits (mapv #(get measurements % 0) ancilla-qubits)]
                     [original-logical syndrome-bits]))
                 logical-to-ancillas)))))

(defn decode-syndrome-bits
  "Decode syndrome bits to identify detected errors.
   
   Interprets syndrome measurements to determine which physical qubits
   have errors. Uses the syndrome table from the error correction code.
   
   Parameters:
   - syndrome-bits: Vector of syndrome measurement outcomes
   - code-key: Error correction code keyword (:bit-flip or :shor)
   - syndrome-table: Lookup table mapping syndromes to error patterns
   
   Returns:
   Map with error information:
   {:syndrome syndrome-bits
    :error-detected? boolean
    :error-location string (Pauli operator indicating error)
    :error-type :x or :z or :both
    :correction-needed? boolean}
   
   Example:
   Bit-flip syndrome [1 0] → {:error-location \"XII\", :error-type :x}"
  [syndrome-bits code-key syndrome-table]
  {:pre [(vector? syndrome-bits)
         (keyword? code-key)
         (or (nil? syndrome-table) (map? syndrome-table))]}
  (let [error-pattern (get syndrome-table syndrome-bits)
        no-error? (every? zero? syndrome-bits)]
    {:syndrome syndrome-bits
     :error-detected? (not no-error?)
     :error-location (or error-pattern "I...") ; Identity if no error
     :error-type (cond
                   no-error? :none
                   :else (case code-key
                           :bit-flip :x
                           :shor :both
                           :unknown))
     :correction-needed? (not no-error?)}))

(defn apply-classical-correction
  "Apply classical post-processing correction to measurement results.
   Works with original logical qubit indices (already mapped by extract-syndrome-bits).
   
   This function performs software correction of measurement outcomes based
   on syndrome information. It does NOT modify the quantum circuit, but
   corrects the classical measurement data after execution.
   
   Use cases:
   - Correcting final measurement results from simulators
   - Post-processing experimental data
   - Analyzing error patterns without dynamic circuits
   
   Parameters:
   - measurements: Map of qubit indices to measurement outcomes {0 1, 1 0, 2 1, ...}
   - syndrome-info: Map from ORIGINAL logical qubits to syndrome decode results
   - ctx: Optimization context with mappings
     {:logical-to-physical {compacted-logical [physical...], ...}
      :inverse-qubit-mapping {compacted-logical original-logical, ...}
      :qubit-mapping {original-logical compacted-logical, ...}}
   - code-key: Error correction code keyword
   
   Returns:
   Map with corrected measurements:
   {:original-measurements {...}
    :corrected-measurements {...}
    :corrections-applied [{:logical-qubit original-q, :physical-qubit p, :correction :x}, ...]
    :syndrome-info {...}}
   
   Example:
   Original circuit qubit 2, compacted to 1, physical qubits [3 4 5]
   - Syndrome for original qubit 2: [1 0] → error on first physical qubit (3)
   - Correction flips qubit 3"
  [measurements syndrome-info ctx code-key]
  {:pre [(map? measurements)
         (map? syndrome-info)
         (map? ctx)
         (keyword? code-key)]}
  (let [corrections-applied (atom [])
        corrected-measurements (atom measurements)
        logical-to-physical (:logical-to-physical ctx)
        qubit-mapping (:qubit-mapping ctx)]
    (doseq [[original-logical syndrome-decode] syndrome-info]
      (when (:correction-needed? syndrome-decode)
        ;; Map original logical to compacted logical to get physical qubits
        (let [compacted-logical (get qubit-mapping original-logical original-logical)
              physical-qubits (get logical-to-physical compacted-logical)
              syndrome (:syndrome syndrome-decode)]
          (case code-key
            :bit-flip
            (let [error-qubit-offset (cond
                                       (= [0 0] syndrome) nil
                                       (= [1 0] syndrome) 0
                                       (= [1 1] syndrome) 1
                                       (= [0 1] syndrome) 2)
                  error-qubit (when error-qubit-offset
                                (nth physical-qubits error-qubit-offset))]
              (when error-qubit
                ;; Flip the bit
                (swap! corrected-measurements update error-qubit #(if (= % 1) 0 1))
                (swap! corrections-applied conj
                       {:logical-qubit original-logical
                        :physical-qubit error-qubit
                        :correction :x})))
            
            :shor
            ;; Shor code has more complex correction logic
            ;; For now, just record that correction is needed
            (swap! corrections-applied conj
                   {:logical-qubit original-logical
                    :syndrome syndrome
                    :correction :complex-shor-correction
                    :note "Shor correction requires full syndrome analysis"})
            
            nil))))
    
    {:original-measurements measurements
     :corrected-measurements @corrected-measurements
     :corrections-applied @corrections-applied
     :syndrome-info syndrome-info}))

(defn analyze-error-correction-results
  "Comprehensive analysis of error correction results from circuit execution.
   Uses reverse mappings to present all results in terms of ORIGINAL circuit qubits.
   
   This is the main entry point for Phase 2 post-processing. It combines
   syndrome extraction, decoding, and classical correction to provide a
   complete analysis of error-corrected circuit results.
   
   Parameters:
   - results: Raw results from circuit execution
     {:measurements {qubit-idx value, ...}, ...}
   - ctx: Complete optimization context including reverse mappings
     {:error-correction-code code-definition
      :logical-to-physical {compacted-logical [physical...], ...}
      :logical-to-ancillas {compacted-logical [ancilla...], ...}
      :syndrome-table {...}
      :inverse-qubit-mapping {compacted-logical original-logical, ...}
      :qubit-mapping {original-logical compacted-logical, ...}
      :options {:error-correction-code :bit-flip/:shor, ...}}
   
   Returns:
   Comprehensive analysis map (all logical qubit keys are ORIGINAL indices):
   {:raw-results results
    :syndrome-bits {original-logical [bits...], ...}
    :decoded-syndromes {original-logical {...}, ...}
    :error-detected? boolean
    :corrected-measurements {...}
    :corrections-applied [{:logical-qubit original-q, ...}, ...]
    :error-rate float (fraction of logical qubits with errors)}
   
   Example usage:
   (def optimized-ctx (hw/optimize circuit device {...}))
   (def results (backend/execute-circuit (:circuit optimized-ctx)))
   (def analysis (analyze-error-correction-results results optimized-ctx))
   
   ;; All results in terms of original circuit
   (:syndrome-bits analysis)        ; {0 [1 0], 2 [0 1]} (original qubits 0 and 2)
   (:corrected-measurements analysis)  ; Get corrected results
   (:error-rate analysis)              ; Check error rate"
  [results ctx]
  {:pre [(map? results)
         (map? ctx)
         (:logical-to-ancillas ctx)
         (:inverse-qubit-mapping ctx)]}  ; Require reverse mappings
  (let [code-key (get-in ctx [:options :error-correction-code])
        syndrome-table (:syndrome-table ctx)
        
        ;; Extract syndrome bits from ancilla measurements
        ;; This now returns {original-logical [bits...], ...}
        syndrome-bits (extract-syndrome-bits results ctx)
        
        ;; Decode each syndrome (keys are already original logical qubits)
        decoded-syndromes (when syndrome-bits
                            (into {}
                                  (map (fn [[original-logical bits]]
                                         [original-logical
                                          (decode-syndrome-bits bits code-key syndrome-table)])
                                       syndrome-bits)))
        
        ;; Check if any errors were detected
        error-detected? (when decoded-syndromes
                          (some :error-detected? (vals decoded-syndromes)))
        
        ;; Apply classical correction to measurements
        ;; syndrome-info keys are already original logical qubits
        correction-results (when decoded-syndromes
                             (apply-classical-correction
                              (:measurements results)
                              decoded-syndromes
                              ctx
                              code-key))
        
        ;; Calculate error rate based on original logical qubits
        num-logical-qubits (count (:qubit-mapping ctx))  ; Original qubits that were used
        num-errors (when decoded-syndromes
                     (count (filter :error-detected? (vals decoded-syndromes))))
        error-rate (when (and num-errors (pos? num-logical-qubits))
                     (/ num-errors num-logical-qubits))]
    
    {:raw-results results
     :syndrome-bits syndrome-bits
     :decoded-syndromes decoded-syndromes
     :error-detected? error-detected?
     :corrected-measurements (:corrected-measurements correction-results)
     :corrections-applied (:corrections-applied correction-results)
     :error-rate error-rate
     :num-logical-qubits num-logical-qubits
     :num-errors num-errors}))

;;;
;;; Steane Code Implementation (7-qubit CSS code)
;;;
(defn encode-steane
  "Encode a logical qubit into the 7-qubit Steane code.
   
   The Steane code is a CSS code derived from the classical [7,4] Hamming code.
   It can correct any single-qubit error using only 7 physical qubits.
   
   The encoding circuit implements the generator matrix of the [7,4,3] Hamming code:
   - Starts with logical qubit in position 0
   - Applies CNOT gates according to the parity-check matrix structure
   - Creates an encoded state that spans the stabilizer code space
   
   Encoding circuit structure:
   1. Apply CNOTs from qubit 0 to create X-type stabilizers
   2. Apply Hadamards to transform to Z-type stabilizers (CSS structure)
   3. Final CNOTs complete the encoding
   
   Parameters:
   - circuit: Quantum circuit
   - logical-qubit: Index of the first qubit (will use qubits logical-qubit to logical-qubit+6)
   
   Returns:
   Updated circuit with Steane encoding operations"
  [circuit logical-qubit]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (>= logical-qubit 0)
         (< (+ logical-qubit 6) (:num-qubits circuit))]}
  (let [q0 logical-qubit
        q1 (+ logical-qubit 1)
        q2 (+ logical-qubit 2)
        q3 (+ logical-qubit 3)
        q4 (+ logical-qubit 4)
        q5 (+ logical-qubit 5)
        q6 (+ logical-qubit 6)]
    (-> circuit
        ;; First layer: Create X-type stabilizers
        ;; These CNOTs implement the generator matrix columns
        (circuit/cnot-gate q0 q1)
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q3)
        
        ;; Second layer: Complete X-stabilizer structure
        (circuit/cnot-gate q1 q4)
        (circuit/cnot-gate q2 q4)
        (circuit/cnot-gate q3 q4)
        
        ;; Third layer: Build Y-stabilizer components
        (circuit/cnot-gate q1 q5)
        (circuit/cnot-gate q2 q5)
        (circuit/cnot-gate q0 q5)
        
        ;; Fourth layer: Complete encoding
        (circuit/cnot-gate q1 q6)
        (circuit/cnot-gate q3 q6)
        (circuit/cnot-gate q0 q6))))

(defn measure-steane-syndrome
  "Measure the syndrome for the 7-qubit Steane code.
   
   The Steane code has 6 stabilizer generators (3 X-type, 3 Z-type),
   requiring 6 ancilla qubits for syndrome measurement.
   
   The syndrome measurements check:
   - 3 X-type stabilizers: IIIXXXX, IXXIIXX, XIXIXIX
   - 3 Z-type stabilizers: IIIZZZZ, IZZIIZZ, ZIZIZIZ
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 7 qubit indices for the encoded logical qubit
   - ancilla-qubits: Vector of 6 ancilla qubit indices (3 for X, 3 for Z)
   
   Returns:
   Updated circuit with syndrome measurement operations"
  [circuit encoded-qubits ancilla-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 7 (count encoded-qubits))
         (= 6 (count ancilla-qubits))]}
  (let [[q0 q1 q2 q3 q4 q5 q6] encoded-qubits
        [a0 a1 a2 a3 a4 a5] ancilla-qubits]
    (-> circuit
        ;; Measure X-type stabilizers (using Hadamard conjugation)
        ;; Stabilizer 1: IIIXXXX (qubits 3, 4, 5, 6)
        (circuit/h-gate a0)
        (circuit/cnot-gate a0 q3)
        (circuit/cnot-gate a0 q4)
        (circuit/cnot-gate a0 q5)
        (circuit/cnot-gate a0 q6)
        (circuit/h-gate a0)
        
        ;; Stabilizer 2: IXXIIXX (qubits 1, 2, 5, 6)
        (circuit/h-gate a1)
        (circuit/cnot-gate a1 q1)
        (circuit/cnot-gate a1 q2)
        (circuit/cnot-gate a1 q5)
        (circuit/cnot-gate a1 q6)
        (circuit/h-gate a1)
        
        ;; Stabilizer 3: XIXIXIX (qubits 0, 2, 4, 6)
        (circuit/h-gate a2)
        (circuit/cnot-gate a2 q0)
        (circuit/cnot-gate a2 q2)
        (circuit/cnot-gate a2 q4)
        (circuit/cnot-gate a2 q6)
        (circuit/h-gate a2)
        
        ;; Measure Z-type stabilizers (direct CNOT)
        ;; Stabilizer 4: IIIZZZZ (qubits 3, 4, 5, 6)
        (circuit/cnot-gate q3 a3)
        (circuit/cnot-gate q4 a3)
        (circuit/cnot-gate q5 a3)
        (circuit/cnot-gate q6 a3)
        
        ;; Stabilizer 5: IZZIIZZ (qubits 1, 2, 5, 6)
        (circuit/cnot-gate q1 a4)
        (circuit/cnot-gate q2 a4)
        (circuit/cnot-gate q5 a4)
        (circuit/cnot-gate q6 a4)
        
        ;; Stabilizer 6: ZIZIZIZ (qubits 0, 2, 4, 6)
        (circuit/cnot-gate q0 a5)
        (circuit/cnot-gate q2 a5)
        (circuit/cnot-gate q4 a5)
        (circuit/cnot-gate q6 a5)
        
        ;; Measure all ancilla qubits
        (circuit/measure-operation ancilla-qubits))))

(defn correct-steane-error
  "Apply correction based on Steane code syndrome.
   
   The syndrome is a 6-bit vector [x0 x1 x2 z0 z1 z2] where:
   - [x0 x1 x2] identifies X errors (bit-flips)
   - [z0 z1 z2] identifies Z errors (phase-flips)
   
   The syndrome bits form a binary number indicating which qubit has an error:
   - For X errors: bits [x2 x1 x0] give the qubit index (1-7)
   - For Z errors: bits [z2 z1 z0] give the qubit index (1-7)
   - Syndrome 0 means no error
   
   Parameters:
   - circuit: Quantum circuit
   - encoded-qubits: Vector of 7 qubit indices containing the encoded logical qubit
   - syndrome: Vector of 6 measurement outcomes [x0 x1 x2 z0 z1 z2]
   
   Returns:
   Updated circuit with correction applied"
  [circuit encoded-qubits syndrome]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 7 (count encoded-qubits))
         (= 6 (count syndrome))]}
  (let [[x0 x1 x2 z0 z1 z2] syndrome
        ;; Convert syndrome bits to qubit indices (syndrome table decoding)
        x-syndrome-value (+ (* x2 4) (* x1 2) x0)
        z-syndrome-value (+ (* z2 4) (* z1 2) z0)
        
        ;; Steane code syndrome table (index 0 = no error, 1-7 = qubit positions)
        x-error-qubit (when (pos? x-syndrome-value)
                        (dec x-syndrome-value))
        z-error-qubit (when (pos? z-syndrome-value)
                        (dec z-syndrome-value))]
    (cond-> circuit
      ;; Apply X correction if X error detected
      x-error-qubit
      (circuit/x-gate (nth encoded-qubits x-error-qubit))
      
      ;; Apply Z correction if Z error detected
      z-error-qubit
      (circuit/z-gate (nth encoded-qubits z-error-qubit)))))

(defn decode-steane
  "Decode a 7-qubit Steane code back to a logical qubit.
   
   The decoding circuit is the inverse of the encoding circuit,
   applying the same gates in reverse order.
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 7 qubit indices [q0 q1 q2 q3 q4 q5 q6]
   
   Returns:
   Updated circuit with decoding operations, logical qubit is in q0"
  [circuit encoded-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 7 (count encoded-qubits))]}
  (let [[q0 q1 q2 q3 q4 q5 q6] encoded-qubits]
    (-> circuit
        ;; Reverse of fourth layer
        (circuit/cnot-gate q0 q6)
        (circuit/cnot-gate q3 q6)
        (circuit/cnot-gate q1 q6)
        
        ;; Reverse of third layer
        (circuit/cnot-gate q0 q5)
        (circuit/cnot-gate q2 q5)
        (circuit/cnot-gate q1 q5)
        
        ;; Reverse of second layer
        (circuit/cnot-gate q3 q4)
        (circuit/cnot-gate q2 q4)
        (circuit/cnot-gate q1 q4)
        
        ;; Reverse of first layer
        (circuit/cnot-gate q0 q3)
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q1))))

;;;
;;; Five-Qubit Code Implementation (5-qubit perfect code)
;;;
(defn encode-five-qubit
  "Encode a logical qubit into the 5-qubit perfect code.
   
   The 5-qubit code is the smallest quantum error-correcting code that can
   protect against arbitrary single-qubit errors. It encodes 1 logical qubit
   into 5 physical qubits with distance 3.
   
   The encoding circuit uses a specific sequence of CNOT and Hadamard gates
   that creates the stabilizer code space spanned by the four stabilizer generators:
   - XZZXI
   - IXZZX
   - XIXZZ
   - ZXIXZ
   
   Parameters:
   - circuit: Quantum circuit
   - logical-qubit: Index of the first qubit (will use qubits logical-qubit to logical-qubit+4)
   
   Returns:
   Updated circuit with 5-qubit code encoding operations"
  [circuit logical-qubit]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (>= logical-qubit 0)
         (< (+ logical-qubit 4) (:num-qubits circuit))]}
  (let [q0 logical-qubit
        q1 (+ logical-qubit 1)
        q2 (+ logical-qubit 2)
        q3 (+ logical-qubit 3)
        q4 (+ logical-qubit 4)]
    (-> circuit
        ;; First layer: Create entanglement structure
        (circuit/cnot-gate q0 q1)
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q3)
        (circuit/cnot-gate q0 q4)
        
        ;; Second layer: Apply Hadamards to create superpositions
        (circuit/h-gate q1)
        (circuit/h-gate q2)
        (circuit/h-gate q3)
        (circuit/h-gate q4)
        
        ;; Third layer: Create stabilizer structure with cyclic CNOTs
        (circuit/cnot-gate q1 q2)
        (circuit/cnot-gate q2 q3)
        (circuit/cnot-gate q3 q4)
        (circuit/cnot-gate q4 q1))))

(defn measure-five-qubit-syndrome
  "Measure the syndrome for the 5-qubit perfect code.
   
   The 5-qubit code has 4 stabilizer generators, requiring 4 ancilla qubits
   for syndrome measurement:
   - XZZXI
   - IXZZX
   - XIXZZ
   - ZXIXZ
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 5 qubit indices for the encoded logical qubit
   - ancilla-qubits: Vector of 4 ancilla qubit indices
   
   Returns:
   Updated circuit with syndrome measurement operations"
  [circuit encoded-qubits ancilla-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 5 (count encoded-qubits))
         (= 4 (count ancilla-qubits))]}
  (let [[q0 q1 q2 q3 q4] encoded-qubits
        [a0 a1 a2 a3] ancilla-qubits]
    (-> circuit
        ;; Stabilizer 1: XZZXI (X on q0, Z on q1, Z on q2, X on q3, I on q4)
        (circuit/h-gate a0)
        (circuit/cnot-gate a0 q0)
        (circuit/cnot-gate q1 a0)
        (circuit/cnot-gate q2 a0)
        (circuit/cnot-gate a0 q3)
        (circuit/h-gate a0)
        
        ;; Stabilizer 2: IXZZX (I on q0, X on q1, Z on q2, Z on q3, X on q4)
        (circuit/h-gate a1)
        (circuit/cnot-gate a1 q1)
        (circuit/cnot-gate q2 a1)
        (circuit/cnot-gate q3 a1)
        (circuit/cnot-gate a1 q4)
        (circuit/h-gate a1)
        
        ;; Stabilizer 3: XIXZZ (X on q0, I on q1, X on q2, Z on q3, Z on q4)
        (circuit/h-gate a2)
        (circuit/cnot-gate a2 q0)
        (circuit/cnot-gate a2 q2)
        (circuit/cnot-gate q3 a2)
        (circuit/cnot-gate q4 a2)
        (circuit/h-gate a2)
        
        ;; Stabilizer 4: ZXIXZ (Z on q0, X on q1, I on q2, X on q3, Z on q4)
        (circuit/h-gate a3)
        (circuit/cnot-gate q0 a3)
        (circuit/cnot-gate a3 q1)
        (circuit/cnot-gate a3 q3)
        (circuit/cnot-gate q4 a3)
        (circuit/h-gate a3)
        
        ;; Measure all ancilla qubits
        (circuit/measure-operation ancilla-qubits))))

(defn correct-five-qubit-error
  "Apply correction based on 5-qubit code syndrome.
   
   The syndrome is a 4-bit vector that identifies which qubit (if any) has an error
   and what type of error occurred (X, Y, or Z).
   
   The 5-qubit code has a syndrome lookup table mapping 4-bit syndromes to
   specific error patterns. This function uses that table to determine and
   apply the appropriate correction.
   
   Parameters:
   - circuit: Quantum circuit
   - encoded-qubits: Vector of 5 qubit indices containing the encoded logical qubit
   - syndrome: Vector of 4 measurement outcomes
   - syndrome-table: Optional lookup table mapping syndromes to corrections
   
   Returns:
   Updated circuit with correction applied"
  [circuit encoded-qubits syndrome & {:keys [syndrome-table]}]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 5 (count encoded-qubits))
         (= 4 (count syndrome))]}
  (if (every? zero? syndrome)
    ;; No error detected
    circuit
    ;; Use syndrome table to determine correction
    (if-let [error-info (get syndrome-table syndrome)]
      (let [{:keys [qubit-index error-type]} error-info
            target-qubit (nth encoded-qubits qubit-index)]
        (case error-type
          :x (circuit/x-gate circuit target-qubit)
          :y (circuit/y-gate circuit target-qubit)
          :z (circuit/z-gate circuit target-qubit)
          circuit))
      ;; Unknown syndrome - might be a multi-qubit error
      (throw (ex-info "Unknown syndrome for 5-qubit code"
                      {:syndrome syndrome
                       :encoded-qubits encoded-qubits})))))

(defn decode-five-qubit
  "Decode a 5-qubit perfect code back to a logical qubit.
   
   The decoding circuit is the inverse of the encoding circuit,
   applying the same gates in reverse order.
   
   Parameters:
   - circuit: Quantum circuit with encoded qubits
   - encoded-qubits: Vector of 5 qubit indices [q0 q1 q2 q3 q4]
   
   Returns:
   Updated circuit with decoding operations, logical qubit is in q0"
  [circuit encoded-qubits]
  {:pre [(s/valid? ::circuit/circuit circuit)
         (= 5 (count encoded-qubits))]}
  (let [[q0 q1 q2 q3 q4] encoded-qubits]
    (-> circuit
        ;; Reverse of third layer (cyclic CNOTs in reverse)
        (circuit/cnot-gate q4 q1)
        (circuit/cnot-gate q3 q4)
        (circuit/cnot-gate q2 q3)
        (circuit/cnot-gate q1 q2)
        
        ;; Reverse of second layer (Hadamards are self-inverse)
        (circuit/h-gate q4)
        (circuit/h-gate q3)
        (circuit/h-gate q2)
        (circuit/h-gate q1)
        
        ;; Reverse of first layer
        (circuit/cnot-gate q0 q4)
        (circuit/cnot-gate q0 q3)
        (circuit/cnot-gate q0 q2)
        (circuit/cnot-gate q0 q1))))

(comment
  ;;
  ;; Steane Code Tests (7-qubit CSS code)
  ;;
  
  ;; Create circuit and encode
  (def steane-circuit (circuit/create-circuit 7))
  (def steane-encoded (encode-steane steane-circuit 0))
  
  (count (:operations steane-encoded))
  ;=> 12 operations (CNOT gates implementing the generator matrix)
  
  ;; Test with ancilla for syndrome measurement
  (def steane-with-ancilla (circuit/create-circuit 13))  ; 7 data + 6 ancilla
  (def steane-prepared
    (-> steane-with-ancilla
        (circuit/h-gate 0)
        (encode-steane 0)))
  
  (def steane-with-syndrome
    (measure-steane-syndrome steane-prepared
                             [0 1 2 3 4 5 6]
                             [7 8 9 10 11 12]))
  
  (count (:operations steane-with-syndrome))
  ;=> 44 operations (13 encoding + 31 syndrome measurement)
  
  ;; Test error correction
  ;; X error on qubit 3: syndrome [1 0 0 1 0 0] → binary 100 = qubit 4 (index 3+1)
  (def steane-corrected
    (correct-steane-error steane-prepared
                          [0 1 2 3 4 5 6]
                          [1 0 0 1 0 0]))
  
  (def correction (last (:operations steane-corrected)))
  (:operation-type correction)
  ;=> :x or :z (depending on syndrome)
  
  ;; Test decoding
  (def steane-decoded (decode-steane steane-prepared [0 1 2 3 4 5 6]))
  (count (:operations steane-decoded))
  ;=> 25 operations (13 encoding + 12 decoding)
  
  ;;
  ;; Five-Qubit Code Tests (5-qubit perfect code)
  ;;
  
  ;; Create circuit and encode
  (def five-qubit-circuit (circuit/create-circuit 5))
  (def five-qubit-encoded (encode-five-qubit five-qubit-circuit 0))
  
  (count (:operations five-qubit-encoded))
  ;=> 12 operations (4 CNOTs + 4 Hs + 4 CNOTs)
  
  (mapv :operation-type (:operations five-qubit-encoded))
  ;=> [:cnot :cnot :cnot :cnot :h :h :h :h :cnot :cnot :cnot :cnot]
  
  ;; Test with ancilla for syndrome measurement
  (def five-qubit-with-ancilla (circuit/create-circuit 9))  ; 5 data + 4 ancilla
  (def five-qubit-prepared
    (-> five-qubit-with-ancilla
        (circuit/h-gate 0)
        (encode-five-qubit 0)))
  
  (def five-qubit-with-syndrome
    (measure-five-qubit-syndrome five-qubit-prepared
                                 [0 1 2 3 4]
                                 [5 6 7 8]))
  
  (count (:operations five-qubit-with-syndrome))
  ;=> 38 operations (13 encoding + 25 syndrome measurement)
  
  ;; Test decoding
  (def five-qubit-decoded (decode-five-qubit five-qubit-prepared [0 1 2 3 4]))
  (count (:operations five-qubit-decoded))
  ;=> 25 operations (13 encoding + 12 decoding)
  
  ;;
  ;; Integration with Optimization Pipeline
  ;;
  
  (require '[org.soulspace.qclojure.application.hardware-optimization :as hw])
  
  (def test-circuit
    (-> (circuit/create-circuit 2)
        (circuit/h-gate 0)
        (circuit/cnot-gate 0 1)))
  
  ;; Test Steane code in pipeline
  (def steane-optimized
    (hw/optimize
     {:circuit test-circuit
      :options {:apply-error-correction? true
                :error-correction-code :steane}}))
  
  {:code-name (get-in steane-optimized [:error-correction-code :name])
   :physical-qubits (get-in steane-optimized [:circuit :num-qubits])
   :logical-to-physical (:logical-to-physical steane-optimized)}
  ;=> {:code-name "Steane code", :physical-qubits 26, 
  ;    :logical-to-physical {0 [0 1 2 3 4 5 6], 1 [7 8 9 10 11 12 13]}}
  
  ;; Test Five-qubit code in pipeline
  (def five-qubit-optimized
    (hw/optimize
     {:circuit test-circuit
      :options {:apply-error-correction? true
                :error-correction-code :five-qubit}}))
  
  {:code-name (get-in five-qubit-optimized [:error-correction-code :name])
   :physical-qubits (get-in five-qubit-optimized [:circuit :num-qubits])
   :logical-to-physical (:logical-to-physical five-qubit-optimized)}
  ;=> {:code-name "Five-qubit code", :physical-qubits 18,
  ;    :logical-to-physical {0 [0 1 2 3 4], 1 [5 6 7 8 9]}}
  
  ;;
  ;; Comparison of All Error Correction Codes
  ;;
  
  (defn compare-ec-codes []
    (let [codes [:bit-flip :steane :five-qubit :shor]
          results (map (fn [code-key]
                        (let [ctx {:circuit test-circuit
                                   :options {:apply-error-correction? true}
                                            :error-correction-code code-key}
                              result (apply-error-correction ctx)]
                          {:code code-key
                           :name (get-in result [:error-correction-code :name])
                           :distance (get-in result [:error-correction-code :distance])
                           :physical-per-logical (get-in result [:error-correction-code :num-physical-qubits])
                           :ancillas (if-let [a (get (:logical-to-ancillas result) 0)]
                                      (count a)
                                      0)
                           :total-qubits (get-in result [:circuit :num-qubits])}))
                      codes)]
      (doseq [r results]
        (println (format "%-20s | Dist: %d | Phys: %d | Ancillas: %d | Total: %2d"
                        (:name r) (:distance r) (:physical-per-logical r)
                        (:ancillas r) (:total-qubits r))))
      results))
  
  (compare-ec-codes)
  ;=> Bit-flip code        | Dist: 3 | Phys: 3 | Ancillas: 2 | Total: 10
  ;   Steane code          | Dist: 3 | Phys: 7 | Ancillas: 6 | Total: 26
  ;   Five-qubit code      | Dist: 3 | Phys: 5 | Ancillas: 4 | Total: 18
  ;   Shor code            | Dist: 3 | Phys: 9 | Ancillas: 8 | Total: 34
  
  ;;
  ;; Efficiency Analysis
  ;;
  
  (defn ec-efficiency []
    {:most-efficient-per-qubit :five-qubit  ; Only 5 physical qubits per logical
     :most-efficient-7-qubit :steane         ; Best 7-qubit code
     :most-robust :shor                      ; Can handle any error type
     :simplest :bit-flip                     ; Easiest to implement
     
     :recommendation-by-use-case
     {:minimal-overhead :five-qubit
      :balanced-efficiency :steane
      :maximum-protection :shor
      :simple-bit-errors :bit-flip}})
  
  (ec-efficiency))
  
  ;;
