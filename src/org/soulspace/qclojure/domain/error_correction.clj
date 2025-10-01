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
            [org.soulspace.qclojure.domain.circuit :as circuit]))

;;;
;;; Pauli Operators and Strings
;;;

(def pauli-operators
  "The four single-qubit Pauli operators: Identity, X (bit-flip), Y (both), Z (phase-flip)"
  #{:I :X :Y :Z})

(s/def ::pauli-operator pauli-operators)

(s/def ::pauli-string
  (s/and string?
         #(every? (fn [c] (contains? #{\I \X \Y \Z} c)) %)))

(defn pauli-string?
  "Check if a string is a valid Pauli string."
  [s]
  (and (string? s)
       (every? (fn [c] (contains? #{\I \X \Y \Z} c)) s)))

(defn pauli-weight
  "Calculate the weight of a Pauli string (number of non-identity operators).
   
   Parameters:
   - pauli-str: Pauli string like 'XYZII'
   
   Returns:
   Integer count of non-I operators"
  [pauli-str]
  {:pre [(pauli-string? pauli-str)]}
  (count (filter #(not= \I %) pauli-str)))

(defn pauli-commute?
  "Check if two Pauli operators commute.
   
   Pauli operators commute if they differ on an even number of positions
   (where both are non-identity and different).
   
   Parameters:
   - p1: First Pauli string
   - p2: Second Pauli string
   
   Returns:
   Boolean indicating if operators commute"
  [p1 p2]
  {:pre [(pauli-string? p1)
         (pauli-string? p2)
         (= (count p1) (count p2))]}
  (let [diff-count (count (filter (fn [[c1 c2]]
                                    (and (not= c1 \I)
                                         (not= c2 \I)
                                         (not= c1 c2)))
                                  (map vector p1 p2)))]
    (even? diff-count)))

;;;
;;; Stabilizer Code Definition
;;;

(s/def ::num-physical-qubits pos-int?)
(s/def ::num-logical-qubits pos-int?)
(s/def ::stabilizer-generators (s/coll-of ::pauli-string :kind vector?))
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
                                       (if (pauli-commute? error gen) 0 1))
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
;;; Error Correction Application
;;;

(defn apply-error-correction
  "Apply error correction encoding to a circuit context.
   
   This function integrates error correction into the optimization pipeline,
   encoding logical qubits using the specified QEC code.
   
   Parameters:
   - ctx: Optimization context containing:
       :circuit - The quantum circuit
       :options - Map with:
         :error-correction-code - Keyword for the code to use (e.g., :bit-flip)
         :apply-error-correction? - Whether to apply error correction (default: false)
   
   Returns:
   Updated context with error correction applied (if enabled)"
  [ctx]
  (if (get-in ctx [:options :apply-error-correction?] false)
    (let [code-key (get-in ctx [:options :error-correction-code] :bit-flip)
          code (get-code code-key)]
      (if code
        (assoc ctx
               :error-correction-code code
               :syndrome-table (build-syndrome-table code)
               :error-correction-applied? true)
        (throw (ex-info "Unknown error correction code"
                        {:code-key code-key
                         :available-codes (keys available-codes)}))))
    ctx))

(comment
  ;; Test Pauli string operations
  (pauli-string? "XYZII")
  ;=> true
  
  (pauli-weight "XYZII")
  ;=> 3
  
  (pauli-commute? "XII" "IXI")
  ;=> true
  
  (pauli-commute? "XII" "ZII")
  ;=> false

  ;; Test stabilizer codes
  (list-available-codes)
  
  (get-code :bit-flip)
  
  (get-code :shor)

  ;; Test syndrome table generation
  (build-syndrome-table bit-flip-code)
  
  (build-syndrome-table shor-code)

  ;; Test error correction application
  (apply-error-correction
   {:circuit (circuit/create-circuit 3)
    :options {:apply-error-correction? true
              :error-correction-code :bit-flip}}))
  
  ;

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
  (pauli-string? "XYZII")
  ;=> true
  
  (pauli-weight "XYZII")
  ;=> 3
  
  (pauli-commute? "XII" "IXI")
  ;=> true
  
  (pauli-commute? "XII" "ZII")
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
