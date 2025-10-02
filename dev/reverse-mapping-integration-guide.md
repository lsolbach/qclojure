# Reverse Mapping Integration Guide

## Overview

The quantum circuit optimization pipeline transforms circuits through multiple stages, each creating mappings that change qubit indices. To present results to users in terms of the **original circuit**, we need reverse mappings to trace qubits backwards from the final optimized circuit.

## Mapping Chain

### Forward Mappings (Circuit Optimization)

1. **Qubit Optimization** (qo/optimize-qubit-usage)
   - Removes unused qubits and compacts the circuit
   - Creates: `:qubit-mapping` {original-logical → compacted-logical}
   - Example: {0→0, 2→1} (removed unused qubit 1)

2. **Error Correction** (ec/apply-error-correction)
   - Encodes each logical qubit into multiple physical qubits + ancillas
   - Creates: `:logical-to-physical` {compacted-logical → [physical...]}
   - Creates: `:logical-to-ancillas` {compacted-logical → [ancilla...]}
   - Example: {0→[0,1,2], 1→[3,4,5]} and {0→[6,7], 1→[8,9]}

3. **Topology Optimization** (topo/topology-aware-transform)
   - Maps logical qubits to hardware qubits based on coupling
   - May update or create: `:logical-to-physical` (hardware placement)
   - May create: `:physical-to-logical` (reverse mapping)

### Reverse Mappings (Result Extraction)

Created by `mapping/create-all-reverse-mappings` after optimization:

1. **`:inverse-qubit-mapping`** {compacted-logical → original-logical}
   - Inverts qubit optimization mapping
   - Example: {0→0, 1→2}

2. **`:physical-to-logical`** {physical-qubit → compacted-logical}
   - Inverts error correction's logical-to-physical mapping
   - Example: {0→0, 1→0, 2→0, 3→1, 4→1, 5→1}

3. **`:ancilla-to-logical`** {ancilla-qubit → compacted-logical}
   - Inverts error correction's logical-to-ancillas mapping
   - Example: {6→0, 7→0, 8→1, 9→1}

## Complete Mapping Example

```clojure
;; Original circuit: 3 qubits (uses 0, 2)
;; Qubit 1 is unused, so qubit optimization removes it

Original:  [0]  [1-unused]  [2]
              ↓                ↓
Compacted: [0]              [1]       ← qubit-mapping: {0→0, 2→1}
              ↓                ↓
Physical:  [0,1,2]       [3,4,5]      ← logical-to-physical: {0→[0,1,2], 1→[3,4,5]}
           [6,7]         [8,9]        ← logical-to-ancillas: {0→[6,7], 1→[8,9]}

;; Reverse mappings for result extraction:
inverse-qubit-mapping: {0→0, 1→2}
physical-to-logical:   {0→0, 1→0, 2→0, 3→1, 4→1, 5→1}
ancilla-to-logical:    {6→0, 7→0, 8→1, 9→1}

;; Example: Physical qubit 4 maps to original qubit 2:
;;   physical 4 → (physical-to-logical) → compacted 1 
;;              → (inverse-qubit-mapping) → original 2
```

## Integration Points

### 1. Error Correction Post-Processing

**File**: `src/org/soulspace/qclojure/domain/error_correction.clj`

**Functions to Update**:

#### `extract-syndrome-bits` (lines 1269-1316)
Currently extracts syndrome bits from measurement results using ancilla qubits.

**Required Changes**:
```clojure
(defn extract-syndrome-bits
  "Extract syndrome measurement results from full measurement data.
   Maps ancilla measurements to original logical qubits."
  [measurement-results ctx]
  (let [logical-to-ancillas (:logical-to-ancillas ctx)
        inverse-qubit-mapping (:inverse-qubit-mapping ctx)
        ancilla-to-logical (:ancilla-to-logical ctx)]
    ;; For each logical qubit in the compacted space
    (reduce
      (fn [syndromes compacted-logical]
        (let [original-logical (get inverse-qubit-mapping compacted-logical compacted-logical)
              ancilla-qubits (get logical-to-ancillas compacted-logical [])]
          ;; Extract syndrome bits for this original logical qubit
          (assoc syndromes original-logical
                 (mapv #(get measurement-results % 0) ancilla-qubits))))
      {}
      (keys logical-to-ancillas))))
```

#### `decode-syndrome-bits` (lines 1318-1377)
Decodes syndromes to error types. Already works with logical qubit indices.

**Required Changes**: None (already uses logical qubit keys from syndrome map)

#### `apply-classical-correction` (lines 1379-1453)
Applies corrections based on decoded errors. Works with logical qubits.

**Required Changes**: None (uses logical qubit keys from error map)

#### `analyze-error-correction-results` (lines 1455-1502)
Main entry point for post-processing. Coordinates extraction, decoding, and correction.

**Required Changes**:
```clojure
(defn analyze-error-correction-results
  "Complete error correction post-processing pipeline with reverse mapping."
  [measurement-results ctx]
  {:pre [(map? measurement-results)
         (map? ctx)
         (:logical-to-ancillas ctx)
         (:inverse-qubit-mapping ctx)]} ;; Require reverse mappings
  
  ;; Step 1: Extract syndromes (maps to original logical qubits)
  (let [syndromes (extract-syndrome-bits measurement-results ctx)
        
        ;; Step 2: Decode syndromes to errors
        code-key (get-in ctx [:options :error-correction-code] :bit-flip)
        errors (decode-syndrome-bits syndromes code-key ctx)
        
        ;; Step 3: Apply classical corrections
        correction-stats (apply-classical-correction measurement-results errors ctx)]
    
    {:syndromes syndromes           ;; Keyed by original logical qubits
     :errors errors                 ;; Keyed by original logical qubits
     :correction-stats correction-stats}))
```

### 2. Backend Result Extraction

**File**: `src/org/soulspace/qclojure/domain/result.clj`

**Functions to Update**:

#### `extract-measurement-results`
Extracts measurement results from backend job results.

**Required Changes**:
```clojure
(defn extract-measurement-results
  "Extract measurement results and map to original circuit qubits."
  [job-result ctx]
  (let [raw-measurements (:measurements job-result {})
        mapped-measurements (if (:inverse-qubit-mapping ctx)
                             (mapping/map-measurements-to-original raw-measurements ctx)
                             raw-measurements)]
    {:raw-measurements raw-measurements        ;; Final circuit qubits
     :mapped-measurements mapped-measurements  ;; Original circuit qubits
     :qubit-mappings {:inverse-qubit-mapping (:inverse-qubit-mapping ctx)
                     :physical-to-logical (:physical-to-logical ctx)
                     :ancilla-to-logical (:ancilla-to-logical ctx)}}))
```

#### Result Presentation Functions
Any function presenting results to users should use mapped measurements.

**Required Changes**:
```clojure
(defn format-results-for-user
  "Format results in terms of original circuit qubits."
  [result-data]
  (let [measurements (or (:mapped-measurements result-data)
                        (:raw-measurements result-data))]
    ;; Present measurements using original qubit indices
    {:measurements measurements
     :success (:success result-data)
     :metadata (:metadata result-data)}))
```

### 3. Visualization

**File**: `src/org/soulspace/qclojure/adapter/visualization/*.clj`

When visualizing state or measurements:
- Use mapped measurements for qubit labels
- Show original qubit indices in circuit diagrams
- Optionally show mapping chain in debug/advanced views

**Required Changes**:
```clojure
(defn visualize-measurements
  "Visualize measurement results with original qubit labels."
  [measurements ctx]
  (let [original-measurements (if (:inverse-qubit-mapping ctx)
                                (mapping/map-measurements-to-original measurements ctx)
                                measurements)]
    ;; Visualize with original qubit indices
    (render-measurement-histogram original-measurements)))
```

## Usage Patterns

### Pattern 1: Complete Pipeline with Result Extraction

```clojure
;; Optimize circuit
(def optimized-ctx
  (hw/optimize circuit device
               {:optimize-qubits? true
                :apply-error-correction? true
                :error-correction-code :bit-flip
                :include-syndrome-measurement? true}))

;; Execute on backend (produces measurements for final circuit)
(def job-result (backend/execute (:circuit optimized-ctx) device))

;; Extract and map results
(def raw-measurements (:measurements job-result))

;; Option A: Map entire result set
(def original-measurements
  (mapping/map-measurements-to-original raw-measurements optimized-ctx))

;; Option B: Map individual qubits
(def original-qubit
  (mapping/map-qubit-to-original final-qubit optimized-ctx))

;; Present to user in terms of original circuit
{:measurements original-measurements
 :original-qubits (keys original-measurements)}
```

### Pattern 2: Error Correction Post-Processing

```clojure
;; After execution, analyze error correction
(def ec-analysis
  (ec/analyze-error-correction-results raw-measurements optimized-ctx))

;; Results already mapped to original logical qubits
(:syndromes ec-analysis)     ;; {0 [0 1], 2 [1 0]}  (original qubit keys)
(:errors ec-analysis)        ;; {0 :x-error, 2 :no-error}
(:correction-stats ec-analysis)
```

### Pattern 3: Forward Mapping (Less Common)

```clojure
;; Find which physical qubits correspond to original logical qubit
(def physical-qubits
  (mapping/get-physical-qubits-for-logical 2 optimized-ctx))
;; => [3 4 5]

;; Find ancilla qubits for original logical qubit
(def ancilla-qubits
  (mapping/get-ancilla-qubits-for-logical 0 optimized-ctx))
;; => [6 7]
```

## Testing

### Test 1: Basic Mapping

```clojure
(def circuit (-> (circ/create-circuit 3)
                (circ/add-gate :h :target 0)
                (circ/add-gate :x :target 2)))

(def ctx (hw/optimize circuit device
                     {:optimize-qubits? true
                      :apply-error-correction? true
                      :error-correction-code :bit-flip}))

;; Verify mappings created
(assert (:inverse-qubit-mapping ctx))
(assert (:physical-to-logical ctx))
(assert (:ancilla-to-logical ctx))

;; Test reverse mapping
(def original-q (mapping/map-qubit-to-original 4 ctx))
(assert (= original-q 2))  ;; Physical 4 → compacted 1 → original 2
```

### Test 2: Measurement Mapping

```clojure
;; Simulate measurements on all physical + ancilla qubits
(def measurements {0 1, 1 0, 2 1,  ;; Physical for original 0
                  3 0, 4 1, 5 0,   ;; Physical for original 2
                  6 0, 7 1,         ;; Ancillas for original 0
                  8 1, 9 0})        ;; Ancillas for original 2

;; Map to original qubits
(def original-measurements
  (mapping/map-measurements-to-original measurements ctx))

;; Should aggregate by original logical qubit
(assert (contains? original-measurements 0))
(assert (contains? original-measurements 2))
(assert (not (contains? original-measurements 1)))  ;; Was unused
```

### Test 3: Error Correction Integration

```clojure
;; Post-process with reverse mappings
(def ec-results
  (ec/analyze-error-correction-results measurements ctx))

;; Syndromes keyed by original logical qubits
(assert (contains? (:syndromes ec-results) 0))
(assert (contains? (:syndromes ec-results) 2))

;; Errors keyed by original logical qubits
(assert (contains? (:errors ec-results) 0))
(assert (contains? (:errors ec-results) 2))
```

## Implementation Status

✅ **Completed**:
- `mapping` namespace with all utility functions
- Reverse mapping creation integrated into pipeline
- Forward mapping helpers (`get-physical-qubits-for-logical`, etc.)
- Full reverse mapping chain composition
- Measurement mapping to original qubits

🔄 **In Progress**:
- Error correction post-processing integration
- Backend result extraction integration
- Visualization integration

📋 **Pending**:
- Comprehensive end-to-end tests
- Documentation of best practices
- Performance optimization for large circuits

## API Reference

### Core Functions

#### `mapping/create-all-reverse-mappings`
Creates all reverse mappings from forward mappings in context.

**Input**: Context with `:qubit-mapping`, `:logical-to-physical`, `:logical-to-ancillas`

**Output**: Context with added `:inverse-qubit-mapping`, `:physical-to-logical`, `:ancilla-to-logical`

#### `mapping/map-qubit-to-original`
Maps a final circuit qubit back to the original circuit.

**Parameters**:
- `qubit`: Final circuit qubit index (physical or ancilla)
- `ctx`: Optimization context with reverse mappings

**Returns**: Original circuit qubit index, or nil if not mapped

#### `mapping/map-measurements-to-original`
Maps entire measurement result set to original circuit qubits.

**Parameters**:
- `measurements`: Map of {qubit → measurement-value}
- `ctx`: Optimization context with reverse mappings

**Returns**: Map of {original-qubit → measurement-value}

#### `mapping/get-physical-qubits-for-logical`
Forward mapping: Find physical qubits for an original logical qubit.

**Parameters**:
- `original-logical`: Original circuit qubit index
- `ctx`: Optimization context with forward mappings

**Returns**: Vector of physical qubit indices

#### `mapping/get-ancilla-qubits-for-logical`
Forward mapping: Find ancilla qubits for an original logical qubit.

**Parameters**:
- `original-logical`: Original circuit qubit index
- `ctx`: Optimization context with forward mappings

**Returns**: Vector of ancilla qubit indices

## Best Practices

1. **Always use reverse mappings for user-facing results**
   - Users think in terms of their original circuit
   - Internal optimizations should be transparent

2. **Preserve both raw and mapped measurements**
   - Raw for debugging/verification
   - Mapped for presentation

3. **Document mapping chain in result metadata**
   - Helps with debugging
   - Enables advanced users to understand transformations

4. **Test mapping chain with realistic circuits**
   - Include unused qubits (tests qubit optimization)
   - Include error correction (tests logical-to-physical)
   - Include topology constraints (tests hardware mapping)

5. **Handle missing mappings gracefully**
   - If no optimization, mappings may not exist
   - Fall back to identity mapping (qubit → qubit)

## See Also

- [`/src/org/soulspace/qclojure/domain/mapping.clj`](../src/org/soulspace/qclojure/domain/mapping.clj) - Implementation
- [`/dev/qubit-optimization-error-correction-analysis.md`](./qubit-optimization-error-correction-analysis.md) - Compatibility analysis
- [`/dev/error-correction-testing-results.md`](./error-correction-testing-results.md) - Test results
