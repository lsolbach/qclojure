# Visualization with Reverse Mapping - Concept Document

## Executive Summary

This document proposes three approaches for integrating reverse qubit mappings into quantum circuit visualization, enabling users to view optimized circuit results in terms of their original circuit's qubit indices. Each approach is evaluated on implementation complexity, user experience, and real-world value.

## Background

The optimization pipeline transforms circuits through multiple stages:
1. **Qubit Optimization**: Removes unused qubits (e.g., circuit using qubits 0, 2, 4 → compacted to 0, 1, 2)
2. **Error Correction**: Expands each logical qubit to multiple physical qubits + ancillas
3. **Topology Optimization**: Maps to hardware-specific qubit layout

**User Problem**: After optimization, all visualizations show final circuit indices (physical qubits 0-17), but users think in terms of their original circuit (logical qubits 0, 2, 4).

**Example Scenario**:
```
Original: H(qubit-0), CNOT(0→2), X(qubit-4)
Optimized: 15 physical qubits after qubit opt + error correction
Visualization shows: qubits 0-14 with encoded operations
User wants to see: operations labeled by original qubits 0, 2, 4
```

## Use Cases

### 1. Circuit Diagram Visualization
**Current State**: Circuit diagrams show final optimized circuit with:
- Physical qubit indices (0 through N)
- Expanded error correction gates
- Topology-optimized SWAP gates

**User Need**: Understand which operations correspond to original circuit qubits

**Value**: ★★★★★ (Critical)
- Primary use case for debugging
- Essential for understanding optimization transformations
- Directly impacts developer productivity

**Frequency**: Every debugging session, most analyses

---

### 2. Measurement Result Histograms
**Current State**: Histograms show measurement frequencies by final circuit qubit index

**User Need**: See measurement distributions for original logical qubits (aggregated across physical copies)

**Example**:
```
Current:  Physical qubit 4: |1⟩ 67%, |0⟩ 33%
          Physical qubit 5: |1⟩ 65%, |0⟩ 35%
          Physical qubit 6: |1⟩ 68%, |0⟩ 32%

Desired:  Original qubit 2: |1⟩ ~67%, |0⟩ ~33%
          (aggregated from physicals 4, 5, 6)
```

**Value**: ★★★★☆ (High)
- Important for result interpretation
- Reduces cognitive load (no manual aggregation)
- Enables direct comparison with original circuit expectations

**Frequency**: Every result analysis, particularly with error correction

---

### 3. State Vector / Probability Visualization
**Current State**: Shows probability amplitudes for all 2^N basis states (N = final qubits)

**User Need**: View probabilities in terms of original logical qubit states

**Example**:
```
Current:  10 qubits → 1024 basis states
Desired:  2 original qubits → 4 meaningful basis states
          (with error correction ancillas aggregated away)
```

**Value**: ★★★☆☆ (Medium)
- Useful for understanding logical state
- Complex to aggregate with error correction
- May require marginalizing over physical/ancilla qubits

**Frequency**: Occasional deep analysis, educational use

---

### 4. Error Correction Syndrome Visualization
**Current State**: N/A (syndrome data not yet visualized)

**User Need**: Visual representation of:
- Which original qubits have errors
- Syndrome patterns mapped to original indices
- Error correction effectiveness

**Example**:
```
Original qubit 0: ✓ No error detected
Original qubit 2: ⚠ X-error detected and corrected
Original qubit 4: ✓ No error detected
```

**Value**: ★★★★☆ (High)
- Essential for error correction analysis
- Helps validate QEC implementation
- Enables hardware error rate comparison

**Frequency**: When using error correction, debugging QEC

---

### 5. Mapping Chain Visualization (Debug/Advanced)
**Current State**: N/A

**User Need**: Visual representation of qubit transformation chain

**Example**:
```
Original → Compacted → Physical
   0     →     0     →  [0, 1, 2]  + ancillas [9, 10]
   2     →     1     →  [3, 4, 5]  + ancillas [11, 12]
   4     →     2     →  [6, 7, 8]  + ancillas [13, 14]
```

**Value**: ★★☆☆☆ (Low-Medium)
- Primarily educational
- Helps understand optimization pipeline
- Useful for debugging mapping issues
- Not needed for typical usage

**Frequency**: Rare (learning phase, debugging edge cases)

---

### 6. Interactive Circuit Explorer
**Current State**: N/A (future feature)

**User Need**: Click on optimized circuit element → highlight original circuit element

**Example**: Click on physical qubit 5 → highlights "original qubit 2"

**Value**: ★★★☆☆ (Medium, if interactive viz exists)
- Excellent UX for interactive tools
- Requires interactive visualization infrastructure
- Lower priority without existing interactive features

**Frequency**: If available, would be used frequently

## Proposed Approaches

### Approach A: Create New Visualization Helper Functions

**Description**: Create parallel visualization functions that automatically map to original qubits

```clojure
;; New functions alongside existing ones
(ns org.soulspace.qclojure.adapter.visualization.mapped)

(defn visualize-circuit-with-original-labels
  "Renders circuit diagram with original qubit labels"
  [circuit ctx] ...)

(defn visualize-measurements-by-original-qubits
  "Histogram with original qubit indices"
  [measurements ctx] ...)

(defn visualize-syndrome-by-original-qubits
  "Syndrome visualization keyed by original qubits"
  [syndrome-data ctx] ...)
```

**Pros**:
- ✅ Clean separation of concerns
- ✅ Doesn't break existing code
- ✅ Easy to test independently
- ✅ Clear naming indicates mapped behavior

**Cons**:
- ❌ Code duplication with existing viz functions
- ❌ Two parallel APIs to maintain
- ❌ Users must know which function to call
- ❌ May miss updates to original viz functions

**Implementation Effort**: Medium (2-3 days)
- Create new namespace
- Implement 3-5 new visualization functions
- Integrate mapping logic into each
- Write tests for mapped versions

**Maintenance Burden**: High
- Must keep in sync with original viz functions
- Changes propagate to two places

**Real Value**: ★★☆☆☆
- Provides functionality but at high maintenance cost
- Risk of divergence between original and mapped versions

---

### Approach B: Update Existing Visualization with Optional Context

**Description**: Add optional `ctx` parameter to existing visualization functions

```clojure
;; Updated existing functions
(defn visualize-circuit
  "Renders circuit diagram. If ctx with mappings provided, labels with original qubits."
  ([circuit] (visualize-circuit circuit nil))
  ([circuit ctx]
    (let [qubit-labels (if (:inverse-qubit-mapping ctx)
                        (compute-original-labels circuit ctx)
                        (default-labels circuit))]
      (render-circuit circuit qubit-labels))))

(defn visualize-measurements
  "Measurement histogram. With ctx, aggregates by original qubits."
  ([measurements] (visualize-measurements measurements nil))
  ([measurements ctx]
    (let [data (if (:inverse-qubit-mapping ctx)
                (aggregate-by-original measurements ctx)
                measurements)]
      (render-histogram data))))
```

**Pros**:
- ✅ Single API to learn and maintain
- ✅ Backward compatible (ctx is optional)
- ✅ Logical progression: optimize → visualize with same ctx
- ✅ All updates automatically apply to both modes
- ✅ Lower maintenance burden

**Cons**:
- ⚠️ Slightly more complex function signatures
- ⚠️ Need to handle both modes within each function
- ⚠️ Existing code unaffected (no ctx = original behavior)

**Implementation Effort**: Medium (2-3 days)
- Update 3-5 existing visualization functions
- Add mapping logic conditionally
- Update tests to cover both modes
- Update documentation

**Maintenance Burden**: Low-Medium
- Single codebase to maintain
- Changes automatically apply to both modes
- Need to ensure both modes work correctly

**Real Value**: ★★★★☆
- Best balance of functionality and maintainability
- Natural API: pass same ctx through pipeline
- Enables gradual adoption (optional parameter)

---

### Approach C: Create Visualization Mapper Utility

**Description**: Create a pre-processing layer that transforms data before visualization

```clojure
(ns org.soulspace.qclojure.adapter.visualization.mapper)

(defn map-circuit-to-original
  "Transforms circuit representation to use original qubit labels"
  [circuit ctx] 
  ;; Returns modified circuit with relabeled qubits
  ...)

(defn map-measurements-to-original
  "Transforms measurement data to original qubit space"
  [measurements ctx]
  ;; Already exists in mapping namespace!
  (mapping/map-measurements-to-original measurements ctx))

;; Usage:
(let [mapped-circuit (mapper/map-circuit-to-original circuit ctx)
      mapped-measurements (mapper/map-measurements-to-original measurements ctx)]
  (viz/visualize-circuit mapped-circuit)
  (viz/visualize-measurements mapped-measurements))
```

**Pros**:
- ✅ Existing viz functions unchanged
- ✅ Reuses existing mapping utilities
- ✅ Flexible composition
- ✅ Can be used outside visualization context

**Cons**:
- ❌ Requires data structure transformation (potentially expensive)
- ❌ Users must remember two-step process
- ❌ May not be possible for all visualization types
- ❌ Circuit structure modification could break assumptions

**Implementation Effort**: Low-Medium (1-2 days)
- Create mapping utilities for circuit structures
- Document usage patterns
- Some work already done (measurement mapping exists)

**Maintenance Burden**: Low
- Minimal changes to existing code
- Mapping logic isolated in one place

**Real Value**: ★★★☆☆
- Flexible but requires extra step from users
- May not work for all visualization types
- Good for ad-hoc analysis, less good for automated pipelines

## Recommendation Matrix

| Approach | Value | Effort | Maintenance | User Experience | TOTAL |
|----------|-------|--------|-------------|-----------------|-------|
| A: New Parallel Functions | ★★☆☆☆ | Medium | High | ★★★☆☆ | ★★☆☆☆ |
| B: Optional Context Parameter | ★★★★☆ | Medium | Low-Med | ★★★★★ | ★★★★☆ |
| C: Mapper Utility | ★★★☆☆ | Low-Med | Low | ★★★☆☆ | ★★★☆☆ |

## Recommended Approach: **B - Optional Context Parameter**

### Justification

**Best User Experience**:
```clojure
;; Natural flow: same context throughout
(def ctx (hw/optimize circuit device {...}))
(def results (backend/execute (:circuit ctx)))
(viz/visualize-circuit (:circuit ctx) ctx)        ; Original labels
(viz/visualize-measurements results ctx)           ; Original qubits
(viz/visualize-syndromes results ctx)              ; Original qubits
```

**Maintainability**: Single codebase, all improvements benefit both modes

**Flexibility**: 
- Old code works unchanged (ctx = nil)
- New code can choose: pass ctx for mapped view, omit for raw view
- Easy to add ctx parameter incrementally

**Extensibility**: Pattern extends naturally to future visualizations

### Implementation Plan

**Phase 1: Core Visualizations** (Priority)
1. Update `visualize-circuit` - circuit diagrams with original qubit labels
2. Update `visualize-measurements` - histograms aggregated by original qubits
3. Update `visualize-state-probabilities` - marginal probabilities for original qubits

**Phase 2: Error Correction** (High Priority if using EC)
4. Create `visualize-error-correction-syndromes` - syndrome display by original qubits
5. Update any existing syndrome visualizations

**Phase 3: Advanced/Debug** (Lower Priority)
6. Create `visualize-mapping-chain` - show qubit transformations
7. Add debug mode to existing visualizations showing both views

**Phase 4: Interactive** (Future)
8. Add hover/click interactions to show mapping relationships
9. Side-by-side original vs optimized circuit views

### API Examples

```clojure
;; Circuit diagram
(viz/visualize-circuit optimized-circuit)           ; Shows: qubits 0-9
(viz/visualize-circuit optimized-circuit ctx)       ; Shows: "Original qubit 0", "Orig 2"

;; Measurement histogram
(viz/visualize-measurements results)                ; Shows: all physical qubits
(viz/visualize-measurements results ctx)            ; Shows: aggregated by original

;; State probabilities
(viz/visualize-state-probabilities state)           ; Shows: all 2^10 = 1024 states
(viz/visualize-state-probabilities state ctx)       ; Shows: 2^2 = 4 logical states

;; Error correction (new)
(viz/visualize-ec-syndromes syndrome-data ctx)      ; Requires ctx for mapping
```

## Implementation Checklist

### Visualization Functions to Update

- [ ] **Circuit Diagrams** (`visualization/circuit.clj`)
  - [ ] Add optional `ctx` parameter
  - [ ] Compute qubit labels based on inverse mapping
  - [ ] Handle physical + ancilla labeling
  - [ ] Test with and without ctx

- [ ] **Measurement Histograms** (`visualization/measurements.clj`)
  - [ ] Add optional `ctx` parameter
  - [ ] Aggregate measurements by original qubits
  - [ ] Update axis labels
  - [ ] Test aggregation correctness

- [ ] **State/Probability Visualizations** (`visualization/state.clj`)
  - [ ] Add optional `ctx` parameter
  - [ ] Marginalize over physical/ancilla qubits
  - [ ] Show logical state probabilities
  - [ ] Test probability conservation

### New Visualization Functions

- [ ] **Error Correction Syndromes** (`visualization/error_correction.clj`)
  - [ ] Create syndrome display by original qubit
  - [ ] Color-code error types
  - [ ] Show correction actions taken
  - [ ] Integrate with analyze-error-correction-results

- [ ] **Mapping Chain** (`visualization/mappings.clj`)
  - [ ] Visualize original → compacted → physical chain
  - [ ] Show ancilla assignments
  - [ ] Display topology constraints
  - [ ] Optional: interactive exploration

### Documentation Updates

- [ ] Update visualization namespace docs with ctx parameter
- [ ] Add examples showing both modes
- [ ] Document when to use original vs final qubit views
- [ ] Create visualization guide in `/doc/topics/`

### Testing

- [ ] Unit tests for each visualization with ctx
- [ ] Integration tests with full optimization pipeline
- [ ] Visual regression tests (if applicable)
- [ ] Performance tests with large circuits

## Alternative: Hybrid Approach

If Approach B proves too complex for some visualizations:

**Combination Strategy**:
- Use **Approach B** for most visualizations (circuit, measurements)
- Use **Approach C** for complex state visualizations where data transformation is cleaner
- Provide consistent helper functions in both cases

```clojure
;; Pattern 1: Direct context parameter (most cases)
(viz/visualize-circuit circuit ctx)
(viz/visualize-measurements results ctx)

;; Pattern 2: Pre-mapping (complex cases)
(let [logical-state (mapper/marginalize-to-logical state ctx)]
  (viz/visualize-state-vector logical-state))
```

## Risks and Mitigations

### Risk 1: Performance with Large Circuits
**Impact**: Mapping transformations could be expensive for large circuits

**Mitigation**:
- Lazy evaluation where possible
- Cache mapping computations
- Provide "fast mode" that skips mapping for performance-critical paths

### Risk 2: Complexity in State Marginalisation
**Impact**: Marginalizing state vector over physical/ancilla qubits is non-trivial

**Mitigation**:
- Start with measurement-based visualizations (easier)
- State vector marginalization is Phase 2
- Document limitations clearly

### Risk 3: User Confusion About Views
**Impact**: Users might not understand when they're seeing original vs final qubits

**Mitigation**:
- Clear labeling in all visualizations
- Add "View: Original Circuit" / "View: Optimized Circuit" indicators
- Consistent naming: "Original qubit N" vs "Physical qubit N"

### Risk 4: Breaking Changes
**Impact**: Adding parameters might break existing code

**Mitigation**:
- Make ctx parameter optional (backward compatible)
- Provide clear migration guide
- Add deprecation warnings if changing signatures

## Success Metrics

**Must Have**:
- ✓ Circuit diagrams show original qubit labels when ctx provided
- ✓ Measurement results aggregatable by original qubits
- ✓ Backward compatibility maintained (no ctx = current behavior)
- ✓ Error correction syndromes map to original qubits

**Should Have**:
- ✓ State probabilities marginalized to logical qubits
- ✓ Comprehensive documentation with examples
- ✓ Test coverage for both modes

**Nice to Have**:
- ✓ Interactive mapping exploration
- ✓ Side-by-side original/optimized views
- ✓ Performance optimization for large circuits

## Timeline Estimate

**Phase 1 (Core Visualizations)**: 2-3 days
- Update 3 main visualization functions
- Basic testing and documentation

**Phase 2 (Error Correction)**: 1-2 days
- Create EC-specific visualizations
- Integration with existing EC analysis

**Phase 3 (Advanced Features)**: 2-3 days
- Mapping chain visualization
- State marginalization
- Advanced features

**Total Estimate**: 5-8 days for full implementation

**MVP Estimate**: 2-3 days (Phase 1 only)

## Conclusion

**Recommended**: Approach B (Optional Context Parameter)

**Rationale**:
- Best balance of value, maintainability, and user experience
- Natural API progression (same ctx through entire pipeline)
- Backward compatible with existing code
- Extensible to future features

**Next Steps**:
1. Review and approve this concept
2. Start with Phase 1 (core visualizations)
3. Gather user feedback
4. Iterate based on real-world usage
5. Expand to Phases 2-3 based on demand

**Decision Point**: Should we proceed with Approach B, or would you prefer a different approach or hybrid strategy?
