(ns org.soulspace.qclojure.domain.device
  "Device-specific configurations and utilities for quantum circuits.
   
   This namespace provides functions to manage device configurations,
   validate circuits against device constraints, and handle device-specific
   optimizations."
  (:require [clojure.set :as set] 
            [clojure.spec.alpha :as s]
            [org.soulspace.qclojure.domain.operation-registry :as op-reg]
            [org.soulspace.qclojure.domain.topology :as topo] 
            [org.soulspace.qclojure.domain.noise :as noise]) 
  )

;;;
;;; Device Specs
;;;
(s/def ::id string?)
(s/def ::name string?)
(s/def ::provider string?)
(s/def ::platform string?)
(s/def ::technology keyword?) ;; e.g., :superconducting, :trapped-ion, :photonic

(s/def ::num-qubits pos-int?)
(s/def ::native-gates ::op-reg/operation-set)
(s/def ::virtual-gates ::op-reg/operation-set)
(s/def ::supported-operations ::op-reg/operation-set)

(s/def ::topology topo/supported-topologies)
(s/def ::coupling (s/nilable ::topo/coupling))
(s/def ::noise-model (s/nilable ::noise/noise-model))

(s/def ::device-map
  (s/keys :req-un [::name
                   ::num-qubits
                   ::native-gates]
          :opt-un [::id
                   ::provider
                   ::platform
                   ::technology
                   ::virtual-gates
                   ::supported-operations ;; TODO native gates or supported operations?
                   ::topology
                   ::coupling
                   ::noise-model
                   ::performance]))

;;;
;;; Device Normalization
;;;
(defn supported-operations
  "Return supported operations for a device, deriving from native+virtual when missing."
  [device]
  (or (:supported-operations device)
      (set/union (or (:native-gates device) #{})
                 (or (:virtual-gates device) #{}))))

(defn validate-device
  "Normalize a device map (e.g., read from EDN).
   - Adds :supported-operations = union(native, virtual) if missing
   - Leaves other keys intact"
  [device]
  (let [supported (supported-operations device)
        coupling (or (:coupling device)
                     (when-let [topo (:topology device)]
                       (topo/coupling-for-topology topo (:num-qubits device))))]
    (assoc device
           :supported-operations supported
           :coupling coupling)))

(defn validate-devices
  "Normalize a collection of devices."
  [devices]
  (map validate-device devices))

(defn noise-model
  "Get the noise model of the device.
  
   Parameters:
   - backend: Backend instance
   
   Returns: Noise model map or empty map if none defined"
  [device]
  (get device :noise-model {}))

(defn gate-fidelity
  "Get the average gate fidelity of the device.
  
   Parameters:
   - device: Device map containing performance metrics
   
   Returns: Average gate fidelity (0.0 to 1.0) or nil if not defined"
  [device]
  ; we have single-qubit-gate-fidelity, two-qubit-gate-fidelity or just gate-fidelity
  (or (get-in device [:performance :gate-fidelity])
      (get-in device [:performance :two-qubit-gate-fidelity])
      (get-in device [:performance :single-qubit-gate-fidelity])))

(defn estimated-fidelity
  "Estimate the fidelity of a circuit on the device based on gate fidelities.
  
   Parameters:
   - device: Device map containing performance metrics
   - gate-count: Total number of gates in the circuit
   
   Returns: Estimated fidelity (0.0 to 1.0) or nil if data is insufficient"
  [device gate-count]
  (when-let [gate-fidelity (gate-fidelity device)]
    (when (and device gate-count (> gate-count 0))
      (Math/pow gate-fidelity gate-count))))

;;;
;;; Device Validation
;;;
(defn validate-circuit
  "Validate a quantum circuit against device constraints.
   
   Parameters:
   - device: Device map containing:
       :supported-operations - Set of natively supported operations
       :coupling - Qubit coupling for hardware topology (optional)
   - circuit: Quantum circuit to validate
   
   Returns:
   Validation result map containing:
   - :all-gates-supported? - Boolean indicating if all gates are supported
   - :unsupported-gates - List of unsupported gate types (if any)
   - :topology-valid? - Boolean indicating if circuit respects topology (if coupling provided)
   - :topology-issues - List of topology issues (if any)"
  [device circuit]
  (let [supported-operations (:supported-operations device)
        gate-types (set (map :operation-type (:operations circuit)))
        unsupported-gates (remove #(contains? supported-operations %) gate-types)
        all-gates-supported? (empty? unsupported-gates)]
    {:all-gates-supported? all-gates-supported?
     :unsupported-gates (vec unsupported-gates)}))

(defn compare-gate-support
  "Compare gate support between two backends.
  
  Parameters:
  - device1: First device
  - device2: Second device
  
  Returns: Map with comparison results including:
   - :device1-operations - Set of operations supported by device1
   - :device2-operations - Set of operations supported by device2
   - :shared-operations - Set of operations supported by both devices
   - :device1-only - Set of operations unique to device1
   - :device2-only - Set of operations unique to device2
   - :total-unique-operations - Set of all unique operations across both devices"
  [device1 device2]
  (let [ops1 (:supported-operations device1)
        ops2 (:supported-operations device2)]
    {:device1-operations ops1
     :device2-operations ops2
     :shared-operations (set/intersection ops1 ops2)
     :device1-only (set/difference ops1 ops2)
     :device2-only (set/difference ops2 ops1)
     :total-unique-operations (set/union ops1 ops2)}))
