(ns org.soulspace.qclojure.domain.ansatz
  "Quantum ansatz (parameterized circuit) construction for variational algorithms.
   
   This namespace provides various ansatz types used in VQE and other variational
   quantum algorithms. Each ansatz is designed for specific problem types and
   hardware constraints.
   
   Key Responsibilities:
   - Hardware-efficient ansatz for NISQ devices
   - Chemistry-inspired ansatz for molecular systems
   - UCCSD-inspired ansatz for quantum chemistry
   - Symmetry-preserving ansatz for fermionic systems
   - Parameter initialization strategies
      
   Design Principles:
   - Pure functions returning circuit constructors
   - Configurable parameters for flexibility
   - Hardware-aware circuit structures
   - Reusable across different algorithms"
  (:require [fastmath.core :as m]
            [org.soulspace.qclojure.domain.circuit :as qc]))

;;
;; Ansatz Circuit Construction
;;
(defn hardware-efficient-ansatz
  "Create a hardware-efficient ansatz circuit.
  
  This ansatz consists of layers of single-qubit rotations followed by
  entangling gates, designed to be implementable on near-term quantum devices.
  
  Parameters:
  - num-qubits: Number of qubits
  - num-layers: Number of ansatz layers (default: 1)
  - entangling-gate: Type of entangling gate (:cnot, :cz, :crz) (default: :cnot)
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits]
   (hardware-efficient-ansatz num-qubits 1 :cnot))
  ([num-qubits num-layers]
   (hardware-efficient-ansatz num-qubits num-layers :cnot))
  ([num-qubits num-layers entangling-gate]
   {:pre [(pos-int? num-qubits) (pos-int? num-layers)
          (contains? #{:cnot :cz :crz} entangling-gate)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)
            (= (count parameters) (* num-layers num-qubits 3))]}
     (let [circuit (qc/create-circuit num-qubits "Hardware Efficient Ansatz")]
       (loop [c circuit
              layer 0
              param-idx 0]
         (if (>= layer num-layers)
           c
           ;; Add rotation gates for each qubit
           (let [c-with-rotations 
                 (loop [c-rot c
                        qubit 0
                        p-idx param-idx]
                   (if (>= qubit num-qubits)
                     c-rot
                     (let [rx-angle (nth parameters p-idx)
                           ry-angle (nth parameters (inc p-idx))
                           rz-angle (nth parameters (+ p-idx 2))]
                       (recur (-> c-rot
                                  (qc/rx-gate qubit rx-angle)
                                  (qc/ry-gate qubit ry-angle)
                                  (qc/rz-gate qubit rz-angle))
                              (inc qubit)
                              (+ p-idx 3)))))
                 ;; Add entangling gates
                 c-with-entangling
                 (case entangling-gate
                   :cnot (loop [c-ent c-with-rotations
                               qubit 0]
                           (if (>= qubit (dec num-qubits))
                             c-ent
                             (recur (qc/cnot-gate c-ent qubit (inc qubit))
                                    (inc qubit))))
                   :cz (loop [c-ent c-with-rotations
                             qubit 0]
                         (if (>= qubit (dec num-qubits))
                           c-ent
                           (recur (qc/cz-gate c-ent qubit (inc qubit))
                                  (inc qubit))))
                   :crz (loop [c-ent c-with-rotations
                              qubit 0]
                          (if (>= qubit (dec num-qubits))
                            c-ent
                            (recur (qc/crz-gate c-ent qubit (inc qubit) 
                                                (/ m/PI 4)) ; Fixed angle
                                   (inc qubit)))))]
             (recur c-with-entangling
                    (inc layer)
                    (+ param-idx (* num-qubits 3))))))))))

(defn uccsd-inspired-ansatz
  "Create a UCCSD-inspired ansatz circuit.
  
  Unitary Coupled Cluster with Singles and Doubles (UCCSD) is a quantum chemistry
  ansatz that captures important electronic correlations. This is a simplified
  version suitable for VQE implementations.
  
  Parameters:
  - num-qubits: Number of qubits (must be even for electron pairs)
  - num-excitations: Number of excitation operators to include
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits]
   (uccsd-inspired-ansatz num-qubits (/ num-qubits 2)))
  ([num-qubits num-excitations]
   {:pre [(pos-int? num-qubits) (even? num-qubits) (pos-int? num-excitations)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)
            (= (count parameters) num-excitations)]}
     (-> (qc/create-circuit num-qubits "UCCSD Inspired Ansatz")
         ;; Start with Hartree-Fock state (fill lower orbitals)
         ((fn [circuit]
            (loop [c circuit
                   qubit 0]
              (if (>= qubit (/ num-qubits 2))
                c
                (recur (qc/x-gate c qubit) (inc qubit))))))
         ;; Add excitation operators
         ((fn [hf-circuit]
            (loop [c hf-circuit
                   exc 0]
              (if (>= exc num-excitations)
                c
                (let [angle (nth parameters exc)
                      ;; Single excitation: rotate between occupied and virtual orbitals
                      i (mod exc (/ num-qubits 2))  ; occupied orbital
                      a (+ (/ num-qubits 2) (mod exc (/ num-qubits 2)))] ; virtual orbital
                  (recur (-> c
                             (qc/ry-gate i (/ angle 2))
                             (qc/cnot-gate i a)
                             (qc/ry-gate a (/ angle 2))
                             (qc/cnot-gate i a)
                             (qc/ry-gate i (- (/ angle 2))))
                         (inc exc)))))))))))

(defn symmetry-preserving-ansatz
  "Create a symmetry-preserving ansatz circuit.
  
  This ansatz preserves certain symmetries like particle number conservation,
  important for quantum chemistry and condensed matter applications.
  
  Parameters:
  - num-qubits: Number of qubits
  - num-particles: Number of particles to conserve
  - num-layers: Number of ansatz layers
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits num-particles]
   (symmetry-preserving-ansatz num-qubits num-particles 1))
  ([num-qubits num-particles num-layers]
   {:pre [(pos-int? num-qubits) (<= num-particles num-qubits) (pos-int? num-layers)]}
   (fn ansatz-circuit [parameters]
     {:pre [(vector? parameters)]}
     (-> (qc/create-circuit num-qubits "Symmetry Preserving Ansatz")
         ;; Initialize with correct particle number
         ((fn [circuit]
            (loop [c circuit
                   qubit 0
                   particles-placed 0]
              (if (or (>= qubit num-qubits) 
                      (>= particles-placed num-particles))
                c
                (recur (qc/x-gate c qubit)
                       (inc qubit)
                       (inc particles-placed))))))
         ;; Add particle-conserving gates
         ((fn [init-circuit]
            (let [param-per-layer (dec num-qubits)]
              (loop [c init-circuit
                     layer 0
                     param-idx 0]
                (if (>= layer num-layers)
                  c
                  (let [c-with-swaps
                        (loop [c-swap c
                               qubit 0
                               p-idx param-idx]
                          (if (>= qubit (dec num-qubits))
                            c-swap
                            (let [angle (if (< p-idx (count parameters))
                                          (nth parameters p-idx)
                                          0.0)]
                              ;; Particle-conserving swap rotation
                              (recur (qc/cry-gate c-swap qubit (inc qubit) angle)
                                     (inc qubit)
                                     (inc p-idx)))))]
                    (recur c-with-swaps
                           (inc layer)
                           (+ param-idx param-per-layer))))))))))))

(defn chemistry-inspired-ansatz
  "Create a chemistry-inspired ansatz circuit optimized for molecular problems.
  
  This ansatz is specifically designed for quantum chemistry problems like H2.
  It creates proper electron correlation patterns and can represent states like
  c₁|1100⟩ + c₂|0011⟩ which are typical in molecular ground states.
  
  The ansatz structure:
  1. Prepare approximate Hartree-Fock reference state
  2. Add single excitations within electron pairs  
  3. Add double excitations between electron pairs
  
  Parameters:
  - num-qubits: Number of qubits (should be even for electron pairs)
  - num-excitation-layers: Number of excitation layers (default: 1)
  
  Returns:
  Function that takes parameters and returns a quantum circuit"
  ([num-qubits]
   (chemistry-inspired-ansatz num-qubits 1))
  ([num-qubits num-excitation-layers]
   {:pre [(pos-int? num-qubits) (even? num-qubits) (pos-int? num-excitation-layers)]}
   (let [num-electron-pairs (/ num-qubits 2)
         params-per-layer (+ num-qubits                    ; Initial state preparation
                            num-electron-pairs              ; Single excitations  
                            (/ (* num-electron-pairs (dec num-electron-pairs)) 2))] ; Double excitations
     (fn ansatz-circuit [parameters]
       {:pre [(vector? parameters)
              (= (count parameters) (* num-excitation-layers params-per-layer))]}
       (let [circuit (qc/create-circuit num-qubits "Chemistry Inspired Ansatz")]
         (loop [c circuit
                layer 0
                param-idx 0]
           (if (>= layer num-excitation-layers)
             c
             (let [;; Initial state preparation (Hartree-Fock-like)
                   c-with-prep 
                   (loop [c-prep c
                          qubit 0
                          p-idx param-idx]
                     (if (>= qubit num-qubits)
                       c-prep
                       (recur (qc/ry-gate c-prep qubit (nth parameters p-idx))
                              (inc qubit)
                              (inc p-idx))))
                   
                   ;; Single excitations within electron pairs
                   c-with-singles
                   (loop [c-single c-with-prep
                          pair 0
                          p-idx (+ param-idx num-qubits)]
                     (if (>= pair num-electron-pairs)
                       c-single
                       (let [qubit1 (* pair 2)
                             qubit2 (inc qubit1)
                             angle (nth parameters p-idx)]
                         (recur (-> c-single
                                    (qc/cnot-gate qubit1 qubit2)
                                    (qc/ry-gate qubit2 angle)
                                    (qc/cnot-gate qubit1 qubit2))
                                (inc pair)
                                (inc p-idx)))))
                   
                   ;; Double excitations between electron pairs
                   c-with-doubles
                   (loop [c-double c-with-singles
                          pair1 0
                          p-idx (+ param-idx num-qubits num-electron-pairs)]
                     (if (>= pair1 (dec num-electron-pairs))
                       c-double
                       (recur (loop [c-inner c-double
                                    pair2 (inc pair1)
                                    p-inner p-idx]
                                (if (>= pair2 num-electron-pairs)
                                  c-inner
                                  (let [qubit1 (* pair1 2)
                                        qubit2 (inc qubit1)
                                        qubit3 (* pair2 2)
                                        qubit4 (inc qubit3)
                                        angle (nth parameters p-inner)]
                                    (recur (-> c-inner
                                               ;; Create double excitation |1100⟩ ↔ |0011⟩
                                               (qc/cnot-gate qubit1 qubit3)
                                               (qc/cnot-gate qubit2 qubit4)
                                               (qc/ry-gate qubit3 angle)
                                               (qc/cnot-gate qubit2 qubit4)
                                               (qc/cnot-gate qubit1 qubit3))
                                           (inc pair2)
                                           (inc p-inner)))))
                              (inc pair1)
                              (+ p-idx (- num-electron-pairs pair1 1)))))]
               (recur c-with-doubles
                      (inc layer)
                      (+ param-idx params-per-layer))))))))))

(defn hartree-fock-initialization
  "Generate Hartree-Fock initialization parameters for a given ansatz.
  
  For molecular systems, this provides a good starting point by initializing
  parameters to create a state close to the Hartree-Fock reference state.
  
  Parameters:
  - ansatz-type: Type of ansatz (:hardware-efficient, :chemistry-inspired, etc.)
  - num-qubits: Number of qubits
  - num-electrons: Number of electrons (particles) in the system
  - additional-options: Additional options specific to ansatz type
  
  Returns:
  Vector of initialization parameters"
  [ansatz-type num-qubits num-electrons & {:as additional-options}]
  (case ansatz-type
    :hardware-efficient
    (let [num-layers (:num-layers additional-options 1)]
      (vec (concat
            ;; Initialize occupied orbitals with Y rotations to create |1⟩ states
            (flatten (for [_layer (range num-layers)]
                       (flatten (for [qubit (range num-qubits)]
                                  (if (< qubit num-electrons)
                                    [0.1 m/PI 0.1]      ; RX small, RY=π for |1⟩, RZ small
                                    [0.1 0.1 0.1])))))))) ; Small rotations for unoccupied
    :chemistry-inspired
    (let [num-layers (:num-excitation-layers additional-options 1)
          num-electron-pairs (/ num-qubits 2)
          params-per-layer (+ num-qubits num-electron-pairs
                              (/ (* num-electron-pairs (dec num-electron-pairs)) 2))]
      (vec (flatten (for [_layer (range num-layers)]
                      (concat
                       ;; Initial state preparation: π for occupied, 0 for unoccupied
                       (for [qubit (range num-qubits)]
                         (if (< qubit num-electrons) m/PI 0.0))
                       ;; Small values for excitation parameters  
                       (repeat (- params-per-layer num-qubits) 0.1))))))

    :uccsd-inspired
    (vec (repeat (:num-excitations additional-options (/ num-qubits 2)) 0.1))

    :symmetry-preserving
    (vec (repeat (* (:num-layers additional-options 1) (dec num-qubits)) 0.1))

    ;; Default: small random initialization
    (vec (repeatedly (* num-qubits 3) #(* 0.2 (- (rand) 0.5))))))

