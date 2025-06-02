(ns examples.basic-quantum-concepts
  "Examples demonstrating basic quantum computing concepts using QClojure"
  (:require [org.soulspace.qclojure.domain.quantum-state :as qs]
            [org.soulspace.qclojure.domain.quantum-gate :as qg]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]
            [org.soulspace.qclojure.application.quantum-algorithms :as qa]
            [org.soulspace.qclojure.adapter.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.ascii :as ascii-viz]
            [org.soulspace.qclojure.adapter.io :as qio]))

(defn quantum-superposition-demo
  "Demonstrate quantum superposition using Hadamard gates."
  []
  (println "=== Quantum Superposition Demo ===\n")

  ;; Start with |0⟩ state
  (println "1. Starting with |0⟩ state:")
  (let [initial-state qs/|0⟩]
    (println (viz/visualize-quantum-state :ascii initial-state))
    (println)

    ;; Apply Hadamard gate to create superposition
    (println "2. After applying Hadamard gate (creates |+⟩ = (|0⟩ + |1⟩)/√2):")
    (let [superposition-state (qg/h-gate initial-state)]
      (println (viz/visualize-quantum-state :ascii superposition-state :show-amplitudes true))
      (println)

      ;; Show Bloch sphere representation
      (println "3. Bloch sphere representation:")
      (println (viz/visualize-bloch-sphere :ascii superposition-state))
      (println)

      ;; Measure the state multiple times to show probabilistic outcomes
      (println "4. Measurement outcomes (10 trials):")
      (let [measurements (repeatedly 10 #(:outcome (qs/measure-state superposition-state)))]
        (println "Results:" measurements)
        (println (qio/format-measurement-result measurements))))))

(defn quantum-entanglement-demo
  "Demonstrate quantum entanglement using Bell states."
  []
  (println "\n=== Quantum Entanglement Demo ===\n")

  ;; Create Bell state using circuit
  (println "1. Creating Bell state using quantum circuit:")
  (let [bell-circuit (qc/bell-state-circuit)]
    (println (viz/visualize-circuit :ascii bell-circuit))
    (println)

    ;; Execute circuit
    (println "2. Initial state |00⟩:")
    (let [initial-state (qs/zero-state 2)]
      (println (viz/visualize-quantum-state :ascii initial-state))
      (println)

      (println "3. After Hadamard on qubit 0:")
      (let [after-h (qg/h-gate initial-state 0)]
        (println (viz/visualize-quantum-state :ascii after-h :show-amplitudes true))
        (println)

        (println "4. After CNOT gate (final Bell state):")
        (let [bell-state (qg/cnot after-h)]
          (println (viz/visualize-quantum-state :ascii bell-state :show-amplitudes true))
          (println)

          ;; Demonstrate measurement correlations
          (println "5. Measurement correlations (20 trials):")
          (let [measurements (repeatedly 20
                                         (fn []
                                           (let [result (qs/measure-state bell-state)]
                                             ;; Convert single measurement to binary representation
                                             (Integer/toBinaryString (:outcome result)))))]
            (println "Bell state measurements (binary):" measurements)
            (println "Notice: measurements are always 00 or 11 (correlated!)")
            (println (qio/format-measurement-result measurements))))))))

(defn quantum-interference-demo
  "Demonstrate quantum interference using Mach-Zehnder interferometer analogy."
  []
  (println "\n=== Quantum Interference Demo ===\n")

  (println "1. Quantum interference with phase gates:")
  (let [initial (qs/zero-state 1)]

    ;; Path 1: H - Z - H (destructive interference)
    (println "Path 1: |0⟩ → H → Z → H")
    (let [path1 (-> initial
                    (qg/h-gate)
                    (qg/z-gate)
                    (qg/h-gate))]
      (println "Result:" (viz/visualize-quantum-state :ascii path1 :show-amplitudes true))
      (println)

      ;; Path 2: H - I - H (constructive interference)  
      (println "Path 2: |0⟩ → H → I → H")
      (let [path2 (-> initial
                      (qg/h-gate)
                      ;; Identity gate (no operation)
                      (qg/h-gate))]
        (println "Result:" (viz/visualize-quantum-state :ascii path2 :show-amplitudes true))
        (println)

        (println "Observation: Different phase paths lead to different interference patterns!")))))

(defn quantum-gates-showcase
  "Showcase different quantum gates and their effects."
  []
  (println "\n=== Quantum Gates Showcase ===\n")

  (let [states [["Zero state |0⟩" qs/|0⟩]
                ["One state |1⟩" qs/|1⟩]
                ["Plus state |+⟩" qs/|+⟩]
                ["Minus state |-⟩" qs/|-⟩]]

        gates [["Pauli-X (NOT)" qg/x-gate]
               ["Pauli-Y" qg/y-gate]
               ["Pauli-Z" qg/z-gate]
               ["Hadamard" qg/h-gate]]]

    (doseq [[state-name state] states]
      (println (str state-name ":"))
      (println (viz/visualize-quantum-state :ascii state :show-amplitudes true))

      (doseq [[gate-name gate-fn] gates]
        (let [result (gate-fn state)]
          (println (str "  After " gate-name ":"))
          (println (str "    " (qio/format-quantum-state result :precision 2)))))
      (println))))

(defn quantum-circuit-composition-demo
  "Demonstrate quantum circuit composition and analysis."
  []
  (println "\n=== Quantum Circuit Composition Demo ===\n")

  ;; Create individual circuits
  (println "1. Individual Circuits:")
  (let [prep-circuit (-> (qc/create-circuit 2 "Preparation")
                         (qc/h-gate 0)
                         (qc/x-gate 1))

        entangle-circuit (-> (qc/create-circuit 2 "Entanglement")
                             (qc/cnot-gate 0 1))

        measure-circuit (-> (qc/create-circuit 2 "Rotation")
                            (qc/ry-gate 0 (/ Math/PI 4)))]

    (println "Preparation Circuit:")
    (println (viz/visualize-circuit :ascii prep-circuit))
    (println)

    (println "Entanglement Circuit:")
    (println (viz/visualize-circuit :ascii entangle-circuit))
    (println)

    ;; Compose circuits
    (println "2. Composed Circuit:")
    (let [composed (-> prep-circuit
                       (qc/compose-circuits entangle-circuit)
                       (qc/compose-circuits measure-circuit))]
      (println (viz/visualize-circuit :ascii composed))
      (println)

      ;; Analyze circuit properties
      (println "3. Circuit Analysis:")
      (println "  Total gates:" (count (:gates composed)))
      (println "  Circuit depth:" (qc/circuit-depth composed))
      (println "  Gate count by type:" (qc/circuit-gate-count composed))
      (println)

      ;; Execute step by step
      (println "4. Step-by-step Execution:")
      (let [initial-state (qs/zero-state 2)
            evolution (viz/visualize-state-evolution :ascii composed initial-state)]
        (doseq [frame (take 3 evolution)]  ; Show first 3 steps
          (println (str "Step " (:step frame)
                        (when (:gate frame)
                          (str " - Applied " (get-in frame [:gate :gate-type])))))
          (println (viz/visualize-quantum-state :ascii (:state frame))))))))

(defn quantum-measurement-demo
  "Demonstrate different aspects of quantum measurement."
  []
  (println "\n=== Quantum Measurement Demo ===\n")

  ;; 1. Measurement of computational basis states
  (println "1. Measurement of basis states:")
  (let [states [["Zero |0⟩" qs/|0⟩]
                ["One |1⟩" qs/|1⟩]]]
    (doseq [[name state] states]
      (let [measurements (repeatedly 5 #(:outcome (qs/measure-state state)))]
        (println (str name " measurements: " measurements)))))

  (println)

  ;; 2. Measurement of superposition states
  (println "2. Measurement of superposition states:")
  (let [superposition-states [["Plus |+⟩" qs/|+⟩]
                              ["Minus |-⟩" qs/|-⟩]]]
    (doseq [[name state] superposition-states]
      (let [measurements (repeatedly 20 #(:outcome (qs/measure-state state)))]
        (println (str name " (20 measurements):"))
        (println (qio/format-measurement-result measurements)))))

  (println)

  ;; 3. Measurement statistics
  (println "3. Measurement statistics for |+⟩ state:")
  (let [many-measurements (repeatedly 1000 #(:outcome (qs/measure-state qs/|+⟩)))
        zero-count (count (filter #(= % 0) many-measurements))
        one-count (count (filter #(= % 1) many-measurements))]
    (println (str "1000 measurements: " zero-count " zeros, " one-count " ones"))
    (println (str "Ratio: " (/ zero-count 1000.0) " : " (/ one-count 1000.0)))
    (println "Expected: ~0.5 : ~0.5 (50-50 split for |+⟩ state)")))

(defn run-basic-concepts-demo
  "Run all basic quantum concepts demonstrations."
  []
  (println "╔════════════════════════════════════════════════════════════╗")
  (println "║            qclojure: Basic Quantum Concepts Demo           ║")
  (println "╚════════════════════════════════════════════════════════════╝")

  (quantum-superposition-demo)
  (quantum-entanglement-demo)
  (quantum-interference-demo)
  (quantum-gates-showcase)
  (quantum-circuit-composition-demo)
  (quantum-measurement-demo)

  (println "\n╔════════════════════════════════════════════════════════════╗")
  (println "║                     Demo Complete!                         ║")
  (println "╚════════════════════════════════════════════════════════════╝"))

(comment
  ;; Run the complete demo
  (run-basic-concepts-demo)

  ;; Run individual demonstrations
  (quantum-superposition-demo)
  (quantum-entanglement-demo)
  (quantum-interference-demo)
  (quantum-gates-showcase)
  (quantum-circuit-composition-demo)
  (quantum-measurement-demo))
