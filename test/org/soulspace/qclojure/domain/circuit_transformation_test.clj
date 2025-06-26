(ns org.soulspace.qclojure.domain.circuit-transformation-test
  "Tests for circuit transformation functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]
            [org.soulspace.qclojure.domain.operation-registry :as gr]))

;;
;; Helper functions for testing
;;
(defn create-test-circuit-1
  "Create a simple 2-qubit Bell state circuit."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

(defn create-test-circuit-2
  "Create a 3-qubit circuit with mixed operations."
  []
  (-> (qc/create-circuit 3 "Mixed Circuit")
      (qc/x-gate 0)
      (qc/h-gate 1)
      (qc/cnot-gate 0 2)
      (qc/z-gate 1)))

(defn create-test-circuit-with-gaps
  "Create a circuit that uses qubits with gaps (0, 2, 5)."
  []
  (-> (qc/create-circuit 6 "Sparse Circuit")
      (qc/h-gate 0)
      (qc/x-gate 2)
      (qc/cnot-gate 0 5)))

(defn create-circuit-with-swap
  "Create a circuit with SWAP gates."
  []
  (-> (qc/create-circuit 3 "SWAP Circuit")
      (qc/h-gate 0)
      (qc/swap-gate 0 2)
      (qc/cnot-gate 0 1)))

(defn operations-of-type
  "Count operations of a specific type in a circuit."
  [circuit operation-type]
  (->> (:operations circuit)
       (filter #(= (:operation-type %) operation-type))
       count))

;;
;; Tests for circuit composition and extension
;;
(deftest test-extend-circuit
  (testing "Basic circuit extension"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 4)]
      (is (= (:num-qubits extended) 4))
      (is (= (:num-qubits original) 2))
      (is (= (count (:operations extended)) (count (:operations original))))
      (is (s/valid? ::qc/quantum-circuit extended))))

  (testing "Extension with qubit mapping"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 4 :qubit-mapping #(+ % 2))]
      (is (= (:num-qubits extended) 4))
      ;; H gate should now be on qubit 2 instead of 0
      (let [h-operation (first (:operations extended))]
        (is (= (get-in h-operation [:operation-params :target]) 2)))
      ;; CNOT should now be from qubit 2 to 3 instead of 0 to 1
      (let [cnot-operation (second (:operations extended))]
        (is (= (get-in cnot-operation [:operation-params :control]) 2))
        (is (= (get-in cnot-operation [:operation-params :target]) 3)))))

  (testing "Extension with same size updates name but keeps operations"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 2)]
      (is (= (:num-qubits original) (:num-qubits extended)))
      (is (= (:operations original) (:operations extended)))
      (is (not= (:name original) (:name extended)))))

  (testing "Extension with identity mapping"
    (let [original (create-test-circuit-1)
          extended (cc/extend-circuit original 3 :qubit-mapping identity)]
      (is (= (:operations original) (:operations extended))))))

(deftest test-compose-circuits
  (testing "Basic circuit composition"
    (let [circuit1 (-> (qc/create-circuit 2) (qc/h-gate 0))
          circuit2 (-> (qc/create-circuit 2) (qc/x-gate 1))
          composed (cc/compose-circuits circuit1 circuit2)]
      (is (= (:num-qubits composed) 2))
      (is (= (count (:operations composed)) 2))
      (is (= (get-in composed [:operations 0 :operation-type]) :h))
      (is (= (get-in composed [:operations 1 :operation-type]) :x))))

  (testing "Composition with different qubit counts"
    (let [circuit1 (-> (qc/create-circuit 1) (qc/h-gate 0))
          circuit2 (-> (qc/create-circuit 3) (qc/cnot-gate 0 2))
          composed (cc/compose-circuits circuit1 circuit2)]
      (is (= (:num-qubits composed) 3))
      (is (= (count (:operations composed)) 2))))

  (testing "Composition with offset"
    (let [circuit1 (qc/create-circuit 5)
          circuit2 (-> (qc/create-circuit 2) (qc/h-gate 0) (qc/cnot-gate 0 1))
          composed (cc/compose-circuits circuit1 circuit2 {:offset 3})]
      (is (= (:num-qubits composed) 5))
      ;; H gate should be on qubit 3
      (let [h-op (first (:operations composed))]
        (is (= (get-in h-op [:operation-params :target]) 3)))
      ;; CNOT should be from qubit 3 to 4
      (let [cnot-op (second (:operations composed))]
        (is (= (get-in cnot-op [:operation-params :control]) 3))
        (is (= (get-in cnot-op [:operation-params :target]) 4))))))

;;
;; Tests for qubit usage analysis and optimization
;;

(deftest test-analyze-qubit-usage
  (testing "Full qubit usage"
    (let [circuit (create-test-circuit-1)
          analysis (ct/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{0 1}))
      (is (= (:total-qubits analysis) 2))
      (is (= (:unused-qubits analysis) #{}))
      (is (= (:max-qubit-id analysis) 1))
      (is (= (:qubit-usage-efficiency analysis) 1.0))))

  (testing "Sparse qubit usage"
    (let [circuit (create-test-circuit-with-gaps)
          analysis (ct/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{0 2 5}))
      (is (= (:total-qubits analysis) 6))
      (is (= (:unused-qubits analysis) #{1 3 4}))
      (is (= (:max-qubit-id analysis) 5))
      (is (= (:qubit-usage-efficiency analysis) 0.5))))

  (testing "Empty circuit"
    (let [circuit (qc/create-circuit 3)
          analysis (ct/analyze-qubit-usage circuit)]
      (is (= (:used-qubits analysis) #{}))
      (is (= (:total-qubits analysis) 3))
      (is (= (:unused-qubits analysis) #{0 1 2}))
      (is (= (:max-qubit-id analysis) -1))
      (is (= (:qubit-usage-efficiency analysis) 0.0)))))

(deftest test-optimize-qubit-usage
  (testing "Optimization of sparse circuit"
    (let [circuit (create-test-circuit-with-gaps)
          result (ct/optimize-qubit-usage circuit)]
      (is (= (:optimized-qubits result) 3))
      (is (= (:qubits-saved result) 3))
      (is (= (:original-qubits result) 6))
      (is (= (:qubit-mapping result) {0 0, 2 1, 5 2}))))

  (testing "Optimization of already efficient circuit"
    (let [circuit (create-test-circuit-1)
          result (ct/optimize-qubit-usage circuit)]
      (is (= (:optimized-qubits result) 2))
      (is (= (:qubits-saved result) 0))
      (is (= (:qubit-mapping result) {0 0, 1 1}))))

  (testing "Optimized circuit operations are remapped correctly"
    (let [circuit (create-test-circuit-with-gaps)
          result (ct/optimize-qubit-usage circuit)
          optimized-circuit (:quantum-circuit result)]
      ;; Original H gate on qubit 0 should stay on qubit 0
      (is (= (get-in optimized-circuit [:operations 0 :operation-params :target]) 0))
      ;; Original X gate on qubit 2 should be on qubit 1
      (is (= (get-in optimized-circuit [:operations 1 :operation-params :target]) 1))
      ;; Original CNOT from 0 to 5 should be from 0 to 2
      (let [cnot-op (nth (:operations optimized-circuit) 2)]
        (is (= (get-in cnot-op [:operation-params :control]) 0))
        (is (= (get-in cnot-op [:operation-params :target]) 2))))))

;;
;; Tests for hardware topology creation
;;

(deftest test-topology-creation
  (testing "Linear topology"
    (let [topology (ct/create-linear-topology 4)]
      (is (= topology [[1] [0 2] [1 3] [2]]))
      (is (= (count topology) 4))))

  (testing "Ring topology"
    (let [topology (ct/create-ring-topology 4)]
      (is (= topology [[3 1] [0 2] [1 3] [2 0]]))
      (is (= (count topology) 4))))

  (testing "Star topology"
    (let [topology (ct/create-star-topology 4)]
      (is (= topology [[1 2 3] [0] [0] [0]]))
      (is (= (count topology) 4))))

  (testing "Grid topology"
    (let [topology (ct/create-grid-topology 2 2)]
      (is (= topology [[2 1] [3 0] [0 3] [1 2]]))
      (is (= (count topology) 4))))

  (testing "Single qubit topologies"
    (is (= (ct/create-linear-topology 1) [[]]))
    (is (= (ct/create-ring-topology 1) [[]]))
    (is (= (ct/create-star-topology 1) [[]]))))

(deftest test-validate-topology
  (testing "Valid topologies"
    (is (ct/validate-topology [[1] [0]]))
    (is (ct/validate-topology [[1 2] [0] [0]])))

  (testing "Invalid topologies"
    ;; Self-connection
    (is (not (ct/validate-topology [[0 1] [0]])))
    ;; Asymmetric connection
    (is (not (ct/validate-topology [[1] []])))))

;;
;; Tests for topology analysis
;;

(deftest test-topology-analysis
  (testing "Linear topology analysis"
    (let [topology (ct/create-linear-topology 4)
          analysis (ct/analyze-topology-connectivity topology)]
      (is (= (:num-qubits analysis) 4))
      (is (= (:total-edges analysis) 3))
      (is (= (:max-degree analysis) 2))
      (is (= (:min-degree analysis) 1))
      (is (:is-connected analysis))))

  (testing "Star topology analysis"
    (let [topology (ct/create-star-topology 4)
          analysis (ct/analyze-topology-connectivity topology)]
      (is (= (:num-qubits analysis) 4))
      (is (= (:total-edges analysis) 3))
      (is (= (:max-degree analysis) 3))
      (is (= (:min-degree analysis) 1))
      (is (:is-connected analysis))))

  (testing "Disconnected topology"
    (let [topology [[1] [0] [] []]  ; Two components: {0,1} and {2,3} isolated
          analysis (ct/analyze-topology-connectivity topology)]
      (is (not (:is-connected analysis)))))

  (testing "Single node topology"
    (let [topology [[]]
          analysis (ct/analyze-topology-connectivity topology)]
      (is (= (:num-qubits analysis) 1))
      (is (= (:total-edges analysis) 0))
      (is (= (:max-degree analysis) 0))
      (is (= (:min-degree analysis) 0))
      (is (:is-connected analysis))))  ; Single node is considered connected

  (testing "Empty topology"
    (let [topology []
          analysis (ct/analyze-topology-connectivity topology)]
      (is (= (:num-qubits analysis) 0))
      (is (= (:total-edges analysis) 0)))))

;;
;; Tests for circuit transformation
;;

(deftest test-transform-circuit
  (testing "Transform circuit with unsupported gates"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/t-gate 1)  ; T gate might need decomposition
                      (qc/cnot-gate 0 1))
          supported-ops #{:h :x :z :rz :cnot}  ; T not supported
          result (ct/transform-circuit circuit supported-ops)]
      (is (s/valid? ::ct/transformation-result result))
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))
      ;; All operations in result should be supported
      (let [transformed-circuit (:quantum-circuit result)
            operation-types (set (map :operation-type (:operations transformed-circuit)))]
        (is (set/subset? operation-types supported-ops)))))

  (testing "Transform circuit with all supported gates"
    (let [circuit (create-test-circuit-1)
          supported-ops #{:h :cnot}
          result (ct/transform-circuit circuit supported-ops)]
      (is (= (:transformed-operation-count result) 0))
      (is (empty? (:unsupported-operations result)))
      (is (= (:quantum-circuit result) circuit))))

  (testing "Transform circuit with options"
    (let [circuit (-> (qc/create-circuit 1) (qc/t-gate 0))
          supported-ops #{:h :x :z :rz}
          result (ct/transform-circuit circuit supported-ops {:max-iterations 10})]
      (is (s/valid? ::ct/transformation-result result)))))

;;
;; Tests for two-qubit operation extraction
;;

(deftest test-extract-two-qubit-operations
  (testing "Circuit with two-qubit operations"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 1)
                      (qc/cz-gate 1 2)
                      (qc/x-gate 2))
          two-qubit-ops (ct/extract-two-qubit-operations circuit)]
      (is (= (count two-qubit-ops) 2))
      (is (= (set (map :control two-qubit-ops)) #{0 1}))
      (is (= (set (map :target two-qubit-ops)) #{1 2}))))

  (testing "Circuit with only single-qubit operations"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/x-gate 1))
          two-qubit-ops (ct/extract-two-qubit-operations circuit)]
      (is (empty? two-qubit-ops))))

  (testing "Circuit with SWAP operations"
    (let [circuit (create-circuit-with-swap)
          two-qubit-ops (ct/extract-two-qubit-operations circuit)]
      ;; Should find CNOT and potentially SWAP
      (is (>= (count two-qubit-ops) 1)))))

;;
;; Tests for distance matrix calculation
;;

(deftest test-calculate-distance-matrix
  (testing "Linear topology distances"
    (let [topology (ct/create-linear-topology 4)
          distances (ct/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [0 2]) 2))
      (is (= (get-in distances [0 3]) 3))
      (is (= (get-in distances [1 3]) 2))))

  (testing "Star topology distances"
    (let [topology (ct/create-star-topology 4)
          distances (ct/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [1 2]) 2))  ; Through center
      (is (= (get-in distances [2 3]) 2)))) ; Through center

  (testing "Ring topology distances"
    (let [topology (ct/create-ring-topology 4)
          distances (ct/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [0 2]) 2))
      (is (= (get-in distances [0 3]) 1)))))  ; Shorter way around

;;
;; Tests for shortest path finding
;;

(deftest test-find-shortest-path
  (testing "Path in linear topology"
    (let [topology (ct/create-linear-topology 4)
          path (ct/find-shortest-path topology 0 3)]
      (is (= path [0 1 2 3]))))

  (testing "Path in star topology"
    (let [topology (ct/create-star-topology 4)
          path (ct/find-shortest-path topology 1 2)]
      (is (= path [1 0 2]))))  ; Through center

  (testing "Same start and end"
    (let [topology (ct/create-linear-topology 3)
          path (ct/find-shortest-path topology 1 1)]
      (is (= path [1]))))

  (testing "Adjacent qubits"
    (let [topology (ct/create-linear-topology 3)
          path (ct/find-shortest-path topology 0 1)]
      (is (= path [0 1])))))

;;
;; Tests for SWAP operation generation
;;

(deftest test-generate-swap-operations
  (testing "No SWAPs needed for adjacent qubits"
    (let [path [0 1]
          swaps (ct/generate-swap-operations path 0)]
      (is (empty? swaps))))

  (testing "SWAP operations for routing"
    (let [path [0 1 2 3]
          swaps (ct/generate-swap-operations path 0)]
      (is (= (count swaps) 3))  ; Path length - 1
      ;; Should generate SWAPs to move qubit through path
      (is (every? #(= (:operation-type %) :swap) swaps))))

  (testing "Single qubit path"
    (let [path [0]
          swaps (ct/generate-swap-operations path 0)]
      (is (empty? swaps)))))

;;
;; Tests for topology optimization
;;

(deftest test-optimize-for-topology
  (testing "Optimize circuit for linear topology"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 2))  ; Non-adjacent
          topology (ct/create-linear-topology 3)
          result (ct/optimize-for-topology circuit topology)]
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))
      (is (map? (:logical-to-physical result)))
      (is (map? (:physical-to-logical result)))
      (is (>= (:swap-count result) 0))
      (is (>= (:total-cost result) 0))))

  (testing "Optimize circuit for star topology"
    (let [circuit (-> (qc/create-circuit 4)
                      (qc/cnot-gate 1 2)
                      (qc/cnot-gate 2 3))
          topology (ct/create-star-topology 4)
          result (ct/optimize-for-topology circuit topology)]
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result)))))

  (testing "Optimize with options"
    (let [circuit (create-test-circuit-1)
          topology (ct/create-linear-topology 2)
          result (ct/optimize-for-topology circuit topology {:insert-swaps? false})]
      (is (s/valid? ::qc/quantum-circuit (:quantum-circuit result))))))

;;
;; Tests for topology comparison
;;

(deftest test-compare-topologies
  (testing "Compare topologies for a circuit"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/cnot-gate 0 2))
          topologies {"linear" (ct/create-linear-topology 3)
                      "star" (ct/create-star-topology 3)
                      "ring" (ct/create-ring-topology 3)}
          comparison (ct/compare-topologies circuit topologies)]
      (is (vector? comparison))
      (is (= (count comparison) 3))
      ;; Results should be sorted by cost
      (is (every? #(contains? % :topology-name) comparison))
      (is (every? #(contains? % :total-cost) comparison))
      (is (every? #(contains? % :swap-count) comparison)))))

;;
;; Tests for operation parameter updates
;;

(deftest test-update-operation-params
  (testing "Single-qubit operation parameter update"
    (let [h-op {:operation-type :h :operation-params {:target 0}}
          updated (cc/update-operation-params h-op #(+ % 2))]
      (is (= (:operation-type updated) :h))
      (is (= (get-in updated [:operation-params :target]) 2))))

  (testing "Two-qubit operation parameter update"
    (let [cnot-op {:operation-type :cnot :operation-params {:control 0 :target 1}}
          updated (cc/update-operation-params cnot-op #(+ % 3))]
      (is (= (get-in updated [:operation-params :control]) 3))
      (is (= (get-in updated [:operation-params :target]) 4))))

  (testing "Operation with non-qubit parameters preserved"
    (let [rx-op {:operation-type :rx :operation-params {:target 1 :angle Math/PI}}
          updated (cc/update-operation-params rx-op #(+ % 5))]
      (is (= (get-in updated [:operation-params :target]) 6))
      (is (= (get-in updated [:operation-params :angle]) Math/PI))))

  (testing "Measurement operation with qubit vector"
    (let [measure-op {:operation-type :measure :operation-params {:measurement-qubits [0 1 2]}}
          updated (cc/update-operation-params measure-op #(+ % 10))]
      (is (= (get-in updated [:operation-params :measurement-qubits]) [10 11 12]))))

  (testing "Operation without parameters"
    (let [op {:operation-type :barrier}
          updated (cc/update-operation-params op #(+ % 1))]
      (is (= updated op))))

  (testing "Complex multi-control operation"
    (let [toffoli-op {:operation-type :toffoli :operation-params {:control1 0 :control2 1 :target 2}}
          updated (cc/update-operation-params toffoli-op #(* % 2))]
      (is (= (get-in updated [:operation-params :control1]) 0))
      (is (= (get-in updated [:operation-params :control2]) 2))
      (is (= (get-in updated [:operation-params :target]) 4)))))

;;
;; Tests for optimal mapping finding
;;

(deftest test-find-optimal-mapping
  (testing "Optimal mapping for linear topology"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 2))  ; Non-adjacent operation
          topology [[1] [0 2] [1]]
          distance-matrix (ct/calculate-distance-matrix topology)
          mapping (ct/find-optimal-mapping circuit topology distance-matrix)]
      (is (map? mapping))
      (is (= (count mapping) 3))
      ;; Should map logical qubits to minimize CNOT cost
      (is (contains? mapping 0))
      (is (contains? mapping 2))))

  (testing "Optimal mapping for star topology"  
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/cnot-gate 0 1)
                      (qc/cnot-gate 1 2))
          topology [[1 2] [0] [0]]  ; Star with center at 0
          distance-matrix (ct/calculate-distance-matrix topology)
          mapping (ct/find-optimal-mapping circuit topology distance-matrix)]
      (is (map? mapping))
      (is (= (count mapping) 3))))

  (testing "Small circuit with large topology"
    (let [circuit (-> (qc/create-circuit 2) (qc/cnot-gate 0 1))
          topology (ct/create-linear-topology 5)
          distance-matrix (ct/calculate-distance-matrix topology)
          mapping (ct/find-optimal-mapping circuit topology distance-matrix)]
      (is (map? mapping))
      (is (= (count mapping) 2)))))

;;
;; Tests for topology info generation
;;

(deftest test-get-topology-info
  (testing "Linear topology info"
    (let [topology (ct/create-linear-topology 4)
          info (ct/get-topology-info topology "Linear-4")]
      (is (string? info))
      (is (.contains info "Linear-4"))
      (is (.contains info "Qubits: 4"))
      (is (.contains info "Connected: true"))))

  (testing "Star topology info"
    (let [topology (ct/create-star-topology 5)
          info (ct/get-topology-info topology "Star-5")]
      (is (string? info))
      (is (.contains info "Star-5"))
      (is (.contains info "Qubits: 5"))
      (is (.contains info "degree:"))))

  (testing "Disconnected topology info"
    (let [topology [[1] [0] [] []]  ; Two isolated components
          info (ct/get-topology-info topology "Disconnected")]
      (is (string? info))
      (is (.contains info "Connected: false")))))

(comment
  (run-tests)
  ;
  )