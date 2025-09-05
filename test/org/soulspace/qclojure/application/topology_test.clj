(ns org.soulspace.qclojure.application.topology-test
  "Tests for hardware optimization functionality."
 (:require [clojure.test :refer [deftest is testing run-tests]]
           [clojure.spec.alpha :as s]
           [clojure.string :as str]
           [org.soulspace.qclojure.domain.circuit :as qc]
           [org.soulspace.qclojure.application.topology :as topo]))

;;;
;;; Helper functions for testing
;;;
(defn create-test-circuit-1
  "Create a simple 2-qubit Bell state circuit."
  []
  (-> (qc/create-circuit 2 "Bell State")
      (qc/h-gate 0)
      (qc/cnot-gate 0 1)))

(defn create-circuit-with-swap
  "Create a circuit with SWAP gates."
  []
  (-> (qc/create-circuit 3 "SWAP Circuit")
      (qc/h-gate 0)
      (qc/swap-gate 0 2)
      (qc/cnot-gate 0 1)))

;;;
;;; Tests for hardware topology creation
;;;
(deftest test-topology-creation
  (testing "Linear topology"
    (let [topology (topo/coupling-for-linear-topology 4)]
      (is (= topology [[1] [0 2] [1 3] [2]]))
      (is (= (count topology) 4))))

  (testing "Ring topology"
    (let [topology (topo/coupling-for-ring-topology 4)]
      (is (= topology [[3 1] [0 2] [1 3] [2 0]]))
      (is (= (count topology) 4))))

  (testing "Star topology"
    (let [topology (topo/coupling-for-star-topology 4)]
      (is (= topology [[1 2 3] [0] [0] [0]]))
      (is (= (count topology) 4))))

  (testing "Grid topology"
    (let [topology (topo/coupling-for-grid-topology 2 2)]
      (is (= topology [[2 1] [3 0] [0 3] [1 2]]))
      (is (= (count topology) 4))))

  #_(testing "Heavy-hex topology - new API"
    (let [basic (topo/coupling-for-heavy-hex-topology :basic)
          falcon (topo/coupling-for-heavy-hex-topology :falcon)
          hummingbird (topo/coupling-for-heavy-hex-topology :hummingbird)
          eagle (topo/coupling-for-heavy-hex-topology :eagle)]
      ;; Test basic topology (7 qubits)
      (is (= (count basic) 7))
      
      ;; Test falcon topology (27 qubits)
      (is (= (count falcon) 27))
      
      ;; Test hummingbird topology (65 qubits)
      (is (= (count hummingbird) 65))
      
      ;; Test eagle topology (127 qubits)
      (is (= (count eagle) 127))
      
      ;; Test validation
      (is (topo/validate-topology basic))
      (is (topo/validate-topology falcon))
      (is (topo/validate-topology hummingbird))
      (is (topo/validate-topology eagle))))

  (testing "Heavy-hex topology error cases"
    ;; Test invalid processor type (precondition)
    (is (thrown? AssertionError (topo/coupling-for-heavy-hex-topology 0)))
    
    ;; Test unsupported processor type
    (is (thrown-with-msg? clojure.lang.ExceptionInfo 
                          #"Unsupported heavy-hex processor type"
                          (topo/coupling-for-heavy-hex-topology :unsupported))))

  (testing "Single qubit topologies"
    (is (= (topo/coupling-for-linear-topology 1) [[]]))
    (is (= (topo/coupling-for-ring-topology 1) [[]]))
    (is (= (topo/coupling-for-star-topology 1) [[]]))))

(deftest test-validate-topology
  (testing "Valid topologies"
    (is (topo/validate-coupling [[1] [0]]))
    (is (topo/validate-coupling [[1 2] [0] [0]])))

  (testing "Invalid topologies"
    ;; Self-connection
    (is (not (topo/validate-coupling [[0 1] [0]])))
    ;; Asymmetric connection
    (is (not (topo/validate-coupling [[1] []])))))

;;
;; Tests for topology analysis
;;
(deftest test-topology-analysis
  (testing "Linear topology analysis"
    (let [topology (topo/coupling-for-linear-topology 4)
          analysis (topo/analyze-coupling-connectivity topology)]
      (is (= (:num-qubits analysis) 4))
      (is (= (:total-edges analysis) 3))
      (is (= (:max-degree analysis) 2))
      (is (= (:min-degree analysis) 1))
      (is (:is-connected analysis))))

  (testing "Star topology analysis"
    (let [topology (topo/coupling-for-star-topology 4)
          analysis (topo/analyze-coupling-connectivity topology)]
      (is (= (:num-qubits analysis) 4))
      (is (= (:total-edges analysis) 3))
      (is (= (:max-degree analysis) 3))
      (is (= (:min-degree analysis) 1))
      (is (:is-connected analysis))))

  #_(testing "Heavy-hex topology analysis"
    (let [basic (topo/coupling-for-heavy-hex-topology :basic)
          analysis1 (topo/analyze-topology-connectivity basic)
          falcon (topo/coupling-for-heavy-hex-topology :falcon)
          analysis2 (topo/analyze-topology-connectivity falcon)]
      ;; Basic heavy-hex analysis (7 qubits)
      (is (= (:num-qubits analysis1) 7))
      (is (= (:total-edges analysis1) 12))
      (is (= (:max-degree analysis1) 6)) ; Center qubit
      (is (= (:min-degree analysis1) 3)) ; Vertex qubits
      (is (= (:diameter analysis1) 2))
      (is (:is-connected analysis1))
      
      ;; Falcon heavy-hex analysis (27 qubits)
      (is (= (:num-qubits analysis2) 27))
      (is (>= (:total-edges analysis2) 30)) ; Should have decent connectivity
      (is (>= (:max-degree analysis2) 4))   ; Multiple hub qubits
      (is (>= (:min-degree analysis2) 2))   ; All qubits should have connections
      (is (>= (:diameter analysis2) 5))     ; Larger diameter than basic
      (is (:is-connected analysis2))))

  (testing "Disconnected topology"
    (let [topology [[1] [0] [] []]  ; Two components: {0,1} and {2,3} isolated
          analysis (topo/analyze-coupling-connectivity topology)]
      (is (not (:is-connected analysis)))))

  (testing "Single node topology"
    (let [topology [[]]
          analysis (topo/analyze-coupling-connectivity topology)]
      (is (= (:num-qubits analysis) 1))
      (is (= (:total-edges analysis) 0))
      (is (= (:max-degree analysis) 0))
      (is (= (:min-degree analysis) 0))
      (is (:is-connected analysis))))  ; Single node is considered connected

  (testing "Empty topology"
    (let [topology []
          analysis (topo/analyze-coupling-connectivity topology)]
      (is (= (:num-qubits analysis) 0))
      (is (= (:total-edges analysis) 0)))))

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
          two-qubit-ops (topo/extract-two-qubit-operations circuit)]
      (is (= (count two-qubit-ops) 2))
      (is (= (set (map :control two-qubit-ops)) #{0 1}))
      (is (= (set (map :target two-qubit-ops)) #{1 2}))))

  (testing "Circuit with only single-qubit operations"
    (let [circuit (-> (qc/create-circuit 2)
                      (qc/h-gate 0)
                      (qc/x-gate 1))
          two-qubit-ops (topo/extract-two-qubit-operations circuit)]
      (is (empty? two-qubit-ops))))

  (testing "Circuit with SWAP operations"
    (let [circuit (create-circuit-with-swap)
          two-qubit-ops (topo/extract-two-qubit-operations circuit)]
      ;; Should find CNOT and potentially SWAP
      (is (>= (count two-qubit-ops) 1)))))

;;
;; Tests for distance matrix calculation
;;
(deftest test-calculate-distance-matrix
  (testing "Linear topology distances"
    (let [topology (topo/coupling-for-linear-topology 4)
          distances (topo/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [0 2]) 2))
      (is (= (get-in distances [0 3]) 3))
      (is (= (get-in distances [1 3]) 2))))

  (testing "Star topology distances"
    (let [topology (topo/coupling-for-star-topology 4)
          distances (topo/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [1 2]) 2))  ; Through center
      (is (= (get-in distances [2 3]) 2)))) ; Through center

  (testing "Ring topology distances"
    (let [topology (topo/coupling-for-ring-topology 4)
          distances (topo/calculate-distance-matrix topology)]
      (is (= (get-in distances [0 0]) 0))
      (is (= (get-in distances [0 1]) 1))
      (is (= (get-in distances [0 2]) 2))
      (is (= (get-in distances [0 3]) 1)))))  ; Shorter way around

;;
;; Tests for shortest path finding
;;
(deftest test-find-shortest-path
  (testing "Path in linear topology"
    (let [topology (topo/coupling-for-linear-topology 4)
          path (topo/find-shortest-path topology 0 3)]
      (is (= path [0 1 2 3]))))

  (testing "Path in star topology"
    (let [topology (topo/coupling-for-star-topology 4)
          path (topo/find-shortest-path topology 1 2)]
      (is (= path [1 0 2]))))  ; Through center

  (testing "Same start and end"
    (let [topology (topo/coupling-for-linear-topology 3)
          path (topo/find-shortest-path topology 1 1)]
      (is (= path [1]))))

  (testing "Adjacent qubits"
    (let [topology (topo/coupling-for-linear-topology 3)
          path (topo/find-shortest-path topology 0 1)]
      (is (= path [0 1])))))

;;
;; Tests for SWAP operation generation
;;
(deftest test-generate-swap-operations
  (testing "No SWAPs needed for adjacent qubits"
    (let [path [0 1]
          swaps (topo/generate-swap-operations path 0)]
      (is (empty? swaps))))

  (testing "SWAP operations for routing"
    (let [path [0 1 2 3]
          swaps (topo/generate-swap-operations path 0)]
      (is (= (count swaps) 3))  ; Path length - 1
      ;; Should generate SWAPs to move qubit through path
      (is (every? #(= (:operation-type %) :swap) swaps))))

  (testing "Single qubit path"
    (let [path [0]
          swaps (topo/generate-swap-operations path 0)]
      (is (empty? swaps)))))

;;
;; Tests for topology optimization
;;
(deftest test-optimize-for-topology
  (testing "Optimize circuit for linear topology"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 2))  ; Non-adjacent
          topology (topo/coupling-for-linear-topology 3)
          result (topo/optimize-for-coupling circuit topology)]
      (is (s/valid? ::qc/circuit (:circuit result)))
      (is (map? (:logical-to-physical result)))
      (is (map? (:physical-to-logical result)))
      (is (>= (:swap-count result) 0))
      (is (>= (:total-cost result) 0))))

  (testing "Optimize circuit for star topology"
    (let [circuit (-> (qc/create-circuit 4)
                      (qc/cnot-gate 1 2)
                      (qc/cnot-gate 2 3))
          topology (topo/coupling-for-star-topology 4)
          result (topo/optimize-for-coupling circuit topology)]
      (is (s/valid? ::qc/circuit (:circuit result)))))

  (testing "Optimize with options"
    (let [circuit (create-test-circuit-1)
          topology (topo/coupling-for-linear-topology 2)
          result (topo/optimize-for-coupling circuit topology {:insert-swaps? false})]
      (is (s/valid? ::qc/circuit (:circuit result))))))

;;
;; Tests for topology comparison
;;
(deftest test-compare-topologies
  (testing "Compare topologies for a circuit"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/cnot-gate 0 2))
          topologies {"linear" (topo/coupling-for-linear-topology 3)
                      "star" (topo/coupling-for-star-topology 3)
                      "ring" (topo/coupling-for-ring-topology 3)}
          comparison (topo/compare-couplings circuit topologies)]
      (is (vector? comparison))
      (is (= (count comparison) 3))
      ;; Results should be sorted by cost
      (is (every? #(contains? % :topology-name) comparison))
      (is (every? #(contains? % :total-cost) comparison))
      (is (every? #(contains? % :swap-count) comparison)))))

;;
;; Tests for optimal mapping finding
;;
(deftest test-find-optimal-mapping
  (testing "Optimal mapping for linear topology"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/cnot-gate 0 2))  ; Non-adjacent operation
          topology [[1] [0 2] [1]]
          distance-matrix (topo/calculate-distance-matrix topology)
          mapping (topo/find-optimal-mapping circuit topology distance-matrix)]
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
          distance-matrix (topo/calculate-distance-matrix topology)
          mapping (topo/find-optimal-mapping circuit topology distance-matrix)]
      (is (map? mapping))
      (is (= (count mapping) 3))))

  (testing "Small circuit with large topology"
    (let [circuit (-> (qc/create-circuit 2) (qc/cnot-gate 0 1))
          topology (topo/coupling-for-linear-topology 5)
          distance-matrix (topo/calculate-distance-matrix topology)
          mapping (topo/find-optimal-mapping circuit topology distance-matrix)]
      (is (map? mapping))
      (is (= (count mapping) 2)))))

;;
;; Tests for topology info generation
;;
(deftest test-get-topology-info
  (testing "Linear topology info"
    (let [topology (topo/coupling-for-linear-topology 4)
          info (topo/get-coupling-info topology "Linear-4")]
      (is (string? info))
      (is (str/includes? info "Linear-4"))
      (is (str/includes? info "Qubits: 4"))
      (is (str/includes? info "Connected: true"))))

  (testing "Star topology info"
    (let [topology (topo/coupling-for-star-topology 5)
          info (topo/get-coupling-info topology "Star-5")]
      (is (string? info))
      (is (str/includes? info "Star-5"))
      (is (str/includes? info "Qubits: 5"))
      (is (str/includes? info "degree:"))))

  (testing "Disconnected topology info"
    (let [topology [[1] [0] [] []]  ; Two isolated components
          info (topo/get-coupling-info topology "Disconnected")]
      (is (string? info))
      (is (str/includes? info "Connected: false")))))


(comment
  ;; Run all tests in this namespace
  (run-tests)
  ;
  )
