(ns org.soulspace.qclojure.domain.circuit-transformation
  "Circuit transformation utilities for quantum backends.
   
   This namespace provides functionality for adapting quantum circuits
   to specific hardware backends by transforming operations not supported
   by the backend into equivalent sequences of supported operations."
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.domain.gate-decomposition :as gd]))

;; Specs
(s/def ::transformation-result
  (s/keys :req-un [::qc/quantum-circuit]
          :opt-un [::transformed-operation-count ::unsupported-operations]))

(defn transform-circuit
  "Transform a quantum circuit to use only operations supported by a given backend.

  This function takes a quantum circuit and the supported operations, and returns a new circuit
  where all operations have been decomposed into the supported operations.
  
  Parameters:
  - circuit: Quantum circuit to transform
  - supported-operations: Set of operation types supported
  - options: Optional map with transformation options:
      :max-iterations - Maximum number of decomposition iterations (default: 100)
      :transform-unsupported? - Whether to transform unsupported operations (default: true)
  
  Returns:
  A map containing:
  - :quantum-circuit - The transformed circuit
  - :transformed-operation-count - Count of operations that were transformed
  - :unsupported-operations - List of operation types that couldn't be transformed
  
  Example:
  (transform-circuit my-circuit #{:h :x :cnot} {:max-iterations 50})
  ;=> {:quantum-circuit <transformed-circuit>, :transformed-operation-count 3, :unsupported-operations []}"
  ([circuit supported-operations]
   (transform-circuit circuit supported-operations {}))

  ([circuit supported-operations options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)]}

   (let [max-iterations (get options :max-iterations 100)
         transform-unsupported? (get options :transform-unsupported? true)

         original-operations (:operations circuit)
         original-operation-count (count original-operations)

         ;; Apply transformation
         transformed-operations (if transform-unsupported?
                                  (gd/transform-operations original-operations supported-operations max-iterations)
                                  original-operations)

         ;; Create new circuit with transformed operations
         transformed-circuit (assoc circuit :operations transformed-operations)

         ;; Calculate stats for return value
         new-types (frequencies (map :operation-type transformed-operations))
         remaining-unsupported (into []
                                     (filter #(not (contains? supported-operations %))
                                             (keys new-types)))]

     {:quantum-circuit transformed-circuit
      :transformed-operation-count (- (count transformed-operations) original-operation-count)
      :unsupported-operations remaining-unsupported})))

;;
;; Hardware Topology Creation Functions
;;
(defn create-linear-topology
  "Create a linear hardware topology where qubits are connected in a line.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for linear topology"
  [num-qubits]
  (cond
    (< num-qubits 1) []
    (= num-qubits 1) [[]]
    :else
    (vec (for [i (range num-qubits)]
           (vec (cond
                  (= i 0) [1]                     ; First qubit connects to second
                  (= i (dec num-qubits)) [(dec i)] ; Last qubit connects to second-to-last
                  :else [(dec i) (inc i)]))))))   ; Middle qubits connect to neighbors

(defn create-ring-topology
  "Create a ring hardware topology where qubits are connected in a circle.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for ring topology"
  [num-qubits]
  (cond
    (< num-qubits 3) (create-linear-topology num-qubits) ; Ring needs at least 3 qubits
    :else
    (vec (for [i (range num-qubits)]
           (let [prev (mod (dec i) num-qubits)
                 next (mod (inc i) num-qubits)]
             [prev next])))))

(defn create-star-topology
  "Create a star hardware topology with one central qubit connected to all others.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for star topology"
  [num-qubits]
  (cond
    (< num-qubits 1) []
    (= num-qubits 1) [[]]
    :else
    (vec (cons (vec (range 1 num-qubits))  ; Center qubit (0) connects to all others
               (repeat (dec num-qubits) [0]))))) ; All other qubits connect only to center

(defn create-grid-topology
  "Create a grid hardware topology with qubits arranged in a rectangular grid.
  
  Parameters:
  - rows: Number of rows in the grid
  - cols: Number of columns in the grid
  
  Returns:
  Vector of vectors representing adjacency list for grid topology"
  [rows cols]
  (let [num-qubits (* rows cols)]
    (vec (for [i (range num-qubits)]
           (let [row (quot i cols)
                 col (mod i cols)]
             (vec (concat
                   ;; Up neighbor
                   (when (> row 0) [(- i cols)])
                   ;; Down neighbor  
                   (when (< row (dec rows)) [(+ i cols)])
                   ;; Left neighbor
                   (when (> col 0) [(dec i)])
                   ;; Right neighbor
                   (when (< col (dec cols)) [(inc i)]))))))))

;;
;; Hardware Topology Optimization Functions
;;
(defn validate-topology
  "Validate that a hardware topology is well-formed.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  
  Returns:
  Boolean indicating if topology is valid"
  [topology]
  (and (vector? topology)
       (every? vector? topology)
       ;; Check that no qubit connects to itself
       (every? (fn [qubit-id]
                 (let [neighbors (get topology qubit-id)]
                   (not (some #(= % qubit-id) neighbors))))
               (range (count topology)))
       ;; Check that connections are symmetric
       (every? (fn [qubit-id]
                 (let [neighbors (get topology qubit-id)]
                   (every? (fn [neighbor]
                             (and (< neighbor (count topology))
                                  (some #(= % qubit-id) (get topology neighbor))))
                           neighbors)))
               (range (count topology)))))

(defn calculate-distance-matrix
  "Calculate shortest path distances between all pairs of qubits in topology.
  
  Parameters:
  - topology: Vector of vectors representing qubit connectivity
  
  Returns:
  2D vector where element [i][j] is the shortest distance from qubit i to qubit j"
  [topology]
  (let [num-qubits (count topology)]
    (letfn [(bfs-distance [start end]
              (if (= start end)
                0
                (loop [queue [[start 0]]
                       visited #{start}]
                  (if (empty? queue)
                    Integer/MAX_VALUE ; No path found
                    (let [[current dist] (first queue)
                          rest-queue (rest queue)]
                      (if (= current end)
                        dist
                        (let [neighbors (get topology current)
                              new-nodes (filter #(not (contains? visited %)) neighbors)
                              new-queue (concat rest-queue (map #(vector % (inc dist)) new-nodes))
                              new-visited (into visited new-nodes)]
                          (recur new-queue new-visited))))))))]

      (mapv (fn [i]
              (mapv (fn [j]
                      (bfs-distance i j))
                    (range num-qubits)))
            (range num-qubits)))))

(defn extract-two-qubit-operations
  "Extract all two-qubit operations from a circuit.
  
  Parameters:
  - circuit: Quantum circuit to analyze
  
  Returns:
  Vector of maps containing :control and :target qubit pairs"
  [circuit]
  (let [operations (:operations circuit)]
    (->> operations
         (filter (fn [op]
                   (let [params (:operation-params op)]
                     ;; Check for two-qubit operations
                     (or (and (:control params) (:target params))
                         (and (:control1 params) (:target params))
                         (and (:target1 params) (:target2 params))
                         (and (:swap1 params) (:swap2 params))))))
         (map (fn [op]
                (let [params (:operation-params op)]
                  (cond
                    ;; Standard control-target operations (CNOT, CZ, etc.)
                    (and (:control params) (:target params))
                    {:control (:control params) :target (:target params) :operation-type (:operation-type op)}

                    ;; Toffoli gate (control1 as primary control)
                    (and (:control1 params) (:target params))
                    {:control (:control1 params) :target (:target params) :operation-type (:operation-type op)}

                    ;; Fredkin gate 
                    (and (:target1 params) (:target2 params))
                    {:control (:target1 params) :target (:target2 params) :operation-type (:operation-type op)}

                    ;; SWAP operations
                    (and (:swap1 params) (:swap2 params))
                    {:control (:swap1 params) :target (:swap2 params) :operation-type (:operation-type op)})))))))

(defn calculate-mapping-cost
  "Calculate the cost of a logical-to-physical qubit mapping.
  
  Parameters:
  - two-qubit-ops: Vector of two-qubit operations with :control and :target
  - mapping: Map from logical qubit to physical qubit
  - distance-matrix: 2D vector of distances between physical qubits
  
  Returns:
  Total cost (sum of distances for all two-qubit operations)"
  [two-qubit-ops mapping distance-matrix]
  (reduce (fn [total-cost op]
            (let [logical-control (:control op)
                  logical-target (:target op)
                  physical-control (get mapping logical-control)
                  physical-target (get mapping logical-target)]
              (if (and physical-control physical-target)
                (+ total-cost (get-in distance-matrix [physical-control physical-target]))
                ;; If mapping is incomplete, return very high cost
                Integer/MAX_VALUE)))
          0
          two-qubit-ops))

(defn find-shortest-path
  "Find shortest path between two qubits in the topology.
  
  Parameters:
  - topology: Hardware topology
  - start: Starting qubit
  - end: Ending qubit
  
  Returns:
  Vector of qubits representing the path from start to end"
  [topology start end]
  (if (= start end)
    [start]
    (loop [queue [[start [start]]]
           visited #{start}]
      (if (empty? queue)
        nil ; No path found
        (let [[current path] (first queue)
              rest-queue (rest queue)]
          (if (= current end)
            path
            (let [neighbors (get topology current)
                  new-nodes (filter #(not (contains? visited %)) neighbors)
                  new-queue (concat rest-queue
                                    (map #(vector % (conj path %)) new-nodes))
                  new-visited (into visited new-nodes)]
              (recur new-queue new-visited))))))))

(defn generate-swap-operations
  "Generate SWAP operations to route a qubit from start to end position.
  
  Parameters:
  - path: Vector of qubits representing routing path
  - target-qubit: The logical qubit that needs to be moved
  
  Returns:
  Vector of SWAP operation maps"
  [path target-qubit]
  (if (<= (count path) 2)
    [] ; No SWAPs needed for adjacent qubits
    (let [swap-pairs (partition 2 1 path)] ; Create adjacent pairs
      (mapv (fn [[q1 q2]]
              {:operation-type :swap
               :operation-params {:target1 q1 :target2 q2}
               :routing-info {:moves-qubit target-qubit :from q1 :to q2}})
            swap-pairs))))

(defn find-optimal-mapping
  "Find an optimal mapping from logical qubits to physical qubits using a greedy approach.
  
  Parameters:
  - When called with 3 args [circuit topology distance-matrix]:
    - circuit: Quantum circuit to optimize
    - topology: Hardware topology
    - distance-matrix: Precomputed distance matrix
  - When called with 3 args [two-qubit-ops num-physical-qubits distance-matrix] (legacy):
    - two-qubit-ops: Vector of two-qubit operations
    - num-physical-qubits: Number of physical qubits available
    - distance-matrix: Precomputed distance matrix
  
  Returns:
  Map from logical qubit to physical qubit"
  [arg1 arg2 arg3]
  (let [;; Detect which signature is being used
        [two-qubit-ops num-logical-qubits num-physical-qubits distance-matrix]
        (if (map? arg1) ; First arg is circuit
          [(extract-two-qubit-operations arg1) (:num-qubits arg1) (count arg2) arg3]
          [arg1 (count (set (concat (map :control arg1) (map :target arg1)))) arg2 arg3])]

    (when (> num-logical-qubits num-physical-qubits)
      (throw (ex-info "Circuit requires more qubits than available in topology"
                      {:logical-qubits num-logical-qubits
                       :physical-qubits num-physical-qubits})))

    ;; Simple greedy approach: try different starting offsets
    (if (<= num-logical-qubits 8) ; Manageable for small circuits
      (let [logical-qubits (range num-logical-qubits)
            all-mappings (for [offset (range num-physical-qubits)]
                           (zipmap logical-qubits
                                   (map #(mod (+ % offset) num-physical-qubits)
                                        logical-qubits)))
            best-mapping (first (sort-by #(calculate-mapping-cost two-qubit-ops % distance-matrix)
                                         all-mappings))]
        best-mapping)
      ;; For larger circuits, use identity mapping as placeholder
      (zipmap (range num-logical-qubits) (range num-logical-qubits)))))

(defn optimize-for-topology
  "Optimize a quantum circuit for a specific hardware topology.
  
  This function performs topology-aware optimization by:
  1. Finding an optimal mapping from logical to physical qubits
  2. Inserting SWAP operations when needed for routing
  3. Minimizing the total cost of the circuit on the given topology
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - topology: Hardware topology as vector of vectors (adjacency list)
  - options: Optional map with optimization options:
      :insert-swaps? - Whether to insert SWAP operations for routing (default: true)
      :optimize-mapping? - Whether to optimize qubit mapping (default: true)
  
  Returns:
  Map containing:
  - :quantum-circuit - The topology-optimized circuit
  - :logical-to-physical - Map from logical qubit to physical qubit
  - :physical-to-logical - Map from physical qubit to logical qubit  
  - :swap-count - Number of SWAP operations inserted
  - :total-cost - Total routing cost of the optimized circuit
  - :topology-summary - Human-readable summary of topology optimization
  
  Example:
  ;; Linear topology for 5 qubits: 0-1-2-3-4
  (def linear-topology [[1] [0 2] [1 3] [2 4] [3]])
  (optimize-for-topology my-circuit linear-topology)
  ;=> {:quantum-circuit <optimized-circuit>, :logical-to-physical {0 1, 1 2, 2 3}, ...}"
  ([circuit topology]
   (optimize-for-topology circuit topology {}))

  ([circuit topology options]
   {:pre [(s/valid? ::qc/quantum-circuit circuit)
          (validate-topology topology)]}

   (let [optimize-mapping? (get options :optimize-mapping? true)

         ;; Calculate distance matrix for topology
         distance-matrix (calculate-distance-matrix topology)

         ;; Find optimal qubit mapping
         logical-to-physical (if optimize-mapping?
                               (find-optimal-mapping circuit topology distance-matrix)
                               ;; Use identity mapping if optimization disabled
                               (zipmap (range (:num-qubits circuit))
                                       (range (:num-qubits circuit))))

         ;; Create reverse mapping
         physical-to-logical (into {} (map (fn [[l p]] [p l]) logical-to-physical))

         ;; Apply the mapping to the circuit operations
         mapped-operations (mapv #(cc/update-operation-params %
                                                           (fn [qubit-id]
                                                             (get logical-to-physical qubit-id qubit-id)))
                                 (:operations circuit))

         ;; Insert SWAP operations for non-adjacent two-qubit operations
         insert-swaps? (get options :insert-swaps? true)
         [final-operations swap-count] (if insert-swaps?
                                         (let [result-operations (atom [])
                                               total-swaps (atom 0)
                                               ;; Track current physical positions of logical qubits
                                               current-mapping (atom logical-to-physical)]
                                           ;; Process each operation in the circuit in order
                                           (doseq [op (:operations circuit)]
                                             (let [params (:operation-params op)]
                                               (if (and (:control params) (:target params)) ; Two-qubit operation
                                                 (let [logical-control (:control params)
                                                       logical-target (:target params)
                                                       physical-control (get @current-mapping logical-control)
                                                       physical-target (get @current-mapping logical-target)
                                                       distance (get-in distance-matrix [physical-control physical-target])]
                                                   (if (> distance 1) ; Not adjacent, need SWAPs
                                                     ;; Find where to move the control qubit to be adjacent to target
                                                     (let [path (find-shortest-path topology physical-control physical-target)
                                                           ;; Move control to the position adjacent to target (second-to-last in path)
                                                           adjacent-position (nth path (- (count path) 2))
                                                           ;; Generate SWAPs to move from current control position to adjacent position
                                                           swap-path (find-shortest-path topology physical-control adjacent-position)
                                                           swap-ops (if (> (count swap-path) 2)
                                                                      (generate-swap-operations swap-path logical-control)
                                                                      ;; For adjacent moves, manually create the SWAP
                                                                      (if (= (count swap-path) 2)
                                                                        [{:operation-type :swap
                                                                          :operation-params {:target1 (first swap-path)
                                                                                             :target2 (second swap-path)}
                                                                          :routing-info {:moves-qubit logical-control
                                                                                         :from (first swap-path)
                                                                                         :to (second swap-path)}}]
                                                                        []))]
                                                       ;; Add SWAP operations FIRST
                                                       (swap! total-swaps + (count swap-ops))
                                                       (swap! result-operations into swap-ops)
                                                       ;; After SWAPs, control is at adjacent position
                                                       (let [final-op {:operation-type (:operation-type op)
                                                                       :operation-params {:control adjacent-position
                                                                                          :target physical-target}}]
                                                         (swap! result-operations conj final-op)))
                                                     ;; Adjacent qubits, no SWAPs needed
                                                     (let [mapped-op {:operation-type (:operation-type op)
                                                                      :operation-params {:control physical-control
                                                                                         :target physical-target}}]
                                                       (swap! result-operations conj mapped-op))))
                                                 ;; Single-qubit operation - map directly
                                                 (let [mapped-op (cc/update-operation-params op
                                                                                          (fn [qubit-id]
                                                                                            (get @current-mapping qubit-id qubit-id)))]
                                                   (swap! result-operations conj mapped-op)))))
                                           [@result-operations @total-swaps])
                                         [mapped-operations 0])

         ;; Calculate total cost using the original logical operations
         original-two-qubit-ops (extract-two-qubit-operations circuit)
         total-cost (calculate-mapping-cost original-two-qubit-ops
                                            logical-to-physical
                                            distance-matrix)

         ;; Create optimized circuit
         optimized-circuit (assoc circuit :operations final-operations)

         ;; Generate summary
         topology-summary (str "Topology optimization summary:\n"
                               "- Hardware qubits: " (count topology) "\n"
                               "- Logical qubits: " (:num-qubits circuit) "\n"
                               "- Qubit mapping: " logical-to-physical "\n"
                               "- SWAP operations added: " swap-count "\n"
                               "- Total routing cost: " total-cost)]

     {:quantum-circuit optimized-circuit
      :logical-to-physical logical-to-physical
      :physical-to-logical physical-to-logical
      :swap-count swap-count
      :total-cost total-cost
      :topology-summary topology-summary})))

;; Topology Analysis and Utility Functions

(defn analyze-topology-connectivity
  "Analyze the connectivity properties of a hardware topology.
  
  Parameters:
  - topology: Hardware topology as vector of vectors
  
  Returns:
  Map containing topology analysis:
  - :num-qubits - Total number of qubits
  - :total-edges - Total number of edges (connections)
  - :avg-degree - Average degree (connections per qubit)
  - :max-degree - Maximum degree
  - :min-degree - Minimum degree
  - :diameter - Maximum shortest path distance between any two qubits
  - :is-connected - Whether the topology is fully connected"
  [topology]
  (let [num-qubits (count topology)]
    (if (> num-qubits 1)
     (let [degrees (mapv count topology)
           degrees-count (count degrees)
           total-edges (/ (reduce + degrees) 2) ; Each edge counted twice
           avg-degree (/ (reduce + degrees) (double num-qubits)) ; Total degree sum divided by qubits
           max-degree (if (> degrees-count 1) (apply max degrees) 0)
           min-degree (if (> degrees-count 1) (apply min degrees) 0)

           ;; Calculate diameter using distance matrix
           distance-matrix (calculate-distance-matrix topology)
           all-distances (for [i (range num-qubits)
                               j (range num-qubits)
                               :when (not= i j)]
                           (get-in distance-matrix [i j]))
           diameter (if (some #(= % Integer/MAX_VALUE) all-distances)
                      Integer/MAX_VALUE ; Disconnected
                      (apply max all-distances))
           is-connected (not= diameter Integer/MAX_VALUE)]

       {:num-qubits num-qubits
        :total-edges total-edges
        :avg-degree avg-degree
        :max-degree max-degree
        :min-degree min-degree
        :diameter diameter
        :is-connected is-connected})
     {:num-qubits num-qubits
      :total-edges 0
      :avg-degree 0
      :max-degree 0
      :min-degree 0
      :diameter 0
      :is-connected (= num-qubits 1)})))

(defn get-topology-info
  "Get human-readable information about a topology.
  
  Parameters:
  - topology: Hardware topology
  - name: Optional name for the topology
  
  Returns:
  String with topology information"
  ([topology]
   (get-topology-info topology "Topology"))
  ([topology name]
   (let [analysis (analyze-topology-connectivity topology)]
     (str name " Analysis:\n"
          "- Qubits: " (:num-qubits analysis) "\n"
          "- Edges: " (:total-edges analysis) "\n"
          "- Average degree: " (format "%.2f" (:avg-degree analysis)) "\n"
          "- Degree range: " (:min-degree analysis) "-" (:max-degree analysis) "\n"
          "- Diameter: " (if (= (:diameter analysis) Integer/MAX_VALUE)
                           "∞ (disconnected)"
                           (:diameter analysis)) "\n"
          "- Connected: " (:is-connected analysis)))))

(defn compare-topologies
  "Compare multiple hardware topologies for a given circuit.
  
  Parameters:
  - circuit: Quantum circuit to optimize
  - topologies: Map of topology-name to topology
  
  Returns:
  Vector of maps sorted by total cost, each containing:
  - :topology-name - Name of the topology
  - :total-cost - Total routing cost
  - :swap-count - Number of SWAP operations needed
  - :logical-to-physical - Optimal qubit mapping"
  [circuit topologies]
  (->> topologies
       (map (fn [[name topology]]
              (let [result (optimize-for-topology circuit topology)]
                {:topology-name name
                 :total-cost (:total-cost result)
                 :swap-count (:swap-count result)
                 :logical-to-physical (:logical-to-physical result)})))
       (sort-by :total-cost)
       vec))

(defn topology-aware-transform
  "Transform circuit for topology while being aware of supported gates."
  [circuit topology supported-operations options]

  ;; First, check what routing operations we can use
  (let [has-native-swap? (contains? supported-operations :swap)
        routing-strategy (if has-native-swap? :native-swap :cnot-swap)]

    (println (str "Routing strategy: " routing-strategy))

    ;; Apply topology optimization
    (let [topo-result (optimize-for-topology circuit topology options)
          operations-with-swaps (:operations (:quantum-circuit topo-result))]

      ;; Decompose any SWAPs if they're not native
      (if has-native-swap?
        topo-result  ; No decomposition needed
        ;; Decompose all SWAP operations
        (let [decomposed-ops (mapcat (fn [op]
                                       (if (= (:operation-type op) :swap)
                                         (gd/decompose-swap-if-needed op supported-operations)
                                         [op]))
                                     operations-with-swaps)
              updated-circuit (assoc (:quantum-circuit topo-result)
                                     :operations (vec decomposed-ops))]
          (assoc topo-result :quantum-circuit updated-circuit))))))

;; TODO add more tests for all functions
(defn optimize
  "Correct optimization pipeline that handles gate decomposition properly.
    
    The correct order is:
    1. Qubit optimization (minimize qubits before topology constraints)
    2. Topology optimization (with decomposition-aware routing)  
    3. Final gate decomposition (handle any remaining virtual gates)
    4. Validation and cleanup
    
    Parameters:
    - circuit: Quantum circuit to optimize
    - supported-operations: Set of natively supported operations
    - topology: Hardware topology (optional)
    - options: Optimization options
    
    Returns:
    Complete optimization result with corrected pipeline"
  [circuit supported-operations & [topology options]]

  (let [opts (or options {})
        optimize-qubits? (get opts :optimize-qubits? true)
        optimize-topology? (and topology (get opts :optimize-topology? false))
        transform-operations? (get opts :transform-operations? true)

        ;; STEP 1: Qubit optimization FIRST (before topology constraints)
        qubit-result (if optimize-qubits?
                       (qo/optimize-qubit-usage circuit)
                       {:quantum-circuit circuit
                        :qubits-saved 0
                        :original-qubits (:num-qubits circuit)
                        :optimized-qubits (:num-qubits circuit)})

        step1-circuit (:quantum-circuit qubit-result)

        ;; STEP 2: Topology optimization with decomposition-aware routing
        topo-result (if optimize-topology?
                      (topology-aware-transform step1-circuit topology supported-operations opts)
                      {:quantum-circuit step1-circuit
                       :swap-count 0
                       :total-cost 0})

        step2-circuit (:quantum-circuit topo-result)

        ;; STEP 3: Final gate decomposition (including any remaining virtual gates)
        final-transform-result (if transform-operations?
                                 (transform-circuit step2-circuit supported-operations opts)
                                 {:quantum-circuit step2-circuit
                                  :transformed-operation-count 0
                                  :unsupported-operations []})

        final-circuit (:quantum-circuit final-transform-result)

        ;; STEP 4: Final validation
        final-gate-types (map :operation-type (:operations final-circuit))
        unsupported-final (remove #(contains? supported-operations %) final-gate-types)
        all-supported? (empty? unsupported-final)]
    
    ;; Return comprehensive result
    {:quantum-circuit final-circuit
     :pipeline-order [:qubit-optimization :topology-optimization :gate-decomposition :validation]
     :qubit-optimization-result qubit-result
     :topology-optimization-result topo-result
     :gate-decomposition-result final-transform-result
     :all-gates-supported? all-supported?
     :final-unsupported-gates (vec (distinct unsupported-final))
     :optimization-summary (str "Circuit optimization:\n"
                                "- Original qubits: " (:num-qubits circuit) "\n"
                                "- Final qubits: " (:num-qubits final-circuit) "\n"
                                "- Original operations: " (count (:operations circuit)) "\n"
                                "- Final operations: " (count (:operations final-circuit)) "\n"
                                "- All gates supported: " all-supported?)}))

(comment

  ;; Let me test this problem with a concrete example
  (def test-circuit
    {:num-qubits 3
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :t :operation-params {:target 0}}  ; Virtual gate
                  {:operation-type :cnot :operation-params {:control 0 :target 2}}]}) ; Non-adjacent

  (s/valid? ::qc/quantum-circuit test-circuit)

  (def supported-gates #{:h :x :z :rz :cnot})  ; SWAP is NOT supported
  (def linear-topology [[1] [0 2] [1]])        ; Linear: 0-1-2

  ;; Current pipeline would:
  ;; 1. Decompose T → RZ (good)
  ;; 2. Add SWAP for 0→2 connection (bad - SWAP not in supported gates!)
  ;; 3. Result has unsupported SWAP operations

  ;; Test the topology-aware transformation
  (topology-aware-transform test-circuit linear-topology #{:h :x :z :rz :cnot} {})

  ;; Test the corrected pipeline
  (optimize test-circuit #{:h :x :z :rz :cnot} linear-topology {:optimize-topology? true})

  ;
  )

(comment
  ;; Example usage of topology optimization

  ;; Create a test circuit
  (def bell-circuit
    {:num-qubits 2
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :cnot :operation-params {:control 0 :target 1}}]})

  ;; Test with different topologies
  (def linear-2 (create-linear-topology 2))
  (def result (optimize-for-topology bell-circuit linear-2))
  (println (:topology-summary result))

  ;; Compare multiple topologies
  (def topologies {"Linear-5" (create-linear-topology 5)
                   "Ring-5" (create-ring-topology 5)
                   "Star-5" (create-star-topology 5)})

  (doseq [[name topo] topologies]
    (println (get-topology-info topo name)))

  ;; Analyze a complex circuit
  (def complex-circuit
    {:num-qubits 4
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :cnot :operation-params {:control 0 :target 1}}
                  {:operation-type :cnot :operation-params {:control 1 :target 2}}
                  {:operation-type :cnot :operation-params {:control 2 :target 3}}
                  {:operation-type :cnot :operation-params {:control 0 :target 3}}]})

  (def comparison (compare-topologies complex-circuit topologies))
  (doseq [[name result] comparison]
    (println (str name ": cost=" (:total-cost result))))

  ;; Test IBM-like topologies
  (def ibm-5q-linear [[1] [0 2] [1 3] [2 4] [3]])
  (def ibm-5q-t [[1] [0 2 3] [1 4] [1] [2]])

  (println "IBM 5-qubit linear:" (get-topology-info ibm-5q-linear "IBM Linear"))
  (println "IBM 5-qubit T:" (get-topology-info ibm-5q-t "IBM T-shape"))
  
  ;
  )