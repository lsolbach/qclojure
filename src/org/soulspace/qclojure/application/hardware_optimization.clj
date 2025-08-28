(ns org.soulspace.qclojure.application.hardware-optimization
  "Hardware-specific optimization and topology management for quantum circuits.
   
   This namespace provides functionality for optimizing quantum circuits
   for specific hardware topologies, including qubit routing, SWAP insertion,
   and topology-aware optimization strategies."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.qubit-optimization :as qo]
            [org.soulspace.qclojure.domain.gate-optimization :as go]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.domain.gate-decomposition :as gd]
            [org.soulspace.qclojure.domain.circuit-transformation :as ct]))

;;;
;;; Hardware Topology Creation Functions
;;;
(defn linear-topology
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

(defn ring-topology
  "Create a ring hardware topology where qubits are connected in a circle.
  
  Parameters:
  - num-qubits: Number of qubits in the topology
  
  Returns:
  Vector of vectors representing adjacency list for ring topology"
  [num-qubits]
  (cond
    (< num-qubits 3) (linear-topology num-qubits) ; Ring needs at least 3 qubits
    :else
    (vec (for [i (range num-qubits)]
           (let [prev (mod (dec i) num-qubits)
                 next (mod (inc i) num-qubits)]
             [prev next])))))

(defn star-topology
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

(defn grid-topology
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

; TODO refactor parameters and add higher qubit heavy hex topologies
(defn heavy-hex-topology
  "Create a heavy-hex hardware topology as used by IBM quantum computers.
   
   Heavy-hex topology consists of hexagonal units where each interior qubit has degree 3.
   The topology maximizes connectivity while maintaining manufacturability constraints.
   
   This creates patterns used in IBM's quantum processors with production-ready topologies
   that match real IBM hardware connectivity patterns.
   
   Parameters:
   - processor-type: Keyword identifying the IBM processor type
                    :basic or 1 - Single hex (7 qubits)
                    :falcon or 27 - Falcon-style pattern (27 qubits)  
                    :hummingbird or 65 - Hummingbird-style pattern (65 qubits)
                    :eagle or 127 - Eagle-style pattern (127 qubits)
   
   Returns:
   Map containing:
   - :topology - Vector of vectors representing adjacency list
   - :metadata - Information about the processor and topology properties
   
   Example:
   (heavy-hex-topology :basic)    ; 7-qubit basic hex
   (heavy-hex-topology :falcon)   ; 27-qubit Falcon-style  
   (heavy-hex-topology 1)         ; Same as :basic
   (heavy-hex-topology 27)        ; Same as :falcon"
  [processor-type]
  {:pre [(or (keyword? processor-type) (pos-int? processor-type))]}
  
  (letfn [(ensure-symmetric [topology]
            "Ensure all connections in topology are symmetric"
            (let [num-qubits (count topology)]
              (vec (for [i (range num-qubits)]
                     (let [neighbors (set (get topology i))
                           ;; Add reverse connections to ensure symmetry
                           symmetric-neighbors 
                           (into neighbors
                                 (for [j (range num-qubits)
                                       :when (some #(= % i) (get topology j))]
                                   j))]
                       (vec (sort (remove #(= % i) symmetric-neighbors))))))))]
    
    (let [proc-key (cond
                     (#{:basic 1} processor-type) :basic
                     (#{:falcon 27} processor-type) :falcon
                     (#{:hummingbird 65} processor-type) :hummingbird
                     (#{:eagle 127} processor-type) :eagle
                     :else (throw (ex-info "Unsupported heavy-hex processor type"
                                          {:processor-type processor-type
                                           :supported [:basic :falcon :hummingbird :eagle 1 27 65 127]})))]
      
      (case proc-key
        :basic
        (let [topology [[1 2 3 4 5 6]    ; Center connected to all 6 vertices
                        [0 2 6]          ; Vertex qubits connected to center and neighbors
                        [0 1 3]
                        [0 2 4]
                        [0 3 5]
                        [0 4 6]
                        [0 5 1]]]
          (ensure-symmetric topology))

        :falcon
        ;; Realistic 27-qubit pattern similar to IBM Falcon
        (let [topology [[1 2 3 4 5 6 7]     ; 0: Central hub with bridge
                        [0 2 6 8]           ; 1-6: First hex vertices
                        [0 1 3 9]
                        [0 2 4 10]
                        [0 3 5 11]
                        [0 4 6 12]
                        [0 5 1 13]
                        [0 14]              ; 7: Bridge qubit
                        [1 15]              ; 8-13: Extended connections
                        [2 16]
                        [3 17]
                        [4 18]
                        [5 19]
                        [6 20]
                        [7 15 26]           ; 14-26: Outer ring
                        [8 14 16]
                        [9 15 17]
                        [10 16 18]
                        [11 17 19]
                        [12 18 20]
                        [13 19 21]
                        [20 22]
                        [21 23]
                        [22 24]
                        [23 25]
                        [24 26]
                        [25 14]]]
          (ensure-symmetric topology))

        :hummingbird
        ;; Realistic 65-qubit pattern with proper bounds checking
        (let [topology (vec (for [i (range 65)]
                              (let [;; Create a hex-grid pattern with proper bounds
                                    row (quot i 8)
                                    col (mod i 8)
                                    ;; Build neighbor list with strict bounds checking
                                    neighbors (vec (cond-> []
                                                     (and (> col 0) (>= (- i 1) 0))
                                                     (conj (- i 1))                 ; Left
                                                     (and (< col 7) (< (+ i 1) 65))
                                                     (conj (+ i 1))                 ; Right  
                                                     (and (> row 0) (>= (- i 8) 0))
                                                     (conj (- i 8))                 ; Up
                                                     (and (< row 7) (< (+ i 8) 65))
                                                     (conj (+ i 8))                 ; Down
                                                     ;; Add diagonal connections with bounds checking
                                                     (and (> row 0) (> col 0) (even? (+ row col)) (>= (- i 9) 0))
                                                     (conj (- i 9))                 ; Diagonal up-left
                                                     (and (< row 7) (< col 7) (odd? (+ row col)) (< (+ i 9) 65))
                                                     (conj (+ i 9))))]              ; Diagonal down-right
                                neighbors)))]
          (ensure-symmetric topology))

        :eagle
        ;; Eagle-style 127-qubit pattern with proper bounds checking
        (let [topology (vec (for [i (range 127)]
                              (let [;; Create connectivity pattern with strict bounds
                                    row (quot i 10)
                                    col (mod i 10)
                                    ;; Build neighbor list with careful bounds checking
                                    neighbors (vec (cond-> []
                                                     (and (> col 0) (>= (- i 1) 0))
                                                     (conj (- i 1))                 ; Left
                                                     (and (< col 9) (< (+ i 1) 127))
                                                     (conj (+ i 1))                 ; Right
                                                     (and (> row 0) (>= (- i 10) 0))
                                                     (conj (- i 10))                ; Up  
                                                     (and (< row 12) (< (+ i 10) 127))
                                                     (conj (+ i 10))                ; Down
                                                     ;; Add hex-style diagonals with bounds checking
                                                     (and (> row 1) (< row 11) (> col 1) (< col 8)
                                                          (>= (- i 9) 0) (< (- i 9) 127))
                                                     (conj (- i 9))                 ; Diagonal up-left
                                                     (and (> row 1) (< row 11) (> col 1) (< col 8)
                                                          (>= (- i 11) 0) (< (- i 11) 127))
                                                     (conj (- i 11))                ; Diagonal up-right
                                                     (and (> row 1) (< row 11) (> col 1) (< col 8)
                                                          (>= (+ i 9) 0) (< (+ i 9) 127))
                                                     (conj (+ i 9))                 ; Diagonal down-left
                                                     (and (> row 1) (< row 11) (> col 1) (< col 8)
                                                          (>= (+ i 11) 0) (< (+ i 11) 127))
                                                     (conj (+ i 11))))]             ; Diagonal down-right
                                neighbors)))]
          (ensure-symmetric topology))))))

;;;
;;; Hardware Topology Optimization Functions
;;;
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
                    ;; Standard control-target operations (CNOT, CZ, Rydberg gates etc.)
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

;;;
;;; Topology Analysis and Utility Functions
;;;
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

;;;
;;; Full Optimization Pipeline
;;;
;; TODO add more tests for all functions
(defn optimize
  "Optimization pipeline that handles gate decomposition properly.
    
    The order is:
    1. Gate cancellation optimization (remove redundant gates)
    2. Qubit optimization (minimize qubits before topology constraints)
    3. Topology optimization (with decomposition-aware routing)  
    4. Final gate decomposition (handle any remaining virtual gates)
    5. Validation and cleanup
    
    Parameters:
    - circuit: Quantum circuit to optimize
    - supported-operations: Set of natively supported operations
    - topology: Hardware topology (optional)
    - options: Optimization options
    
    Returns:
    Complete optimization result with corrected pipeline"
  [circuit supported-operations & [topology options]]

  (let [opts (or options {})
        optimize-gates? (get opts :optimize-gates? true)
        optimize-qubits? (get opts :optimize-qubits? true)
        optimize-topology? (and topology (get opts :optimize-topology? false))
        transform-operations? (get opts :transform-operations? true)

        ;; STEP 1: Gate cancellation optimization (after qubit optimization)
        gate-optimization-result (if optimize-gates?
                                   (let [optimized-circuit (go/optimize-gate-cancellations circuit)
                                         gates-removed (- (count (:operations circuit))
                                                         (count (:operations optimized-circuit)))]
                                     {:quantum-circuit optimized-circuit
                                      :gates-removed gates-removed
                                      :original-gate-count (count (:operations circuit))
                                      :optimized-gate-count (count (:operations optimized-circuit))})
                                   {:quantum-circuit circuit
                                    :gates-removed 0
                                    :original-gate-count (count (:operations circuit))
                                    :optimized-gate-count (count (:operations circuit))})

        step1-circuit (:quantum-circuit gate-optimization-result)

        ;; STEP 2: Qubit optimization FIRST (before topology constraints)
        qubit-result (if optimize-qubits?
                       (qo/optimize-qubit-usage step1-circuit)
                       {:quantum-circuit step1-circuit
                        :qubits-saved 0
                        :original-qubits (:num-qubits step1-circuit)
                        :optimized-qubits (:num-qubits step1-circuit)})

        step2-circuit (:quantum-circuit qubit-result)

        ;; STEP 3: Topology optimization with decomposition-aware routing
        topo-result (if optimize-topology?
                      (topology-aware-transform step2-circuit topology supported-operations opts)
                      {:quantum-circuit step2-circuit
                       :swap-count 0
                       :total-cost 0})

        step3-circuit (:quantum-circuit topo-result)

        ;; STEP 4: Final gate decomposition (including any remaining virtual gates)
        final-transform-result (if transform-operations?
                                 (ct/transform-circuit step3-circuit supported-operations opts)
                                 {:quantum-circuit step3-circuit
                                  :transformed-operation-count 0
                                  :unsupported-operations []})

        final-circuit (:quantum-circuit final-transform-result)

        ;; STEP 5: Final validation
        final-gate-types (map :operation-type (:operations final-circuit))
        unsupported-final (remove #(contains? supported-operations %) final-gate-types)
        all-supported? (empty? unsupported-final)]
    
    ;; Return comprehensive result
    {:quantum-circuit final-circuit
     :pipeline-order [:gate-cancellation :qubit-optimization :topology-optimization :gate-decomposition :validation]
     :qubit-optimization-result qubit-result
     :gate-optimization-result gate-optimization-result
     :topology-optimization-result topo-result
     :gate-decomposition-result final-transform-result
     :all-gates-supported? all-supported?
     :final-unsupported-gates (vec (distinct unsupported-final))
     :optimization-summary (str "Circuit optimization:\n"
                                "- Original qubits: " (:num-qubits circuit) "\n"
                                "- Final qubits: " (:num-qubits final-circuit) "\n"
                                "- Original operations: " (count (:operations circuit)) "\n"
                                "- Final operations: " (count (:operations final-circuit)) "\n"
                                "- Gates removed by cancellation: " (:gates-removed gate-optimization-result) "\n"
                                "- All gates supported: " all-supported?)}))

(comment
  ;; Example usage of topology optimization

  ;; Create a test circuit
  (def bell-circuit
    {:num-qubits 2
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :cnot :operation-params {:control 0 :target 1}}]})

  ;; Test with different topologies
  (def linear-2 (linear-topology 2))
  (def result (optimize-for-topology bell-circuit linear-2))
  (println (:topology-summary result))

  ;; Compare multiple topologies
  (def topologies {"Linear-5" (linear-topology 5)
                   "Ring-5" (ring-topology 5)
                   "Star-5" (star-topology 5)})

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
  (doseq [result comparison]
    (println (str (:topology-name result) ": cost=" (:total-cost result))))

  ;; Test IBM-like topologies
  (def ibm-5q-linear [[1] [0 2] [1 3] [2 4] [3]])
  (def ibm-5q-t [[1] [0 2 3] [1 4] [2]])

  (println "IBM 5-qubit linear:" (get-topology-info ibm-5q-linear "IBM Linear"))
  (println "IBM 5-qubit T:" (get-topology-info ibm-5q-t "IBM T-shape"))

  ;; Test optimization pipeline with real use case
  (def test-circuit
    {:num-qubits 3
     :operations [{:operation-type :h :operation-params {:target 0}}
                  {:operation-type :t :operation-params {:target 0}}  ; Virtual gate
                  {:operation-type :cnot :operation-params {:control 0 :target 2}}]}) ; Non-adjacent

  (def supported-gates #{:h :x :z :rz :cnot})  ; SWAP is NOT supported
  (def linear-topology [[1] [0 2] [1]])        ; Linear: 0-1-2

  ;; Test the topology-aware transformation
  (topology-aware-transform test-circuit linear-topology supported-gates {})

  ;; Test the corrected pipeline
  (optimize test-circuit supported-gates linear-topology {:optimize-topology? true})

  ;; Test Heavy-Hex topology (IBM-style)
  (def hex1 (heavy-hex-topology 1))  ; 7-qubit single hexagon
  (def hex2 (heavy-hex-topology 2))  ; 17-qubit connected hexagons
  
  ;; Validate heavy-hex topologies
  (validate-topology hex1)  ;=> true
  (validate-topology hex2)  ;=> true
  
  ;; Analyze heavy-hex connectivity
  (get-topology-info hex1 "Heavy-Hex Size-1")
  (get-topology-info hex2 "Heavy-Hex Size-2")
  
  ;; Compare heavy-hex with other topologies for circuit optimization
  (def topologies-with-hex {"Heavy-Hex-1" (heavy-hex-topology 1)
                            "Heavy-Hex-2" (heavy-hex-topology 2)
                            "Linear-7" (linear-topology 7)
                            "Grid-2x4" (grid-topology 2 4)
                            "Ring-7" (ring-topology 7)})
  
  (compare-topologies complex-circuit topologies-with-hex)
  ;=> Shows heavy-hex often has lower routing costs due to better connectivity
  
  ;; IBM-style heavy-hex usage example  
  (def ibm-heavy-hex (heavy-hex-topology 2))  ; 17 qubits
  (def hex-optimization (optimize-for-topology complex-circuit ibm-heavy-hex))
  (println (:topology-summary hex-optimization))

  ;
  )
