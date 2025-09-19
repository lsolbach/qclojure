(ns org.soulspace.qclojure.application.algorithm.qaoa
  "Quantum Approximate Optimization Algorithm (QAOA) Implementation
  
  QAOA is a quantum-classical hybrid algorithm for solving combinatorial optimization
  problems. It uses alternating application of a problem Hamiltonian and a mixer
  Hamiltonian to explore the solution space and find approximate solutions.
  
  Key Features:
  - Problem Hamiltonian construction for MaxCut, Max-SAT, and TSP
  - Mixer Hamiltonian (typically X-gates on all qubits)  
  - Alternating evolution structure: U(β,γ) = e^(-iβH_M)e^(-iγH_P)
  - Integration with classical optimization for parameter tuning
  - Performance analysis and approximation ratio calculation
  
  Algorithm Flow:
  1. Initialize equal superposition state |+⟩^n
  2. Apply p layers of alternating evolution U(β_i,γ_i)
  3. Measure expectation value of the problem Hamiltonian
  4. Use classical optimizer to update parameters (β,γ)
  5. Repeat until convergence or maximum iterations
  
  The algorithm targets NP-hard combinatorial optimization problems and provides
  quantum advantage for certain problem instances."
  (:require [clojure.spec.alpha :as s]
            [clojure.math.combinatorics :as combi]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.circuit-composition :as cc]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.hamiltonian :as ham]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.application.algorithm.variational-algorithm :as va]))

;;;
;;; Specs for QAOA components
;;;
(s/def ::qaoa-problem
  #{:max-cut :max-sat :tsp :custom})

(s/def ::graph
  (s/coll-of (s/tuple pos-int? pos-int? number?)))  ; [(i j weight), ...]

(s/def ::qaoa-config
  (s/keys :req-un [::num-qubits ::num-layers]
          :opt-un [::problem-hamiltonian ::mixer-hamiltonian ::initial-parameters ::max-iterations
                   ::tolerance ::optimization-method ::shots ::graph ::problem-type
                   ::problem-instance ::parameter-strategy]))

(s/def ::problem-type #{:max-cut :max-sat :tsp :custom})
(s/def ::num-layers pos-int?)
(s/def ::problem-instance any?)  ; Problem-specific data structure
(s/def ::parameter-strategy #{:theoretical :adiabatic :random-smart})

;;;
;;; QAOA-specific result specs (moved from domain.result)
;;;
(s/def ::clauses (s/coll-of (s/coll-of any? :min-count 1)))      ; Boolean clauses
(s/def ::distance-matrix (s/coll-of (s/coll-of number? :min-count 1) :min-count 1))
(s/def ::cut-edges (s/coll-of (s/tuple pos-int? pos-int? number?)))
(s/def ::cut-weight number?)
(s/def ::partition (s/map-of pos-int? #{0 1}))  ; {vertex-id -> partition}
(s/def ::boolean-assignment (s/map-of pos-int? boolean?))  ; {variable-id -> value}
(s/def ::satisfied-clauses pos-int?)
(s/def ::tour (s/coll-of pos-int? :kind vector? :min-count 1))
(s/def ::tour-distance number?)
(s/def ::solution-probability number?)
(s/def ::approximation-ratio number?)

(s/def ::max-cut-solution
  (s/keys :req-un [::graph ::problem-type] :opt-un [::shots]))
(s/def ::max-sat-solution
  (s/keys :req-un [::clauses ::problem-type] :opt-un [::shots]))
(s/def ::tsp-solution
  (s/keys :req-un [::distance-matrix ::problem-type] :opt-un [::shots]))
(s/def ::approximation-ratio-spec
  (s/keys :req-un [::problem-type] :opt-un [::classical-optimum]))
(s/def ::solution-distribution
  (s/keys :req-un [::problem-type] :opt-un [::top-solutions ::shots]))

(s/def ::classical-optimum number?)
(s/def ::top-solutions pos-int?)

;; QAOA-specific result types
(s/def ::max-cut-solution-result
  (s/keys :req-un [::cut-edges ::cut-weight ::partition ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::max-sat-solution-result
  (s/keys :req-un [::boolean-assignment ::satisfied-clauses ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::tsp-solution-result
  (s/keys :req-un [::tour ::tour-distance ::solution-probability]
          :opt-un [::approximation-ratio]))

(s/def ::approximation-ratio-result
  (s/keys :req-un [::approximation-ratio ::problem-type]
          :opt-un [::classical-optimum]))

(s/def ::solution-distribution-result
  (s/keys :req-un [::solutions ::total-count]
          :opt-un [::top-solutions]))

(s/def ::solutions (s/coll-of (s/keys :req-un [::solution-string ::probability ::decoded-solution])))
(s/def ::solution-string string?)
(s/def ::decoded-solution any?)  ; Problem-specific decoded solution
(s/def ::total-count pos-int?)

;;;
;;; Problem Hamiltonian Construction
;;;
(defn max-cut-hamiltonian
  "Create a MaxCut problem Hamiltonian for a given graph.
  
  The MaxCut problem aims to find a partition of vertices that maximizes
  the number of edges crossing the partition. The problem Hamiltonian is:
  
  H_P = Σ_{(i,j)∈E} w_{ij} * (1 - Z_i Z_j) / 2
  
  Where w_{ij} is the edge weight between vertices i and j.
  The ground state corresponds to the maximum cut.
  
  Parameters:
  - graph: Collection of edges as [vertex1 vertex2 weight] tuples
  - num-vertices: Number of vertices in the graph (determines number of qubits)
  
  Returns:
  Collection of Pauli terms representing the MaxCut Hamiltonian
  
  Example:
  (max-cut-hamiltonian [[0 1 1.0] [1 2 2.0] [0 2 1.5]] 3)
  ;=> Hamiltonian for triangle graph with weighted edges"
  [graph num-vertices]
  {:pre [(coll? graph) (pos-int? num-vertices)
         (every? #(and (= (count %) 3)
                       (< (first %) num-vertices)
                       (< (second %) num-vertices)
                       (number? (nth % 2))) graph)]}
  (mapcat (fn [[i j weight]]
            ;; Each edge contributes: weight * (1 - Z_i Z_j) / 2
            ;; = weight/2 * I - weight/2 * Z_i Z_j
            (let [coeff (/ weight 2.0)
                  identity-string (apply str (repeat num-vertices \I))
                  zz-string (-> (vec (repeat num-vertices \I))
                                (assoc i \Z)
                                (assoc j \Z)
                                (->> (apply str)))]
              [{:coefficient coeff :pauli-string identity-string}
               {:coefficient (- coeff) :pauli-string zz-string}]))
          graph))

(defn max-sat-hamiltonian
  "Create a Maximum Satisfiability (Max-SAT) Hamiltonian.
  
  Max-SAT aims to satisfy the maximum number of Boolean clauses.
  Each clause is a disjunction (OR) of literals, where a literal
  is either a variable xᵢ or its negation ¬xᵢ.
  
  The Hamiltonian assigns energy 0 to satisfied clauses and 1 to
  unsatisfied clauses, so minimizing energy maximizes satisfaction.
  
  Encoding: Each Boolean variable maps to one qubit where |0⟩ = false, |1⟩ = true.
  For a clause C = (l₁ ∨ l₂ ∨ ... ∨ lₖ), the penalty Hamiltonian is:
  H_C = ∏ᵢ (1 - lᵢ) where lᵢ = (1+Zᵢ)/2 for positive literal xᵢ
                              = (1-Zᵢ)/2 for negative literal ¬xᵢ
  
  Parameters:
  - clauses: Collection of clauses, where each clause is a collection
            of literals. For positive literals use positive integers,
            for negative literals use keywords like :not-0, :not-1, etc.
            or maps like {:variable 0 :negated true}
  - num-variables: Number of Boolean variables (determines number of qubits)
  
  Returns:
  Collection of Pauli terms representing the Max-SAT Hamiltonian
  
  Example:
  (max-sat-hamiltonian [[0 1] [:not-0 2] [1 :not-2]] 3)
  ;=> Hamiltonian for (x₀ ∨ x₁) ∧ (¬x₀ ∨ x₂) ∧ (x₁ ∨ ¬x₂)"
  [clauses num-variables]
  {:pre [(coll? clauses) (pos-int? num-variables)
         (every? #(and (coll? %) (seq %)) clauses)]}

  (letfn [(parse-literal [literal]
            "Parse a literal into [var-index negated?] pair"
            (cond
              ;; Positive integer literal
              (and (integer? literal) (>= literal 0))
              [literal false]

              ;; Keyword :not-X format
              (and (keyword? literal) (.startsWith (name literal) "not-"))
              (let [var-str (subs (name literal) 4)]
                [(Integer/parseInt var-str) true])

              ;; Map format {:variable X :negated boolean}
              (map? literal)
              [(:variable literal) (:negated literal false)]

              :else
              (throw (ex-info "Invalid literal format" {:literal literal}))))

          (clause-hamiltonian [clause]
            "Generate Hamiltonian terms for a single clause"
            (let [parsed-literals (map parse-literal clause)
                  num-literals (count parsed-literals)]

              (if (= num-literals 1)
                ;; Single literal clause: H = (1 - l) where l = (1±Z)/2
                (let [[var-idx negated?] (first parsed-literals)
                      pauli-i (apply str (repeat num-variables \I))
                      pauli-z (apply str (assoc (vec (repeat num-variables \I)) var-idx \Z))]
                  (if negated?
                    ;; For negative literal ¬xᵢ: H = (1 - (1-Zᵢ)/2) = (1+Zᵢ)/2
                    [{:coefficient 0.5 :pauli-string pauli-i}
                     {:coefficient 0.5 :pauli-string pauli-z}]
                    ;; For positive literal xᵢ: H = (1 - (1+Zᵢ)/2) = (1-Zᵢ)/2
                    [{:coefficient 0.5 :pauli-string pauli-i}
                     {:coefficient -0.5 :pauli-string pauli-z}]))

                ;; Multi-literal clause: Expand product ∏ᵢ (1 - lᵢ)
                ;; This creates 2^k terms where k is number of literals
                (let [all-subsets (combi/subsets (range num-literals))]
                  (for [subset all-subsets]
                    (let [coeff (if (even? (count subset)) 1.0 -1.0)
                          ;; Build Pauli string for this subset
                          pauli-chars (reduce
                                       (fn [chars idx]
                                         (let [[var-idx negated?] (nth parsed-literals idx)]
                                           (assoc chars var-idx
                                                  (if negated? \Z \Z))))  ; Both use Z, sign handled by coeff
                                       (vec (repeat num-variables \I))
                                       subset)
                          pauli-string (apply str pauli-chars)]
                      {:coefficient (* coeff (/ 1.0 (Math/pow 2 num-literals)))
                       :pauli-string pauli-string}))))))]

    (mapcat clause-hamiltonian clauses)))

(defn travelling-salesman-hamiltonian
  "Create a Travelling Salesman Problem (TSP) Hamiltonian.
  
  The TSP aims to find the shortest route visiting all cities exactly once.
  This uses the standard n² qubit encoding where qubit q_{i,j} represents
  whether city i is visited at time step j.
  
  The Hamiltonian has three components:
  1. Cost function: Σ_{i,j,k} d_{i,j} * x_{i,k} * x_{j,k+1}
  2. City constraints: Σ_i (Σ_j x_{i,j} - 1)²  (each city visited exactly once)
  3. Time constraints: Σ_j (Σ_i x_{i,j} - 1)²  (exactly one city per time step)
  
  The encoding maps qubit index q = i*n + j to city i at time j.
  
  Parameters:
  - distance-matrix: n×n matrix of distances between cities
  - penalty-weight: Weight for constraint penalty terms (default: auto-calculated)
  
  Returns:
  Collection of Pauli terms representing the TSP Hamiltonian"
  [distance-matrix & {:keys [penalty-weight]}]
  {:pre [(coll? distance-matrix)
         (let [n (count distance-matrix)]
           (and (pos? n)
                (every? #(= (count %) n) distance-matrix)
                (every? #(every? number? %) distance-matrix)))]}

  (let [n (count distance-matrix)
        num-qubits (* n n)
        max-distance (apply max (flatten distance-matrix))
        ;; Auto-calculate penalty weight to ensure constraints dominate
        A (or penalty-weight (* 2 max-distance n))

        ;; Helper function to get qubit index for city i at time j
        qubit-index (fn [city time] (+ (* city n) time))

        ;; Helper to create single Z Pauli string
        single-z (fn [qubit-idx]
                   (apply str (assoc (vec (repeat num-qubits \I)) qubit-idx \Z)))

        ;; Helper to create ZZ Pauli string
        double-z (fn [q1 q2]
                   (apply str (-> (vec (repeat num-qubits \I))
                                  (assoc q1 \Z)
                                  (assoc q2 \Z))))]

    (flatten
     (concat
      ;; 1. Cost function: minimize travel distances
      ;; H_cost = Σ_{i,j,t} d_{i,j} * (1 + Z_{i,t})/2 * (1 + Z_{j,t+1})/2
      ;; Expanding: d_{i,j}/4 * (1 + Z_{i,t} + Z_{j,t+1} + Z_{i,t}Z_{j,t+1})
      (for [i (range n)
            j (range n)
            t (range n)
            :when (not= i j)]  ; No self-loops
        (let [distance (nth (nth distance-matrix i) j)
              q1 (qubit-index i t)
              q2 (qubit-index j (mod (inc t) n))  ; Next time step (periodic)
              coeff (/ distance 4.0)]
          ;; Generate all four terms from the expansion
          [{:coefficient coeff :pauli-string (apply str (repeat num-qubits \I))}      ; constant
           {:coefficient coeff :pauli-string (single-z q1)}                          ; Z_i
           {:coefficient coeff :pauli-string (single-z q2)}                          ; Z_j  
           {:coefficient coeff :pauli-string (double-z q1 q2)}]))                     ; Z_i Z_j

      ;; 2. City constraints: each city visited exactly once
      ;; Σ_i (Σ_j x_{i,j} - 1)² = Σ_i (Σ_j (1+Z_{i,j})/2 - 1)²
      ;; = Σ_i (Σ_j Z_{i,j}/2 + (n-2)/2)²
      (for [i (range n)]
        (let [;; Expand (Σ_j Z_{i,j}/2 + (n-2)/2)²
              ;; = (Σ_j Z_{i,j})²/4 + (n-2)/2 * Σ_j Z_{i,j} + (n-2)²/4

              ;; Quadratic terms: Σ_{j₁,j₂} Z_{i,j₁} Z_{i,j₂} / 4
              quadratic-terms
              (for [j1 (range n)
                    j2 (range n)]
                (let [q1 (qubit-index i j1)
                      q2 (qubit-index i j2)
                      coeff (/ A 4.0)]
                  (if (= j1 j2)
                    {:coefficient coeff :pauli-string (single-z q1)}      ; Z²=I, so just Z term  
                    {:coefficient coeff :pauli-string (double-z q1 q2)})))  ; ZZ term

              ;; Linear terms: (n-2)/2 * Σ_j Z_{i,j}
              linear-terms
              (for [j (range n)]
                {:coefficient (* A (- n 2) 0.5)
                 :pauli-string (single-z (qubit-index i j))})

              ;; Constant term: (n-2)²/4
              constant-term
              {:coefficient (* A (/ (* (- n 2) (- n 2)) 4.0))
               :pauli-string (apply str (repeat num-qubits \I))}]

          (concat quadratic-terms linear-terms [constant-term])))

      ;; 3. Time constraints: exactly one city visited at each time
      ;; Similar structure to city constraints but summing over cities for each time
      (for [t (range n)]
        (let [;; Quadratic terms: Σ_{i₁,i₂} Z_{i₁,t} Z_{i₂,t} / 4
              quadratic-terms
              (for [i1 (range n)
                    i2 (range n)]
                (let [q1 (qubit-index i1 t)
                      q2 (qubit-index i2 t)
                      coeff (/ A 4.0)]
                  (if (= i1 i2)
                    {:coefficient coeff :pauli-string (single-z q1)}      ; Z²=I term
                    {:coefficient coeff :pauli-string (double-z q1 q2)})))  ; ZZ term

              ;; Linear terms: (n-2)/2 * Σ_i Z_{i,t}
              linear-terms
              (for [i (range n)]
                {:coefficient (* A (- n 2) 0.5)
                 :pauli-string (single-z (qubit-index i t))})

              ;; Constant term: (n-2)²/4
              constant-term
              {:coefficient (* A (/ (* (- n 2) (- n 2)) 4.0))
               :pauli-string (apply str (repeat num-qubits \I))}]

          (concat quadratic-terms linear-terms [constant-term])))))))

;; Add TSP solution decoder
(defn custom-ising-hamiltonian
  "Create a custom Ising model Hamiltonian.
  
  General Ising model: H = Σᵢ hᵢZᵢ + Σᵢⱼ Jᵢⱼ ZᵢZⱼ
  
  Parameters:
  - h-fields: Vector of local magnetic fields [h₀ h₁ ... hₙ₋₁]
  - j-couplings: Collection of coupling terms [[i j Jᵢⱼ], ...]
  
  Returns:
  Collection of Pauli terms representing the Ising Hamiltonian"
  [h-fields j-couplings]
  {:pre [(vector? h-fields) (coll? j-couplings)]}
  (let [n (count h-fields)]
    (concat
     ;; Local field terms: hᵢZᵢ
     (map-indexed (fn [i h]
                    (let [z-string (-> (vec (repeat n \I))
                                       (assoc i \Z)
                                       (->> (apply str)))]
                      {:coefficient h :pauli-string z-string}))
                  h-fields)
     ;; Coupling terms: JᵢⱼZᵢZⱼ
     (map (fn [[i j coupling]]
            (let [zz-string (-> (vec (repeat n \I))
                                (assoc i \Z)
                                (assoc j \Z)
                                (->> (apply str)))]
              {:coefficient coupling :pauli-string zz-string}))
          j-couplings))))

;;;
;;; Mixer Hamiltonian Construction
;;;
(defn standard-mixer-hamiltonian
  "Create the standard QAOA mixer Hamiltonian.
  
  The mixer Hamiltonian is typically H_M = Σᵢ Xᵢ, which creates
  transitions between computational basis states and allows exploration
  of the solution space.
  
  Parameters:
  - num-qubits: Number of qubits in the system
  
  Returns:
  Collection of Pauli terms representing the mixer Hamiltonian"
  [num-qubits]
  {:pre [(pos-int? num-qubits)]}
  (map (fn [i]
         (let [x-string (-> (vec (repeat num-qubits \I))
                            (assoc i \X)
                            (->> (apply str)))]
           {:coefficient 1.0 :pauli-string x-string}))
       (range num-qubits)))

(defn xy-mixer-hamiltonian
  "Create an XY mixer Hamiltonian for problems with particle number conservation.
  
  The XY mixer preserves the number of |1⟩ states and is useful for problems
  where the constraint is to select exactly k items out of n.
  
  H_M = Σᵢ (Xᵢ₊₁Xᵢ + Yᵢ₊₁Yᵢ) = Σᵢ (σᵢ₊ σᵢ₋ + σᵢ₋ σᵢ₊₁)
  
  Parameters:
  - num-qubits: Number of qubits in the system
  - periodic: Whether to include periodic boundary conditions
  
  Returns:
  Collection of Pauli terms representing the XY mixer Hamiltonian"
  [num-qubits periodic]
  {:pre [(pos-int? num-qubits)]}
  (let [pairs (if periodic
                (map vector (range num-qubits) (map #(mod % num-qubits) (range 1 (inc num-qubits))))
                (map vector (range (dec num-qubits)) (range 1 num-qubits)))]
    (mapcat (fn [[i j]]
              (let [xx-string (-> (vec (repeat num-qubits \I))
                                  (assoc i \X)
                                  (assoc j \X)
                                  (->> (apply str)))
                    yy-string (-> (vec (repeat num-qubits \I))
                                  (assoc i \Y)
                                  (assoc j \Y)
                                  (->> (apply str)))]
                [{:coefficient 1.0 :pauli-string xx-string}
                 {:coefficient 1.0 :pauli-string yy-string}]))
            pairs)))

;;;
;;; QAOA Circuit Construction
;;;
(defn hamiltonian-evolution-circuit
  "Create a quantum circuit for time evolution under a Hamiltonian.
  
  For a Hamiltonian H and evolution time t, this creates the circuit
  implementing e^(-iHt). For Pauli string Hamiltonians, this decomposes
  into products of rotations and CNOT gates.
  
  Parameters:
  - hamiltonian: Collection of Pauli terms
  - evolution-time: Evolution time parameter
  - num-qubits: Number of qubits
  
  Returns:
  Quantum circuit implementing the evolution"
  [hamiltonian evolution-time num-qubits]
  {:pre [(ham/validate-hamiltonian hamiltonian) (number? evolution-time) (pos-int? num-qubits)]}
  (let [circuit (qc/create-circuit num-qubits "Hamiltonian Evo")]
    (reduce (fn [circ term]
              (let [coeff (:coefficient term)
                    pauli-str (:pauli-string term)
                    angle (* 2.0 evolution-time coeff)]  ; Factor of 2 for rotation angle
                (cond
                  ;; Identity terms contribute only global phase (can be ignored)
                  (every? #(= % \I) pauli-str)
                  circ

                  ;; Single Pauli terms: direct rotation
                  (= 1 (count (remove #(= % \I) pauli-str)))
                  (let [qubit-idx (first (keep-indexed #(when (not= %2 \I) %1) pauli-str))
                        pauli-op (nth pauli-str qubit-idx)]
                    (case pauli-op
                      \X (qc/rx-gate circ qubit-idx angle)
                      \Y (qc/ry-gate circ qubit-idx angle)
                      \Z (qc/rz-gate circ qubit-idx angle)
                      circ))  ; Default case for unknown Pauli operators

                  ;; Multi-Pauli terms: use CNOT ladder for ZZ interactions
                  :else
                  (let [pauli-positions (keep-indexed #(when (not= %2 \I) [%1 %2]) pauli-str)]
                    ;; For ZZ terms, we can use direct RZ with CNOT ladders
                    ;; For more complex terms, we'd need more sophisticated decomposition
                    (if (every? #(= (second %) \Z) pauli-positions)
                      ;; ZZ interaction: create CNOT ladder, apply RZ to last qubit, uncompute
                      (let [qubit-indices (mapv first pauli-positions)]
                        (if (= (count qubit-indices) 2)
                          ;; Two-qubit ZZ: simple case
                          (let [[q1 q2] qubit-indices]
                            (-> circ
                                (qc/cnot-gate q1 q2)
                                (qc/rz-gate q2 angle)
                                (qc/cnot-gate q1 q2)))
                          ;; Multi-qubit ZZ: use CNOT ladder
                          (let [target (last qubit-indices)
                                controls (butlast qubit-indices)
                                c-with-cnots
                                (reduce (fn [c ctrl]
                                          (qc/cnot-gate c ctrl target))
                                        circ
                                        controls)]
                            (-> c-with-cnots
                                (qc/rz-gate target angle)
                                ;; Uncompute CNOT ladder
                                (#(reduce (fn [c ctrl]
                                            (qc/cnot-gate c ctrl target))
                                          %
                                          (reverse controls)))))))
                      ;; For X/Y terms, would need more complex decomposition
                      circ)))))
            circuit
            hamiltonian)))

(defn qaoa-ansatz-circuit
  "Create a QAOA ansatz circuit with alternating problem and mixer evolution.
  
  The QAOA ansatz consists of p layers of alternating evolution:
  U(β,γ) = ∏ᵢ₌₁ᵖ e^(-iβᵢH_M) e^(-iγᵢH_P)
  
  Where H_P is the problem Hamiltonian and H_M is the mixer Hamiltonian.
  
  Parameters:
  - problem-hamiltonian: Problem Hamiltonian (collection of Pauli terms)
  - mixer-hamiltonian: Mixer Hamiltonian (collection of Pauli terms)
  - parameters: Vector of [γ₁ β₁ γ₂ β₂ ... γₚ βₚ] parameters
  - num-qubits: Number of qubits
  
  Returns:
  Quantum circuit implementing the QAOA ansatz"
  [problem-hamiltonian mixer-hamiltonian parameters num-qubits]
  {:pre [(ham/validate-hamiltonian problem-hamiltonian)
         (ham/validate-hamiltonian mixer-hamiltonian)
         (vector? parameters)
         (even? (count parameters))
         (pos-int? num-qubits)]}
  (let [param-pairs (partition 2 parameters)  ; [(γ₁ β₁) (γ₂ β₂) ...]
        initial-circuit (qc/create-circuit num-qubits "QAOA Ansatz")
        ;; Start with equal superposition state |+⟩^⊗n
        circuit-with-init (reduce (fn [circ qubit]
                                    (qc/h-gate circ qubit))
                                  initial-circuit
                                  (range num-qubits))]

    ;; Apply layers of alternating evolution
    (reduce (fn [circ [gamma beta]]
              (-> circ
                  ;; Apply problem Hamiltonian evolution e^(-iγH_P)
                  (#(let [prob-circuit (hamiltonian-evolution-circuit problem-hamiltonian gamma num-qubits)]
                      (cc/compose-circuits % prob-circuit)))
                  ;; Apply mixer Hamiltonian evolution e^(-iβH_M)  
                  (#(let [mixer-circuit (hamiltonian-evolution-circuit mixer-hamiltonian beta num-qubits)]
                      (cc/compose-circuits % mixer-circuit)))))
            circuit-with-init
            param-pairs)))

(defn smart-parameter-initialization
  "Generate smart initial parameters for QAOA based on theoretical insights and heuristics.
  
  Different strategies:
  - :theoretical - Based on known optimal values for small p
  - :adiabatic - Linear interpolation from adiabatic evolution  
  - :random-smart - Improved random initialization in good ranges
  
  Parameters:
  - num-layers: Number of QAOA layers (p)
  - problem-type: Type of optimization problem (:max-cut, :max-sat, etc.)
  - strategy: Initialization strategy keyword
  
  Returns:
  Vector of [γ₁ β₁ γ₂ β₂ ...] parameters"
  [num-layers problem-type strategy]
  (case strategy
    :theoretical
    (case num-layers
      1 (case problem-type
          :max-cut [0.39 0.20]  ; Near-optimal for MaxCut p=1 from literature
          :max-sat [0.35 0.25]  ; Heuristic for Max-SAT
          [0.4 0.2])            ; Default
      2 (case problem-type
          :max-cut [0.37 0.19 0.42 0.21]  ; Optimized for MaxCut p=2
          :max-sat [0.35 0.22 0.38 0.24]  ; Heuristic for Max-SAT p=2  
          [0.4 0.2 0.35 0.18])            ; Default p=2
      ;; For p > 2, use interpolation
      (let [base-gamma (case problem-type :max-cut 0.38 :max-sat 0.36 0.37)
            base-beta (case problem-type :max-cut 0.20 :max-sat 0.23 0.21)]
        (vec (mapcat (fn [layer]
                       (let [scale (+ 0.8 (* 0.4 (/ layer num-layers)))]
                         [(* base-gamma scale) (* base-beta scale)]))
                     (range num-layers)))))

    :adiabatic
    ;; Linear schedule: γᵢ = γₘₐₓ * i/p, βᵢ = βₘₐₓ * (1 - i/p)
    (let [gamma-max (case problem-type :max-cut 0.75 :max-sat 0.70 0.6)
          beta-max (case problem-type :max-cut 0.4 :max-sat 0.45 0.35)]
      (vec (mapcat (fn [layer]
                     (let [t (/ (inc layer) num-layers)]
                       [(* gamma-max t) (* beta-max (- 1 t))]))
                   (range num-layers))))

    :random-smart
    ;; Random in theoretically good ranges, alternating γ and β
    (let [gamma-range (case problem-type :max-cut [0.2 0.6] :max-sat [0.1 0.5] [0.15 0.55])
          beta-range (case problem-type :max-cut [0.1 0.3] :max-sat [0.15 0.35] [0.1 0.3])]
      (vec (mapcat (fn [_]
                     [(+ (first gamma-range)
                         (* (rand) (- (second gamma-range) (first gamma-range))))
                      (+ (first beta-range)
                         (* (rand) (- (second beta-range) (first beta-range))))])
                   (range num-layers))))

    ;; Unknown strategy - throw error
    (throw (ex-info "Unknown parameter initialization strategy"
                    {:strategy strategy
                     :valid-strategies #{:theoretical :adiabatic :random-smart}}))))

;;;
;;; Template Integration Functions for Variational Algorithm Framework
;;;
(defn qaoa-hamiltonian-constructor
  "Create problem Hamiltonian for QAOA based on the problem type and configuration.
  
  This function serves as the hamiltonian-constructor for the variational-algorithm template.
  It handles all supported QAOA problem types and creates the appropriate Hamiltonian.
  
  Parameters:
  - options: QAOA configuration map containing:
    - :problem-type - Type of problem (:max-cut, :max-sat, :tsp, :custom)
    - :problem-instance - Problem-specific data structure
    - :num-qubits - Number of qubits (for some problem types)
    - :problem-hamiltonian - Pre-constructed Hamiltonian (for :custom type)
  
  Returns:
  Collection of Pauli terms representing the problem Hamiltonian"
  [options]
  (let [problem-type (:problem-type options)
        problem-instance (:problem-instance options)
        num-qubits (:num-qubits options)]
    (case problem-type
      :max-cut (max-cut-hamiltonian problem-instance num-qubits)
      :max-sat (max-sat-hamiltonian problem-instance num-qubits)
      :tsp (travelling-salesman-hamiltonian problem-instance)
      :custom (:problem-hamiltonian options)
      (throw (ex-info "Unknown problem type for QAOA"
                      {:problem-type problem-type
                       :supported-types [:max-cut :max-sat :tsp :custom]})))))

(defn qaoa-mixer-hamiltonian-constructor
  "Create mixer Hamiltonian for QAOA.
  
  This function creates the mixer Hamiltonian, defaulting to the standard X mixer
  but allowing for custom mixers based on the configuration.
  
  Parameters:
  - options: QAOA configuration map containing:
    - :num-qubits - Number of qubits
    - :mixer-hamiltonian - Custom mixer Hamiltonian (optional)
    - :mixer-type - Type of mixer (:standard, :xy) (optional, default: :standard)
  
  Returns:
  Collection of Pauli terms representing the mixer Hamiltonian"
  [options]
  (let [num-qubits (:num-qubits options)
        mixer-type (:mixer-type options :standard)]
    (or (:mixer-hamiltonian options)
        (case mixer-type
          :standard (standard-mixer-hamiltonian num-qubits)
          :xy (xy-mixer-hamiltonian num-qubits (:periodic-xy options true))
          (standard-mixer-hamiltonian num-qubits)))))

(defn qaoa-circuit-constructor
  "Create circuit construction function for QAOA.
  
  This function serves as the circuit-constructor for the variational-algorithm template.
  It captures the problem and mixer Hamiltonians and returns a function that constructs
  QAOA circuits given parameters.
  
  Parameters:
  - options: QAOA configuration map containing all necessary configuration
  
  Returns:
  Function that takes parameters and returns a QAOA circuit"
  [options]
  (let [problem-hamiltonian (qaoa-hamiltonian-constructor options)
        mixer-hamiltonian (qaoa-mixer-hamiltonian-constructor options)
        num-qubits (:num-qubits options)]
    (fn [parameters]
      (qaoa-ansatz-circuit problem-hamiltonian mixer-hamiltonian parameters num-qubits))))

(defn qaoa-parameter-count
  "Calculate the number of parameters for QAOA.
  
  This function serves as the parameter-count function for the variational-algorithm template.
  QAOA requires 2 parameters per layer: γ (gamma) and β (beta).
  
  Parameters:
  - options: QAOA configuration map containing :num-layers
  
  Returns:
  Number of parameters (2 * num-layers)"
  [options]
  (* 2 (:num-layers options)))

(defn qaoa-parameter-initialization
  "Initialize QAOA parameters using sophisticated strategies.
  
  This function provides QAOA-specific parameter initialization that's more sophisticated
  than the generic random initialization in the template.
  
  Parameters:
  - options: QAOA configuration map
  
  Returns:
  Vector of initial parameters [γ₁ β₁ γ₂ β₂ ...]"
  [options]
  (let [num-layers (:num-layers options)
        problem-type (:problem-type options)
        strategy (:parameter-strategy options :theoretical)]
    (smart-parameter-initialization num-layers problem-type strategy)))

;;;
;;; QAOA-Specific Helper Functions and Result Extraction (moved from domain.result)
;;;

(defn decode-max-cut-solution
  "Decode measurement outcomes for MaxCut problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length
  - graph: Collection of edges as [vertex1 vertex2 weight] tuples
  
  Returns:
  Map with decoded MaxCut solution information"
  [index num-qubits graph]
  (let [bits (qs/index-to-bits index num-qubits)
        partition (zipmap (range num-qubits) bits)
        cut-edges (filter (fn [[i j _weight]]
                            (not= (get partition i) (get partition j)))
                          graph)
        cut-weight (reduce + (map #(nth % 2) cut-edges))]
    {:partition partition
     :cut-edges cut-edges
     :cut-weight cut-weight}))

(defn decode-max-sat-solution
  "Decode measurement outcomes for MaxSAT problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length
  - clauses: Collection of Boolean clauses
  
  Returns:
  Map with decoded MaxSAT solution information"
  [index num-qubits clauses]
  (let [bits (qs/index-to-bits index num-qubits)
        assignment (zipmap (range num-qubits) (map #(= % 1) bits))

        ;; Function to evaluate a literal given the assignment
        eval-literal (fn [literal]
                       (cond
                         ;; Positive integer literal
                         (and (integer? literal) (>= literal 0))
                         (get assignment literal false)

                         ;; Keyword :not-X format
                         (and (keyword? literal) (.startsWith (name literal) "not-"))
                         (let [var-str (subs (name literal) 4)]
                           (not (get assignment (Integer/parseInt var-str) false)))

                         ;; Map format {:variable X :negated boolean}
                         (map? literal)
                         (let [var-idx (:variable literal)
                               negated? (:negated literal false)
                               var-value (get assignment var-idx false)]
                           (if negated? (not var-value) var-value))

                         :else false))

        ;; Evaluate each clause (OR of literals)
        clause-results (map (fn [clause]
                              (some eval-literal clause))
                            clauses)

        satisfied-count (count (filter true? clause-results))]

    {:boolean-assignment assignment
     :satisfied-clauses satisfied-count
     :total-clauses (count clauses)
     :satisfaction-ratio (/ satisfied-count (count clauses))}))

(defn decode-tsp-solution
  "Decode measurement outcomes for TSP problem.
  
  Parameters:
  - index: Integer index from measurement
  - num-qubits: Number of qubits to determine bit vector length (should be n²)
  - distance-matrix: n×n matrix of distances between cities
  
  Returns:
  Map with decoded TSP solution information"
  [index num-qubits distance-matrix]
  (let [n (count distance-matrix)

        ;; Decode index to bit vector
        bits (qs/index-to-bits index num-qubits)

        ;; Build assignment matrix: assignment[i][j] = 1 if city i visited at time j
        assignment-matrix (partition n bits)

        ;; Extract tour by finding which city is visited at each time step
        tour (vec (for [time-step (range n)]
                    (let [cities-at-time (keep-indexed
                                          (fn [city bit]
                                            (when (= bit 1) city))
                                          (map #(nth % time-step) assignment-matrix))]
                      (if (= (count cities-at-time) 1)
                        (first cities-at-time)
                        -1))))  ; Invalid assignment

        ;; Calculate tour distance if valid
        tour-distance (if (and (every? #(>= % 0) tour)
                               (= (count (set tour)) n))  ; All cities visited exactly once
                        (reduce + (map (fn [i]
                                         (let [from-city (nth tour i)
                                               to-city (nth tour (mod (inc i) n))]
                                           (nth (nth distance-matrix from-city) to-city)))
                                       (range n)))
                        Double/POSITIVE_INFINITY)]  ; Invalid tour

    {:tour tour
     :tour-distance tour-distance
     :valid-tour? (not= tour-distance Double/POSITIVE_INFINITY)
     :assignment-matrix assignment-matrix}))

(defn calculate-approximation-ratio
  "Calculate approximation ratio for optimization problems.
  
  Parameters:
  - qaoa-solution: Solution value from QAOA
  - classical-optimum: Known or estimated classical optimum
  - problem-type: Type of problem (:max-cut, :max-sat, :tsp)
  
  Returns:
  Approximation ratio (for maximization: qaoa/classical, for minimization: classical/qaoa)"
  [qaoa-solution classical-optimum problem-type]
  (when (and qaoa-solution classical-optimum (pos? classical-optimum))
    (case problem-type
      (:max-cut :max-sat) (/ qaoa-solution classical-optimum)
      :tsp (/ classical-optimum qaoa-solution)
      nil)))

(defn extract-best-solution
  "Extract the best solution from a collection of decoded solutions.
  
  Parameters:
  - solutions: Collection of solution maps with objective values
  - problem-type: Type of problem to determine maximization vs minimization
  
  Returns:
  Best solution according to the problem objective"
  [solutions problem-type]
  (when (seq solutions)
    (case problem-type
      (:max-cut :max-sat) (apply max-key :objective-value solutions)
      :tsp (first (filter #(not= (:objective-value %) Double/POSITIVE_INFINITY)
                          (sort-by :objective-value solutions)))
      (first solutions))))









;;;
;;; Measurement-based Solution Extraction (Hardware Compatible)
;;;






(defn extract-solution-from-frequencies
  "Extract problem-specific solutions from measurement frequency data.
  
  This function provides a hardware-compatible way to extract QAOA solutions
  using measurement frequencies directly from the backend results, which is
  more efficient than converting to individual outcome strings.
  
  Parameters:
  - measurement-frequencies: Map of {outcome-index count} from backend
  - problem-type: Type of problem (:max-cut, :max-sat, :tsp)
  - problem-instance: Problem-specific data (graph, clauses, distance matrix)
  - shots: Total number of measurement shots
  - num-qubits: Number of qubits
  - classical-optimum: Known classical optimum (optional)
  
  Returns:
  Map with problem-specific solution analysis"
  [measurement-frequencies problem-type problem-instance shots num-qubits & {:keys [classical-optimum]}]
  (case problem-type
    :max-cut
    (let [;; Process frequencies into solution analyses
          solution-analyses (map (fn [[index count]]
                                   (let [probability (/ count shots)
                                         decoded (decode-max-cut-solution index num-qubits problem-instance)]
                                     (assoc decoded
                                            :index index
                                            :count count
                                            :probability probability
                                            :objective-value (:cut-weight decoded))))
                                 measurement-frequencies)
          best-solution (extract-best-solution solution-analyses :max-cut)
          ;; Calculate classical optimum for small instances
          computed-classical-optimum (or classical-optimum
                                         (when (<= num-qubits 12)
                                           (let [all-partitions (range (bit-shift-left 1 num-qubits))
                                                 all-cuts (map (fn [partition]
                                                                 (:cut-weight (decode-max-cut-solution partition num-qubits problem-instance)))
                                                               all-partitions)]
                                             (apply max all-cuts))))
          approx-ratio (calculate-approximation-ratio (:cut-weight best-solution) computed-classical-optimum :max-cut)]
      {:solution-type :max-cut
       :cut-edges (:cut-edges best-solution)
       :cut-weight (:cut-weight best-solution)
       :partition (:partition best-solution)
       :solution-probability (:probability best-solution)
       :approximation-ratio approx-ratio
       :classical-optimum computed-classical-optimum
       :all-solutions solution-analyses
       :measurement-distribution (into {} measurement-frequencies)})

    :max-sat
    (let [solution-analyses (map (fn [[index count]]
                                   (let [probability (/ count shots)
                                         decoded (decode-max-sat-solution index num-qubits problem-instance)]
                                     (assoc decoded
                                            :index index
                                            :count count
                                            :probability probability
                                            :objective-value (:satisfied-clauses decoded))))
                                 measurement-frequencies)
          best-solution (extract-best-solution solution-analyses :max-sat)
          computed-classical-optimum (or classical-optimum (count problem-instance))
          approx-ratio (calculate-approximation-ratio (:satisfied-clauses best-solution) computed-classical-optimum :max-sat)]
      {:solution-type :max-sat
       :boolean-assignment (:boolean-assignment best-solution)
       :satisfied-clauses (:satisfied-clauses best-solution)
       :solution-probability (:probability best-solution)
       :approximation-ratio approx-ratio
       :classical-optimum computed-classical-optimum
       :all-solutions solution-analyses
       :measurement-distribution (into {} measurement-frequencies)})

    :tsp
    (let [solution-analyses (map (fn [[index count]]
                                   (let [probability (/ count shots)
                                         decoded (decode-tsp-solution index num-qubits problem-instance)]
                                     (assoc decoded :index index
                                            :count count
                                            :probability probability)))
                                 measurement-frequencies)
          valid-solutions (filter :valid-tour? solution-analyses)
          best-solution (when (seq valid-solutions)
                          (apply min-key :tour-distance valid-solutions))]
      (if best-solution
        {:solution-type :tsp
         :tour (:tour best-solution)
         :tour-distance (:tour-distance best-solution)
         :solution-probability (:probability best-solution)
         :valid-tour? true
         :approximation-ratio (when classical-optimum (/ classical-optimum (:tour-distance best-solution)))
         :classical-optimum classical-optimum
         :all-solutions solution-analyses
         :valid-solutions-count (count valid-solutions)
         :measurement-distribution (into {} measurement-frequencies)}

        {:solution-type :tsp
         :tour nil
         :tour-distance Double/POSITIVE_INFINITY
         :solution-probability 0.0
         :valid-tour? false
         :approximation-ratio nil
         :classical-optimum classical-optimum
         :all-solutions solution-analyses
         :valid-solutions-count 0
         :measurement-distribution (into {} measurement-frequencies)}))

    ;; Unknown problem type
    {:solution-type problem-type
     :error (str "Unknown problem type: " problem-type)}))

(defn qaoa-result-processor
  "Process and enhance QAOA results with algorithm-specific analysis.
  
  This function serves as the result-processor for the variational-algorithm template.
  It adds QAOA-specific analysis including parameter extraction, approximation ratios,
  and problem-specific solution quality metrics.
  
  Enhanced to include hardware-compatible solution extraction using measurement outcomes.
  
  Parameters:
  - base-result: Base optimization result from the template
  - options: QAOA configuration options (including :backend for measurement-based solutions)
  
  Returns:
  Enhanced result map with QAOA-specific analysis and problem-specific solutions"
  [base-result options]
  (let [optimal-params (:optimal-parameters base-result)
        problem-type (:problem-type options)
        num-layers (:num-layers options)
        num-qubits (:num-qubits options)
        backend (:backend options)

        ;; Extract γ and β parameters
        gamma-params (vec (take-nth 2 optimal-params))
        beta-params (vec (take-nth 2 (rest optimal-params)))

        ;; Build final circuit for inspection
        problem-hamiltonian (qaoa-hamiltonian-constructor options)
        mixer-hamiltonian (qaoa-mixer-hamiltonian-constructor options)
        final-circuit (qaoa-ansatz-circuit problem-hamiltonian mixer-hamiltonian
                                           optimal-params num-qubits)

        ;; Calculate approximation ratio if classical optimum is known
        approximation-ratio (when (:classical-optimum options)
                              (/ (:optimal-energy base-result) (:classical-optimum options)))

        ;; Hardware-compatible solution extraction using measurement result-specs
        problem-solutions (when (and backend problem-type (:problem-instance options))
                            (try
                              (let [shots (get options :shots 1024)
                                    ;; Use result-specs to request measurements - no manual circuit modification needed
                                    measurement-options {:shots shots
                                                         :result-specs {:measurements {:shots shots}}}
                                    ;; Execute final circuit with measurement result-specs
                                    measurement-result (backend/execute-circuit backend final-circuit measurement-options)
                                    ;; Extract measurement frequencies directly - more efficient than individual outcomes
                                    measurement-results (get-in measurement-result [:results :measurement-results])
                                    measurement-frequencies (:frequencies measurement-results)]

                                ;; Extract problem-specific solutions from measurement frequencies
                                (extract-solution-from-frequencies
                                 measurement-frequencies
                                 problem-type
                                 (:problem-instance options)
                                 shots
                                 num-qubits
                                 :classical-optimum (:classical-optimum options)))
                              (catch Exception e
                                ;; Handle measurement execution errors gracefully
                                {:solution-type problem-type
                                 :error (str "Failed to extract solutions from measurements: " (.getMessage e))
                                 :measurement-extraction-available false})))]

    (merge base-result
           {:algorithm "QAOA"
            :problem-type problem-type
            :num-qubits num-qubits
            :num-layers num-layers
            :problem-hamiltonian problem-hamiltonian
            :mixer-hamiltonian mixer-hamiltonian
            :circuit final-circuit
            :approximation-ratio approximation-ratio
            :gamma-parameters gamma-params
            :beta-parameters beta-params

            ;; Add problem-specific solutions from measurements
            :problem-solutions problem-solutions

            ;; Add QAOA-specific performance analysis
            :qaoa-analysis {:parameter-summary
                            {:gamma-range [(apply min gamma-params) (apply max gamma-params)]
                             :beta-range [(apply min beta-params) (apply max beta-params)]
                             :gamma-mean (/ (reduce + gamma-params) (count gamma-params))
                             :beta-mean (/ (reduce + beta-params) (count beta-params))
                             :total-parameters (count optimal-params)}
                            :convergence-analysis (:convergence-analysis base-result)
                            :performance-metrics
                            {:function-evaluations (:function-evaluations base-result 0)
                             :total-runtime-ms (:total-runtime-ms base-result)
                             :approximation-ratio approximation-ratio
                             :measurement-based-solutions (some? problem-solutions)}}})))

;;;
;;; QAOA Algorithm Implementation
;;;
(defn qaoa-objective
  "Create the objective function for QAOA optimization.
  
  This function creates the objective function that:
  1. Takes QAOA parameters [γ₁ β₁ γ₂ β₂ ...] as input
  2. Constructs the QAOA ansatz circuit with those parameters
  3. Executes the circuit on the backend or simulator
  4. Calculates the problem Hamiltonian expectation value
  5. Returns the energy (to be minimized by the optimizer)
  
  Parameters:
  - problem-hamiltonian: Problem Hamiltonian to minimize
  - mixer-hamiltonian: Mixer Hamiltonian (optional, defaults to standard X mixer)
  - num-qubits: Number of qubits
  - backend: Quantum backend for circuit execution (can be nil for direct simulation)
  - options: Execution options (shots, etc.)
  
  Returns:
  Function that takes parameters and returns energy expectation value"
  [problem-hamiltonian mixer-hamiltonian num-qubits backend options]
  {:pre [(ham/validate-hamiltonian problem-hamiltonian)
         (ham/validate-hamiltonian mixer-hamiltonian)
         (pos-int? num-qubits)]}
  ;; Create circuit construction function that captures QAOA-specific structure
  (let [circuit-construction-fn (fn [parameters]
                                  (qaoa-ansatz-circuit problem-hamiltonian mixer-hamiltonian
                                                       parameters num-qubits))]
    ;; Use the common variational objective
    (va/variational-hamiltonian-objective problem-hamiltonian circuit-construction-fn backend options)))

(defn quantum-approximate-optimization-algorithm
  "Main QAOA algorithm implementation using the variational algorithm template.

  This function orchestrates the QAOA process using the enhanced variational-algorithm
  template, providing improved optimization features, convergence monitoring, and
  standardized result analysis while maintaining full backward compatibility.
  
  Key Enhancements from Template Integration:
  - Advanced convergence monitoring with intelligent stopping criteria
  - Enhanced optimization methods with gradient support where applicable
  - Standardized result analysis and performance metrics
  - Consistent error handling and timing across all variational algorithms
  - Parameter landscape analysis capabilities
  - Future extensibility for new optimization methods
  
  Supported problem types:
  - :max-cut - Maximum cut problem on graphs
  - :max-sat - Maximum satisfiability problem
  - :tsp - Travelling salesman problem (simplified encoding)
  - :custom - Custom problem with provided Hamiltonian
   
  Supported optimization methods:
  - :gradient-descent - Basic gradient descent with parameter shift gradients
  - :adam - Adam optimizer with parameter shift gradients (recommended default)
  - :quantum-natural-gradient - Quantum Natural Gradient using Fisher Information Matrix
  - :nelder-mead - Derivative-free Nelder-Mead simplex method
  - :powell - Derivative-free Powell's method
  - :cmaes - Covariance Matrix Adaptation Evolution Strategy (robust)
  - :bobyqa - Bound Optimization BY Quadratic Approximation (handles bounds well)
  
  Parameters:
  - backend: Quantum backend implementing QuantumBackend protocol
  - options: QAOA configuration options
    - :problem-type - Type of problem (:max-cut, :max-sat, :tsp, :custom)
    - :problem-instance - Problem-specific data (graph, clauses, distance matrix, etc.)
    - :num-qubits - Number of qubits in the system
    - :num-layers - Number of QAOA layers (p parameter)
    - :optimization-method - Optimization method to use (default: :adam)
    - :max-iterations - Maximum iterations for optimization (default: 500)
    - :tolerance - Convergence tolerance (default: 1e-6)
    - :gradient-tolerance - Gradient norm tolerance (default: 1e-4)
    - :min-iterations - Minimum iterations before convergence (default: 10)
    - :patience - Convergence analysis window (default: 20)
    - :shots - Number of shots for circuit execution (default: 1024)
    - :initial-parameters - Custom initial parameters (optional)
    - :parameter-strategy - Parameter initialization strategy (default: :theoretical)
      * :theoretical - Literature-based optimal values for small p (recommended)
      * :adiabatic - Linear interpolation schedule inspired by adiabatic evolution
      * :random-smart - Random sampling in theoretically good parameter ranges
    - :classical-optimum - Known classical optimum for approximation ratio calculation
    - :mixer-hamiltonian - Custom mixer Hamiltonian (optional)
    - :mixer-type - Type of mixer (:standard, :xy) (optional, default: :standard)
  
  Returns:
  Enhanced map containing QAOA results and comprehensive analysis
  
  Example:
  (quantum-approximate-optimization-algorithm backend
    {:problem-type :max-cut
     :problem-instance [[0 1 1.0] [1 2 1.0] [0 2 1.0]]  ; Triangle graph
     :num-qubits 3
     :num-layers 2
     :optimization-method :adam
     :max-iterations 100
     :tolerance 1e-6})"
  [backend options]
  {:pre [(s/valid? ::qaoa-config options)]}
  ;; Use QAOA-specific parameter initialization if not provided
  (let [enhanced-options (-> options
                             ;; Add backend for result processing
                             (assoc :backend backend)
                             ;; Add initial parameters if not provided
                             (#(if (:initial-parameters %)
                                 %
                                 (assoc % :initial-parameters
                                        (qaoa-parameter-initialization %)))))

        ;; Define QAOA-specific functions for the template
        algorithm-fns {:hamiltonian-constructor qaoa-hamiltonian-constructor
                       :circuit-constructor qaoa-circuit-constructor
                       :parameter-count qaoa-parameter-count
                       :result-processor qaoa-result-processor}]

    ;; Delegate to the variational algorithm template
    (va/variational-algorithm backend enhanced-options algorithm-fns)))

;;;
;;; Analysis and Utilities
;;;
(defn analyze-qaoa-parameters
  "Analyze QAOA parameter landscape and patterns.
  
  Parameters:
  - optimization-result: Result from QAOA optimization
  
  Returns:
  Map with parameter analysis"
  [optimization-result]
  (let [optimal-params (:optimal-parameters optimization-result)
        gamma-params (:gamma-parameters optimization-result)
        beta-params (:beta-parameters optimization-result)]

    {:parameter-summary
     {:gamma-range [(apply min gamma-params) (apply max gamma-params)]
      :beta-range [(apply min beta-params) (apply max beta-params)]
      :gamma-mean (/ (reduce + gamma-params) (count gamma-params))
      :beta-mean (/ (reduce + beta-params) (count beta-params))
      :total-parameters (count optimal-params)}

     :convergence-analysis
     ;; Use the generic convergence analysis
     (va/analyze-convergence optimization-result)

     :performance-metrics
     {:function-evaluations (:function-evaluations optimization-result 0)
      :total-runtime-ms (:total-runtime-ms optimization-result)
      :approximation-ratio (:approximation-ratio optimization-result)}}))

;;;
;;; Rich comment section
;;;
(comment
  (require '[org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim])
  (def backend (sim/create-simulator))

  ;; Example 1: MaxCut on triangle graph
  (def triangle-graph [[0 1 1.0] [1 2 1.0] [0 2 1.0]])
  (def triangle-maxcut-config
    {:problem-type :max-cut
     :problem-hamiltonian (max-cut-hamiltonian triangle-graph 3)
     :problem-instance triangle-graph  ; Triangle with unit weights
     :num-qubits 3
     :num-layers 2
     :optimization-method :adam
     :max-iterations 100
     :tolerance 1e-6
     :shots 10000})

  (def qaoa-result (quantum-approximate-optimization-algorithm backend triangle-maxcut-config))

  ;; Analyze results
  (def analysis (analyze-qaoa-parameters qaoa-result))

  ;; Print key results
  (println "QAOA Energy:" (:optimal-energy qaoa-result))
  (println "Optimal γ parameters:" (:gamma-parameters qaoa-result))
  (println "Optimal β parameters:" (:beta-parameters qaoa-result))

  ;; Parameter strategy comparison
  ;; :theoretical - Best for small p, based on literature (recommended)
  ;; :adiabatic - Good for larger p, inspired by adiabatic quantum computation
  ;; :random-smart - Robust fallback, random sampling in good ranges

  ;; Example 2: Max-SAT on Boolean satisfiability problem
  ;; SAT formula: (x₀ ∨ x₁) ∧ (¬x₀ ∨ x₂) ∧ (x₁ ∨ ¬x₂) ∧ (¬x₀ ∨ ¬x₁ ∨ x₂)
  ;; Using proper literal encoding: keywords :not-X for negated variables
  (def sat-clauses [[0 1]           ; (x₀ ∨ x₁)
                    [:not-0 2]      ; (¬x₀ ∨ x₂) 
                    [1 :not-2]      ; (x₁ ∨ ¬x₂)
                    [:not-0 :not-1 2]]) ; (¬x₀ ∨ ¬x₁ ∨ x₂)

  (def maxsat-config
    {:problem-type :max-sat
     :problem-hamiltonian (max-sat-hamiltonian sat-clauses 3)
     :problem-instance sat-clauses
     :num-qubits 3
     :num-layers 2
     :parameter-strategy :theoretical
     :optimization-method :adam
     :max-iterations 150
     :tolerance 1e-6
     :shots 10000})

  (def maxsat-result (quantum-approximate-optimization-algorithm backend maxsat-config))

  (println "\n=== Max-SAT Results ===")
  (println "SAT clauses:" sat-clauses)
  (println "QAOA Energy:" (:optimal-energy maxsat-result))
  (println "Success:" (:success maxsat-result))

  ;; Example 3: Traveling Salesman Problem (TSP)
  ;; Full n² qubit encoding with proper constraint handling
  ;; Note: This uses exponential quantum resources - practical for n ≤ 4
  (def production-tsp-distances
    [[0 2 3]   ; Distances from city 0 to cities 0,1,2
     [2 0 1]   ; Distances from city 1 to cities 0,1,2  
     [3 1 0]]) ; Distances from city 2 to cities 0,1,2

  (def tsp-config
    {:problem-type :tsp
     :problem-hamiltonian (travelling-salesman-hamiltonian production-tsp-distances)
     :problem-instance production-tsp-distances
     :num-qubits 9  ; 3² = 9 qubits for 3 cities
     :num-layers 3
     :parameter-strategy :adiabatic
     :optimization-method :adam
     :max-iterations 200
     :tolerance 1e-6
     :shots 10000})

  (def tsp-result (quantum-approximate-optimization-algorithm backend tsp-config))

  (println "\n=== TSP Results ===")
  (println "Distance matrix:" production-tsp-distances)
  (println "QAOA Energy:" (:optimal-energy tsp-result))
  (println "Success:" (:success tsp-result))

  ;; Decode TSP solution from quantum measurement
  (when (:success tsp-result)
    (let [best-measurement-index 273  ; Example: binary 100010001 = 273 in decimal
          decoded-tour (decode-tsp-solution best-measurement-index 9 production-tsp-distances)]
      (println "Decoded Tour:" (:tour decoded-tour))
      (println "Tour Valid:" (:valid decoded-tour))
      (println "Tour Cost:" (:cost decoded-tour))))

  ;; Example 4: Custom Ising model
  ;; Example 4: Custom Ising model
  ;; H = Σᵢ hᵢZᵢ + Σᵢⱼ Jᵢⱼ ZᵢZⱼ  
  ;; External fields: h = [0.5, -0.3, 0.2]
  ;; Coupling terms: J = {(0,1): 0.1, (1,2): -0.2}
  (def ising-config
    {:problem-type :custom
     :problem-hamiltonian (custom-ising-hamiltonian [0.5 -0.3 0.2] [[0 1 0.1] [1 2 -0.2]])
     :num-qubits 3
     :num-layers 1
     :parameter-strategy :random-smart
     :optimization-method :nelder-mead
     :max-iterations 50})

  (def ising-result (quantum-approximate-optimization-algorithm backend ising-config))

  (println "\n=== Custom Ising Model Results ===")
  (println "QAOA Energy:" (:optimal-energy ising-result))
  (println "Ground state energy (QAOA approximation):" (:optimal-energy ising-result))

  ;
  )
