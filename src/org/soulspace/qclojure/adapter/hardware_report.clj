(ns org.soulspace.qclojure.adapter.hardware-report
  (:require [org.soulspace.qclojure.domain.topology :as topo]
            [org.soulspace.qclojure.application.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.ascii :as ascii]
            [org.soulspace.qclojure.adapter.visualization.svg :as svg]
            [clojure.string :as str]))

;;;
;;; Topology Visualization Integration
;;;
(defn visualize-topology
  "Visualize a hardware topology using the visualization system.
  
  Parameters:
  - topology: Hardware topology (vector of vectors)
  - format: Visualization format (:ascii or :svg)
  - options: Visualization options (passed to visualizer)
  
  Returns:
  String containing the visualization"
  [topology format & [options]]
  (viz/visualize-hardware-topology format topology (or options {})))

(defn compare-topologies-visually
  "Create visual comparison of different hardware topologies.
  
  Parameters:
  - topologies: Map of name to topology
  - format: Visualization format (:ascii or :svg)
  - options: Visualization options
  
  Returns:
  Map of topology name to visualization string"
  [topologies format & [options]]
  (into {} (map (fn [[name topology]]
                  [name (visualize-topology topology format options)])
                topologies)))

(defn create-topology-comparison-report
  "Create a comprehensive report comparing different topologies.
  
  Parameters:
  - topologies: Map of name to topology
  - circuit: Optional circuit to test optimization on
  - options: Report options
  
  Returns:
  String containing the complete comparison report"
  [topologies & [circuit options]]
  (let [format (get options :format :ascii)
        show-visualizations (get options :show-visualizations true)
        show-analysis (get options :show-analysis true)
        show-optimization (and circuit (get options :show-optimization true))]
    
    (str "═══ Hardware Topology Comparison Report ═══\n\n"
         
         ;; Analysis section
         (when show-analysis
           (str "TOPOLOGY ANALYSIS:\n"
                (str/join "\n"
                          (map (fn [[name topology]]
                                 (str "• " (topo/get-coupling-info topology name)))
                               topologies))
                "\n\n"))
         
         ;; Circuit optimization comparison
         (when show-optimization
           (str "CIRCUIT OPTIMIZATION COMPARISON:\n"
                (let [comparisons (topo/compare-couplings circuit topologies)]
                  (str/join "\n"
                            (map (fn [result]
                                   (str "• " (:topology-name result) 
                                        ": cost=" (:total-cost result) 
                                        ", swaps=" (:swap-count result)))
                                 comparisons)))
                "\n\n"))
         
         ;; Visual representations
         (when show-visualizations
           (str "TOPOLOGY VISUALIZATIONS:\n"
                (str/join "\n\n"
                          (map (fn [[name topology]]
                                 (str "--- " name " ---\n"
                                      (visualize-topology topology format options)))
                               topologies)))))))

(defn demonstrate-topologies
  "Demonstrate different topology types with visualizations and analysis.
  
  Parameters:
  - size: Size parameter for topologies (default 5)
  - format: Visualization format (default :ascii)
  
  Returns:
  String containing the demonstration"
  [& {:keys [size format] :or {size 5 format :ascii}}]
  (let [demo-topologies {"Linear" (topo/linear-coupling size)
                         "Ring" (topo/ring-coupling size)
                         "Star" (topo/star-coupling size)
                         "Grid-2x3" (topo/grid-coupling 2 3)
                         "All-to-all" (topo/all-to-all-coupling size)
                         "Heavy-Hex" (topo/heavy-hex-coupling 7)}]
    
    (create-topology-comparison-report demo-topologies nil 
                                       {:format format 
                                        :show-visualizations true 
                                        :show-analysis true})))

(comment

  ;; Example usage:
  (def demo-report (demonstrate-topologies :size 6 :format :ascii))
  (println demo-report)

  ;; Save SVG visualizations to files
  (let [couplings {"Linear" (topo/linear-coupling 5)
                    "Ring" (topo/ring-coupling 5)
                    "Star" (topo/star-coupling 5)}
        svg-visuals (compare-topologies-visually couplings :svg {:width 600 :height 400})]
    (doseq [[name svg] svg-visuals]
      (let [filename (str (str/lower-case (str/replace name " " "-")) "-topology.svg")]
        (spit filename svg)
        (println "Saved visualization to" filename))))

  ;; Basic topology visualization
  (def linear-5 (topo/linear-coupling 5))
  (println (visualize-topology linear-5 :ascii))

  ;; Create SVG visualization and save to file
  (def ring-svg (visualize-topology (topo/ring-coupling 6) :svg {:layout :circular}))
  ;; (spit "ring-topology.svg" ring-svg)

  ;; Compare multiple topologies visually
  (def demo-topologies {"Linear-5" (topo/linear-coupling 5)
                        "Ring-5" (topo/ring-coupling 5)
                        "Star-5" (topo/star-coupling 5)})

  (def ascii-comparisons (compare-topologies-visually demo-topologies :ascii))
  (doseq [[name viz] ascii-comparisons]
    (println (str "\n=== " name " ==="))
    (println viz))

  ;; Generate comprehensive comparison report
  (def test-circuit {:num-qubits 3
                     :operations [{:operation-type :h :operation-params {:target 0}}
                                  {:operation-type :cnot :operation-params {:control 0 :target 1}}
                                  {:operation-type :cnot :operation-params {:control 1 :target 2}}]})

  (println (create-topology-comparison-report demo-topologies test-circuit
                                              {:format :ascii
                                               :show-visualizations true
                                               :show-analysis true
                                               :show-optimization true}))

  ;; Demonstrate different topology types
  (println (demonstrate-topologies :size 6 :format :ascii))

  ;
  )