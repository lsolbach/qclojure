(ns org.soulspace.qclojure.application.visualization
  "Core visualization API - unified interface to quantum visualization tools.
  
  This namespace provides a single entry point for all quantum visualization
  functionality. It now supports both the new unified multimethod API and
  maintains backward compatibility with the original delegation-based API.
  
  New Unified API:
  - visualize (format, data, options) - multimethod-based unified interface
  
  Legacy API:
  - ascii-*, svg-*, html-* functions - maintained for backward compatibility"
  )

(defn visualization-format
  "Return the visualization `format` for the given input."
  [format & _rest]
  format)

;;
;; Visualization API
;;
(defmulti visualize-quantum-state
  "Visualize quantum state in specified format.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - state: Quantum state to visualize
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted visualization"
  visualization-format)

(defmulti visualize-bloch-sphere
  "Visualize single-qubit state on Bloch sphere in specified format.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - state: Single-qubit quantum state
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted Bloch sphere visualization"
  visualization-format)

(defmulti visualize-circuit
  "Visualize quantum circuit in specified format.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - circuit: Quantum circuit to visualize
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted circuit diagram"
  visualization-format)

(defmulti visualize-bar-chart
  "Visualize probability distribution as bar chart in specified format.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - state: Quantum state to visualize
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted bar chart"
  visualization-format)

(defmulti visualize-state-evolution
  "Visualize quantum state evolution through circuit execution.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - circuit: Quantum circuit
  - initial-state: Starting quantum state
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted state evolution visualization"
  visualization-format)

(defmulti visualize-algorithm-summary
  "Visualize quantum algorithm execution summary.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - algorithm-result: Algorithm execution result map
  - options: Format-specific visualization options
  
  Returns:
  String containing formatted algorithm summary"
  visualization-format)

(defmulti visualize-measurement-histogram
  "Visualize measurement results as histogram in specified format.
  
  Dispatches on format keyword (:ascii, :svg, :html).
  
  Parameters:
  - format: Visualization format (:ascii, :svg, :html)
  - measurement-results: Map of measurement outcomes to counts
  - options: Format-specific visualization options
    - :threshold - Minimum count threshold (default 1)
    - :max-bars - Maximum number of bars (default 16)
    - :width, :height - Chart dimensions
    - :title - Chart title
  
  Returns:
  String containing formatted measurement histogram"
  visualization-format)

(defmulti visualize-hardware-topology
  "Visualize hardware topology in specified format.
  
  Dispatches on format keyword (:ascii, :svg).
  
  Parameters:
  - format: Visualization format (:ascii, :svg)
  - topology: Hardware topology as vector of vectors (adjacency list)
  - options: Format-specific visualization options
    Common options:
    - :layout - Layout algorithm (:auto, :grid, :circular, :force, :hierarchical)
    - :show-labels - Whether to show qubit labels (default true)
    - :show-connectivity - Whether to show connections (default true)
    - :show-info - Whether to show topology information (default true)
    ASCII-specific options:
    - :style - Visualization style (:grid, :linear)
    SVG-specific options:
    - :width, :height - Canvas dimensions (default 600x400)
    - :interactive - Enable hover effects (default true)
  
  Returns:
  String containing formatted topology visualization"
  visualization-format)

(comment
  ;; REPL examples for both the new unified API and legacy API
  (require '[org.soulspace.qclojure.domain.circuit :as qc])

  (def cz-circuit
    (-> (qc/create-circuit 2 "CZ Test")
        (qc/h-gate 0)
        (qc/h-gate 1)
        (qc/cz-gate 0 1)))
  (visualize-circuit :ascii cz-circuit)
  (visualize-circuit :svg cz-circuit)
  (spit "viz.html" (visualize-circuit :html cz-circuit))
  )
