(ns org.soulspace.qclojure.adapter.visualization.html
  "HTML page templates and styling for quantum visualizations"
  (:require [hiccup2.core :as h]
            [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.adapter.visualization.visualization :as viz]
            [org.soulspace.qclojure.adapter.visualization.common :as common]
            [org.soulspace.qclojure.domain.quantum-circuit :as qc]))

;;;
;;; HTML functions
;;;

;; CSS styling definition
(def quantum-visualization-css
  "Default CSS styling for quantum visualization HTML pages.
  
  Provides modern, responsive design with:
  - Gradient background
  - Card-based layout  
  - Professional typography
  - Responsive design
  - Clean spacing and shadows"
  "
  body { 
    font-family: 'SF Pro Display', 'Segoe UI', system-ui, sans-serif; 
    margin: 0; 
    padding: 20px; 
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
  }
  .container { 
    max-width: 1200px; 
    margin: 0 auto; 
    background: white; 
    padding: 20px; 
    border-radius: 12px; 
    box-shadow: 0 10px 25px rgba(0,0,0,0.1);
  }
  .visualization { 
    margin: 20px 0; 
    text-align: center; 
  }
  h1 { 
    color: #111827; 
    text-align: center; 
    margin-bottom: 30px; 
  }
  h2 { 
    color: #374151; 
    margin: 30px 0 15px 0; 
  }
  .description {
    color: #6b7280;
    font-size: 14px;
    margin-bottom: 15px;
    text-align: left;
  }")

(defn create-html-page-with-svg
  "Create complete HTML page containing SVG visualizations.
  
  Generates a modern, responsive HTML page with embedded SVG visualizations.
  Includes beautiful styling with gradient backgrounds and card-based layout.
  
  Parameters:
  - svg-elements: Vector of SVG strings to embed in the page
  - options: Page customization options
    :title - Page title (default 'Quantum Visualizations')  
    :description - Page description for meta tags and header
    :include-css - Include default CSS styling (default true)
  
  Returns:
  Complete HTML string ready for saving to file
  
  Example:
  (create-html-page-with-svg 
   [svg-chart svg-bloch] 
   :title \"Quantum Analysis\"
   :description \"Bell state preparation and measurement\")"
  [svg-elements & {:keys [title description include-css]
                   :or {title "Quantum Visualizations" include-css true}}]

  (str
    (h/html
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       [:title title]
       (when description
         [:meta {:name "description" :content description}])
       (when include-css
         [:style quantum-visualization-css])]
      [:body
       [:div.container
        [:h1 title]
        (when description
          [:p.description description])
        (map-indexed
          (fn [i svg]
            [:div.visualization {:key i}
             (h/raw svg)])
          svg-elements)]]])))

(defn create-minimal-html-page
  "Create minimal HTML page with custom CSS and content.
  
  For cases where you need more control over the page structure.
  
  Parameters:
  - content: Hiccup data structure for page content
  - options: Page options
    :title - Page title
    :css - Custom CSS string
    :meta - Additional meta tags
  
  Returns:
  HTML string"
  [content & {:keys [title css meta]}]
  
  (str
    (h/html
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
       (when title [:title title])
       (when meta meta)
       (when css [:style css])]
      [:body content]])))

;;;
;;; HTML Format Implementations
;;;
(defmethod viz/visualize-quantum-state :html
  [_format state & options]
  (let [svg-content (viz/visualize-bar-chart :svg state options)]
    (create-html-page-with-svg [svg-content]
                                    :title "Quantum State Visualization"
                                    :description "Interactive quantum state probability distribution")))

(defmethod viz/visualize-bloch-sphere :html
  [_format state & options]
  (let [svg-content (viz/visualize-bloch-sphere :svg state options)]
    (create-html-page-with-svg [svg-content]
                                    :title "Bloch Sphere Visualization"
                                    :description "Interactive Bloch sphere representation")))

(defmethod viz/visualize-circuit :html
  [_format circuit & options]
  (let [svg-content (viz/visualize-circuit :svg circuit options)]
    (create-html-page-with-svg [svg-content]
                                    :title "Quantum Circuit Diagram"
                                    :description "Interactive quantum circuit visualization")))

(defmethod viz/visualize-bar-chart :html
  [_format state & options]
  (let [svg-content (viz/visualize-bar-chart :svg state options)]
    (create-html-page-with-svg [svg-content]
                                    :title "Probability Distribution"
                                    :description "Interactive probability bar chart")))

(defmethod viz/visualize-state-evolution :html
  [_format circuit initial-state & options]
  ;; Create multiple SVG frames and combine into HTML page
  (let [frames (apply viz/visualize-state-evolution circuit initial-state options)
        svg-frames (map #(viz/visualize-bar-chart :svg (:state %) :width 400 :height 300) frames)
        combined-html (create-html-page-with-svg svg-frames
                                                      :title "Quantum State Evolution"
                                                      :description "Step-by-step state evolution through circuit execution")]
    combined-html))

(defmethod viz/visualize-algorithm-summary :html
  [_format algorithm-result & options]
  ;; Create HTML page with algorithm summary and any relevant visualizations
  (let [ascii-summary (apply viz/visualize-algorithm-summary :ascii algorithm-result options)
        ;; Convert ASCII to HTML with proper formatting
        html-summary (str "<div class='algorithm-summary'><pre>" ascii-summary "</pre></div>")]
    (create-html-page-with-svg []
                               :title "Algorithm Summary"
                               :description "Quantum algorithm execution summary"
                               :additional-content html-summary)))


(comment
  ;; REPL examples for HTML page generation

  ;; Create simple HTML page with multiple SVGs
  (def sample-svgs
    ["<svg><circle cx='50' cy='50' r='20'/></svg>"
     "<svg><rect x='10' y='10' width='30' height='30'/></svg>"])

  (def html-page
    (create-html-page-with-svg
     sample-svgs
     :title "Quantum Visualization Demo"
     :description "Demonstration of quantum state visualizations"))

  ;; Save to file
  (qio/save-file html-page "demo.html")

  ;; Create minimal custom page
  (def custom-page
    (create-minimal-html-page
     [:div
      [:h1 "Custom Quantum Page"]
      [:p "This is a custom layout"]]
     :title "Custom Page"
     :css "body { background: #f0f0f0; }"))

  (qio/save-file custom-page "custom.html")

  ;; Create circuit diagram
  (require '[org.soulspace.qclojure.domain.quantum-state :as qs]
           '[org.soulspace.qclojure.domain.quantum-gate :as qg]
           '[org.soulspace.qclojure.domain.quantum-circuit :as qc])

  (def bell-state (-> (qs/zero-state 2)
                      (qg/h-gate 0)
                      (qg/cnot)))
  (def bell-circuit (qc/bell-state-circuit))
  (def circuit-html (viz/visualize-circuit :html bell-circuit))
  (qio/save-file circuit-html "bell-circuit.html")

  (def complex-circuit (-> (qc/create-circuit 3 "Complex Circuit")
                           (qc/h-gate 0)          ;; Layer 1
                           (qc/h-gate 1)          ;; Also Layer 1 (parallel with previous)
                           (qc/cnot-gate 0 2)     ;; Layer 2
                           (qc/x-gate 1)          ;; Also Layer 2 (parallel with previous)
                           (qc/cnot-gate 1 2))    ;; Layer 3 (must wait for previous gates)
    )
  (def complex-circuit-html (viz/visualize-circuit :html complex-circuit))
  (qio/save-file complex-circuit-html "complex-circuit.html")
  
  ;
  )
