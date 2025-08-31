(ns clay
  "Tools for building the QClojure tutorial notebook."
  (:require [scicloj.clay.v2.api :as clay]))

(def tutorial-config
  {:base-target-path "docs"
   :clean-up-target-dir true
   :base-source-path "notebook"
   :source-path ["tutorial.clj"]
   :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                 :branch "main"}
   :format [:html]
   :title "QClojure Tutorial"
   :hide-ui-header true
   :hide-info-line false
   ;:live-reload true
   :browse true})

(def tutorial-quarto-config
  {:base-target-path "quarto"
   :clean-up-target-dir true
   :base-source-path "notebook"
   :source-path ["tutorial.clj"]
   :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                 :branch "main"}
   :format [:quarto]
   :quarto {:highlight-style :solarized}
   :title "QClojure Tutorial"
   :hide-ui-header true
   :hide-info-line false
   ;:live-reload true
   :browse true})

(defn make-tutorial-notebook
  "Render the QClojure tutorial notebook."
  ([]
   (make-tutorial-notebook tutorial-config))
  ([config]
   (println "")
   (println "Rendering the tutorial...")
   (clay/make! config)))

(comment ; Clay 
  (make-tutorial-notebook)
  (make-tutorial-notebook tutorial-quarto-config)
  ;
  )
