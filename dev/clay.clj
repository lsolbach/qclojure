(ns clay
  "Tools for building the QClojure tutorial notebook."
  (:require [scicloj.clay.v2.api :as clay]))

(def tutorial-config
  {:base-source-path "notebook"
   :source-path ["tutorial.clj"]
   :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                 :branch "main"}
   :format [:html]
   :title "QClojure Tutorial"
   :hide-ui-header true
   :hide-info-line false
   ;:live-reload true
   :browse true})

(defn make-tutorial-notebook
  "Render the QClojure tutorial notebook to HTML."
  ([]
   (make-tutorial-notebook tutorial-config))
  ([config]
   (println "")
   (println "Rendering the tutorial...")
   (clay/make! config)))

(comment ; Clay 
  (make-tutorial-notebook)
  ;
  )
