(ns clay
  "Tools for building the QClojure tutorial notebook."
  (:require [scicloj.clay.v2.api :as clay]))

(def tutorial-base-config
  {:base-source-path "notebook"
   :source-path ["tutorial.clj"]
   :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                 :branch "main"}
   :title "QClojure Tutorial"})

(def tutorial-html-config
  (merge tutorial-base-config
         {:format [:html]
          :base-target-path "docs"
          :clean-up-target-dir true
          :hide-ui-header true
          :hide-info-line false
          ;:live-reload true
          :browse true}))

(def tutorial-quarto-config
  (merge tutorial-base-config
         {:format [:quarto]
          :base-target-path "quarto"
          :clean-up-target-dir true
          :quarto {:highlight-style :solarized}
          :hide-ui-header true
          :hide-info-line false
          :live-reload false
          :browse false
          }))

(defn make-tutorial-notebook
  "Render the QClojure tutorial notebook."
  ([]
   (make-tutorial-notebook tutorial-html-config))
  ([config]
   (println "")
   (println "Rendering the tutorial...")
   (clay/make! config)))

(comment ; Clay 
  (make-tutorial-notebook)
  (make-tutorial-notebook tutorial-quarto-config)
  ;
  )
