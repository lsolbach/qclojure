(ns clay
  "Tools for building the QClojure tutorial notebook."
  (:require [scicloj.clay.v2.api :as clay]))

(def tutorial-base-config
  "Base configuration for building the tutorial."
  {:base-source-path "notebook"
   :source-path ["tutorial.clj"]
   :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                 :branch "main"}
   :title "QClojure Tutorial"})

(def tutorial-html-config
  "HTML configuration for building the tutorial."
  (merge tutorial-base-config
         {:format [:html]
          :base-target-path "docs"
          :clean-up-target-dir true
          :hide-ui-header true
          :hide-info-line false
          ;:live-reload true
          :browse true}))

(def tutorial-quarto-config
  "Quarto configuration for building the tutorial."
  (merge tutorial-base-config
         {:format [:quarto :html]
          :base-target-path "generated/quarto"
          :clean-up-target-dir true
          :quarto {:highlight-style :solarized}
          :hide-ui-header true
          :hide-info-line false
          :live-reload false
          :browse false }))

(def tutorial-gfm-config
  "Github flavoured markdown configuration for building the tutorial."
  (merge tutorial-base-config
         {:format [:quarto :gfm]
          :base-target-path "generated/gfm"
          :clean-up-target-dir true
          :hide-ui-header true
          :hide-info-line false
          :live-reload false
          :browse false}))

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
  (make-tutorial-notebook tutorial-gfm-config)
  ;
  )
