(ns build-presentations
  "Tools for building the QClojure presentations."
  (:require [scicloj.clay.v2.api :as clay]))

(def presentations-base-config
  "Base configuration for building the presentations."
  {:base-source-path "notebooks"})

(def presentations-revealjs-config
  "Revealjs configuration for building the presentations."
  (merge presentations-base-config
         {:format [:quarto :revealjs]
          :quarto {:format {:revealjs {:theme :night
                                       :transition :fade
                                       :incremental false 
                                       :navigation-mode :linear ; :linear :vertical :grid
                                       :controls true
                                       :progress true
                                       :center true
                                       :width 1080
                                       :height 720}}}
          :clean-up-target-dir true
          :hide-ui-header true
          :hide-info-line false
          ;:live-reload true
          :browse true}))

(def fqc-qclojure-config
  "Configuration for building the 'Functional Quantum Computing with QClojure' presentation."
  (merge presentations-revealjs-config
         {:source-path ["qclojure_intro.clj"]
          :base-target-path "generated/presentation/FQC_QClojure"
          :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                        :branch "main"}
          :title "Functional Quantum Computing with QClojure"}))

  (defn make-presentations
    "Render the QClojure presentations."
    ([])
    ([config]
     (println "")
     (println "Building presentation with config:" config)
     (clay/make! config)))

  (comment ; Clay
    (clay/stop!)

    (clay/browse!)
    
    (make-presentations fqc-qclojure-config)

    )