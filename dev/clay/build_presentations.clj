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
          :quarto {:format {:revealjs {:author "Ludger Solbach"
                                       :theme :league ; :blood :dark :dracula :night :moon :league :beige :sky :simple :serif :solarized
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

(def ai-assist-qclojure-config
  "Configuration for building the 'Building QClojure with AI Assistance' presentation."
  (merge presentations-revealjs-config
         {:source-path ["qclojure_ai.clj"]
          :base-target-path "generated/presentation/AIAssist_QClojure"
          :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                        :branch "main"}
          :title "Building QClojure with AI Assistance"}))

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
    (make-presentations ai-assist-qclojure-config)

    )