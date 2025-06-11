(ns dev
  (:require [scicloj.clay.v2.api :as clay]))

(clay/make! {:base-source-path "notebook"
             :source-path ["qclojure.clj"
                           "quantum_computing.clj"
                           "tutorial.clj"]
             :format [:html]
             :title "QClojure Docs"
             :hide-ui-header true
             :hide-info-line true
             ;:live-reload true
             :browse true})