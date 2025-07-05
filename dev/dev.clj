(ns dev
  (:require [scicloj.clay.v2.api :as clay]))

(comment ; Clay 
  (clay/make! {:base-source-path "notebook"
               :source-path ["tutorial.clj"]
               :remote-repo {:git-url "https://github.com/lsolbach/qclojure"
                             :branch "main"}
               :format [:html]
               :title "QClojure Tutorial"
               :hide-ui-header true
               :hide-info-line false
               ;:live-reload true
               :browse true})
  ;
  )
