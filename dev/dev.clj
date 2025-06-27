(ns dev
  (:require [clojure.repl :as repl]
            [scicloj.clay.v2.api :as clay]))

(comment ; Clay 
  (clay/make! {:base-source-path "notebook"
               :source-path ["tutorial.clj"]
               :format [:html]
               :title "QClojure Docs"
               :hide-ui-header true
               :hide-info-line true
               ;:live-reload true
               :browse true})
  ;
  )
