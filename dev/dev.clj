(ns dev
  (:require [scicloj.clay.v2.api :as clay]))

(clay/make! {:base-source-path "notebook"
             :source-path ["tutorial.clj"]
             :format [:html]
             :title "QCLojure Docs"
             :hide-ui-header true
             :hide-info-line true
             ;:live-reload true
             :browse true})