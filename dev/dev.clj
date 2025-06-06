(ns dev
  (:require [scicloj.clay.v2.api :as clay]))

(clay/make! {:source-path ["notebook/tutorial.clj"]
             :format [:html]
             :browse true})