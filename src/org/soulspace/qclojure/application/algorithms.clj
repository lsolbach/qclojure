(ns org.soulspace.qclojure.application.algorithms
  "Implementation of fundamental quantum algorithms using the qclojure domain"
  (:require [clojure.spec.alpha :as s]))

;; Specs for algorithm inputs and outputs
(s/def ::oracle-function fn?)
(s/def ::search-items (s/coll-of any?))
(s/def ::algorithm-result (s/keys :req-un [::result ::measurements ::circuit]))

