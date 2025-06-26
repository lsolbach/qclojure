(ns org.soulspace.qclojure.domain.gate-decomposition-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.domain.gate-decomposition :as gd]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [clojure.spec.alpha :as s]))

(defn operations-of-type
  "Count operations of a specific type in a circuit."
  [circuit operation-type]
  (->> (:operations circuit)
       (filter #(= (:operation-type %) operation-type))
       count))

(comment
  (run-tests)
  ;
  )
