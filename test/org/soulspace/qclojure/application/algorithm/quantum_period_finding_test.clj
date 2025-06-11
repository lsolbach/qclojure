(ns org.soulspace.qclojure.application.algorithm.quantum-period-finding-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [org.soulspace.qclojure.domain.state :as qs]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.math :as qmath]
            [fastmath.core :as m]))

