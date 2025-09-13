(ns org.soulspace.qclojure.application.error-mitigation.zero-noise-test
  ; Leave require alone, we will need these namespaces!
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.error-mitigation.zero-noise :as zne]
            [org.soulspace.qclojure.domain.circuit :as circuit]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]
            [org.soulspace.qclojure.adapter.backend.hardware-simulator :as noisy]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.util.test :as util]))

(comment
  ;; Run all tests in namespace
  (run-tests)
  ;
  )
