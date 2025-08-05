(ns org.soulspace.qclojure.adapter.visualization.common-test
  "Unit tests for common visualization functions.

  This namespace contains tests for the shared functions used in different
  visualization formats, ensuring they work correctly across various scenarios."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.adapter.visualization.common :as common]
            [org.soulspace.qclojure.domain.state :as qs]))