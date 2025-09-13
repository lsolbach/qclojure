(ns org.soulspace.qclojure.application.backend-test
  "Tests for quantum backend protocol and gate support functionality."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [org.soulspace.qclojure.application.backend :as backend]
            [org.soulspace.qclojure.adapter.backend.ideal-simulator :as sim]))

(deftest test-simulator-backend
  (testing "Simulator creation and gate support"
    (let [simulator (sim/create-simulator)]
      (is (satisfies? backend/QuantumBackend simulator))

      (testing "Backend info"
        (let [info (backend/backend-info simulator)]
          (is (= :simulator (:backend-type info)))
          (is (string? (:backend-name info))))))))

(comment
  ;; Run all tests in this namespace
  (run-tests)
  ;
  )
