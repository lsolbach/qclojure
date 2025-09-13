(ns org.soulspace.qclojure.domain.operation-registry-test
  (:require [clojure.test :refer [deftest testing is are run-tests]]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [org.soulspace.qclojure.domain.operation-registry :as opreg]))

(deftest operation-catalog-structure-test
  (testing "Operation catalog is properly structured"
    (is (map? opreg/operation-catalog))
    (is (not-empty opreg/operation-catalog))
    
    (testing "All operations have required fields"
      (doseq [[op-id op-def] opreg/operation-catalog]
        (is (keyword? op-id) (str "Operation ID should be keyword: " op-id))
        (is (map? op-def) (str "Operation definition should be map: " op-id))
        (is (contains? op-def :operation-kind) (str "Missing :operation-kind for " op-id))
        (is (contains? op-def :operation-id) (str "Missing :operation-id for " op-id))
        (is (contains? op-def :operation-type) (str "Missing :operation-type for " op-id))
        (is (contains? op-def :description) (str "Missing :description for " op-id))
        
        ;; Verify operation-id matches key
        (is (= op-id (:operation-id op-def)) 
            (str "Operation ID mismatch for " op-id))))
    
    (testing "All operations conform to spec"
      (doseq [[op-id op-def] opreg/operation-catalog]
        (is (s/valid? ::opreg/operation-definition op-def)
            (str "Operation definition invalid: " op-id 
                 "\nErrors: " (s/explain-str ::opreg/operation-definition op-def)))))))

(deftest operation-kinds-test
  (testing "Operation kinds are valid"
    (is (set? opreg/operation-kinds))
    (is (contains? opreg/operation-kinds :gate))
    (is (contains? opreg/operation-kinds :measurement))
    
    (testing "All operations use valid kinds"
      (let [catalog-kinds (into #{} (map #(:operation-kind (val %)) opreg/operation-catalog))]
        (is (every? opreg/operation-kinds catalog-kinds)
            (str "Invalid operation kinds found: " 
                 (set/difference catalog-kinds opreg/operation-kinds)))))))

(deftest operation-types-test
  (testing "Operation types are valid"
    (is (set? opreg/operation-types))
    (is (contains? opreg/operation-types :single-qubit))
    (is (contains? opreg/operation-types :two-qubit))
    (is (contains? opreg/operation-types :multi-qubit))
    (is (contains? opreg/operation-types :parametric))
    
    (testing "All operations use valid types"
      (let [catalog-types (into #{} (map #(:operation-type (val %)) opreg/operation-catalog))]
        (is (every? opreg/operation-types catalog-types)
            (str "Invalid operation types found: " 
                 (set/difference catalog-types opreg/operation-types)))))))

(deftest measure-operation-test
  (testing "Measure operation is properly defined"
    (let [measure-op (get opreg/operation-catalog :measure)]
      (is (some? measure-op) "Measure operation should exist")
      (is (= :measurement (:operation-kind measure-op)))
      (is (= :measure (:operation-id measure-op))) 
      (is (= :single-qubit (:operation-type measure-op)))
      (is (string? (:description measure-op)))
      (is (not-empty (:description measure-op))))))

(deftest gate-sets-test
  (testing "Predefined gate sets are valid"
    (testing "Universal gate set"
      (is (set? opreg/universal-gate-set))
      (is (contains? opreg/universal-gate-set :h))
      (is (contains? opreg/universal-gate-set :t))
      (is (contains? opreg/universal-gate-set :cnot))
      
      ;; All gates in the set should exist in catalog
      (is (every? #(contains? opreg/operation-catalog %) opreg/universal-gate-set)
          "All gates in universal set should exist in catalog"))
    
    (testing "Basic gate set"
      (is (set? opreg/basic-gate-set))
      (is (every? #(contains? opreg/operation-catalog %) opreg/basic-gate-set)
          "All gates in basic set should exist in catalog"))
    
    (testing "Parametric gate set"
      (is (set? opreg/parametric-gate-set))
      (is (every? #(contains? opreg/operation-catalog %) opreg/parametric-gate-set)
          "All gates in parametric set should exist in catalog"))
    
    (testing "Native simulator gate set"
      (is (set? opreg/native-simulator-gate-set))
      (is (every? #(contains? opreg/operation-catalog %) opreg/native-simulator-gate-set)
          "All gates in simulator set should exist in catalog"))))

(deftest gate-aliases-test
  (testing "Gate aliases are properly configured"
    (is (map? opreg/gate-aliases))
    
    (testing "All alias targets exist in catalog"
      (doseq [[alias target] opreg/gate-aliases]
        (is (contains? opreg/operation-catalog target)
            (str "Alias target does not exist: " alias " -> " target))))
    
    (testing "Alias resolution works"
      (is (= :x (opreg/resolve-gate-alias :not)))
      (is (= :x (opreg/resolve-gate-alias :bit-flip)))
      (is (= :z (opreg/resolve-gate-alias :phase-flip)))
      (is (= :h (opreg/resolve-gate-alias :h))) ; Non-alias returns itself
      )
    
    (testing "get-gate-info works with aliases"
      (let [not-gate (opreg/get-gate-info :not)
            x-gate (opreg/get-gate-info :x)]
        (is (= not-gate x-gate) "Alias should resolve to same gate info")))))

(deftest utility-functions-test
  (testing "get-gate-info function"
    (let [x-gate (opreg/get-gate-info :x)]
      (is (some? x-gate))
      (is (= :x (:operation-id x-gate)))
      (is (= :gate (:operation-kind x-gate))))
    
    (is (nil? (opreg/get-gate-info :nonexistent-gate))))
  
  (testing "get-gates-by-type function"
    (let [single-qubit-gates (opreg/get-gates-by-type :single-qubit)
          two-qubit-gates (opreg/get-gates-by-type :two-qubit)]
      (is (set? single-qubit-gates))
      (is (set? two-qubit-gates))
      (is (contains? single-qubit-gates :x))
      (is (contains? single-qubit-gates :measure)) ; Our new measurement operation
      (is (contains? two-qubit-gates :cnot))
      (is (not (contains? single-qubit-gates :cnot)))
      (is (not (contains? two-qubit-gates :x)))))
  
  (testing "validate-gate-set function"
    (is (opreg/validate-gate-set #{:x :y :z}))
    (is (opreg/validate-gate-set #{:not :phase-flip})) ; With aliases
    (is (not (opreg/validate-gate-set #{:x :y :nonexistent-gate}))))
  
  (testing "normalize-gate-set function"
    (is (= #{:x :z :h} (opreg/normalize-gate-set #{:not :phase-flip :h})))
    (is (= #{:x :y :z} (opreg/normalize-gate-set #{:x :y :z})))) ; No aliases
  
  (testing "get-gate-dependencies function"
    (let [x-deps (opreg/get-gate-dependencies :x)
          h-deps (opreg/get-gate-dependencies :h)]
      (is (vector? x-deps))
      (is (vector? h-deps))
      ;; H gate should have no decomposition (it's fundamental)
      (is (empty? h-deps))))
  
  (testing "expand-gate-set function"
    (let [expanded (opreg/expand-gate-set #{:s})]
      (is (set? expanded))
      (is (contains? expanded :s))
      ;; S decomposes to T gates, so T should be in expanded set
      (when (not-empty (opreg/get-gate-dependencies :s))
        (is (contains? expanded :t))))))

(deftest pauli-gates-test
  (testing "Pauli gates are properly defined"
    (are [gate-id] (and (contains? opreg/operation-catalog gate-id)
                        (= :gate (:operation-kind (get opreg/operation-catalog gate-id)))
                        (= :single-qubit (:operation-type (get opreg/operation-catalog gate-id))))
      :x :y :z :i)))

(deftest rotation-gates-test
  (testing "Rotation gates are properly defined"
    (are [gate-id] (and (contains? opreg/operation-catalog gate-id)
                        (= :gate (:operation-kind (get opreg/operation-catalog gate-id)))
                        (= :parametric (:operation-type (get opreg/operation-catalog gate-id)))
                        (= 1 (:parameter-count (get opreg/operation-catalog gate-id))))
      :rx :ry :rz)))

(deftest two-qubit-gates-test
  (testing "Two-qubit gates are properly defined"
    (are [gate-id] (and (contains? opreg/operation-catalog gate-id)
                        (= :gate (:operation-kind (get opreg/operation-catalog gate-id)))
                        (= :two-qubit (:operation-type (get opreg/operation-catalog gate-id))))
      :cnot :cz :cy :swap)))

(deftest decomposition-consistency-test
  (testing "Decompositions reference valid operations"
    (doseq [[op-id op-def] opreg/operation-catalog]
      (when-let [decomp (:decomposition op-def)]
        (when (map? decomp)
          ;; Check various decomposition types
          (when-let [universal (:universal decomp)]
            (when (vector? universal)
              (doseq [dep-gate universal]
                (when (keyword? dep-gate)
                  (is (contains? opreg/operation-catalog dep-gate)
                      (str "Decomposition dependency not found: " op-id " -> " dep-gate))))))
          
          ;; For now, just verify the structure exists - detailed testing would need more context
          (when-let [param-fn (:parametric-fn decomp)]
            (is (fn? param-fn) (str "parametric-fn should be a function for " op-id))))))))

(deftest measurement-vs-gates-test
  (testing "Operations are correctly categorized"
    (let [gates (filter #(= :gate (:operation-kind (val %))) opreg/operation-catalog)
          measurements (filter #(= :measurement (:operation-kind (val %))) opreg/operation-catalog)]
      
      (is (pos? (count gates)) "Should have gate operations")
      (is (pos? (count measurements)) "Should have measurement operations")
      
      ;; Ensure measure operation is in measurements
      (is (some #(= :measure (key %)) measurements)
          "Measure operation should be categorized as measurement"))))

(comment
  ;; Run all tests in this namespace
  (run-tests)

  ;; Test the new measure operation
  (opreg/get-gate-info :measure)
  
  ;; Check all operations
  (keys opreg/operation-catalog)
  
  ;; Test gate sets
  opreg/universal-gate-set
  opreg/basic-gate-set
  
  ;; Test aliases
  (opreg/resolve-gate-alias :not)
  (opreg/get-gate-info :not)
  
  ;; Test validation
  (opreg/validate-gate-set #{:x :y :z :measure})
  ;  
  )