(ns org.soulspace.qclojure.domain.circuit-test
  "Tests for quantum circuit operations and composition"
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [fastmath.core :as m]
            [fastmath.complex :as fc]
            [org.soulspace.qclojure.domain.circuit :as qc]
            [org.soulspace.qclojure.domain.state :as qs]))

;;;
;;; Test Data and Utilities
;;;
(def ^:const pi Math/PI)
(def ^:const pi-2 (/ Math/PI 2))
(def ^:const pi-4 (/ Math/PI 4))

(defn amplitude-close?
  "Test if two complex amplitudes are approximately equal"
  [a b tolerance]
  (and (< (m/abs (- (.x a) (.x b))) tolerance)
       (< (m/abs (- (.y a) (.y b))) tolerance)))

(defn state-close?
  "Test if two quantum states are approximately equal"
  [state1 state2 tolerance]
  (and (= (:num-qubits state1) (:num-qubits state2))
       (= (count (:state-vector state1)) (count (:state-vector state2)))
       (every? identity
               (map #(amplitude-close? %1 %2 tolerance)
                    (:state-vector state1)
                    (:state-vector state2)))))

;;;
;;; Circuit Creation Tests
;;;
(deftest test-create-circuit
  (testing "Circuit creation with different qubit counts"
    (let [circuit-1 (qc/create-circuit 1)
          circuit-2 (qc/create-circuit 2)
          circuit-5 (qc/create-circuit 5)]
      
      (is (= (:num-qubits circuit-1) 1))
      (is (= (:operations circuit-1) []))
      
      (is (= (:num-qubits circuit-2) 2))
      (is (= (:operations circuit-2) []))
      
      (is (= (:num-qubits circuit-5) 5))
      (is (= (:operations circuit-5) [])))))

;;;
;;; Single Qubit Gate Tests
;;;
(deftest test-pauli-gates
  (testing "Pauli gates (X, Y, Z)"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "X gate"
        (let [x-circuit (qc/x-gate circuit 0)]
          (is (= (count (:operations x-circuit)) 1))
          (is (= (get-in x-circuit [:operations 0 :operation-type]) :x))
          (is (= (get-in x-circuit [:operations 0 :operation-params :target]) 0))))
      
      (testing "Y gate"
        (let [y-circuit (qc/y-gate circuit 1)]
          (is (= (count (:operations y-circuit)) 1))
          (is (= (get-in y-circuit [:operations 0 :operation-type]) :y))
          (is (= (get-in y-circuit [:operations 0 :operation-params :target]) 1))))
      
      (testing "Z gate"
        (let [z-circuit (qc/z-gate circuit 2)]
          (is (= (count (:operations z-circuit)) 1))
          (is (= (get-in z-circuit [:operations 0 :operation-type]) :z))
          (is (= (get-in z-circuit [:operations 0 :operation-params :target]) 2)))))))

(deftest test-hadamard-gate
  (testing "Hadamard gate"
    (let [circuit (qc/create-circuit 2)
          h-circuit (qc/h-gate circuit 0)]
      
      (is (= (count (:operations h-circuit)) 1))
      (is (= (get-in h-circuit [:operations 0 :operation-type]) :h))
      (is (= (get-in h-circuit [:operations 0 :operation-params :target]) 0)))))

(deftest test-phase-gates
  (testing "Phase gates (S, S†, T, T†)"
    (let [circuit (qc/create-circuit 4)]
      
      (testing "S gate"
        (let [s-circuit (qc/s-gate circuit 0)]
          (is (= (count (:operations s-circuit)) 1))
          (is (= (get-in s-circuit [:operations 0 :operation-type]) :s))
          (is (= (get-in s-circuit [:operations 0 :operation-params :target]) 0))))
      
      (testing "S† gate"
        (let [s-dag-circuit (qc/s-dag-gate circuit 1)]
          (is (= (count (:operations s-dag-circuit)) 1))
          (is (= (get-in s-dag-circuit [:operations 0 :operation-type]) :s-dag))
          (is (= (get-in s-dag-circuit [:operations 0 :operation-params :target]) 1))))
      
      (testing "T gate"
        (let [t-circuit (qc/t-gate circuit 2)]
          (is (= (count (:operations t-circuit)) 1))
          (is (= (get-in t-circuit [:operations 0 :operation-type]) :t))
          (is (= (get-in t-circuit [:operations 0 :operation-params :target]) 2))))
      
      (testing "T† gate"
        (let [t-dag-circuit (qc/t-dag-gate circuit 3)]
          (is (= (count (:operations t-dag-circuit)) 1))
          (is (= (get-in t-dag-circuit [:operations 0 :operation-type]) :t-dag))
          (is (= (get-in t-dag-circuit [:operations 0 :operation-params :target]) 3)))))))

(deftest test-rotation-gates
  (testing "Rotation gates (RX, RY, RZ)"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "RX gate"
        (let [rx-circuit (qc/rx-gate circuit 0 pi)]
          (is (= (count (:operations rx-circuit)) 1))
          (is (= (get-in rx-circuit [:operations 0 :operation-type]) :rx))
          (is (= (get-in rx-circuit [:operations 0 :operation-params :target]) 0))
          (is (= (get-in rx-circuit [:operations 0 :operation-params :angle]) pi))))
      
      (testing "RY gate"
        (let [ry-circuit (qc/ry-gate circuit 1 pi-2)]
          (is (= (count (:operations ry-circuit)) 1))
          (is (= (get-in ry-circuit [:operations 0 :operation-type]) :ry))
          (is (= (get-in ry-circuit [:operations 0 :operation-params :target]) 1))
          (is (= (get-in ry-circuit [:operations 0 :operation-params :angle]) pi-2))))
      
      (testing "RZ gate"
        (let [rz-circuit (qc/rz-gate circuit 2 pi-4)]
          (is (= (count (:operations rz-circuit)) 1))
          (is (= (get-in rz-circuit [:operations 0 :operation-type]) :rz))
          (is (= (get-in rz-circuit [:operations 0 :operation-params :target]) 2))
          (is (= (get-in rz-circuit [:operations 0 :operation-params :angle]) pi-4)))))))

(deftest test-phase-gate-with-angle
  (testing "Phase gate with custom angle"
    (let [circuit (qc/create-circuit 1)
          phase-circuit (qc/phase-gate circuit 0 pi-4)]
      
      (is (= (count (:operations phase-circuit)) 1))
      (is (= (get-in phase-circuit [:operations 0 :operation-type]) :phase))
      (is (= (get-in phase-circuit [:operations 0 :operation-params :target]) 0))
      (is (= (get-in phase-circuit [:operations 0 :operation-params :angle]) pi-4)))))

;;;
;;; Two Qubit Gate Tests
;;;
(deftest test-cnot-gate
  (testing "CNOT gate"
    (let [circuit (qc/create-circuit 3)
          cnot-circuit (qc/cnot-gate circuit 0 1)]
      
      (is (= (count (:operations cnot-circuit)) 1))
      (is (= (get-in cnot-circuit [:operations 0 :operation-type]) :cnot))
      (is (= (get-in cnot-circuit [:operations 0 :operation-params :control]) 0))
      (is (= (get-in cnot-circuit [:operations 0 :operation-params :target]) 1)))))

(deftest test-controlled-pauli-gates
  (testing "Controlled Pauli gates (CY, CZ)"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "CY gate"
        (let [cy-circuit (qc/cy-gate circuit 0 1)]
          (is (= (count (:operations cy-circuit)) 1))
          (is (= (get-in cy-circuit [:operations 0 :operation-type]) :cy))
          (is (= (get-in cy-circuit [:operations 0 :operation-params :control]) 0))
          (is (= (get-in cy-circuit [:operations 0 :operation-params :target]) 1))))
      
      (testing "CZ gate"
        (let [cz-circuit (qc/cz-gate circuit 1 2)]
          (is (= (count (:operations cz-circuit)) 1))
          (is (= (get-in cz-circuit [:operations 0 :operation-type]) :cz))
          (is (= (get-in cz-circuit [:operations 0 :operation-params :control]) 1))
          (is (= (get-in cz-circuit [:operations 0 :operation-params :target]) 2)))))))

(deftest test-controlled-rotation-gates
  (testing "Controlled rotation gates (CRX, CRY, CRZ)"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "CRX gate"
        (let [crx-circuit (qc/crx-gate circuit 0 1 pi)]
          (is (= (count (:operations crx-circuit)) 1))
          (is (= (get-in crx-circuit [:operations 0 :operation-type]) :crx))
          (is (= (get-in crx-circuit [:operations 0 :operation-params :control]) 0))
          (is (= (get-in crx-circuit [:operations 0 :operation-params :target]) 1))
          (is (= (get-in crx-circuit [:operations 0 :operation-params :angle]) pi))))
      
      (testing "CRY gate"
        (let [cry-circuit (qc/cry-gate circuit 1 2 pi-2)]
          (is (= (count (:operations cry-circuit)) 1))
          (is (= (get-in cry-circuit [:operations 0 :operation-type]) :cry))
          (is (= (get-in cry-circuit [:operations 0 :operation-params :control]) 1))
          (is (= (get-in cry-circuit [:operations 0 :operation-params :target]) 2))
          (is (= (get-in cry-circuit [:operations 0 :operation-params :angle]) pi-2))))
      
      (testing "CRZ gate"
        (let [crz-circuit (qc/crz-gate circuit 0 2 pi-4)]
          (is (= (count (:operations crz-circuit)) 1))
          (is (= (get-in crz-circuit [:operations 0 :operation-type]) :crz))
          (is (= (get-in crz-circuit [:operations 0 :operation-params :control]) 0))
          (is (= (get-in crz-circuit [:operations 0 :operation-params :target]) 2))
          (is (= (get-in crz-circuit [:operations 0 :operation-params :angle]) pi-4)))))))

(deftest test-swap-gates
  (testing "Swap gates (SWAP, iSWAP)"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "SWAP gate"
        (let [swap-circuit (qc/swap-gate circuit 0 1)]
          (is (= (count (:operations swap-circuit)) 1))
          (is (= (get-in swap-circuit [:operations 0 :operation-type]) :swap))
          (is (= (get-in swap-circuit [:operations 0 :operation-params :qubit1]) 0))
          (is (= (get-in swap-circuit [:operations 0 :operation-params :qubit2]) 1))))
      
      (testing "iSWAP gate"
        (let [iswap-circuit (qc/iswap-gate circuit 1 2)]
          (is (= (count (:operations iswap-circuit)) 1))
          (is (= (get-in iswap-circuit [:operations 0 :operation-type]) :iswap))
          (is (= (get-in iswap-circuit [:operations 0 :operation-params :qubit1]) 1))
          (is (= (get-in iswap-circuit [:operations 0 :operation-params :qubit2]) 2)))))))

;;;
;;; Multi-Qubit Gate Tests
;;;
(deftest test-toffoli-gate
  (testing "Toffoli (CCX) gate"
    (let [circuit (qc/create-circuit 4)
          toffoli-circuit (qc/toffoli-gate circuit 0 1 2)]
      
      (is (= (count (:operations toffoli-circuit)) 1))
      (is (= (get-in toffoli-circuit [:operations 0 :operation-type]) :toffoli))
      (is (= (get-in toffoli-circuit [:operations 0 :operation-params :control1]) 0))
      (is (= (get-in toffoli-circuit [:operations 0 :operation-params :control2]) 1))
      (is (= (get-in toffoli-circuit [:operations 0 :operation-params :target]) 2)))))

(deftest test-fredkin-gate
  (testing "Fredkin (CSWAP) gate"
    (let [circuit (qc/create-circuit 4)
          fredkin-circuit (qc/fredkin-gate circuit 0 1 2)]
      
      (is (= (count (:operations fredkin-circuit)) 1))
      (is (= (get-in fredkin-circuit [:operations 0 :operation-type]) :fredkin))
      (is (= (get-in fredkin-circuit [:operations 0 :operation-params :control]) 0))
      (is (= (get-in fredkin-circuit [:operations 0 :operation-params :target1]) 1))
      (is (= (get-in fredkin-circuit [:operations 0 :operation-params :target2]) 2)))))

;;;
;;; Measurement Tests
;;;
(deftest test-measure-operation
  (testing "Single qubit measurement operation"
    (let [circuit (qc/create-circuit 3)
          measure-circuit (qc/measure-operation circuit [0])]
      
      (is (= (count (:operations measure-circuit)) 1))
      (is (= (get-in measure-circuit [:operations 0 :operation-type]) :measure))
      (is (= (get-in measure-circuit [:operations 0 :operation-params :measurement-qubits]) [0]))))
  
  (testing "Multi-qubit measurement operation"
    (let [circuit (qc/create-circuit 3)
          measure-circuit (qc/measure-operation circuit [0 2])]
      
      (is (= (count (:operations measure-circuit)) 1))
      (is (= (get-in measure-circuit [:operations 0 :operation-type]) :measure))
      (is (= (get-in measure-circuit [:operations 0 :operation-params :measurement-qubits]) [0 2])))))

(deftest test-measure-all-operation
  (testing "Measure all qubits operation"
    (let [circuit (qc/create-circuit 3)
          measure-all-circuit (qc/measure-all-operation circuit)]
      
      (is (= (count (:operations measure-all-circuit)) 1))
      (is (= (get-in measure-all-circuit [:operations 0 :operation-type]) :measure))
      (is (= (get-in measure-all-circuit [:operations 0 :operation-params :measurement-qubits]) [0 1 2])))))

;;;
;;; Circuit Composition Tests
;;;
(deftest test-add-gate-and-operation
  (testing "Adding gates and operations to circuit"
    (let [circuit (qc/create-circuit 2)]
      
      (testing "Chain multiple gates"
        (let [multi-gate-circuit (-> circuit
                                     (qc/h-gate 0)
                                     (qc/cnot-gate 0 1)
                                     (qc/measure-operation [0 1]))]
          
          (is (= (count (:operations multi-gate-circuit)) 3))
          (is (= (get-in multi-gate-circuit [:operations 0 :operation-type]) :h))
          (is (= (get-in multi-gate-circuit [:operations 1 :operation-type]) :cnot))
          (is (= (get-in multi-gate-circuit [:operations 2 :operation-type]) :measure)))))))

;;;
;;; Predefined Circuit Tests
;;;
(deftest test-bell-state-circuit
  (testing "Bell state circuit"
    (let [bell-circuit (qc/bell-state-circuit)]
      
      (is (= (:num-qubits bell-circuit) 2))
      (is (= (count (:operations bell-circuit)) 2))
      (is (= (:name bell-circuit) "Bell State"))
      (is (= (:description bell-circuit) "Prepares (|00⟩ + |11⟩)/√2"))
      (is (= (get-in bell-circuit [:operations 0 :operation-type]) :h))
      (is (= (get-in bell-circuit [:operations 1 :operation-type]) :cnot)))))

(deftest test-ghz-state-circuit
  (testing "GHZ state circuit"
    (let [ghz-3 (qc/ghz-state-circuit 3)
          ghz-4 (qc/ghz-state-circuit 4)]
      
      (testing "3-qubit GHZ"
        (is (= (:num-qubits ghz-3) 3))
        (is (= (count (:operations ghz-3)) 3))
        (is (= (:name ghz-3) "GHZ State"))
        (is (= (:description ghz-3) "Prepares 3-qubit GHZ state"))
        (is (= (get-in ghz-3 [:operations 0 :operation-type]) :h))
        (is (= (get-in ghz-3 [:operations 1 :operation-type]) :cnot))
        (is (= (get-in ghz-3 [:operations 2 :operation-type]) :cnot)))
      
      (testing "4-qubit GHZ"
        (is (= (:num-qubits ghz-4) 4))
        (is (= (count (:operations ghz-4)) 4))
        (is (= (get-in ghz-4 [:operations 0 :operation-type]) :h))
        (is (every? #(= (:operation-type %) :cnot) (rest (:operations ghz-4))))))))

;;;
;;; Circuit Analysis Tests
;;;
(deftest test-circuit-depth
  (testing "Circuit depth calculation"
    (let [empty-circuit (qc/create-circuit 2)
          simple-circuit (-> (qc/create-circuit 2)
                            (qc/h-gate 0)
                            (qc/cnot-gate 0 1))
          complex-circuit (-> (qc/create-circuit 3)
                             (qc/h-gate 0)
                             (qc/h-gate 1)  ; parallel with previous H
                             (qc/cnot-gate 0 1)
                             (qc/x-gate 2)  ; parallel with CNOT
                             (qc/cnot-gate 1 2))]
      
      (is (= (qc/circuit-depth empty-circuit) 0))
      (is (= (qc/circuit-depth simple-circuit) 2))
      (is (= (qc/circuit-depth complex-circuit) 3)))))

(deftest test-circuit-gate-count
  (testing "Circuit gate count"
    (let [empty-circuit (qc/create-circuit 2)
          simple-circuit (-> (qc/create-circuit 2)
                            (qc/h-gate 0)
                            (qc/cnot-gate 0 1))
          circuit-with-measurement (-> simple-circuit
                                      (qc/measure-operation [0 1]))]
      
      (is (= (qc/circuit-gate-count empty-circuit) 0))
      (is (= (qc/circuit-gate-count simple-circuit) 2))
      (is (= (qc/circuit-gate-count circuit-with-measurement) 2)))))  ; measurements don't count

(deftest test-circuit-operation-count
  (testing "Circuit operation count (including measurements)"
    (let [empty-circuit (qc/create-circuit 2)
          simple-circuit (-> (qc/create-circuit 2)
                            (qc/h-gate 0)
                            (qc/cnot-gate 0 1))
          circuit-with-measurement (-> simple-circuit
                                      (qc/measure-operation [0 1]))]
      
      (is (= (qc/circuit-operation-count empty-circuit) 0))
      (is (= (qc/circuit-operation-count simple-circuit) 2))
      (is (= (qc/circuit-operation-count circuit-with-measurement) 3)))))

(deftest test-circuit-gate-types
  (testing "Circuit gate type counts"
    (let [circuit (-> (qc/create-circuit 3)
                      (qc/h-gate 0)
                      (qc/h-gate 1)
                      (qc/cnot-gate 0 1)
                      (qc/x-gate 2)
                      (qc/measure-operation [0 1 2]))
          gate-types (qc/circuit-gate-types circuit)]

      (is (= (:h gate-types) 2))
      (is (= (:cnot gate-types) 1))
      (is (= (:x gate-types) 1))
      (is (nil? (:measure gate-types)))))  ; measurements excluded
  )

(deftest test-circuit-operation-types
  (testing "Circuit operation type counts (including measurements)"
    (let [circuit (-> (qc/create-circuit 3)
                     (qc/h-gate 0)
                     (qc/h-gate 1)
                     (qc/cnot-gate 0 1)
                     (qc/x-gate 2)
                     (qc/measure-operation [0 1 2]))
          operation-types (qc/circuit-operation-types circuit)]
      
      (is (= (:h operation-types) 2))
      (is (= (:cnot operation-types) 1))
      (is (= (:x operation-types) 1))
      (is (= (:measure operation-types) 1)))))

;;;
;;; Circuit Execution Tests
;;;
(deftest test-execute-circuit
  (testing "Circuit execution"
    (let [bell-circuit (qc/bell-state-circuit)
          initial-state qs/|00⟩
          result-state (qc/execute-circuit bell-circuit initial-state)]
      
      (is (= (:num-qubits result-state) 2))
      (is (= (count (:state-vector result-state)) 4))
      
      ; Check that we get the expected Bell state: (|00⟩ + |11⟩)/√2
      ; The state vector should be [1/√2, 0, 0, 1/√2]
      (is (state-close? result-state
                       {:num-qubits 2
                        :state-vector [(fc/complex (/ 1 (Math/sqrt 2)) 0)
                                      (fc/complex 0 0)
                                      (fc/complex 0 0)
                                      (fc/complex (/ 1 (Math/sqrt 2)) 0)]}
                       1e-10)))))

;;;
;;; Circuit Transformation Tests
;;;
(deftest test-inverse-circuit
  (testing "Circuit inversion"
    (let [original-circuit (-> (qc/create-circuit 2)
                              (qc/h-gate 0)
                              (qc/cnot-gate 0 1)
                              (qc/rz-gate 1 pi-4))
          inverse-circuit (qc/inverse-circuit original-circuit)]
      
      (is (= (:num-qubits inverse-circuit) (:num-qubits original-circuit)))
      (is (= (count (:operations inverse-circuit)) (count (:operations original-circuit))))
      
      ; Check that operations are reversed and inverted
      ; Original: H, CNOT, RZ(π/4)
      ; Inverse: RZ(-π/4), CNOT, H
      (is (= (get-in inverse-circuit [:operations 0 :operation-type]) :rz))
      (is (= (get-in inverse-circuit [:operations 0 :operation-params :angle]) (- pi-4)))
      (is (= (get-in inverse-circuit [:operations 1 :operation-type]) :cnot))
      (is (= (get-in inverse-circuit [:operations 2 :operation-type]) :h)))))

(deftest test-inverse-circuit-with-measurements
  (testing "Circuit inversion correctly filters out measurement operations"
    (let [circuit-with-measurement (-> (qc/create-circuit 3)
                                      (qc/h-gate 0)
                                      (qc/cnot-gate 0 1)
                                      (qc/x-gate 2)
                                      (qc/measure-operation [0 1 2]))
          inverse-circuit (qc/inverse-circuit circuit-with-measurement)]
      
      ; Measurements should be filtered out
      (is (= (count (:operations inverse-circuit)) 3)) ; No measurement in inverse
      (is (= (:num-qubits inverse-circuit) (:num-qubits circuit-with-measurement)))
      
      ; Check that only gate operations are present and reversed
      ; Original gates: H(0), CNOT(0,1), X(2)
      ; Inverse: X(2), CNOT(0,1), H(0)
      (is (= (get-in inverse-circuit [:operations 0 :operation-type]) :x))
      (is (= (get-in inverse-circuit [:operations 0 :operation-params :target]) 2))
      (is (= (get-in inverse-circuit [:operations 1 :operation-type]) :cnot))
      (is (= (get-in inverse-circuit [:operations 2 :operation-type]) :h))
      (is (= (get-in inverse-circuit [:operations 2 :operation-params :target]) 0))))
  
  (testing "Circuit with only measurements produces empty inverse"
    (let [only-measurements (-> (qc/create-circuit 2)
                               (qc/measure-operation [0 1]))
          inverse-circuit (qc/inverse-circuit only-measurements)]
      
      (is (empty? (:operations inverse-circuit)))
      (is (= (:num-qubits inverse-circuit) (:num-qubits only-measurements)))))
  
  (testing "Measurement operations cannot be individually inverted"
    (let [measurement-op {:operation-type :measure
                         :operation-params {:measurement-qubits [0 1]}}]
      (is (nil? (qc/inverse-operation measurement-op))))))

;;;
;;; Global Gate Tests
;;;
(deftest test-global-gates
  (testing "Global gates apply to all qubits"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "Global X gate"
        (let [global-x-circuit (qc/global-x-gate circuit)]
          (is (= (count (:operations global-x-circuit)) 1))
          (is (= (get-in global-x-circuit [:operations 0 :operation-type]) :global-x))
          (is (nil? (get-in global-x-circuit [:operations 0 :operation-params])))))
      
      (testing "Global Y gate"
        (let [global-y-circuit (qc/global-y-gate circuit)]
          (is (= (count (:operations global-y-circuit)) 1))
          (is (= (get-in global-y-circuit [:operations 0 :operation-type]) :global-y))))
      
      (testing "Global Z gate"
        (let [global-z-circuit (qc/global-z-gate circuit)]
          (is (= (count (:operations global-z-circuit)) 1))
          (is (= (get-in global-z-circuit [:operations 0 :operation-type]) :global-z))))
      
      (testing "Global Hadamard gate"
        (let [global-h-circuit (qc/global-hadamard-gate circuit)]
          (is (= (count (:operations global-h-circuit)) 1))
          (is (= (get-in global-h-circuit [:operations 0 :operation-type]) :global-hadamard))))
      
      (testing "Global RX gate with angle"
        (let [global-rx-circuit (qc/global-rx-gate circuit pi)]
          (is (= (count (:operations global-rx-circuit)) 1))
          (is (= (get-in global-rx-circuit [:operations 0 :operation-type]) :global-rx))
          (is (= (get-in global-rx-circuit [:operations 0 :operation-params :angle]) pi))))
      
      (testing "Global RY gate with angle"
        (let [global-ry-circuit (qc/global-ry-gate circuit pi-2)]
          (is (= (count (:operations global-ry-circuit)) 1))
          (is (= (get-in global-ry-circuit [:operations 0 :operation-type]) :global-ry))
          (is (= (get-in global-ry-circuit [:operations 0 :operation-params :angle]) pi-2))))
      
      (testing "Global RZ gate with angle"
        (let [global-rz-circuit (qc/global-rz-gate circuit pi-4)]
          (is (= (count (:operations global-rz-circuit)) 1))
          (is (= (get-in global-rz-circuit [:operations 0 :operation-type]) :global-rz))
          (is (= (get-in global-rz-circuit [:operations 0 :operation-params :angle]) pi-4)))))))

;;;
;;; Rydberg Gate Tests (for neutral atom quantum computing)
;;;
(deftest test-rydberg-gates
  (testing "Rydberg gates for neutral atom quantum computing"
    (let [circuit (qc/create-circuit 3)]
      
      (testing "Rydberg CZ gate"
        (let [rydberg-cz-circuit (qc/rydberg-cz-gate circuit 0 1)]
          (is (= (count (:operations rydberg-cz-circuit)) 1))
          (is (= (get-in rydberg-cz-circuit [:operations 0 :operation-type]) :rydberg-cz))
          (is (= (get-in rydberg-cz-circuit [:operations 0 :operation-params :control]) 0))
          (is (= (get-in rydberg-cz-circuit [:operations 0 :operation-params :target]) 1))))
      
      (testing "Rydberg CPhase gate"
        (let [rydberg-cphase-circuit (qc/rydberg-cphase-gate circuit 0 1 pi-4)]
          (is (= (count (:operations rydberg-cphase-circuit)) 1))
          (is (= (get-in rydberg-cphase-circuit [:operations 0 :operation-type]) :rydberg-cphase))
          (is (= (get-in rydberg-cphase-circuit [:operations 0 :operation-params :control]) 0))
          (is (= (get-in rydberg-cphase-circuit [:operations 0 :operation-params :target]) 1))
          (is (= (get-in rydberg-cphase-circuit [:operations 0 :operation-params :angle]) pi-4))))
      
      (testing "Rydberg blockade gate"
        (let [rydberg-blockade-circuit (qc/rydberg-blockade-gate circuit [0 1] 0.0)]
          (is (= (count (:operations rydberg-blockade-circuit)) 1))
          (is (= (get-in rydberg-blockade-circuit [:operations 0 :operation-type]) :rydberg-blockade))
          (is (= (get-in rydberg-blockade-circuit [:operations 0 :operation-params :qubit-indices]) [0 1])))))))

;;;
;;; Circuit Printing Tests
;;;
(deftest test-print-circuit
  (testing "Circuit printing functionality"
    (let [unnamed-circuit (-> (qc/create-circuit 2)
                             (qc/h-gate 0)
                             (qc/cnot-gate 0 1))
          named-circuit (-> (qc/create-circuit 3 "Test Circuit")
                           (qc/x-gate 0)
                           (qc/y-gate 1)
                           (qc/z-gate 2))]
      
      (testing "Print unnamed circuit"
        ;; Note: print-circuit returns nil and prints to stdout
        ;; We can't easily test the output, but we can test it doesn't throw
        (is (nil? (qc/print-circuit unnamed-circuit))))
      
      (testing "Print named circuit"
        (is (nil? (qc/print-circuit named-circuit))))
      
      (testing "Print empty circuit"
        (is (nil? (qc/print-circuit (qc/create-circuit 1))))))))

;;;
;;; All Gates Circuit Test
;;;
(deftest test-all-gates-circuit
  (testing "Predefined circuit with all gate types"
    (let [all-gates (qc/all-gates-circuit)]
      
      (is (= (:num-qubits all-gates) 3))
      (is (> (count (:operations all-gates)) 15)) ; Should have many operations
      (is (= (:name all-gates) "All Gates"))
      (is (string? (:description all-gates)))
      
      ;; Check that various gate types are present
      (let [operation-types (map :operation-type (:operations all-gates))]
        (is (some #(= % :x) operation-types))
        (is (some #(= % :y) operation-types))
        (is (some #(= % :z) operation-types))
        (is (some #(= % :h) operation-types))
        (is (some #(= % :cnot) operation-types))))))

;;;
;;; Helper Function Tests
;;;
(deftest test-helper-functions
  (testing "Circuit analysis helper functions"

    (testing "spans-conflict? function"
      (is (true? (qc/spans-conflict? [0 1] [1 2]))) ; Overlapping spans
      (is (true? (qc/spans-conflict? [0 2] [1 3]))) ; Overlapping spans
      (is (false? (qc/spans-conflict? [0 1] [2 3]))) ; Non-overlapping spans
      (is (false? (qc/spans-conflict? [0 0] [1 1])))) ; Non-overlapping single points

    (testing "qubit-in-span? function"
      (is (true? (qc/qubit-in-span? 1 [[0 2]]))) ; Qubit within span
      (is (true? (qc/qubit-in-span? 0 [[0 2]]))) ; Qubit at start of span
      (is (true? (qc/qubit-in-span? 2 [[0 2]]))) ; Qubit at end of span
      (is (nil? (qc/qubit-in-span? 3 [[0 2]]))) ; Qubit outside span
      (is (true? (qc/qubit-in-span? 1 [[0 0] [1 2]]))) ; Qubit in second span
      (is (nil? (qc/qubit-in-span? 5 [[0 1] [2 3]])))) ; Qubit outside all spans

    (testing "safe-max function"
      (is (= 5 (qc/safe-max 0 [1 5 3 2]))) ; Maximum of non-empty collection
      (is (= 0 (qc/safe-max 0 []))) ; Default for empty collection
      (is (= -1 (qc/safe-max -1 []))) ; Custom default for empty collection
      (is (= 10 (qc/safe-max 0 [10]))) ; Single element collection
      (is (= 7 (qc/safe-max 0 [7 1 3 7]))))) ; Collection with duplicates
  )

;;;
;;; Measurement System Tests
;;;
(deftest test-measure-subsystem
  (testing "Measuring quantum subsystems"
    (let [bell-state (qc/execute-circuit (qc/bell-state-circuit) qs/|00⟩)]
      
      (testing "Measure single qubit from Bell state"
        (let [measurement-result (qc/measure-subsystem bell-state [0])]
          (is (contains? measurement-result :outcome))
          (is (contains? measurement-result :collapsed-state))
          (is (contains? measurement-result :probability))
          
          ;; Outcome should be 0 or 1
          (is (or (= (:outcome measurement-result) 0)
                  (= (:outcome measurement-result) 1)))
          
          ;; Probability should be approximately 0.5 for Bell state
          (is (< (Math/abs (- (:probability measurement-result) 0.5)) 0.01))
          
          ;; Collapsed state should have fewer qubits
          (is (= (:num-qubits (:collapsed-state measurement-result)) 1))))
      
      (testing "Measure multiple qubits"
        (let [measurement-result (qc/measure-subsystem bell-state [0 1])]
          (is (contains? measurement-result :outcome))
          (is (contains? measurement-result :collapsed-state))
          
          ;; When measuring both qubits of Bell state, outcome should be 0 or 3 (|00⟩ or |11⟩)
          (is (or (= (:outcome measurement-result) 0)
                  (= (:outcome measurement-result) 3)))
          
          ;; After measuring all qubits, no state remains
          (is (= (:num-qubits (:collapsed-state measurement-result)) 0)))))))

;;;
;;; Inverse Gate Tests  
;;;
(deftest test-inverse-gate
  (testing "Individual gate inversion"
    
    (testing "Pauli gates are self-inverse"
      (is (= (qc/inverse-gate :x) :x))
      (is (= (qc/inverse-gate :y) :y))
      (is (= (qc/inverse-gate :z) :z)))
    
    (testing "Hadamard gate is self-inverse"
      (is (= (qc/inverse-gate :h) :h)))
    
    (testing "S and S-dagger are inverses"
      (is (= (qc/inverse-gate :s) :s-dag))
      (is (= (qc/inverse-gate :s-dag) :s)))
    
    (testing "T and T-dagger are inverses"
      (is (= (qc/inverse-gate :t) :t-dag))
      (is (= (qc/inverse-gate :t-dag) :t)))
    
    (testing "Two-qubit gates"
      (is (= (qc/inverse-gate :cnot) :cnot)) ; CNOT is self-inverse
      (is (= (qc/inverse-gate :cz) :cz)) ; CZ is self-inverse
      (is (= (qc/inverse-gate :swap) :swap))) ; SWAP is self-inverse
    
    (testing "Unknown gates return nil"
      (is (nil? (qc/inverse-gate :unknown-gate))))))

(comment
  (run-tests)
  ;
  )