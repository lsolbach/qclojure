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

(comment
  (run-tests)

  ;
  )