(ns org.soulspace.qclojure.adapter.io.qasm
  "QASM (Quantum Assembly Language) based import and export of quantum circuits.
   
   Contains implementations of the `io/export-quantum-circuit` and `io/import-quantum-circuit`
   multimethods for the `:qasm2` and `:qasm3` formats."
  (:require [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.adapter.io :as io]
            [org.soulspace.qclojure.application.format.qasm2 :as qasm2]
            [org.soulspace.qclojure.application.format.qasm3 :as qasm3]))

;; QASM 2.0 methods
(defmethod io/export-quantum-circuit :qasm2
  [_format circuit filename]
  (qio/save-file filename (qasm2/circuit-to-qasm circuit)))

(defmethod io/import-quantum-circuit :qasm2
  [_format filename]
  (qasm2/qasm-to-circuit (slurp filename)))

;; QASM 3.0 methods
(defmethod io/export-quantum-circuit :qasm3
  [_format circuit filename]
  (qio/save-file filename (qasm3/circuit-to-qasm circuit)))

(defmethod io/import-quantum-circuit :qasm3
  [_format filename]
  (qasm3/qasm-to-circuit (slurp filename)))
