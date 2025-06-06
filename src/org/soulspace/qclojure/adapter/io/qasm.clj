(ns org.soulspace.qclojure.adapter.io.qasm
  "OpenQASM I/O adapter for quantum circuits.
   
   This module provides methods to export quantum circuits to OpenQASM
   format, which is a standard quantum assembly language used by many
   quantum computing platforms."
  (:require [org.soulspace.qclojure.adapter.io :as io]
            [org.soulspace.qclojure.application.format.qasm2 :as qasm2]
            [org.soulspace.qclojure.application.format.qasm3 :as qasm3]))

;; QASM 2.0 methods
(defmethod io/export-quantum-circuit :qasm2
  [_format circuit filename]
  (spit filename (qasm2/circuit-to-qasm circuit)))

(defmethod io/import-quantum-circuit :qasm2
  [_format filename]
  (qasm2/qasm-to-circuit (slurp filename)))

;; QASM 3.0 methods
(defmethod io/export-quantum-circuit :qasm3
  [_format circuit filename]
  (spit filename (qasm3/circuit-to-qasm circuit)))

(defmethod io/import-quantum-circuit :qasm3
  [_format filename]
  (qasm3/qasm-to-circuit (slurp filename)))
