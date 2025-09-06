(ns org.soulspace.qclojure.adapter.io.edn
  "EDN (Extensible Data Notation) based import and export of quantum states, circuits, and data.
   
   Contains implementations of the `io/export-quantum-state`, `io/import-quantum-state`,
   `io/export-quantum-circuit`, `io/import-quantum-circuit`,
   `io/export-quantum-data`, and `io/import-quantum-data` multimethods for the `:edn` format."
  (:require [clojure.edn :as edn]
            [org.soulspace.qclojure.adapter.io :as io]))

(defmethod io/export-quantum-state :edn
  [_format state filename]
  ((io/serialize-quantum-state state)
   (spit filename (pr-str (io/serialize-quantum-state state)))))

(defmethod io/import-quantum-state :edn
  [_format filename]
  (io/deserialize-quantum-state (edn/read-string (slurp filename))))

(defmethod io/export-quantum-circuit :edn
  [_format circuit filename]
  (spit filename (pr-str (io/serialize-quantum-circuit circuit))))

(defmethod io/import-quantum-circuit :edn
  [_format filename]
  (edn/read-string (slurp filename)))

(defmethod io/export-quantum-data :edn
  [_format data filename]
  (spit filename (pr-str (io/serialize-quantum-data data))))

(defmethod io/import-quantum-data :edn
  [_format filename]
  (io/deserialize-quantum-data (edn/read-string (slurp filename))))
