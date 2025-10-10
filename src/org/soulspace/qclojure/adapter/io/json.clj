(ns org.soulspace.qclojure.adapter.io.json
  "JSON (JavaScript Object Notation) based import and export of quantum states, circuits, and data.

   Contains implementations of the `io/export-quantum-state`, `io/import-quantum-state`,
   `io/export-quantum-circuit`, `io/import-quantum-circuit`,
   `io/export-quantum-data`, and `io/import-quantum-data` multimethods for the `:json` format."
  (:require [clojure.data.json :as json]
            [org.soulspace.qclojure.util.io :as qio]
            [org.soulspace.qclojure.adapter.io :as io]))

(defmethod io/export-quantum-state :json
  [_format state filename]
  (let [json-data (json/write-str (io/serialize-quantum-state state))]
    (qio/save-file  filename json-data)))

(defmethod io/import-quantum-state :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-state (json/read-str json-data :key-fn keyword))))

(defmethod io/export-quantum-circuit :json
  [_format circuit filename]
  (let [json-data (json/write-str (io/serialize-quantum-circuit circuit))]
    (qio/save-file  filename json-data)))

(defmethod io/import-quantum-circuit :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-circuit (json/read-str json-data :key-fn keyword))))

(defmethod io/export-quantum-data :json
  [_format data filename]
  (let [json-data (json/write-str (io/serialize-quantum-data data))]
    (qio/save-file  filename json-data)))

(defmethod io/import-quantum-data :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-data (json/read-str json-data :key-fn keyword))))

