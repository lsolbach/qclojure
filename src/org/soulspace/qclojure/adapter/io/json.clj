(ns org.soulspace.qclojure.adapter.io.json
  "JSON I/O adapter for quantum circuits and data.
   
   This module provides methods to export and import quantum circuits
   and data in JSON format, allowing for easy serialization and
   deserialization of quantum information."
  (:require [clojure.data.json :as json]
            [org.soulspace.qclojure.adapter.io :as io]))

(defmethod io/export-quantum-state :json
  [_format state filename]
  (let [json-data (json/write-str (io/serialize-quantum-state state))]
    (spit filename json-data)))

(defmethod io/import-quantum-state :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-state (json/read-str json-data :key-fn keyword))))

(defmethod io/export-quantum-circuit :json
  [_format circuit filename]
  (let [json-data (json/write-str (io/serialize-quantum-circuit circuit))]
    (spit filename json-data)))

(defmethod io/import-quantum-circuit :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-circuit (json/read-str json-data :key-fn keyword))))

(defmethod io/export-quantum-data :json
  [_format data filename]
  (let [json-data (json/write-str (io/serialize-quantum-data data))]
    (spit filename json-data)))

(defmethod io/import-quantum-data :json
  [_format filename]
  (let [json-data (slurp filename)]
    (io/deserialize-quantum-data (json/read-str json-data :key-fn keyword))))

