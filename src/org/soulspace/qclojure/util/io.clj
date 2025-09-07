(ns org.soulspace.qclojure.util.io
  "Utility functions for file and resource I/O operations."
  (:require [clojure.java.io :as io]))

;; File I/O functions
(defn save-file
  "Save content to a file.
  
  Parameters:
  - content: SString content to save
  - filename: Output filename
  
  Returns:
  Path to saved file"
  [content filename]
  (spit filename content)
  filename)

(defn load-resource
  "Load a resource file from the classpath.
   
   Parameters:
   -path: Resource path relative to classpath root
   
   Returns:
   Sequence of lines from the resource"
  [path]
  (with-open [reader (io/reader (io/resource path))]
    (doall (line-seq reader))))
