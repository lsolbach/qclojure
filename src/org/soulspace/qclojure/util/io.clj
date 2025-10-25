(ns org.soulspace.qclojure.util.io
  "Utility functions for file and resource I/O operations."
  (:require [clojure.java.io :as io]))

(defn write-edn
  "Write a Clojure object as EDN to a writer.
   
   Parameters:
    - w: Writer to write to
    - obj: Clojure object to serialize
   
   Returns: nil
   
   See https://ask.clojure.org/index.php/14203/how-to-persist-clojure-data-structures-to-disk?show=14205#a14205."
  [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

;; File I/O functions
(defn save-file
  "Save content to a file.
  
  Parameters:
  - content: SString content to save
  - filename: Output filename
  
  Returns:
  Path to saved file"
  [filename content]
  (io/make-parents filename)
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
