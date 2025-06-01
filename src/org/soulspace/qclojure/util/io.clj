(ns org.soulspace.qclojure.util.io)

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

