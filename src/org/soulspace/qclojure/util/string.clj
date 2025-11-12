(ns org.soulspace.qclojure.util.string
  "Utility functions for string manipulation."
  (:require [clojure.string :as str]))

(defn upper-case?
  "Returns true if the char is upper case."
  [^Character c]
  (Character/isUpperCase c))

(defn lower-case?
  "Returns true if the char is lower case."
  [^Character c]
  (Character/isLowerCase c))

(defn camel-case?
  "Returns true if the string 's' is in camel case format."
  [^String s]
  (some upper-case? s))

(defn kebab-case?
  "Returns true if the string 's' is in kebab case format (lower case with hyphens)."
  [^String s]
  (and (= s (str/lower-case s))
       (some #(= % \-) s)))

(defn snake-case?
  "Returns true if the string 's' is in snake case format (lower case with underscores)."
  [^String s]
  (and (= s (str/lower-case s))
       (some #(= % \_) s)))

(defn to-camel-case
  "Converts a string 's' into camel case. Removes occurences of the character
  'c' and converts the next character to upper case.
   
   Parameters:
    - s: Input string
    - c: Character to remove and capitalize next character (default: \\-)
   
   Returns: Camel case string
   
   Examples:
    (to-camel-case \"to-camel-case\") -> \"toCamelCase\"
    (to-camel-case \"from-camel-case-string\" \\_) -> \"fromCamelCaseString\""
  ([^String s] (to-camel-case s \-))
  ([^String s ^Character c]
   (loop [chars (seq s) cc-chars []]
     (if (seq chars)
       (if (= (first chars) c)
         (recur (rest (rest chars)) (conj cc-chars (str/upper-case (second chars))))
         (recur (rest chars) (conj cc-chars (str (first chars)))))
       (apply str cc-chars)))))

(defn from-camel-case
  "Converts a string 's' from camel case to a lower case string with the spacer character
   'c' inserted in front of intra word uppercase chars. Spacer chars are not inserted into
   upper case abbreviations. The case of the chars is retained.
   
   Parameters:
    - s: Input camel case string
    - c: Character to insert before upper case chars (default: \\-)
    
   Returns: String with spacer characters inserted
  
   Examples:
    (from-camel-case \\- \"fromCamelCase\") -> \"from-camel-case\"
    (from-camel-case \\- \"getHTTPRequest\") -> \"get-http-request\""
  ([^String s]
   (from-camel-case s \-))
  ([^String s ^Character c]
  (loop [chars (seq s) r-chars [] start? true in-upper? false]
    (if (seq chars)
      (let [current-char (char (first chars))]
        (if (not (or (Character/isDigit current-char)
                     (Character/isLetter current-char)))
          ;; special char or white space, replace with c
          (recur (rest chars) (conj r-chars c) false false)
          (if (or (lower-case? current-char) (Character/isDigit current-char))
            ;; lower case or digit, don't add spacer char
            (recur (rest chars) (conj r-chars current-char) false false)
            (if start?
              ;; start of word, don't add spacer
              (recur (rest chars) (conj r-chars current-char) false true)
              (if-not (seq (rest chars))
                ;; last char, dont add spacer
                (recur (rest chars) (conj r-chars current-char) false true)
                ;; not the last char of the string
                (if in-upper?
                  (if (upper-case? (fnext chars))
                    ;; in an upper case word and the next char is upper case too
                    ;; don't add spacer here
                    (recur (rest chars) (conj r-chars current-char) false true)
                    ;; in an upper case word but the next char is lower case
                    ;; add a spacer char in front of the last upper case char
                    (recur (rest chars) (conj r-chars c current-char) false true))
                  ;; first upper case char after a lower case char, add spacer char
                  (recur (rest chars) (conj r-chars c current-char) false true)))))))
      (str/lower-case (apply str r-chars))))))

(defn camel-case-to-kebab-case
  "Converts a camel case string to kebab case (lower case with hyphens)."
  [^String s]
  (str/lower-case (from-camel-case s \-)))

(defn camel-case-to-snake-case
  "Converts a camel case string to snake case (lower case with underscores)."
  [^String s]
  (str/lower-case (from-camel-case s \_)))

(defn kebab-case-to-camel-case
  "Converts a kebab case string (lower case with hyphens) to camel case."
  [^String s]
  (to-camel-case s \-))

(defn snake-case-to-camel-case
  "Converts a snake case string (lower case with underscores) to camel case."
  [^String s]
  (to-camel-case s \_))

(comment

  (camel-case? "fromCamelCase")
  ;; => true

  (camel-case? "from-camel-case")
  ;; => false

  (kebab-case? "from-camel-case")
  ;; => true

  (kebab-case? "from_camel_case")
  ;; => false

  (kebab-case? "fromCamelCase")
  ;; => false

  (snake-case? "from_camel_case")
  ;; => true

  (snake-case? "from-camel-case")
  ;; => false

  (snake-case? "fromCamelCase")
  ;; => false

  (to-camel-case "to-camel-case")
  ;; => "toCamelCase"

  (from-camel-case "fromCamelCase")
  ;; => "from-camel-case"

  (from-camel-case "from Camel Case")
  ;; => "from-camel-case"

  (camel-case-to-kebab-case "fromCamelCase")
  ;; => "from-camel-case"

  (camel-case-to-snake-case "fromCamelCase")
  ;; => "from_camel_case"

  (kebab-case-to-camel-case "to-camel-case")
  ;; => "toCamelCase"

  (snake-case-to-camel-case "to_camel_case")
  ;; => "toCamelCase"


  )