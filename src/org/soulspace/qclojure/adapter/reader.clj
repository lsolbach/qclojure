(ns org.soulspace.qclojure.adapter.reader)

(defmulti read-quantum-circuit
  "Read a quantum circuit from a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format of the input circuit
  - input: Input data containing the circuit definition
  
  Returns:
  Parsed quantum circuit data structure"
  (fn [format _input] format))