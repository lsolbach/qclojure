(ns org.soulspace.qclojure.adapter.writer)

(defmulti write-quantum-circuit
  "Write a quantum circuit to a specified format.
  
  Dispatches on format keyword.
  
  Parameters:
  - format: Format to write the circuit in
  - circuit: Quantum circuit data structure to write
  - options: Format-specific options
  
  Returns:
  String containing the formatted quantum circuit"
  (fn [format _circuit _options] format))
