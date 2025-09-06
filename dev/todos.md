# TODOs

## Next steps
* integrate optional error mitigation in the hardware simulator workflow
* integrate unified result types in all algorithms
  * missing in QFT/iQFT and QPE
  * check Shor, VQE and QAOA
* extraxt relevant results from QAOA
  * edges in Max-CUT and TSP
  * boolean assignments in Max-SAT
* add reset operation (Measure and conditional X)
* enhance single qubit gate functions to take a qubit vector and add the gate for all qubits
* new measure operation function that takes a single qubit or a vector of qubits (replaces measure-all) 
* check for inconsistant usage of "operation"/"gate"
  * a gate is an operation, so is a measurement

## Backlog
* consistently handle backend errors in all algorithms
  * maybe add :timeout and :retry to backend options
* fix and optimize quantum arithmetic
* optimize shor's algorithm implementation and quantum period finding
* integrate gate and qubit optimization into ideal simulator workflow
* enhance circuit optimizations
  * check for rotation folding in gate optimization
  * check topology mapping before inserting SWAP gates
  * apply gate optimization and qubit optimization again after gate decomposition
  * check RESET operation usage to minimize ancilla qubits
  * make optimizations composable by defining a common format for input/output
    *maybe only the circuit and options :native-gates, :coupling
    * extraxt all analysis to separate function taking original and optimized circuits
* use :operation-registry/operation-id instead of ::operation-type in circuit and other namespaces
* check for missing type hints in calculations to improve simulator performance
* extract formatting/styling information for SVG to edn
* add real linear algebra backends
