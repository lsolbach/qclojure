# TODOs

## Next steps
* extend noisy simulator to hardware simulator
  * integrate optional error mitigation in the noisy simulator workflow
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
* fix and optimize quantum arithmetic
* optimize shor's algorithm implementation and quantum period finding
* use :operation-registry/operation-id instead of ::operation-type in circuit and other namespaces
* check for missing type hints in calculations to improve simulator performance
* extract formatting/styling information for SVG to edn
* add real linear algebra backends
