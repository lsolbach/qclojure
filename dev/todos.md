# TODOs

## Next steps
* use measurement outcomes and trajectories for noisy result extraction
* check for result handling based on final states in algorithms
  * replace with hardware compatible result processing
* integrate optional error mitigation in the hardware simulator workflow (as template for hardware backends)
* integrate unified result types in all algorithms
  * missing in QFT/iQFT and QPE
  * check Shor
* enhance single qubit gate functions to take a qubit vector and add the gate for all qubits
* new measure operation function that takes a single qubit or a vector of qubits (replaces measure-all)

## Backlog
* consistently handle backend errors in all algorithms
  * maybe add :timeout and :retry to backend options
* fix and optimize quantum arithmetic
* optimize shor's algorithm implementation and quantum period finding
* check memoization of circuit building (e.g. for variational algorithms)
* integrate gate and qubit optimization into ideal simulator workflow
* enhance circuit optimizations
  * check topology mapping before inserting SWAP gates
  * apply gate optimization and qubit optimization again after gate decomposition
  * check reset operation usage to minimize ancilla qubits
  * add reset operation (Measure and conditional X), if useful
* use :operation-registry/operation-id instead of ::operation-type in circuit and other namespaces
* check for missing type hints in calculations to improve simulator performance
* extract formatting/styling information for SVG to edn
* add real linear algebra backends
