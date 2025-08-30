# TODOs

## Next steps
* integrate unified result types in backend workflows
* integrate optional error mitigation in the noisy simulator workflow
* extraxt relevant results from QAOA
  * edges in Max-CUT and TSP
  * boolean assignments in Max-SAT

## Backlog
* consistently handle backend errors in all algorithms
* fix and optimize quantum arithmetic
* optimize shor's algorithm implementation and quantum period finding
* use :operation-registry/operation-id instead of ::operation-type in circuit and other namespaces
* check for missing type hints in calculations to improve simulator performance
* check for inconsistant usage of "operation"/"gate"
  * a gate is an operation, so is a measurement
* extract formatting/styling information for SVG to edn
* remove hardware references from gate-decomposition (domain layer)
