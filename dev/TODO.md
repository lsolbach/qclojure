# TODOs

## Next steps
* Handle measurement operations in QASM 2
* integrate error mitigation in the noisy simulator
* consistently handle backend errors in all algorithms
* optimize shor's algorithm implementation and quantum period finding

## Backlog
* use :operation-registry/operation-id instead of ::operation-type in circuit
  and other namespaces
* check for missing type hints in calculations to improve simulator performance
* check for inconsistant usage of "operation"/"gate"
  * a gate is an operation, so is a measurement
* add separation between text and graphics in svg visualization
* extract formatting information to edn
* remove hardware references from gate-decomposition (domain layer)
* consistently handle backend errors in all algorithms
