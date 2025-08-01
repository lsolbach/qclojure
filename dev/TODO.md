# TODOs

## Next steps
* integrate error mitigation in the noisy simulator
* handle backend job failures in algorithms
* optimize shor's algorithm implementation and quantum period finding

## Backlog
* use :operation-registry/operation-id instead of ::operation-type in circuit
  and other namespaces
* check qubit mapping on circuit tansformations
  * check usage of qubit mapping on measure operations
* reduce code duplication
* check for missing type hints in calculations to improve simulator performance
* check for inconsistant usage of "operation"/"gate"
  * a gate is an operation, so is a measurement
* add separation between text and graphics in svg visualization
* extract formatting information to edn
* remove hardware references from    gate-decomposition (domain layer)
* move general math code to math namespace
