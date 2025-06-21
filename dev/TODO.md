# TODOs

## Next steps
* handle backend job failures in algorithms
* optimize shor's algorithm implementation and quantum period finding
* refactor grover algorithm to use a provided backend

## Backlog
* add parameters to the decomposition rules in the operation registry.
* use :operation-registry/operation-id instead of ::operation-type in circuit
  and other namespaces
* track qubit mapping on circuit tansformations
  * use qubit mapping on measure operations
* reduce code duplication
* check for missing type hints in calculations to improve simulator performance
* check for inconsistant usage of "operation"/"gate"
  * a gate is an operation, so is a measurement
* fix vertical alignment of gates in ascii circuit visualization
* add separation between text and graphics in svg visualization
* extract formatting information to edn
