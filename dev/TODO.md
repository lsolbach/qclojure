# TODOs

## Next steps
* check for missing type hinds in calculations to improve simulator performance
* add state initalization in backend
* handle backend job failures in algorithms
* optimize shor's algorithm implementation and quantum period finding
* refactor algorithms to use a provided backend
  * grover

## Backlog
* use :operation-registry/operation-id instead of ::operation-type in circuit
  and other namespaces
* track qubit mapping on circuit tansformations
  * use qubit mapping on measure operations
* reduce code duplication
* check for inconsistant usage of operations/gates
* check order of the optimizations (what if backend doesn't support SWAP)
* fix vertical alignment of gates in ascii circuit visualization
* add separation between text and graphics in svg visualization
* extract formatting information to edn
