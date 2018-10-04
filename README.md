# ServiceFabricMath

Math, written with concurrency as a priority, intended to be run on Service Fabric. The idea is that you can have a Stateful Service that determines the sparsity of a matrix and then breaks down e.g. diagonalization into constituent matrices, then Reliable Actors do the actual work concurrently.

Specifically: we are using fairly slow, immutable, functional-first numerical algorithms, which are by design non-optimal and very non-BLAS. In exchange, we get free thread-safety and can "outsource" computations to separate processors (virtual or physical) without having to do tedious state management. A sketch of the architecture:

* a 

A most in-progressiest work-in-progress.