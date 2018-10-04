# ServiceFabricMath

Math, written with concurrency as a priority, intended to be run on Service Fabric. The idea is that you can have a Stateful Service that determines the sparsity of a matrix and then breaks down e.g. diagonalization into constituent matrices, then Reliable Actors do the actual work concurrently.

A most in-progressiest work-in-progress.