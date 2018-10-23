module ServiceFabricMath.Math.LinearAlgebra.BasicMatrixOperations

open ServiceFabricMath.Math.LinearAlgebra.Vectors
open ServiceFabricMath.Math.LinearAlgebra.Matrices


type Empty = | Empty

let inline gaussianElimination (matrix: Matrix< ^T>) (rhsVector : ColumnVector< ^T>) : ColumnVector< ^T> =
    failwith "not done"