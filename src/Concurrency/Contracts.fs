module Concurrency.Contracts

open ServiceFabricMath.Math.LinearAlgebra.Matrices

type ILinearAlgebraDeterminationSystem =
    interface end

type ILinearAlgebraSingleThreadedActorDoublePrecision =
    abstract member Diagonalize : Matrix<float> -> Matrix<float>