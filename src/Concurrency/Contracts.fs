module Concurrency.Contracts

open ServiceFabricMath.Math.LinearAlgebra.Matrices
open ServiceFabricMath.Math.LinearAlgebra.Vectors
open System.Runtime.CompilerServices

type ILinearAlgebraProblemDeterminationSystem =
    interface end

type ILinearAlgebraCalculator<'TScalar,'TVector,'TMatrix> =
    /// Vector operations
    abstract member L2Norm : 'TVector -> 'TScalar
    abstract member pNorm : p:'TScalar -> 'TVector -> 'TScalar
    abstract member InnerProduct : 'TVector -> 'TVector -> 'TScalar
    abstract member Diagonalize : 'TMatrix -> 'TMatrix
    abstract member Multiply : 'TMatrix -> 'TMatrix -> 'TMatrix
    abstract member Add : 'TMatrix -> 'TMatrix -> 'TMatrix
    abstract member VectorMultiply : 'TMatrix -> 'TVector-> 'TVector
    



type ILinearAlgebraSingleThreadedActorDoublePrecision =
    inherit ILinearAlgebraCalculator<float, ColumnVector<float>, Matrix<float>>