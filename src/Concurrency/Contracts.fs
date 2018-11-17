module ServiceFabricMath.Concurrency.Contracts

open ServiceFabricMath.LinearAlgebra
open RealMatrices
open Vectors
open MultithreadedTypes
open System.Runtime.CompilerServices
open ActorInterfaces

type LinearAlgebraJobResult<'TScalar,'TVector,'TMatrix> =
    | ScalarJob of JobToken<'TScalar>
    | VectorJob of JobToken<'TVector>
    | MatrixJob of JobToken<'TMatrix>


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
  //  inherit IWorkerActor<LinearAlgebraJobResult<ColumnVector<float>, Matrix<float>>>

type IMultithreadedMatrixService_Float = 
    inherit ILinearAlgebraCalculator<float, GeneralBlockVector<float>, MultiThreadedMatrix<float>>
    abstract member BaseMatrix : MultiThreadedMatrix<float>
    abstract member BlockSolverActors: seq<ILinearAlgebraSingleThreadedActorDoublePrecision>