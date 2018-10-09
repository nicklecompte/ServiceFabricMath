module VectorTests

open System
open System.Collections
open System.Collections.Generic
open Xunit
open ServiceFabricMath.Math.LinearAlgebra.Vectors

type RandomDenseVectorTestData() =
    let rand = new Random()
    let sequence = seq {for i in 0..10 do 
                        yield
                            [|0..10|]
                            |> Array.map (fun _ -> float(rand.Next())*rand.NextDouble()) :> obj
                            |> Array.singleton
                            }
    interface System.Collections.Generic.IEnumerable< obj array > with
        member x.GetEnumerator() = sequence.GetEnumerator()
        member x.GetEnumerator() : IEnumerator = (sequence :> IEnumerable).GetEnumerator()

[<Fact>]
let ``Deep sparse array copy works`` =
    let hs = new HashSet<int>()
    hs.Add(1) |> ignore
    hs.Add(3) |> ignore
    let dict = new Dictionary<int,float>()
    dict.Add(1,5.0) |> ignore
    dict.Add(1,10.0) |> ignore
    let sparse = {length=5; nonzeroIndices=hs; nonzeroValues = dict}
    let sparse2 = sparse.DeepCopy()
    sparse2.nonzeroValues.[3] <- 11.0
    Assert.Equal(sparse.nonzeroValues.[3],10.0)
    Assert.Equal(sparse2.nonzeroValues.[3],11.0)

[<Theory>]
[<ClassData(typedefof<RandomDenseVectorTestData>)>]
let ``Inner product of any dense vector with zero vector is zero`` (vec: float array) =
    let rowVector = DenseRow vec
    let zeroCol = Array.zeroCreate (rowVector.Dimension) |> DenseColumn
    Assert.Equal(innerProduct rowVector zeroCol, 0.0)
    let colVector = DenseColumn vec
    let zeroRow = zeroCol.Transpose()
    Assert.Equal(innerProduct zeroRow colVector, 0.0)

[<Theory>]
[<ClassData(typedefof<RandomDenseVectorTestData>)>]
let ``Sum of any dense vector with zero vector leaves vector unchanged`` (vec: float array) =
    let nonzeroColVector = DenseColumn vec
    let zeroCol = Array.zeroCreate (nonzeroColVector.Dimension) |> DenseColumn
    Assert.Equal(nonzeroColVector + zeroCol, nonzeroColVector)
    let nonzeroRowVector = nonzeroColVector.Transpose()
    let zeroRow = zeroCol.Transpose()
    Assert.Equal(nonzeroRowVector + zeroRow, nonzeroRowVector)

[<Theory>]
[<ClassData(typedefof<RandomDenseVectorTestData>)>]
let ``Multiplying by zero gives zero vector`` (vec: float array) =
    let nonzeroColVector = DenseColumn vec
    let zeroCol = Array.zeroCreate (nonzeroColVector.Dimension) |> DenseColumn
    Assert.Equal(0.0*nonzeroColVector, zeroCol)
    let nonzeroRowVector = nonzeroColVector.Transpose()
    let zeroRow = zeroCol.Transpose()
    Assert.Equal(0.0*nonzeroRowVector, zeroRow)

[<Theory>]
[<ClassData(typedefof<RandomDenseVectorTestData>)>]
let ``Adding dense vec to itself is same as multiplying by 2`` (vec: float array) =
    let nonzeroColVector = DenseColumn vec
    Assert.Equal(nonzeroColVector + nonzeroColVector, 2.0*nonzeroColVector)
    let nonzeroRowVector = nonzeroColVector.Transpose()
    Assert.Equal(nonzeroRowVector + nonzeroRowVector, 2.0*nonzeroRowVector)
