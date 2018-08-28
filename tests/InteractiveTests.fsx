#load "GraphTheory.fs"
#load "ComplexAnalysis.fs"
#load "LinearAlgebra.fs"

open PowerSystemsAnalysis.Math
open GraphTheory
open ComplexAnalysis
open LinearAlgebra

let testVertices = [|1..10|]
type EdgeFromInts = {
    int1:int
    int2:int
}
with member this.AsVertexPair = (this.int1,this.int2)

let testEdges = testVertices |> Array.map(fun a -> testVertices |> Array.filter(fun b ->(a%b = 0 )) |> Array.map(fun b -> (a,b))) |> Array.concat |> Array.map(fun (a,b) -> {int1=a;int2=b})

let testGraph = {edges=testEdges;vertices=testVertices}
testGraph.ToAdjacencyMatrix()
#time
// type UndirectedEdges< ^TVertex when ^TVertex : comparison> = {
//     vertices: seq< ^TVertex>
// }
//    with member this.AsVertexPa

//    type DirectedGraph< ^TVertex
#time

let testAr = Array2D.init 3 6 (fun a b -> System.Math.Pow(float(a),float(b)))

testAr.[2,0..]

testAr.[2,0..(testAr |> Array2D.length2-1)]

testAr |> Seq.head