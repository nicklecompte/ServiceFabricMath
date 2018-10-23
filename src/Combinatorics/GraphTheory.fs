namespace PowerSystemsAnalysis.Math

module GraphTheory =

    open System.Collections.Generic

    /// A fairly no-frills directed multigraph, allowing loops but no degenerate edges.
    /// We are going ahead and using statically-resolved type parameters to get math computations to inline.
    /// I am told this is less idiomatic that it should be \_o_/
    type DirectedMultiGraph< ^TVertex, ^TEdge when ^TEdge : (member AsVertexPair : ^TVertex * ^TVertex) and ^TVertex: equality> = {
        vertices: seq< ^TVertex>
        edges: seq< ^TEdge >
    }
    with member inline this.ToAdjacencyMatrix () : bool[,] =
            let vertArr = this.vertices |> Seq.toArray
            let size = vertArr.Length
            let adjacencyDict =
                let adjMap = new Dictionary< ^TVertex, HashSet< ^TVertex>>()
                this.edges
                |> Seq.iter(fun edge -> let (v1,v2) = (^TEdge : (member AsVertexPair : ^TVertex * ^TVertex) edge)
                                        if adjMap.ContainsKey(v1) then adjMap.[v1].Add(v2) |> ignore
                                        else adjMap.Add(v1,new HashSet< ^TVertex>([|v2|])) |> ignore)
                adjMap
            Array2D.init<bool> size size (fun x y -> adjacencyDict.ContainsKey(vertArr.[x]) && adjacencyDict.[vertArr.[x]].Contains(vertArr.[y]))

    // [<CustomEquality>]
    // [<CustomComparison>]
    // type UndirectedEdge< 'TVertex when 'TVertex : equality and 'TVertex : comparison> = {
    //     v1 : 'TVertex
    //     v2 : 'TVertex
    // }
    // with member this.AsVertexPair = (this.v1,this.v2)
    //      override this.Equals (other:obj) =
    //         match other with
    //         | :? UndirectedEdge< 'TVertex > as otherEdge -> (this.v1 = otherEdge.v1 || this.v1 = otherEdge.v2) && (this.v2 = otherEdge.v1 || this.v2 = otherEdge.v2)
    //         | _ -> false

    //      override this.GetHashCode () =
    //         this.v1.GetHashCode() + this.v2.GetHashCode()
    //      override this.CompareTo (yObj:obj) =
    //                 match yObj with
    //                 | :? UndirectedEdge< 'TVertex > as otherEdge -> (this.v1.CompareTo otherEdge.v1) + (this.v1.CompareTo = otherEdge.v2) + (this.v2.CompareTo otherEdge.v1) + (this.v2.CompareTo otherEdge.v2)
    //                 | _ -> invalidArg "yObj" "can only compare UndirectedEdge of same type"