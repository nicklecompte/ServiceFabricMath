module ServiceFabricMath.Math.LinearAlgebra.Matrices

open System
open System.Text
open System.Collections.Generic

// With the statically-resolved type parameters, F# sometimes throws
// what I hope are extraneous warnings about type resolution errors
// and indentation problems. I am pretty sure these are invalid :/
#nowarn "20" "193" "58"

[<Struct>]
type MatrixCoordinate = {row: int; column: int}

type SparseMatrix< 'T when 'T : struct> = {
    numRows : int
    numCols : int
    nonzeroIndices : HashSet<MatrixCoordinate>
    nonzeroValues : Dictionary<MatrixCoordinate, 'T>
}

type Fast2DArray< 'T when 'T : struct> 
    (numRows: int, numCols: int) =
    let innerArray : 'T array = Array.zeroCreate (numRows*numCols)

    member __.To1dArray = innerArray
    /// Get according to 0-based indexing
    member __.Item
        with get(row:int, col: int) : ^T = innerArray.[numCols*(row) + col] 
        and set (row,col) (value : ^T) = innerArray.[numCols*(row) + col] <- value

    member __.GetRow (rowNum:int) : 'T array = innerArray.[rowNum*(numCols)..(rowNum*(numCols) + numCols - 1)]

    override x.ToString() = 
        let builder = new StringBuilder()
        let rowStringArray = 
            [|0..(numRows-1)|] |> Array.map(fun i -> sprintf "%A" (x.GetRow i))
        rowStringArray |> Array.iter(fun row -> builder.Append(row + "\n") |> ignore)
        builder.ToString()
//        builder.Append('┌').Append(String.replicate numCols "-").Append('┐')
//        builder.ToString()

type Matrix< ^T when ^T: struct> =
    | Diagonal of ^T array
    | Dense of Fast2DArray< ^T>

(*
let testAr = new Fast2DArray<int>(3,3);;
testAr.[0,0] <- 2
testAr.[1,1] <- 3
testAr.[2,2] <- 4
testAr.GetRow 0
testAr.GetRow 1
testAr.GetRow 2
*)