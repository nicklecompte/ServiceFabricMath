module ServiceFabricMath.LinearAlgebra.MultithreadedTypes

open ServiceFabricMath.LinearAlgebra
open Vectors
open RealMatrices
open System.Collections.Generic

[<Struct>]
type BlockMatrixCoordinate = {
    llhs: int
    lrhs: int
    ulhs: int
    urhs: int
}

/// Type alias for a lazily-accessed matrix (a parameterless function that returns a matrix)
type AsyncMatrix< ^T 
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> 
        = unit -> Matrix< ^T>

type GeneralBlockVector< ^T 
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> =
    | BlockRow of seq<RowVector< ^T>>
    | BlockColumn of seq<ColumnVector< ^T>>

type GeneralBlockMatrix< ^T 
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> = {
    matrices: Dictionary<BlockMatrixCoordinate, AsyncMatrix< ^T>>
    rowCount: int
    columnCount: int
}

type BlockDiagonalMatrix< ^T
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> = {
    rowCount: int
    diagonal: seq<Matrix< ^T>>
}

type BlockUpperTriangularMatrix< ^T
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> = {
    rowCount: int
    matrices: seq<Matrix< ^T>>
}

type MultiThreadedMatrix< ^T
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    struct and
        ^T :    equality> =
    | BlockDiagonal of BlockDiagonalMatrix< ^T>