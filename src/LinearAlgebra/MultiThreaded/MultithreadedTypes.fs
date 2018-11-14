module ServiceFabricMath.LinearAlgebra.MultithreadedTypes

open ServiceFabricMath.Math.LinearAlgebra
open Vectors
open Matrices
open System.Collections.Generic

[<Struct>]
type BlockMatrixCoordinate = {
    llhs: int
    lrhs: int
    ulhs: int
    urhs: int
}

type GeneralBlockVector< ^T 
    when 
        ^T :    (static member (+) :  ^T * ^T -> ^T ) and 
        ^T :    (static member (-) : ^T * ^T -> ^T ) and 
        ^T :    (static member (*) : ^T * ^T -> ^T) and 
        ^T :    (static member (/) : ^T * ^T -> ^T) and 
        ^T :    (static member Zero : ^T) and 
        ^T :    (static member One : ^T) and 
        ^T :    (static member Sqrt : ^T -> ^T) and 
        ^T :    unmanaged and
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
        ^T :    unmanaged and
        ^T :    struct and
        ^T :    equality> = {
    matrices: Dictionary<BlockMatrixCoordinate, Matrix< ^T>>
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
        ^T :    unmanaged and
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
        ^T :    unmanaged and
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
        ^T :    unmanaged and
        ^T :    struct and
        ^T :    equality> =
    | BlockDiagonal of BlockDiagonalMatrix< ^T>