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

type BlockMatrix< ^T 
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
    rows: int
    columns: int
}