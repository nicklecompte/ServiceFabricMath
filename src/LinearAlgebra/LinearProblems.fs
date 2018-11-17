module  ServiceFabricMath.Math.LinearAlgebra.LinearProblems

open ServiceFabricMath.LinearAlgebra

open RealMatrices
open BasicMatrixOperations
open Vectors

type AxEqualsBStatement< ^T when 
                    ^T : (static member (+) :  ^T * ^T -> ^T ) and 
                    ^T : (static member (-) : ^T * ^T -> ^T ) and 
                    ^T : (static member (*) : ^T * ^T -> ^T) and 
                    ^T : (static member (/) : ^T * ^T -> ^T) and 
                    ^T : (static member Zero : ^T) and 
                    ^T : (static member One : ^T) and 
                    ^T : (static member Sqrt: ^T -> ^T) and
                    ^T : unmanaged and
                    ^T : struct and
                    ^T : equality> = {
    A : Matrix< ^T>
    b : ColumnVector< ^T>
}

type AxEqualsBSolutionStep< ^T when 
                        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
                        ^T : (static member (-) : ^T * ^T -> ^T ) and 
                        ^T : (static member (*) : ^T * ^T -> ^T) and 
                        ^T : (static member (/) : ^T * ^T -> ^T) and 
                        ^T : (static member Zero : ^T) and 
                        ^T : (static member One : ^T) and 
                        ^T : (static member Sqrt: ^T -> ^T) and
                        ^T : unmanaged and
                        ^T : struct and
                        ^T : equality> = {
        problem: AxEqualsBStatement< ^T>
        matrixOps: MatrixOperations list
        currentA : Matrix< ^T>
        currentb : ColumnVector< ^T>
        }