module ServiceFabricMath.LinearAlgebra.VectorSpace

open ServiceFabricMath.LinearAlgebra.Vector

open System

/// A vector space is defined by its basis.
type VectorSpace< 'T when 
        'T : (static member (+) :  ^T * ^T -> ^T ) and 
        'T : (static member (-) : ^T * ^T -> ^T ) and 
        'T : (static member (*) : ^T * ^T -> ^T) and 
        'T : (static member (/) : ^T * ^T -> ^T) and
        'T : (static member Zero : ^T) and ^T : equality >
        (basis : Vector< ^T> list) =

        let validateBasis: bool =
            let dimensions = basis |> List.map(fun vec -> vec.Dim) |> List.distinct
            if dimensions.Length <> 1 then invalidArg "basis" "invalid constructor, vectors must be all the same dimension"
            else true
        member inline x.Dimension = failwith "not done"
    
