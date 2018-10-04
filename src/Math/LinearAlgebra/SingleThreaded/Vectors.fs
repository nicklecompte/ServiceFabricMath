module ServiceFabricMath.Math.LinearAlgebra.Vectors

// With the statically-resolved type parameters, F# sometimes throws
// what I hope are extraneous warnings about type resolution errors
// and indentation problems. I am pretty sure these are invalid :/
#nowarn "20" "193" "58"

open System.Collections
open System.Collections.Generic

type SparseArray<'T when 'T : struct> = {
    length: int
    nonzeroIndices : HashSet<int>
    nonzeroValues : Dictionary<int,'T>
}
with
    interface IEnumerable<'T> with
        member x.GetEnumerator() : IEnumerator<'T> =
        (seq 
            {for i in 0..(x.length - 1) do 
                yield (
                        if x.nonzeroIndices.Contains(i) then x.nonzeroValues.[i] 
                        else Unchecked.defaultof<'T>)}
        ).GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator() = (x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator
        // let mutable curIndex = 0
        // {
        //     new IEnumerator<'T> with
        //         member y.MoveNext() =
        //             if curIndex = x.length - 1 then false
        //             else
        //                 curIndex <- curIndex + 1
        //                 true
        //         member y.Current
        //             with get() =
        //                 if x.nonzeroIndices.Contains(curIndex)
        //                     then x.nonzeroValues.[curIndex] :> obj
        //                 else Unchecked.defaultof<'T> :> obj
        //         member y.Current() = y.Current
        //         member __.Reset() =
        //             curIndex <- 0
        //         //member y.Current() = y.Current
        // }
        // member x.GetEnumerator() : IEnumerator<'T> =

    member x.ToArray() : 'T array = failwith "not done"

type ColumnVector< ^T when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and 
        ^T : (static member Sqrt : ^T -> ^T) and
        ^T : struct and
        ^T : equality> = 
        | DenseColumn of ^T array
        | SparseColumn of SparseArray< ^T>
with
    member inline x.Array () : 'T array = 
        match x with 
            | DenseColumn c -> c
            | SparseColumn s -> s.ToArray()
    member inline x.Item 
        with get(ind) : ^T =
            match x with 
                | DenseColumn c -> c.[ind]
                | SparseColumn s -> 
                    if s.nonzeroIndices.Contains(ind) then s.nonzeroValues.[ind]
                    else LanguagePrimitives.GenericZero
        and set ind (value : ^T) =
            match x with 
                | DenseColumn c -> c.[ind] <- value
                | SparseColumn c ->
                    if c.nonzeroIndices.Contains(ind) then c.nonzeroValues.[ind] <- value
                    else
                        if value = LanguagePrimitives.GenericZero then ()
                        else
                            c.nonzeroIndices.Add(ind) |> ignore
                            c.nonzeroValues.Add(ind,value)
    member inline x.Dimension = 
        match x with 
            | DenseColumn c -> c.Length
            | SparseColumn c -> c.length

    member inline x.L2Norm : ^T =
        match x with 
            | DenseColumn c ->
                    c |> Array.fold(fun (acc: ^T) (valu : ^T) -> acc + valu*valu) LanguagePrimitives.GenericZero
                    |> sqrt
            | SparseColumn c ->
                    c.nonzeroValues.Values |> Seq.fold(fun (acc: ^T) (valu : ^T) -> acc + valu*valu) LanguagePrimitives.GenericZero
                    |> sqrt

    member inline x.Transpose ()  : RowVector< ^T> = 
        match x with 
            | DenseColumn v -> DenseRow v
            | SparseColumn v -> SparseRow v
    static member inline (*) ((scalar: ^T),(vector: ColumnVector< ^T>)) : ColumnVector< ^T> =
        match vector with 
            | DenseColumn c -> 
                DenseColumn (c |> Array.map(fun vectElt -> scalar * vectElt))
            | SparseColumn c -> 
                let scaledPairs = c.nonzeroValues |> Seq.map(fun kvp -> (kvp.Key, kvp.Value * scalar))
                let newDict = new Dictionary<int, ^T>(c.nonzeroValues.Count)
                scaledPairs |> Seq.iter(fun (a,b) -> newDict.Add(a,b))
                SparseColumn ({length=c.length; nonzeroIndices=c.nonzeroIndices; nonzeroValues = newDict})
    static member inline (+) ((vec1: ColumnVector< ^T >), (vec2: ColumnVector< ^T> )) : ColumnVector< ^T> =
        if vec1.Dimension <> vec2.Dimension then (invalidArg "vec1,vec2" "vectors must be the same length to add them")
        match (vec1,vec2) with
            | (DenseColumn v1,DenseColumn v2) ->
                Array.init v1.Length (fun ind -> v1.[ind] + v2.[ind]) |> DenseColumn
            | (DenseColumn v1, SparseColumn v2) ->
                Array.init v1.Length (fun ind -> 
                                        if v2.nonzeroIndices.Contains(ind) then v1.[ind] + v2.nonzeroValues.[ind]
                                        else v1.[ind]) |> DenseColumn
            | (SparseColumn v1, DenseColumn v2) ->
                Array.init v2.Length (fun ind -> 
                                        if v1.nonzeroIndices.Contains(ind) then v2.[ind] + v1.nonzeroValues.[ind]
                                        else v2.[ind]) |> DenseColumn            
            | (SparseColumn v1, SparseColumn v2) ->
                let v1IndexArray = Array.zeroCreate (v1.nonzeroIndices.Count)
                v1.nonzeroIndices.CopyTo(v1IndexArray)
                let sumAndV1KVP = 
                    v1IndexArray 
                    |> Array.map(fun index -> (index, if v2.nonzeroIndices.Contains(index) then v2.nonzeroValues.[index] + v1.nonzeroValues.[index]
                                                      else v1.nonzeroValues.[index]))
                let v2IndexdArray = Array.zeroCreate (v2.nonzeroIndices.Count)
                v2.nonzeroIndices.CopyTo(v2IndexdArray)
                let v2NotInV1 = 
                    v2IndexdArray |> Array.filter(fun a -> not (v1.nonzeroIndices.Contains(a)))
                let newDict = 
                    let tmpDict = new Dictionary<int, ^T>()
                    sumAndV1KVP |> Array.iter(fun (a,b) -> tmpDict.Add(a,b))
                    v2NotInV1 |> Array.iter(fun a -> tmpDict.Add(a,v2.nonzeroValues.[a]))
                    tmpDict
                let newHashset = new HashSet<int>(v1IndexArray |> Array.append(v2NotInV1))
                SparseColumn ({length = v1.length;nonzeroIndices = newHashset; nonzeroValues = newDict})

and RowVector< ^T when 
        ^T : struct and
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and 
        ^T : (static member Sqrt : ^T -> ^T) and        
        ^T : equality> = 
        | DenseRow of ^T array
        | SparseRow of SparseArray< ^T>
with
    member inline x.Array () : 'T array = 
        match x with 
            | DenseRow r -> r
            | SparseRow r -> r.ToArray()
    member inline x.Item 
        with get(ind) : ^T =
            match x with 
                | DenseRow r -> r.[ind]
                | SparseRow s -> 
                    if s.nonzeroIndices.Contains(ind) then s.nonzeroValues.[ind]
                    else LanguagePrimitives.GenericZero
        and set ind (value : ^T) =
            match x with 
                | DenseRow r -> r.[ind] <- value
                | SparseRow r ->
                    if r.nonzeroIndices.Contains(ind) then r.nonzeroValues.[ind] <- value
                    else
                        if value = LanguagePrimitives.GenericZero then ()
                        else
                            r.nonzeroIndices.Add(ind) |> ignore
                            r.nonzeroValues.Add(ind,value)
    member inline x.Dimension = 
        match x with 
            | DenseRow r -> r.Length
            | SparseRow r -> r.length

    member inline x.L2Norm : ^T =
        match x with 
            | DenseRow r ->
                    r |> Array.fold(fun (acc: ^T) (valu : ^T) -> acc + valu*valu) LanguagePrimitives.GenericZero
                    |> sqrt
            | SparseRow r ->
                    r.nonzeroValues.Values |> Seq.fold(fun (acc: ^T) (valu : ^T) -> acc + valu*valu) LanguagePrimitives.GenericZero
                    |> sqrt

    member inline x.Transpose ()  : ColumnVector< ^T> = 
        match x with 
            | DenseRow v -> DenseColumn v
            | SparseRow v -> SparseColumn v
    static member inline (*) ((scalar: ^T),(vector: RowVector< ^T>)) : RowVector< ^T> =
        match vector with 
            | DenseRow r -> 
                DenseRow (r |> Array.map(fun vectElt -> scalar * vectElt))
            | SparseRow r -> 
                let scaledPairs = r.nonzeroValues |> Seq.map(fun kvp -> (kvp.Key, kvp.Value * scalar))
                let newDict = new Dictionary<int, ^T>(r.nonzeroValues.Count)
                scaledPairs |> Seq.iter(fun (a,b) -> newDict.Add(a,b))
                SparseRow ({length=r.length; nonzeroIndices=r.nonzeroIndices; nonzeroValues = newDict})
    static member inline (+) ((vec1: RowVector< ^T >), (vec2: RowVector< ^T> )) : RowVector< ^T> =
        if vec1.Dimension <> vec2.Dimension then (invalidArg "vec1,vec2" "vectors must be the same length to add them")
        match (vec1,vec2) with
            | (DenseRow v1,DenseRow v2) ->
                Array.init v1.Length (fun ind -> v1.[ind] + v2.[ind]) |> DenseRow
            | (DenseRow v1, SparseRow v2) ->
                Array.init v1.Length (fun ind -> 
                                        if v2.nonzeroIndices.Contains(ind) then v1.[ind] + v2.nonzeroValues.[ind]
                                        else v1.[ind]) |> DenseRow
            | (SparseRow v1, DenseRow v2) ->
                Array.init v2.Length (fun ind -> 
                                        if v1.nonzeroIndices.Contains(ind) then v2.[ind] + v1.nonzeroValues.[ind]
                                        else v2.[ind]) |> DenseRow            
            | (SparseRow v1, SparseRow v2) ->
                let v1IndexArray = Array.zeroCreate (v1.nonzeroIndices.Count)
                v1.nonzeroIndices.CopyTo(v1IndexArray)
                let sumAndV1KVP = 
                    v1IndexArray 
                    |> Array.map(fun index -> (index, if v2.nonzeroIndices.Contains(index) then v2.nonzeroValues.[index] + v1.nonzeroValues.[index]
                                                      else v1.nonzeroValues.[index]))
                let v2IndexdArray = Array.zeroCreate (v2.nonzeroIndices.Count)
                v2.nonzeroIndices.CopyTo(v2IndexdArray)
                let v2NotInV1 = 
                    v2IndexdArray |> Array.filter(fun a -> not (v1.nonzeroIndices.Contains(a)))
                let newDict = 
                    let tmpDict = new Dictionary<int, ^T>()
                    sumAndV1KVP |> Array.iter(fun (a,b) -> tmpDict.Add(a,b))
                    v2NotInV1 |> Array.iter(fun a -> tmpDict.Add(a,v2.nonzeroValues.[a]))
                    tmpDict
                let newHashset = new HashSet<int>(v1IndexArray |> Array.append(v2NotInV1))
                SparseRow ({length = v1.length;nonzeroIndices = newHashset; nonzeroValues = newDict})