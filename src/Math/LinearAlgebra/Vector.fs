module ServiceFabricMath.Math.LinearAlgebra.Vector

open ServiceFabricMath.Math.LinearAlgebra.Primitives

type Vector< ^T when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and ^T : equality > =  
    | Vector of BasicColumnVector< ^T>
with 
    /// shortcut to access the float array
    member inline private v.Values = match v with Vector v' -> v'
   // override this.ToString() = this.Values.ToString()
    /// access a component of the vector
    member inline v.Item with get(i:int) = v.Values.[i]
    /// the vectors dimension = nr of components
    member inline v.Dim = Array.length v.Values

    // your normal vector-operators
    static member inline (+) (a: Vector< ^T>,b: Vector< ^T>)  : Vector< ^T> = (Array.map2 ((fun a b  -> a + b)) (a.Values) (b.Values)) |> Vector
    static member inline (-) (a: Vector< ^T>,b: Vector< ^T>) : Vector< ^T> = Array.zip a.Values b.Values |> Array.map (fun (a,b)  -> a - b) |> Vector
    /// Hadamard product
    static member inline (*) (a: Vector< ^T>, b: Vector< ^T>) : Vector< ^T> =
        if not (a.Dim = b.Dim) then invalidArg "b" "a nd b must have same dimension" 
        (Array.map2 (fun a b -> a*b)  (a.Values)  (b.Values) )|> Vector
    static member inline ScalarMultiply (s : ^T,a: Vector< ^T>) : Vector< ^T> = a.Values |> Array.map (fun a -> a * s) |> Vector

    // computes the inner product (also known as scalar product) of two vectors
    static member inline InnerProd (a : Vector< ^T>, b : Vector< ^T>) : ^T = Array.zip a.Values b.Values |> Array.sumBy(fun (a,b) ->  a * b)
    static member inline (.**.) (a,b) : ^T = Vector.InnerProd(a,b)

    /// the squared length of a vector
//        static member inline Len2 (a : Vector< ^T>) : ^T = a .*. a
    /// the length of a vector
    // static member inline Len = System.Math.Sqrt << Vector< ^T>.Len2
    // member inline v.Length = Vector< ^T>.Len v

    // /// normalize a vector (the result will have the same direction but length 1)
    // static member inline Normalize (a : Vector< ^T>) = (LanguagePrimitives.GenericOne / a.Length) * a
    // member inline v.Normal = Vector< ^T>.Normalize v

    /// create a zero-vector
    static member inline Zero(n) = Array.zeroCreate n |> Vector

    static member inline IsLinearlyDependent (vectors: Vector< ^T> seq) : bool =
        let dimensions = vectors |> Seq.map(fun a -> a.Dim) |> Seq.distinct
        if dimensions |> Seq.length > 1 then failwith "need vectors from same-dimensional vector space"
        let dimension = dimensions |> Seq.head
        if vectors |> Seq.length > dimension then false
        else failwith "not done"

