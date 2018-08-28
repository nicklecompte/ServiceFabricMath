namespace PowerSystemsAnalysis.Math

// www.fssnip.net/3p/title/QRdecomoposition-of-a-squarematrix-using-the-GramSchmidt-method, sorta :)
// want statically resolved type parameter so we can test float/decimal
module LinearAlgebra =

    // let inline ReduceToRowEchelonForm< ^T 
    //                               when ^T : (static member (+) :  ^T * ^T -> ^T ) 
    //                               and ^T : (static member (-) : ^T * ^T -> ^T ) 
    //                               and ^T : (static member (*) : ^T * ^T -> ^T) 
    //                               and ^T : (static member Zero : ^T) 
    //                               and ^T : equality > (ar: ^T [,]) =

    //                               let mutable pivotRow = 0
    //                               let mutable pivotColumn = 0
    //                               let n = Array2D.Length1 ar
    //                               let m = Array2D.Length2 ar
    //                               while (pivotRow < n && pivotColumn < m) do
    //                                 let col = ar.[0..,pivolColumn]
    //                                 let maxIndex = ar |> Array.max

    type Vector< ^T when ^T : (static member (+) :  ^T * ^T -> ^T ) and ^T : (static member (-) : ^T * ^T -> ^T ) and ^T : (static member (*) : ^T * ^T -> ^T) and ^T : (static member Zero : ^T) and ^T : equality > =  
    | Vector of ^T array
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
            else true


    type LinearPolynomial< ^T when ^T: (static member Zero: ^T) and ^T : (static member (+): ^T * ^T -> ^T) and ^T : (static member (-): ^T -> ^T -> ^T) and ^T : (static member (*):  ^T -> ^T -> ^T ) and ^T : equality> =
        | Monomial of Monomial : ^T*int // 0-based integer indexing on underlying vertex space (x_0,x_1,x_2...)
        | Sum of LinearPolynomial< ^T> * LinearPolynomial< ^T>
        with
            static member inline Zero : LinearPolynomial< ^T> = Monomial (LanguagePrimitives.GenericZero, 0)

                            
    // | Sum(Monomial (t,n),Monomial (u,o)) -> if n = o then  sumSimplifierCollection (Monomial(t+u,o)) lpB
    //                                                             else match lpB with
    //                                                                     | Monomial (v,p) -> if p = n then Sum(Monomial ((t+v),n),Monomial(u,o))
    //                                                                                         else if p = o then Sum(Monomial(t,n), Monomial((u+v),o))
    //                                                                                         else Sum(lpA,lpB)
    //                                                                     | Sum(Monomial (v,p), b) -> if p = n then sumSimplifierCollection (Sum(Monomial ((t+v),n),Monomial(u,o))) b
    //                                                                                                 else if p = o then sumSimplifierCollection (Sum(Monomial(t,n), Monomial((u+v),o))) b
    //                                                                                                 else Sum(lpA,(sumSimplifierCollection (Monomial (v,p)) b))
    //                                                                     | Sum(a,b) -> sumSimplifierCollection (sumSimplifierCollection lpA a) b


            member inline this.ImageSubspaceDimension =
                let rec numVariableAccumulator (lp: LinearPolynomial< ^T>) (acc:int) =
                    match lp with
                    | Monomial (_,n) -> (n+1)
                    | Sum (a,b) -> max (numVariableAccumulator a acc) (numVariableAccumulator b acc)
                numVariableAccumulator this 0
            member inline this.ToVector () : Vector< ^T> =
                let mutable initArray = Array.init (this.ImageSubspaceDimension) (fun _ -> LanguagePrimitives.GenericZero)
                let rec arrayDeterminer lp =
                    match lp with
                        | Monomial (t,n) -> initArray.[n] <- initArray.[n] + t
                        | Sum(a,b) -> (arrayDeterminer a) |> ignore
                                      (arrayDeterminer b) |> ignore
                arrayDeterminer this |> ignore
                Vector(initArray)

            static member inline FromVector (vec: Vector< ^T>) : LinearPolynomial< ^T> =
                        let maxIndex = vec.Dim - 1
                        let rec polyBuilder (index:int) (poly:LinearPolynomial< ^T>) =
                            if index = maxIndex then poly
                            else
                                if (vec.[index] = LanguagePrimitives.GenericZero) then polyBuilder (index+1) poly
                                else polyBuilder (index+1) (Sum(poly, (Monomial ((vec.[index]),index))))
                        polyBuilder 0 LinearPolynomial< ^T>.Zero

            static member inline (+) ((a:LinearPolynomial< ^T>),b: LinearPolynomial< ^T>) = 
                    (LinearPolynomial.FromVector (a.ToVector() + b.ToVector()))
            member inline this.Evaluate (x: Vector< ^T>) : ^T =

                let rec evaluateAccumulator (lp: LinearPolynomial< ^T>) (acc: ^T) =
                        match lp with
                            | Monomial (t,n) -> acc + t*x.[n] //(^T : (static member (*) : ^T -> ^T -> ^T) (acc,((^T : (static member op_Multiply : ^T * ^T -> ^T) (x.[n],t)))))
                            | Sum(a,b) -> evaluateAccumulator a (evaluateAccumulator b acc)
                evaluateAccumulator this LanguagePrimitives.GenericZero
            member inline this.ReduceToStandardForm : LinearPolynomial< ^T> = LinearPolynomial.FromVector (this.ToVector())

// #time

// let lp1 = Monomial (5.5,0)
// let lp2 = Monomial (4.5,0)
// let sum1 = lp1 + lp2
// let lp3 = Sum(Monomial (2.5,0),Monomial (5.5,1))
// let lp4 = Sum(Monomial (2.5,0),Monomial (4.5,1))

// let lp5 = LinearPolynomial.FromVector (Vector([|5;2;3;5;2;1;5;6;7|]))
// let lp6 = LinearPolynomial.FromVector (Vector([|2;3;7;1;6;8;2;7;1|]))

// let sum3 = lp5 + lp6
// let sum2 = lp3 + lp4
//     let lp = (Sum((Monomial (0.5,4)),(Monomial (1.2,1))))
//     let vec = lp.ToVector()



    type LinearMapping< ^T when ^T: (static member Zero: ^T) 
                            and ^T : (static member (+): ^T -> ^T -> ^T) 
                            and ^T : (static member (*):  ^T -> ^T -> ^T) 
                            and ^T: (static member (-): ^T -> ^T -> ^T)
                            and ^T: equality> = LinearPolynomial< ^T> array
                 
        //     static member inline ToMatrix (input:seq<LinearPolynomial< ^T>>) :  ^T[,]  =
        //             let rowCount = input |> Seq.length
        //             let colCount = (input |> Seq.maxBy(fun a -> a.ImageSubspaceDimension)).ImageSubspaceDimension

        //             let mutable polArray = Array2D.init rowCount colCount (fun _ _ -> LanguagePrimitives.GenericZero< ^T>)
        //             input
        //             |> Seq.iteri(fun index linearPolynomial ->
        //                                 let rec assignCoefficientToArray lp =
        //                                     match lp with
        //                                     | Monomial (t,n) -> polArray.[index,n-1] <- t
        //                                     | Sum (a,b) -> assignCoefficientToArray (a); assignCoefficientToArray (b)
        //                                 assignCoefficientToArray linearPolynomial)
        //             polArray
        //     // static member inline ToSeqLinearPolynomialFromMatrixAndVector (coefficientMatrix: ^T[,]) (rhsVector: ^T[]) =
            //     let imageDimension = rhsVector.Length
            //     if coefficientMatrix |> Array2D.length1 <> imageDimension then invalidArg "rhsVector" "rhsVector and coefficientMarix dimension mismatch"
            //     let coImageDimension = coefficientMatrix |> Array2D.length2
            //     let rec BuildMonomialFromSeqT (input: ^T list) (acc:int) (builder: LinearPolynomial< ^T>) =
            //         match input with
            //         | [] -> builder
            //         | x:xs -> BuildMonomialFromSeqT xs (acc+1) Sum(builder,Variable(x,acc))

//                for i in 1..imageDimension do
                    //./


    type VectorSpace< ^T when
                     ^T : (static member (+) : ^T -> ^T -> ^T ) 
                     and ^T : (static member (-) : ^T -> ^T -> ^T ) 
                     and ^T : (static member (*) : ^T -> ^T -> ^T) 
                     and ^T : (static member Zero : ^T) 
                     and ^T : equality> = {
            basis: Vector< ^T> array 
    } 
        with member inline this.Dimension = this.basis.Length