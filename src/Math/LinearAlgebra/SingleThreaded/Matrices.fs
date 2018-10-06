module ServiceFabricMath.Math.LinearAlgebra.Matrices

open System
open System.Text
open System.Collections.Generic

// With the statically-resolved type parameters, F# sometimes throws
// what I hope are extraneous warnings about type resolution errors
// and indentation problems. I am pretty sure these are invalid :/
#nowarn "20" "193" "58"

module Array =
    let inline sumPair (ar1:^T array) (ar2: ^T array) : ^T array 
            when ^T : (static member (+) : ^T* ^T -> ^T) =
        ar1 |> Array.zip ar2 |> Array.map (fun (a,b) -> a + b)

[<Struct>]
type MatrixCoordinate = {row: int; column: int}

[<Struct>]
type MatrixDimension = {rowNumber: int; colNumber: int}

type SparseMatrix< 'T when 'T : struct> = {
    numRows : int
    numCols : int
    nonzeroIndices : HashSet<MatrixCoordinate>
    nonzeroValues : Dictionary<MatrixCoordinate, 'T>
}
with
    member x.Copy() = 
        let nonzeroIndicesCopy =
            let blankAr = Array.zeroCreate x.nonzeroIndices.Count
            new HashSet<MatrixCoordinate>(blankAr |> Array.toSeq)
        let nonzeroValuesCopy = new Dictionary<MatrixCoordinate, 'T>(x.nonzeroValues)
        {numRows = x.numRows;
        numCols = x.numCols;
        nonzeroIndices = nonzeroIndicesCopy;
        nonzeroValues = nonzeroValuesCopy}

    member s.Update (value : 'T) (coord: MatrixCoordinate) =
        if s.nonzeroIndices.Contains(coord) 
            then s.nonzeroValues.[coord] <- value
        else 
            s.nonzeroIndices.Add(coord)
            s.nonzeroValues.Add(coord,value)    

type Fast2DArray< 'T when 'T : struct> 
    (numRows: int, numCols: int) =

    let mutable innerArray : 'T array = Array.zeroCreate (numRows*numCols)

    member private __.InnerArray
        with get() = innerArray
        and set(value) = innerArray <- value

    new(input:'T array, numRows : int, numCols : int) 
        as this = Fast2DArray(numRows, numCols) then this.InnerArray <- input

    member __.RowNum = numRows
    member __.ColNum = numCols

    member __.To1dArray = innerArray
    /// Get according to 0-based indexing
    member __.Item
        with get(row:int, col: int) : ^T = innerArray.[numCols*(row) + col] 
        and set (row,col) (value : ^T) = innerArray.[numCols*(row) + col] <- value

    member __.GetRow (rowNum:int) : 'T array = innerArray.[rowNum*(numCols)..(rowNum*(numCols) + numCols - 1)]
    member __.Copy() : Fast2DArray<'T> = 
        new Fast2DArray<'T>(Array.copy innerArray,numRows,numCols)

    override x.ToString() = 
        let builder = new StringBuilder()
        let rowStringArray = 
            [|0..(numRows-1)|] |> Array.map(fun i -> sprintf "%A" (x.GetRow i))
        rowStringArray |> Array.iter(fun row -> builder.Append(row + "\n") |> ignore)
        builder.ToString()
//        builder.Append('┌').Append(String.replicate numCols "-").Append('┐')
//        builder.ToString()

let sparseTo2DArray (s: SparseMatrix< 'T>) : Fast2DArray<'T> = failwith "not done"

type Tridiagonal<'T when 'T : struct>(lower: 'T array, diagonal : 'T array, upper : 'T array) =
    // Validate the argument, throw invalidArg exception if it doesn't work.
    do 
        if lower.Length <> upper.Length && lower.Length <> diagonal.Length - 1 
            then invalidArg "lower, diagonal, upper" "lower, diagonal, and upper arrays must have consistent sizes."
    member __.LowerDiagonal = lower
    member __.Diagonal = diagonal
    member __.UpperDiagonal = upper
    member __.To2dArray() =
        let emptyArray = new Fast2DArray<'T>(diagonal.Length,diagonal.Length)
        upper |> Array.iteri(fun index value -> emptyArray.[index,(index+1)] <- value)
        diagonal |> Array.iteri(fun index value -> emptyArray.[index,index] <- value)
        lower |> Array.iteri(fun index value -> emptyArray.[(index+1),index] <- value)
        emptyArray


type Matrix< ^T when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and 
        ^T : (static member One : ^T) and
        ^T : (static member Sqrt : ^T -> ^T) and
        ^T : struct and
        ^T : equality> =
    | Diagonal of ^T array
    | Tridiagonal of Tridiagonal< ^T>
    | Dense of Fast2DArray< ^T>
    | Sparse of SparseMatrix< ^T>

with
    member inline x.Dimension : MatrixDimension =
        match x with
            | Diagonal ar -> let size = ar.Length
                             {rowNumber = size; colNumber = size}
            | Tridiagonal t -> let size = t.Diagonal.Length
                               {rowNumber = size; colNumber = size}
            | Dense d -> {rowNumber=d.RowNum;colNumber=d.ColNum}
            | Sparse s -> {rowNumber=s.numRows;colNumber=s.numCols}

    static member inline Zero (rows: int) (cols: int) : Matrix< ^T> =
        if rows = cols then Diagonal (Array.zeroCreate rows)
        else Sparse ({nonzeroIndices = new HashSet<MatrixCoordinate>(); 
                      nonzeroValues = new Dictionary<MatrixCoordinate, ^T>();
                      numRows = rows; numCols = cols})

    static member inline (+) (fst: Matrix< ^T>,(snd: Matrix< ^T>)) : Matrix< ^T> =
        if fst.Dimension <> snd.Dimension 
            then invalidArg "fst,snd" "matrices must have same dimension to compute sum"
        match fst with
            | Diagonal d1 ->
                match snd with
                    | Diagonal d2 -> d1 |> Array.zip d2 |> Array.map(fun (a,b) -> a + b) |> Diagonal
                    | Tridiagonal t ->
                        let newDiag = 
                            t.Diagonal |> Array.copy |> Array.zip d1 |> Array.map(fun (a,b) -> a + b)
                        Tridiagonal (new Tridiagonal< ^T>(Array.copy t.LowerDiagonal,newDiag,Array.copy t.UpperDiagonal))
                    | Dense d -> let dCopy = d.Copy()
                                 d1 |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                                 Dense dCopy
                    | Sparse s -> let sparseCopy = s.Copy()
                                  d1 
                                  |> Array.iteri(fun index value ->
                                                    let coord = {row = index; column=index}
                                                    sparseCopy.Update (value + s.nonzeroValues.[coord]) coord)
                                  Sparse sparseCopy // assume adding diagonal entries doesn't meaningfully affect sparsity                                                     
            | Tridiagonal t1 ->
                match snd with
                    | Diagonal d2 -> 
                        let diagSum = t1.Diagonal |> Array.sumPair d2
                        Tridiagonal (new Tridiagonal< ^T>(t1.LowerDiagonal |> Array.copy, diagSum, t1.UpperDiagonal |> Array.copy))
                    | Tridiagonal t2 -> 
                        let lowerSum = t1.LowerDiagonal |> Array.sumPair t2.LowerDiagonal
                        let diagSum = t1.Diagonal |> Array.sumPair t2.Diagonal
                        let upperSum = t1.UpperDiagonal |> Array.sumPair t2.UpperDiagonal
                        Tridiagonal (new Tridiagonal< ^T>(lowerSum,diagSum,upperSum))
                    | Dense d -> let dCopy = d.Copy()
                                 t1.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                                 t1.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                                 t1.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                                 Dense dCopy
                    | Sparse s -> 
                        if (float(s.nonzeroIndices.Count + t1.Diagonal.Length*3))/(float(s.numRows * s.numCols)) > 0.5 then
                           let sparseCopy = s.Copy()
                           t1.Diagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index; column=index}
                                             sparseCopy.Update (value + s.nonzeroValues.[coord]) coord)
                           t1.LowerDiagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index+1; column=index}
                                             sparseCopy.Update (value + s.nonzeroValues.[coord]) coord)
                           t1.UpperDiagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index; column=index+1}
                                             sparseCopy.Update (value + s.nonzeroValues.[coord]) coord)                                                        
                           Sparse sparseCopy    
                        else // it's better to store as dense
                            let dCopy = sparseTo2DArray s
                            t1.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                            t1.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                            t1.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                            Dense dCopy
            | Dense d1 -> 
                match snd with
                    | Diagonal d2 -> let dCopy = d1.Copy()
                                     d2 |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                                     Dense dCopy
                    | Tridiagonal t2 ->
                            let dCopy = d1.Copy()
                            t2.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                            t2.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                            t2.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                            Dense dCopy
                    | Dense d2 ->
                        let newAr = d1.To1dArray |> Array.zip d2.To1dArray |> Array.map(fun (a,b) -> a + b)
                        Dense (new Fast2DArray< ^T>(newAr, d2.RowNum, d2.ColNum))
                    | Sparse s -> let dCopy = sparseTo2DArray s
                                  let newAr = d1.To1dArray |> Array.zip dCopy.To1dArray |> Array.map(fun (a,b) -> a + b)
                                  Dense (new Fast2DArray< ^T>(newAr, d1.RowNum, d1.ColNum))

            | Sparse s1 ->
                match snd with
                    | Diagonal d2 -> 
                        let sparseCopy = s1.Copy()
                        d2 
                        |> Array.iteri(fun index value ->
                                        let coord = {row = index; column=index}
                                        sparseCopy.Update (value + s1.nonzeroValues.[coord]) coord)
                        Sparse sparseCopy
                    | Tridiagonal t2 ->
                        if (float(s1.nonzeroIndices.Count + t2.Diagonal.Length*3))/(float(s1.numRows * s1.numCols)) > 0.5 then
                           let sparseCopy = s1.Copy()
                           t2.Diagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index; column=index}
                                             sparseCopy.Update (value + s1.nonzeroValues.[coord]) coord)
                           t2.LowerDiagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index+1; column=index}
                                             sparseCopy.Update (value + s1.nonzeroValues.[coord]) coord)
                           t2.UpperDiagonal
                           |> Array.iteri(fun index value ->
                                             let coord = {row = index; column=index+1}
                                             sparseCopy.Update (value + s1.nonzeroValues.[coord]) coord)                                                        
                           Sparse sparseCopy    
                        else // it's better to store as dense
                            let dCopy = sparseTo2DArray s1
                            t2.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                            t2.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                            t2.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                            Dense dCopy
                    | Dense d -> let dCopy = sparseTo2DArray s1
                                 let newAr = d.To1dArray |> Array.zip dCopy.To1dArray |> Array.map(fun (a,b) -> a + b)
                                 Dense (new Fast2DArray< ^T>(newAr, d.RowNum, d.ColNum))
                    | Sparse s2 -> //  if density > 1/2 then make it dense
                        let density = 
                            let s1HashsetCopy = 
                                let emptyar = Array.zeroCreate s1.nonzeroIndices.Count
                                s1.nonzeroIndices.CopyTo(emptyar)
                                new HashSet<MatrixCoordinate>(emptyar)
                            let s2HashsetCopy = 
                                let emptyar = Array.zeroCreate s2.nonzeroIndices.Count
                                s2.nonzeroIndices.CopyTo(emptyar)
                                new HashSet<MatrixCoordinate>(emptyar)
                            s1HashsetCopy.IntersectWith(s2HashsetCopy)
                            float(s1HashsetCopy.Count)/(float(s1.numCols*s1.numRows))
                        if density > 0.5 then // dense
                            let s1Dense = sparseTo2DArray s1
                            let s2Dense = sparseTo2DArray s2
                            let sumar = s1Dense.To1dArray |> Array.zip (s2Dense.To1dArray) |> Array.map(fun (a,b) -> a + b)
                            Dense (new Fast2DArray< ^T>(sumar, s1.numRows, s1.numCols))
                        else // sparse
                            let newHashset = new HashSet<MatrixCoordinate>()
                            let newdict = new Dictionary<MatrixCoordinate, ^T>()
                            s1.nonzeroIndices |> Seq.iter(fun coord -> 
                                                    if s2.nonzeroIndices.Contains(coord) then
                                                        let valS1 = s1.nonzeroValues.[coord]
                                                        let valS2 = s2.nonzeroValues.[coord]
                                                        if valS2 + valS1 = LanguagePrimitives.GenericZero then ()
                                                        else
                                                            newHashset.Add(coord)
                                                            newdict.Add(coord,(valS1 + valS2)))
                            s1.nonzeroIndices |> Seq.iter(fun coord ->
                                                            if s1.nonzeroIndices.Contains(coord) then ()
                                                            else
                                                                newHashset.Add(coord)
                                                                newdict.Add(coord,s2.nonzeroValues.[coord]))
                            Sparse ({nonzeroIndices=newHashset;
                                     nonzeroValues=newdict;
                                     numCols=s1.numCols;
                                     numRows=s1.numRows})                                                            
                            

                            



(*
let testAr = new Fast2DArray<int>(3,3);;
testAr.[0,0] <- 2
testAr.[1,1] <- 3
testAr.[2,2] <- 4
testAr.GetRow 0
testAr.GetRow 1
testAr.GetRow 2
*)