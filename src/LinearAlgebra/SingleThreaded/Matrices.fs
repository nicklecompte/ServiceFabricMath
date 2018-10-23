module ServiceFabricMath.Math.LinearAlgebra.Matrices

open System
open System.Text
open System.Collections.Generic
open ServiceFabricMath.Math.LinearAlgebra.Vectors

// With the statically-resolved type parameters, F# sometimes throws
// what I hope are extraneous warnings about type resolution errors
// and indentation problems. I am pretty sure these are invalid :/
#nowarn "20" "193" "58"

[<Struct>]
type MatrixCoordinate = {row: int; column: int}

[<Struct>]
type MatrixDimension = {rowNumber: int; colNumber: int}

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
    member __.GetColumn (colNum:int) : 'T array =
        [|0..numRows - 1|]
        |> Array.map(fun rowNum -> innerArray.[rowNum*numCols + colNum])
    member __.Copy() : Fast2DArray<'T> = 
        new Fast2DArray<'T>(Array.copy innerArray,numRows,numCols)

    member __.Transpose() : Fast2DArray<'T> = 
        let new1dArray = 
            Array.init (innerArray.Length)
                (fun index -> innerArray.[(index % numRows)*numCols + index / numRows])
        new Fast2DArray<'T>(new1dArray, numCols, numRows)

    static member Init (numRows: int) (numCols: int) (initFunc: int -> int -> 'T) : Fast2DArray<'T> =
        let new1dArray = Array.zeroCreate (numRows*numCols)
        for col in 0..(numCols - 1) do
            for row in 0..(numRows - 1) do
                new1dArray.[numCols*(row) + col] <- initFunc row col
        new Fast2DArray<'T>(new1dArray,numRows,numCols)

    override x.ToString() = 
        let builder = new StringBuilder()
        let rowStringArray = 
            [|0..(numRows-1)|] |> Array.map(fun i -> sprintf "%A" (x.GetRow i))
        rowStringArray |> Array.iter(fun row -> builder.Append(row + "\n") |> ignore)
        builder.ToString()
//        builder.Append('┌').Append(String.replicate numCols "-").Append('┐')
//        builder.ToString()

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
    member x.To2DArray() : Fast2DArray<'T> =
        let retArray = new Fast2DArray<'T>(x.numRows,x.numCols)
        x.nonzeroValues |> Seq.iter(fun kvp -> retArray.[kvp.Key.row,kvp.Key.column] <- kvp.Value)
        retArray
    override x.ToString() = x.To2DArray().ToString()

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

    member __.Copy() : Tridiagonal<'T> = 
            let (lCopy,dCopy,uCopy) =
                (lower |> Array.copy,
                 diagonal |> Array.copy,
                 upper |> Array.copy)
            Tridiagonal(lCopy,dCopy,uCopy)
/// Upper triangular matrix. Values are stored by column in a 1d array.
/// The array [1;5;2;6;4;3] gives the following upper-triangular
/// [ 1 5 6 ]
/// [ 0 2 4 ]
/// [ 0 0 3]
type UpperTriangular<'T when 'T: struct and 'T: equality> (rowCount: int, valueAr: 'T array) =
    do
        if valueAr.Length <> ((rowCount+1)*rowCount)/2 then invalidArg "valueAr" "valueAr must have a triagnular number-amount of elements"

    /// 1D array access formula:
    /// (column - row) gives which of the diagonal bands the entry lies in (matrix diagonal = 0)
    /// (column-row)*(rowCount - (column - row)) + row
    member __.Item
        with get(index: MatrixCoordinate) : 'T = 
            if index.row > rowCount - 1 || index.column > rowCount - 1 then invalidArg "index" "index is greater than size of matrix"
            if index.row > index.column then Unchecked.defaultof<'T>
            else
                let (i,j) = (index.row,index.column) in
                    valueAr.[(i + (j+1)*(j)/2)]

        and set (index:MatrixCoordinate) (value: 'T) =
            if index.row > rowCount - 1 || index.column > rowCount - 1 then invalidArg "index" "index is greater than size of matrix"
            if index.row > index.column && value = Unchecked.defaultof<'T> then invalidArg "value" "value must be zero for upper triangular matrix"
            let (i,j) = (index.row,index.column) in
                valueAr.[(i + j*(j-1)/2)] <- value

    member __.GetRow(index:int) : 'T array =
        let zeros = Array.zeroCreate index
        let nonzeros = failwith "not done" //Array.init (rowCount - index) (fun i -> valueAr.[i].[index])
        Array.append zeros nonzeros
    member __.GetColumn(index:int) : 'T array =
        let zeros = Array.zeroCreate (rowCount - (index+1))
        let nonzeros = failwith "not done" //Array.init (index+1) (fun i -> valueAr.[index-i].[i])
        Array.append nonzeros zeros

    member __.Size = rowCount

////// Interactive tests ///////////

(*
[1  2  4  7  11
    3  5  8  12
       6  9  13
          10 14
             15]

let valueAr = [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15|];;
let ut = new UpperTriangular<int>(5,valueAr)
ut.[{row=0;column=0}] = 1
ut.[{row=0;column=1}] = 2
ut.[{row=0;column=2}] = 4
ut.[{row=0;column=3}] = 7
ut.[{row=0;column=4}] = 11
ut.[{row=1;column=0}] = 0
ut.[{row=1;column=1}] = 3
ut.[{row=1;column=2}] = 5
ut.[{row=1;column=3}] = 8
ut.[{row=1;column=4}] = 12
ut.[{row=2;column=0}] = 0
ut.[{row=2;column=1}] = 0
ut.[{row=2;column=2}] = 6
ut.[{row=2;column=3}] = 9
ut.[{row=2;column=4}] = 13
ut.[{row=3;column=0}] = 0
ut.[{row=3;column=1}] = 0
ut.[{row=3;column=2}] = 0
ut.[{row=3;column=3}] = 10
ut.[{row=3;column=4}] = 14
ut.[{row=4;column=0}] = 0
ut.[{row=4;column=1}] = 0
ut.[{row=4;column=2}] = 0
ut.[{row=4;column=3}] = 0
ut.[{row=4;column=4}] = 15



ut.[{row=2;column=1}]
ut.[{row=0;column=2}]
ut.[{row=0;column=3}]
ut.[{row=0;column=4}]

ut.GetRow(0)
ut.GetRow(1)
ut.GetRow(2)
ut.GetRow(3)
ut.GetRow(4)
ut.GetColumn(0)
ut.GetColumn(1)
ut.GetColumn(2)
ut.GetColumn(3)
ut.GetColumn(4)

*)

type Matrix< ^T when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member (/) : ^T * ^T -> ^T) and
        ^T : (static member Zero : ^T) and 
        ^T : (static member One : ^T) and
        ^T : (static member Sqrt : ^T -> ^T) and
        ^T : unmanaged and
        ^T : struct and
        ^T : equality> =
    | Diagonal of ^T array
    | Tridiagonal of Tridiagonal< ^T>
    | Dense of Fast2DArray< ^T>
    | Sparse of SparseMatrix< ^T>
    | UpperTriangular of UpperTriangular< ^T>

with
    member inline x.Dimension : MatrixDimension =
        match x with
            | Diagonal ar -> let size = ar.Length
                             {rowNumber = size; colNumber = size}
            | Tridiagonal t -> let size = t.Diagonal.Length
                               {rowNumber = size; colNumber = size}
            | Dense d -> {rowNumber=d.RowNum;colNumber=d.ColNum}
            | Sparse s -> {rowNumber=s.numRows;colNumber=s.numCols}
            | UpperTriangular ut -> {rowNumber = ut.Size;colNumber=ut.Size}

    member inline x.GetRow (rowNum:int) : RowVector< ^T> =
        if rowNum < 0 || rowNum > x.Dimension.rowNumber - 1 then invalidArg "rowNum" "row is not contained in matrix"
        match x with
            | Diagonal d -> 
                let newHashset = new HashSet<int>()
                newHashset.Add(rowNum)
                let newDict = new Dictionary<int, ^T>()
                newDict.Add(rowNum, d.[rowNum])
                SparseRow ({length=x.Dimension.rowNumber;nonzeroIndices=newHashset;nonzeroValues=newDict})
            | Tridiagonal t ->
                let newHashset = new HashSet<int>()    
                let newDict = new Dictionary<int, ^T>()
                let maxRowIndex = t.Diagonal.Length - 1
                if rowNum = 0 then
                        newHashset.Add(0)
                        newHashset.Add(1)
                        newDict.Add(0, t.Diagonal.[0])
                        newDict.Add(1, t.UpperDiagonal.[0])
                else if rowNum = maxRowIndex then
                        newHashset.Add(maxRowIndex)                   
                        newHashset.Add(maxRowIndex - 1)
                        newDict.Add(maxRowIndex,t.Diagonal.[maxRowIndex])
                        newDict.Add(maxRowIndex-1,t.LowerDiagonal.[maxRowIndex-1])
                else
                    [|rowNum-1;rowNum;rowNum+1|] 
                    |> Array.iter(fun i -> newHashset.Add(i) |> ignore)
                    newDict.Add(rowNum-1,t.LowerDiagonal.[rowNum])
                    newDict.Add(rowNum,t.Diagonal.[rowNum])
                    newDict.Add(rowNum+1,t.UpperDiagonal.[rowNum])
                SparseRow ({length=x.Dimension.rowNumber;nonzeroIndices=newHashset;nonzeroValues=newDict})
            | UpperTriangular ut -> ut.GetRow(rowNum) |> DenseRow // switch on sparse row?
            | Dense d ->
                DenseRow (d.GetRow(rowNum))
            | Sparse s ->
                let newIndices = new HashSet<int>()
                let newDict = new Dictionary<int, ^T>()
                [|0..(x.Dimension.colNumber-1)|]
                |> Array.iter(fun col -> 
                    let coord = {row=rowNum;column=col}
                    if s.nonzeroIndices.Contains(coord)
                        then newIndices.Add(col) |> ignore
                             newDict.Add(col, s.nonzeroValues.[coord]))
                SparseRow ({length=x.Dimension.colNumber; nonzeroIndices=newIndices;nonzeroValues=newDict})

    member inline x.GetColumn(columnNum:int) : ColumnVector< ^T> =
        if columnNum < 0 || columnNum > x.Dimension.rowNumber - 1 then invalidArg "columnNum" "column is not contained in matrix"
        match x with
            | Diagonal d -> 
                let newHS = new HashSet<int>()
                newHS.Add(columnNum) |> ignore
                let newDict = new Dictionary<int, ^T>()
                newDict.Add(columnNum, d.[columnNum])

                let retArray : SparseArray< ^T> =
                    {length = d.Length;
                     nonzeroIndices = newHS;
                    nonzeroValues = newDict}
                SparseColumn retArray
            | Tridiagonal t -> failwith "not done"
            | UpperTriangular ut -> DenseColumn (ut.GetColumn(columnNum))
            | Dense den -> DenseColumn (den.GetColumn(columnNum))
            | Sparse s -> failwith "not done"

    member inline x.Transpose() : Matrix< ^T> =
        match x with
            | Diagonal d -> Diagonal d
            | Tridiagonal t -> Tridiagonal (new Tridiagonal< ^T>(t.UpperDiagonal,t.Diagonal,t.LowerDiagonal))
            | Dense de -> Dense (de.Transpose())


    member inline x.ToDenseMatrix() : Matrix< ^T> =
        match x with
        | Diagonal d -> failwith "not done"
        | Tridiagonal t -> failwith "not done"
        | Dense _ -> x
        | Sparse s -> Dense (s.To2DArray())
        | UpperTriangular ut2 -> failwith "not done"
    static member inline Zero (rows: int) (cols: int) : Matrix< ^T> =
        if rows = cols then Diagonal (Array.zeroCreate rows)
        else Sparse ({nonzeroIndices = new HashSet<MatrixCoordinate>(); 
                      nonzeroValues = new Dictionary<MatrixCoordinate, ^T>();
                      numRows = rows; numCols = cols})

    /// Actually the identity, but called "One" so it can be used in F# static generic programming.
    static member inline One (rows: int) : Matrix< ^T> =
        Diagonal (Array.init rows (fun i -> LanguagePrimitives.GenericOne))

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
            | UpperTriangular ut2 -> failwith "not done"
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
                    let dCopy = s.To2DArray()
                    t1.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                    t1.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                    t1.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                    Dense dCopy
            | UpperTriangular ut2 -> failwith "not done"
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
            | Sparse s -> let dCopy = s.To2DArray()
                          let newAr = d1.To1dArray |> Array.zip dCopy.To1dArray |> Array.map(fun (a,b) -> a + b)
                          Dense (new Fast2DArray< ^T>(newAr, d1.RowNum, d1.ColNum))
            | UpperTriangular ut2 -> failwith "not done"       

        | UpperTriangular ut2 -> failwith "not done"                           

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
                    let dCopy = s1.To2DArray()
                    t2.Diagonal |> Array.iteri(fun index value -> dCopy.[index,index] <- dCopy.[index,index] + value)
                    t2.LowerDiagonal |> Array.iteri(fun index value -> dCopy.[index+1,index] <- dCopy.[index+1,index] + value)
                    t2.UpperDiagonal |> Array.iteri(fun index value -> dCopy.[index,index+1] <- dCopy.[index,index+1] + value)
                    Dense dCopy
            | Dense d -> let dCopy = s1.To2DArray()
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
                if density > 0.5 then // if half the entries are nonzero then store as Dense
                    let s1Dense = s1.To2DArray()
                    let s2Dense = s2.To2DArray()
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
            | UpperTriangular ut2 -> failwith "not done"                                                                                         

    static member inline ScaleRow (m: Matrix< ^T>) (rowIndex: int) (scalar: ^T) : Matrix< ^T> =
        match m with
        | Diagonal d -> 
            let arCopy = d |> Array.copy
            d.[rowIndex] <- d.[rowIndex] * scalar
            Diagonal d
        | Tridiagonal t ->                        
            let t' = t.Copy()
            if rowIndex = 0 then failwith "not done"
            else if rowIndex = m.Dimension.rowNumber - 1 then failwith "not done"
            else
                (t').LowerDiagonal.[rowIndex - 1] <- (t').LowerDiagonal.[rowIndex - 1] * scalar
                (t').Diagonal.[rowIndex] <- (t').Diagonal.[rowIndex - 1] * scalar
                (t').UpperDiagonal.[rowIndex - 1] <- (t').UpperDiagonal.[rowIndex - 1] * scalar
                Tridiagonal(t')
        failwith "not done"

    static member inline (*) (m: Matrix< ^T>,colVec: ColumnVector< ^T>) : ColumnVector< ^T> =
        if m.Dimension.rowNumber <> colVec.Dimension then invalidArg "colVec" "column vector must be same length as matrix has rows"
        match m with
            | Diagonal d -> 
                match colVec with
                    | DenseColumn c -> DenseColumn (Array.productPair d c)
                    | SparseColumn s ->
                        let newSparse = s.DeepCopy()
                        d |> Array.iteri(fun i valu ->
                                            if newSparse.nonzeroIndices.Contains(i) then newSparse.nonzeroValues.[i] <- valu*(newSparse.nonzeroValues.[i])
                                            else ())
                        SparseColumn newSparse
            | Tridiagonal t -> failwith "not done"
            | _ -> // works on sparse or dense, should be ok.
                [|0..m.Dimension.rowNumber|]
                |> Array.map(fun rowNum -> 
                                innerProduct (m.GetRow(rowNum)) colVec) // this should be reasonably fast 
                |> DenseColumn                                          // but is probably a good place for optimization later
                                                                  
    static member inline (*) (fst: Matrix< ^T>,snd: Matrix< ^T>) : Matrix< ^T> =
        if fst.Dimension.colNumber <> snd.Dimension.rowNumber then invalidArg "fst,snd" "snd must have same number of rows as fst has columns"
        match fst with
        | Diagonal d1 ->
            match snd with
            | Diagonal d2 ->
                Diagonal (d1 |> Array.zip d2 |> Array.map(fun (a,b) -> a*b))
            | Tridiagonal t2 -> 
                let tNew = t2.Copy()
                d1 |> Array.iteri(fun i diag -> 
                                    if i = 0 then
                                        tNew.UpperDiagonal.[0] <- tNew.UpperDiagonal.[0]*diag
                                        tNew.Diagonal.[0] <- tNew.Diagonal.[0]*diag
                                    else if i = fst.Dimension.rowNumber - 1 then
                                        tNew.LowerDiagonal.[i-1] <- tNew.LowerDiagonal.[i-1]*diag
                                        tNew.Diagonal.[i] <- tNew.Diagonal.[i]*diag
                                    else
                                        tNew.LowerDiagonal.[i-1] <- tNew.LowerDiagonal.[i-1]*diag
                                        tNew.UpperDiagonal.[i] <- tNew.UpperDiagonal.[i]*diag
                                        tNew.Diagonal.[i] <- tNew.Diagonal.[i]*diag)
                Tridiagonal t2
            | Dense den2 -> failwith "not done"
            | Sparse s2 -> failwith "not done"
            | UpperTriangular ut2 -> failwith "not done"
        | Tridiagonal t1 ->
            match snd with
            | Diagonal d2 -> failwith "not done"
            | Tridiagonal t2 -> failwith "not done"
            | Dense den2 -> failwith "not done"
            | Sparse s2 -> failwith "not done"
            | UpperTriangular ut2 -> failwith "not done"
        | Dense den1 ->
            match snd with
            | Diagonal d2 -> failwith "not done"
            | Tridiagonal t2 -> failwith "not done"
            | Dense den2 ->
                let newAr : ^T array = Array.zeroCreate ((fst.Dimension.rowNumber) * (snd.Dimension.colNumber))
                for i in 0..(fst.Dimension.rowNumber - 1) do
                    for j in 0..(snd.Dimension.colNumber - 1) do
                        try
                            newAr.[(snd.Dimension.colNumber)*(i) + j] <- innerProduct (den1.GetRow(i) |> DenseRow) (den2.GetColumn(j) |> DenseColumn)
                        with
                        | _ -> printfn "erorr was here"
                Dense (new Fast2DArray< ^T>(newAr, fst.Dimension.rowNumber, snd.Dimension.colNumber))
                // Dense 
                //     (Fast2DArray.Init 
                //         (fst.Dimension.colNumber) 
                //         (snd.Dimension.rowNumber) 
                //         (fun i j -> 
                //             innerProduct 
                //                 (den1.GetRow(i) |> DenseRow) 
                //                 (den2.GetColumn(j) |> DenseColumn)))

            | Sparse s2 -> failwith "not done"
            | UpperTriangular ut2 -> failwith "not done"
        | Sparse s1 ->
            match snd with
            | Diagonal d2 -> failwith "not done"
            | Tridiagonal t2 -> failwith "not done"
            | Dense den2 -> failwith "not done"
            | Sparse s2 -> failwith "not done"
            | UpperTriangular ut2 -> failwith "not done"

        | UpperTriangular ut ->
            match snd with
            | Diagonal d2 -> failwith "not done"
            | Tridiagonal t2 -> failwith "not done"
            | Dense den2 -> failwith "not done"
            | Sparse s2 -> failwith "not done"
            | UpperTriangular ut2 -> failwith "not done"

        //failwith "not done"

                            



(*
let testAr = new Fast2DArray<int>(3,3);;
testAr.[0,0] <- 2
testAr.[1,1] <- 3
testAr.[2,2] <- 4
testAr.GetRow 0
testAr.GetRow 1
testAr.GetRow 2

#time
let rand = new System.Random()
let dense1 = Dense (Fast2DArray<float>.Init 1000 1000 (fun _ _ -> rand.NextDouble()))
let dense2 = Dense (Fast2DArray<float>.Init 1000 1000 (fun _ _ -> rand.NextDouble()))
let prod = (dense1*dense2) |> ignore

*)