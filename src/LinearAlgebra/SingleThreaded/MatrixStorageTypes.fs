module ServiceFabricMath.LinearAlgebra.MatrixStorageTypes

open System.Text
open System.Collections.Generic

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
            s.nonzeroIndices.Add(coord) |> ignore
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