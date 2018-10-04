module ServiceFabricMath.Math.LinearAlgebra.BasicMatrixOperations

open ServiceFabricMath.Math.LinearAlgebra.Vectors

let inline rowMatrixToColMatrix (rowVectors : BasicRowVector< ^T> list) : BasicColumnVector< ^T> list =
    match rowVectors with
        | [] -> []
        | firstRow :: _ -> 
            let numOfCols = firstRow.Dimension
            [1..numOfCols] |> List.map(fun colIndex -> 
                                        rowVectors |> List.map(fun vec -> vec.[colIndex - 1]) |> List.toArray)

let inline colMatrixToRowMatrix (columnVectors: BasicColumnVector< ^T> list) : BasicRowVector< ^T> list =
   match columnVectors with
        | [] -> []
        | firstCol :: _ -> 
            let numOfRows = firstCol.Dimension
            [1..numOfRows] |> List.map(fun rowIndex -> 
                                            columnVectors |> List.map(fun vec -> vec.[rowIndex - 1]) |> List.toArray)    

/// Close-enough-to-immutable row-switching. Internal to List.map is a mutable array but that shouldn't go between threads.
let inline swapRows (columnVectors: BasicColumnVector< ^T> list) (rowInd1: int) (rowInd2: int) : BasicColumnVector< ^T> list =
    columnVectors |> List.map(fun vec -> 
                            let copyVec : BasicColumnVector<_> = Array.copy vec
                            copyVec.[rowInd2] <- vec.[rowInd1]
                            copyVec.[rowInd1] <- vec.[rowInd2]
                            copyVec
                            )
let inline swapCols (columnVectors: BasicColumnVector< ^T> list) (colIndLeft: int) (rowInd2: int) : _ array list =
    if colIndLeft >= rowInd2 then invalidArg "rowInd1" "first column must be left of second column"
    let row1 = columnVectors.[colIndLeft]
    let row2 = columnVectors.[rowInd2]
    let firstOtherCols = columnVectors |> List.take (colIndLeft)
    let middleCols = columnVectors |> List.skip (colIndLeft+1) |> List.take (rowInd2-colIndLeft - 1)
    let lastCols = columnVectors |> List.skip (rowInd2 + 1)
    lastCols  |> List.append [row1] |> List.append middleCols |> List.append [row2] |> List.append  firstOtherCols

let inline multiplyRow 
    (scalar: ^T) 
    (rowIndex: int) 
    (vectors: BasicColumnVector< ^T> list) 
        : BasicColumnVector< ^T> list =
    let rec multiplyRowCore (continuation: BasicColumnVector< ^T> list -> BasicColumnVector< ^T> list) = function
                | [] -> continuation []
                | x :: xs -> let colCopy : BasicColumnVector< ^T> = Array.copy x
                             colCopy.[rowIndex] <- scalar * (x.[rowIndex])
                             multiplyRowCore (fun accumulator -> continuation (colCopy :: accumulator)) xs
    multiplyRowCore id vectors

let inline divideRow 
    (scalar: ^T) 
    (rowIndex: int) 
    (vectors: BasicColumnVector< ^T> list) 
    : BasicColumnVector< ^T> list  =
    let rec divideRowCore (continuation: BasicColumnVector< ^T> list -> BasicColumnVector< ^T> list) = function
                | [] -> continuation []
                | x :: xs -> let colCopy : BasicColumnVector< ^T> = Array.copy x
                             colCopy.[rowIndex] <- (x.[rowIndex]) / scalar
                             divideRowCore (fun accumulator -> continuation (colCopy :: accumulator)) xs
    divideRowCore id vectors

let inline multiplyColumn 
    (scalar: ^T) 
    (colIndex: int) 
    (vectors: BasicColumnVector< ^T> list) 
    : BasicColumnVector< ^T> list =
    let rec multiplyColumnCore (vecs: BasicColumnVector< ^T> list) (ind: int) (continuation: BasicColumnVector< ^T> list -> BasicColumnVector< ^T> list) = 
        match vecs with
            | [] -> continuation []
            | x :: xs ->
                if ind = 0 then continuation ((x |> Array.map ((*) scalar)) :: xs )
                else multiplyColumnCore xs (ind - 1) (fun acc -> continuation (x :: acc))
    multiplyColumnCore vectors colIndex id


let inline getRow (matrix: BasicColumnVector< ^T> list) (rowIndex: int) : BasicRowVector< ^T> =
    matrix |> List.map(fun column -> column.[rowIndex]) |> List.toArray

let inline scaleRow 
    (scalar: ^T) 
    (row: BasicRowVector< ^T>) 
    : BasicRowVector< ^T> = 
        row |> Array.map(fun elt -> elt * scalar)


let inline addRow 
    (rowToAdd: BasicRowVector< ^T>) 
    (rowIndexToAddTo: int) 
    (matrix: BasicColumnVector< ^T> list) 
    : BasicColumnVector< ^T> list = 
    matrix |> List.mapi(fun i column -> 
                            let scaledColumn = Array.copy column
                            scaledColumn.[rowIndexToAddTo] <- column.[rowIndexToAddTo] + rowToAdd.[i]
                            scaledColumn)

let inline zeroOutLowerRows (lowerNatrix : BasicRowVector< ^T> list) (normalizedRow : BasicRowVector< ^T>) :  BasicRowVector< ^T> list  = 
    let rowScalings = lowerNatrix |> List.map (Array.head) |> List.toArray
    lowerNatrix |> List.mapi(fun rowIndex rowVec ->
                                    rowVec |> Array.mapi(fun colIndex vectorElement ->
                                                            let rowSacle = rowScalings.[rowIndex]
                                                            vectorElement - (rowSacle * normalizedRow.[colIndex]))
                                                        )

let testVects : BasicColumnVector<double> list = [ [|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|] ]

let testRowVects : BasicRowVector<double> list = [ [|1.0;2.0;3.0|];[|4.0;5.0;6.0|];[|7.0;8.0;9.0|] ]

let testVect : BasicColumnVector<double> list = [ [|1.0;2.0;3.0|] ]

swapRows testVects 1 2

swapCols testVects 1 2

multiplyRow 5.0 1 testVects

multiplyRow 5.0 1 testVect

multiplyColumn 5.0 1 testVects

//getRREF testVects

