module ServiceFabricMath.LinearAlgebra.BasicMatrixOperations

open Vectors
open MatrixStorageTypes
open RealMatrices
open Microsoft.FSharp.NativeInterop


type MatrixOperations = | Empty

let inline gaussianElimination (matrix: Matrix< ^T>) (rhsVector : ColumnVector< ^T>) : ColumnVector< ^T> =
    failwith "not done"

let inline private unsafeDenseMatrixVectorMultiplication
    (matrix: Fast2DArray< ^T>)
    (vector: ^T array) :
    ^T array when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member Zero : ^T) and 
        ^T : unmanaged and
        ^T : struct and
        ^T : equality =
    let mRows = matrix.RowNum
    let mCols = matrix.ColNum
    let vLength = vector.Length
    if vLength <> mCols then invalidArg "matrix, vector" "matrix and vector have mismatched sizes"
    let elemSize = sizeof< ^T>
    let retVectorPtr : nativeptr< ^T> = NativePtr.stackalloc (mRows)
    let matArray = matrix.To1dArray
    let mStartPointer = &&(matArray.[0])
    let vectorStartPointer = &&(vector.[0])        
    //NativePtr.
    failwith "not done"

let inline unsafeDenseMatrixMatrixMultiplication
    (l: Fast2DArray< ^T>)
    (r: Fast2DArray< ^T>) :
    Fast2DArray< ^T> when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member Zero : ^T) and 
        ^T : unmanaged and
        ^T : struct and
        ^T : equality =
    if (l.ColNum) <> (r.RowNum) then invalidArg "l,r" "mismatched 2d arrays for multiplication"
    let lArray = l.To1dArray
    let mutable lArrayNativeint = System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(lArray,0)
    let rArray = r.To1dArray
    let mutable rArrayNativeint = System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(rArray,0)

    let floatSize = nativeint(sizeof< ^T>)
    let newArray : ^T array = Array.zeroCreate (l.RowNum * r.ColNum)
    let mutable newArrayNativeint = System.Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(newArray,0)
    failwith "not finished"
    //for i in 0..(l.RowNum - 1) do
        //let curRowIndex = i*(l.ColNum)
        //for j in 0..(r.ColNum - 1) do
        //    let mutable sum = LanguagePrimitives.GenericZero
        //    for k in 0..(l.ColNum - 1) do
        //        sum <- sum + (Marshal. )  //(Marshal. lArrayIntptr lArrayPtr (curRowIndex + k))*(NativePtr.get rArrayPtr (k*r.ColNum + j))
        //    NativePtr.set newArrayPtr (i*l.RowNum + j) sum
            //NativePtr.set newArrayPtr (i*(r.ColNum) + j) sum
    //Marshal.Copy((NativePtr.toNativeInt  (NativePtr.ofNativeInt newArrayPtr)), newArray,0,(l.RowNum * r.ColNum))
//    Fast2DArray< ^T>(newArray, l.RowNum, r.ColNum)

let inline safeFastArrayMultiplication (l: Fast2DArray< ^T>) (r: Fast2DArray< ^T>) : Fast2DArray< ^T> when 
        ^T : (static member (+) :  ^T * ^T -> ^T ) and 
        ^T : (static member (-) : ^T * ^T -> ^T ) and 
        ^T : (static member (*) : ^T * ^T -> ^T) and 
        ^T : (static member Zero : ^T) and 
        ^T : unmanaged and
        ^T : struct and
        ^T : equality =
    if (l.ColNum) <> (r.RowNum) then invalidArg "l,r" "mismatched 2d arrays for multiplication"
    let retArray =
        Array.init ((l.RowNum * r.ColNum)) 
            (fun i -> let (targetRow, targetCol) = ((i / (r.ColNum)), i % (l.ColNum))
                      let mutable sum = LanguagePrimitives.GenericZero
                      for j in 0..(l.ColNum - 1) do
                          sum <- sum + (l.[targetRow,j] * r.[j,targetCol])
                      sum)
    new Fast2DArray< ^T>(retArray,l.RowNum,r.ColNum)