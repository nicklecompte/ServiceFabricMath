// Learn more about F# at http://fsharp.org

open System
open ServiceFabricMath.Math.LinearAlgebra.Matrices
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

type QuickSpanBenchmark() =
    
    let length = 10000

    let arrayInnerProduct (l: float array) (r: float array) : float =
        if l.Length <> r.Length then invalidArg "l,r" "l and r must be same length for inner product"
        let mutable sum = 0.0
        for i in 0..(l.Length - 1) do
            sum <- sum + (l.[i]*r.[i])
        sum

    let spanInnerProduct (l: Span<float>) (r: Span<float>) : float =
        if l.Length <> r.Length then invalidArg "l,r" "l and r must be same length for inner product"
        let mutable sum = 0.0
        for i in 0..(l.Length - 1) do
            sum <- sum + (l.[i]*r.[i])
        sum

    let rand = new System.Random()
    let testAr1 = Array.init length (fun _ -> rand.NextDouble())
    let testAr2 = Array.init length (fun _ -> rand.NextDouble())

    [<Benchmark>]
    member __.ArInnerProduct() = arrayInnerProduct testAr1 testAr2
    
    [<Benchmark>]
    member __.SpanInnerProduct() = spanInnerProduct (testAr1.AsSpan()) (testAr2.AsSpan())

    //[<Benchmark>]
    //member __.ArraySum() = arraySum()
    //[<Benchmark>]
    //member __.StackSum() = testSafeSumStack()
    //[<Benchmark>]
    //member __.NativeSum() = testSafeSumNative()
    //[<Benchmark>]
    //member __.ManagedSum() = testSafeSumManaged()

//type FloatDenseMatrixMultBenchmark() =
//    let rand = new Random()
//    let inner1dArray = Array.init 40000 (fun _ -> rand.NextDouble())
//    let fast2darray = new Fast2DArray<float>(inner1dArray,200,200)

//    [<Benchmark>]
//    member __.SafeMult () = safeFastArrayMultiplication fast2darray fast2darray // safeFastArrayMultiplication fast2darray fast2darray

//    [<Benchmark>]
//    member __.UnsafeMult () = unsafeDenseMatrixMatrixMultiplication fast2darray fast2darray


[<EntryPoint>]
let main argv =
    let summary = BenchmarkRunner.Run<QuickSpanBenchmark>() //BenchmarkRunner.Run<MatrixMultBenchmark>()
    //let switch = BenchmarkConverter.TypeToBenchmarks(typeof<MatrixMultBenchmark>)
    //let summary = BenchmarkRunnerCore.Run(switch,null)
    ////BenchmarkRunner.Run<MatrixMultBenchmark>()
    Console.ReadLine() |> ignore
    0 // return an integer exit code
