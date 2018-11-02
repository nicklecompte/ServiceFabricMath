namespace Concurrency

open System

type Concurrency() = 
    member this.X = "F#"

//let testSafeSum() = 
//    // managed memory
//    let arrayMemory = Array.zeroCreate<byte>(100)
//    let arraySpan = new Span<byte>(arrayMemory)

//    safeSum(arraySpan) |> printfn "res = %d"

//    // native memory
//    let nativeMemory = Marshal.AllocHGlobal(100);
//    let nativeSpan = new Span<byte>(nativeMemory.ToPointer(), 100)

//    safeSum(nativeSpan) |> printfn "res = %d"
//    Marshal.FreeHGlobal(nativeMemory)

//    // stack memory
//    let mem = NativePtr.stackalloc<byte>(100)
//    let mem2 = mem |> NativePtr.toVoidPtr
//    let stackSpan = Span<byte>(mem2, 100)

//    safeSum(stackSpan) |> printfn "res = %d"