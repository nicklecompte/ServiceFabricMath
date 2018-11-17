module CommonTests.ComplexNumberTests

open System
open System.Collections
open Xunit
open ServiceFabricMath.Common.Complex
open ServiceFabricMath.Common.Complex

type RandomIntegerPairsData() =
    let rand = new Random()
    let sequence = seq {for i in 0..10 do 
                        yield
                            [|rand.Next(1,10000);rand.Next(1,10000);rand.Next(1,10000);rand.Next(1,10000)|] |> Array.map(fun i -> i :> obj)
                            }
    interface System.Collections.Generic.IEnumerable< obj array > with
        member x.GetEnumerator() = sequence.GetEnumerator()
        member x.GetEnumerator() : IEnumerator = (sequence :> IEnumerable).GetEnumerator()

type RandomFloatPairsData() =
    let rand = new Random()
    let sequence = seq {for i in 0..10 do 
                        yield
                            [|rand.NextDouble();rand.NextDouble();rand.NextDouble();rand.NextDouble()|] |> Array.map(fun i -> i :> obj)
                            }
    interface System.Collections.Generic.IEnumerable< obj array > with
        member x.GetEnumerator() = sequence.GetEnumerator()
        member x.GetEnumerator() : IEnumerator = (sequence :> IEnumerable).GetEnumerator()

[<Theory>]
[<ClassData(typedefof<RandomIntegerPairsData>)>]
let ``Complex integer operations work`` (a:int,b:int,c:int,d:int) =
    let complex1 = {real=a;complex=b}
    let complex2 = {real=c;complex=d}
    let sum = complex1 + complex2
    Assert.Equal((a+c),(sum.real))
    Assert.Equal((b + d), sum.complex)
    let diff = complex1 - complex2
    Assert.Equal((a-c),(diff.real))
    Assert.Equal((b - d), diff.complex)
    let prod = complex1 * complex2
    Assert.Equal((a*c-b*d),(prod.real))
    Assert.Equal((a*d + b*c), prod.complex)
    let complex2Modulus = Complex.squaredModulus complex2
    Assert.Equal(c*c + d*d, complex2Modulus)
//    let quotient = complex1 / complex2

[<Theory>]
[<ClassData(typedefof<RandomFloatPairsData>)>]
let ``Complex float operations work`` (a:float,b:float,c:float,d:float) =
    let complex1 = {real=a;complex=b}
    let complex2 = {real=c;complex=d}
    let sum = complex1 + complex2
    Assert.Equal((a+c),(sum.real))
    Assert.Equal((b + d), sum.complex)
    let diff = complex1 - complex2
    Assert.Equal((a-c),(diff.real))
    Assert.Equal((b - d), diff.complex)
    let prod = complex1 * complex2
    Assert.Equal((a*c-b*d),(prod.real))
    Assert.Equal((a*d + b*c), prod.complex)
    let complex2Modulus = Complex.squaredModulus complex2
    Assert.Equal(c*c + d*d, complex2Modulus)
    let quotient = complex1 / complex2
    Assert.Equal((a*c + b*d)/complex2Modulus,quotient.real)
    Assert.Equal((b*c - a*d)/complex2Modulus,quotient.complex)

