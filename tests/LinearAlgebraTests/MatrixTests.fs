module MatrixTests

open System
open System.Collections
open System.Collections.Generic
open Xunit
open Xunit.Abstractions
open ServiceFabricMath.Math.LinearAlgebra
open Vectors
open Matrices

type MatrixTestClass(output:ITestOutputHelper) =

    [<Fact>]
    member __.``Quick dense matrix mult test`` () =
        let rand = new Random()
        let dense1 = Dense (Fast2DArray<double>.Init 4 4 (fun _ _ -> rand.NextDouble()))
        let dense2 = Dense (Fast2DArray<double>.Init 4 4 (fun _ _ -> rand.NextDouble()))
        let prod = (dense1*dense2)
        match prod with
            | Dense d -> output.WriteLine(d.ToString())
            | _ -> failwith "should have been dense"
        Assert.True(true)

  (*

public class MyTestClass
{
    private readonly ITestOutputHelper output;

    public MyTestClass(ITestOutputHelper output)
    {
        this.output = output;
    }

    [Fact]
    public void MyTest()
    {
        var temp = "my class!";
        output.WriteLine("This is output from {0}", temp);
    }
}

    *)