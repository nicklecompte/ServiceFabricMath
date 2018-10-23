namespace PowerSystemsAnalysis.Math

module Numbers =
    let isPrime n : bool =
        let rec isPrimeCore n m : bool =
            if m > n / 2 then true
            else if n % m = 0 then false
            else isPrimeCore n (m+1)
        isPrimeCore n 2

(*
Generate first 10,000 primes:
let firstNOddPrimes n =
    let mutable count = 0
    let mutable initInt = 1
    [|while count < n do
        initInt <- initInt + 2
        if isPrime initInt then 
            count <- count + 1
            yield initInt|]    
*)        

module ComplexAnalysis =
    open System
    open System.Numerics

    /// www.fssnip.net/dC/title/fast-Fourier-transforms-FFT-
    let rec fft = function
      | []  -> []
      | [x] -> [x] 
      | x ->
        x
        |> List.mapi (fun i c -> i % 2 = 0, c)
        |> List.partition fst
        |> fun (even, odd) -> fft (List.map snd even), fft (List.map snd odd)
        ||> List.mapi2 (fun i even odd -> 
            let btf = odd * Complex.FromPolarCoordinates(1., -2. * Math.PI * (float i / float x.Length ))
            even + btf, even - btf)
        |> List.unzip
        ||> List.append

/// Interactive Tests
(*
    
let input = [for x in 0. .. 255. -> cos(2.0*x/(255.0*Math.PI)) + sin(2.0*x/(255.0*Math.PI))]
    
let output = 
    input
    |> List.map (fun r -> Complex(r, 0.)) 
    |> fft
    |> List.map (fun c -> c.Real)
    
    *)        