open System
open System.IO

let printNumbers() =
    printfn "Welcome to the coding challenge where your program should successfully"
    printfn "accept the input of two integers and output their sum and product"
    printfn ""
    printfn "Enter an integer: "
    let a = Console.ReadLine() |> int
    printfn "Enter another integer: "
    let b = Console.ReadLine() |> int
    printfn "%d" (a+b)
    printfn "%d" (a*b)

//Pattern-matching using a "when" guard to establish rules for matching
let nums = [1..100] 
nums
|> Seq.map (function
    | x when x%5=0 && x%3=0 -> "FizzBuzz"
    | x when x%3=0 -> "Fizz"
    | x when x%5=0 -> "Buzz"
    | x -> string x)
|> Seq.iter (printfn "%s")

//Pattern-matching as part of a function that the original function was pipelined into
let nums2 = [1..100]
nums2
|> Seq.map (fun x->
    match x%3, x%5 with
    | 0,0 -> "FizzBuzz"
    | 0,_ -> "Fizz"
    | _,0 -> "Buzz"
    | _ -> string x)
|> Seq.iter (printfn "%s")

//Pattern-matching on the parameters of a function
let fizzbuzz modFizz modBuzz = 
    [1..100]
    |> Seq.map (fun x->
        match x%modFizz, x%modBuzz with
        | 0,0 -> "FizzBuzz"
        | 0,_ -> "Fizz"
        | _,0 -> "Buzz"
        | _ -> string x)
fizzbuzz 3 5
|> Seq.iter (printfn "%s")

//Passing functions as parameters in another function
let fizzBuzz modFizz modBuzz = 
    [1..100]
    |> Seq.map (fun x->
        match modFizz(x:int), modBuzz(x:int) with
        | 0,0 -> "FizzBuzz"
        | 0,_ -> "Fizz"
        | _,0 -> "Buzz"
        | _ -> string x)
fizzBuzz (fun x -> x%3) (fun x -> x%5)
|> Seq.iter (printfn "%s")

(*Demonstrating the power of F# in handling complicated algorithms
Here we compute the limit of an infinite series by applying Cauchy's theorem
to the Riemann zeta function which is a function of a complex variable, k,
that analytically continues the sum of the infinite series, which converges to
a limit of 1 when the real part of k is greater than 1.
*)
let zeta k e =
    let inner(i) = 1.0/(double(i)**k)
    let rec seqInfinite i xp =
        let x = xp + inner(i)
        match abs(xp - x) with
        | dx when dx <= e -> x
        | _ -> seqInfinite(i + 1) x
    seqInfinite 1 0.0
printfn "%f" (zeta 15.0 0.0)

[<EntryPoint>]
let main argv =
    (printNumbers())
   
    0 // return an integer exit code

