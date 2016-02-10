open System
open System.IO

//Pattern-matching
let urlFilter url agent =
    match url, agent with
    | "http://fsharpforfunandprofit.com", _ -> true
    | "http://homepages.inf.ed.ac.uk/wadler/topics/monads.html", _ -> true
    | _ -> invalidOp "Not a valid website address"


let primes = [2;3;5;7;11]
let primesList = 
    seq { for i in 2..11 do
            for j in 2..11 do
                yield (i, j, i*j) }
     

let squaredPrimes =  
    primes
    |> Seq.map(fun n -> n * n)
    |> Seq.iter(printfn "%A")



let printFirst primes =
    match primes with
    | h :: t -> printfn "The first is: %d" h
    | [] -> printfn "No primes found in list"


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
|> Seq.map (fun x ->
    match x%3, x%5 with
    | 0,0 -> "FizzBuzz"
    | 0,_ -> "Fizz"
    | _,0 -> "Buzz"
    | _ -> string x)
|> Seq.iter (printfn "%s")

//Pattern-matching on the parameters of a function
let fizzbuzz modFizz modBuzz = 
    [1..100]
    |> Seq.map (fun x ->
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
    |> Seq.map (fun x ->
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


//Demonstrating tail recursion w/ factorial computation
let factorial n =
    let rec loop i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> loop(i - 1) (acc * i)
    loop n 1 //tail recursion

//Imperative - usedmostly for flow control
let rec fib x =
    if x < 2 then 1
    else fib (x - 1) + fib(x - 2)
let results = Array.Parallel.map fib [|1..10|]

// Demonstrating function composition
let compose (f: 'b -> 'c) (g: 'a -> 'b) =
    fun a -> a |> g |> f


(* Currying
'a -> 'b -> 'c is a signature for a generic function
The ' * ' means that the parameters form a tuple
So 'a * 'b -> 'c would be the signature for fun (a, b) -> a * b 
where you multiply a and b.

'a -> 'b -> 'c would be the signature for fun a -> fun b -> a*b.
In F#, you can also write this as fun a b -> a*b.

The general idea is that fun (a,b) -> a*b takes the two parameters
at the same time.

fun a -> fun b -> a*b takes a then returns a new function 'b -> 'c with 
a captured within a closure: fun b -> a*b where a is already known.

let curry (f:'a * 'b -> 'c) : 'a -> 'b -> 'c =
    fun (a:'a) -> fun (b:'b) -> 'c
        fun(a, b) -> 'c

 So...how we would call f: 
 (a*b) -> 'c
*)

[<EntryPoint>]
let main argv =
    printFirst primes

    squaredPrimes
    (printNumbers())
    printfn "The 5th Fibonnaci number is: %d" (fib 5)
    printfn "%A" results
    printfn "%f" (zeta 15.0 0.0)
    printfn "%d" (factorial 5)
    0 // return an integer exit code

