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

let nums = [1..100] 
nums
|> Seq.map (function
    | x when x%5=0 && x%3=0 -> "FizzBuzz"
    | x when x%3=0 -> "Fizz"
    | x when x%5=0 -> "Buzz"
    | x -> string x)
|> Seq.iter (printfn "%s")


let nums2 = [1..100]
nums2
|> Seq.map (fun x ->
   match x%3, x%5 with
   | 0,0 -> "FizzBuzz"
   | 0,_ -> "Fizz"
   | _,0 -> "Buzz"
   | _ -> string x)
|> Seq.iter (printfn "%s")


let fizzbuzz modFizz modBuzz =
   [1..100]
   |> Seq.map (fun x ->
      match x%3, x%5 with
      | 0,0 -> "FizzBuzz"
      | 0,_ -> "Fizz"
      | _,0 -> "Buzz"
      | _ -> string x)
   |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main argv =
    (printNumbers())
    fizzbuzz 3 5
    0 // return an integer exit code

