module Tests

open System
open Xunit
open TokenParser.Parser

[<Fact>]
let ``It Runs`` () =
    "a+b=a-b+b-a".ToCharArray() |> Array.toList
    |> (fun x -> 
            printf "parsing: %A\n" x
            x) 
    |> parse 
    |> printfn "parsed: %A\n"
