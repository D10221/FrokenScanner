module Program 

open System
open TokenParser.Parser
open TokenParser.Visitor

[<EntryPoint>]
let main argv =
    argv.[0].ToCharArray() |> Array.toList
    |> (fun x -> 
            x 
            |> List.fold (fun a b -> a + b.ToString()) ""
            |>printf "parsing: %A\n" 
            x) 
    |> parse 
    |> visitMany |> List.item 0
    |> printfn "parsed: %A\n"
    0 
