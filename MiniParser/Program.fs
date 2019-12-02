module Program

open System
open MiniParser

[<EntryPoint>]
let main argv =
    // argv.[0].Split(' ') // simplest Tokenizer 
    [ "a";"(";"a";")"]
    // |> Array.toList    
    |> (fun input ->         
        let (expr, _) = parseExpr input 0
        (input |> List.fold (+) "", visit expr)
        ||> printf "input: %A\n expr: %A\n")
    |> ignore
    0 // fin
