module Program

open System
open MiniParser.Parser
open MiniParser.Visitor

[<EntryPoint>]
let main argv =
    // argv.[0].Split(' ') // simplest Tokenizer 
    [ "a";"(";"a";")"]    
    // [ "a"; "(";"a";",";"a";")"]
    // [ "("; "a"; "+";"b";")"; "*"; "c" ]
    // |> Array.toList    
    |> (fun input ->         
        let (expr, _) = ParseExpr input 0
        (input |> List.fold (+) "", visit expr)
        ||> printf "input: %A\n expr: %A\n")
    |> ignore
    0 // fin
