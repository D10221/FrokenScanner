module Program

open System
open MiniParser

[<EntryPoint>]
let main argv =
    let input = ["a";"*";"b";"+";"c"; "*"; "d" ;"="; "e";"/";"f";"-";"g"; "/"; "h"]
    let input = [ "("; "a"; "+";"b";")"; "*"; "c" ]
    let precedence = 0 
    let (expr, _) = parseExpr input precedence
    (input|> List.fold (+) "" , visit expr)
    ||> printf "input: %A\n expr: %A\n"
    0 // fin
