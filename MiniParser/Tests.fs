module Tests

open MiniParser
open Xunit
open System.Text.RegularExpressions

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let Test1() =
    let input = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let precedence = 0
    let (expr, _) = parseExpr input precedence
    input |> List.fold (+) "",
    visit expr
    |> (fun input -> Regex.Replace(input, "\"", ""))
    |> equals "(((a * b) + (c * d)) = ((e / f) - (g / h)))"

[<Fact>]
let Test2() =
    let input = [ "("; "a"; "+";"b";")"; "*"; "c" ]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> (fun input -> Regex.Replace(input, "\"", ""))
    |> equals "(((a + b)) * c)"
