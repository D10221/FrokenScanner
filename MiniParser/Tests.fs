module Tests

open MiniParser
open Xunit
open System.Text.RegularExpressions

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let clean input = Regex.Replace(input, "\"", "")

[<Fact>]
let Test1() =
    let input = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let precedence = 0
    let (expr, _) = parseExpr input precedence
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(((a * b) + (c * d)) = ((e / f) - (g / h)))"
[<Fact>]
let GroupTest() =
    let input = [ "("; "a"; "+";"b";")"; "*"; "c" ]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(((a + b)) * c)"
[<Fact>]
let GroupTest2() =
    let input = [ "("; "a"; ")"]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a)"    
[<Fact>]
let CallTest() =
    let input = [ "a"; "(";")"]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a())"
[<Fact>]
let CallTest2() =
    let input = [ "a";"(";"a";")"]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a(,a))" //TODO the extra , in visitor
[<Fact>]
let CallTest3() =
    let input = [ "a"; "(";"a";",";"a";")"]
    let (expr, _) = parseExpr input 0
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a(,a,a))" //TODO the extra , in visitor