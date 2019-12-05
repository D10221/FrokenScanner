module ParsingTests

open MiniParser.Parsing.Parser
open MiniParser.Parsing.Visitor
open Xunit
open System.Text.RegularExpressions
open MiniParser.Parsing.Types

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let clean input = Regex.Replace(input, "\"", "")

[<Fact>]
let Test1() =
    let input = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let precedence = 0
    let (expr, _) = ParseExpr precedence input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(((a * b) + (c * d)) = ((e / f) - (g / h)))"
[<Fact>]
let GroupTest() =
    let input = [ "("; "a"; "+";"b";")"; "*"; "c" ]
    let (expr, _) = ParseExpr 0 input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(((a + b)) * c)"
[<Fact>]
let GroupTest2() =
    let input = [ "("; "a"; ")"]
    let (expr, _) = ParseExpr 0 input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a)"    
[<Fact>]
let CallTest() =
    let input = [ "a"; "(";")"]
    let (expr, _) = ParseExpr 0 input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a())"
[<Fact>]
let CallTest2() =
    let input = [ "a";"(";"a";")"]
    let (expr, _) = ParseExpr 0 input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a(a))"
[<Fact>]
let CallTest3() =
    let input = [ "a"; "(";"a";",";"a";")"]
    let (expr, _) = ParseExpr 0 input
    input |> List.fold (+) "",
    visit expr
    |> clean
    |> equals "(a(a,a))"
[<Fact>]
let PrefixExpressionTest () =
    let (exp, _) = ParseExpr 0 ["!"; "a"]
    match exp with 
    | PrefixExpression prefix -> 
        equals prefix.token  "!"
        match prefix.right with 
            | NameExpression name -> 
                equals name.token "a"
            | _ -> failwith "Expected NameExpression"
    | _ -> failwith "Expected PrefixExpression"
    