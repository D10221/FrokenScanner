module MiniParser.Tests.Parsing

open Xunit
open System.Text.RegularExpressions
open MiniParser.Parsing.Expressions
open MiniParser.Parsing.Parser
open MiniParser.Parsing.Types
open MiniParser.Visiting

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let clean input = Regex.Replace(input, "\"", "")

let toToken x = (x, "", 0, 0)











[<Fact>]
let Test1() =
    let chars = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let tokens = chars |> List.map toToken
    let precedence = 0
    let (expr, _) = ParseExpr precedence tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(((a * b) + (c * d)) = ((e / f) - (g / h)))"

[<Fact>]
let GroupTest() =
    let chars = [ "("; "a"; "+"; "b"; ")"; "*"; "c" ]
    let tokens = chars |> List.map toToken
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(((a + b)) * c)"

[<Fact>]
let GroupTest2() =
    let chars = [ "("; "a"; ")" ]
    let tokens = chars |> List.map toToken
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a)"

[<Fact>]
let CallTest() =
    let chars = [ "a"; "("; ")" ]
    let tokens = chars |> List.map toToken
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a())"

[<Fact>]
let CallTest2() =
    let chars = [ "a"; "("; "a"; ")" ]
    let tokens = chars |> List.map toToken
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a(a))"

[<Fact>]
let CallTest3() =
    let chars = [ "a"; "("; "a"; ","; "a"; ")" ]
    let tokens = chars |> List.map toToken
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a(a,a))"

[<Fact>]
let PrefixExpressionTest() =
    let (exp, _) = ParseExpr 0 ([ "!"; "a" ] |> List.map toToken)
    match exp with
    | PrefixExpression prefix ->
        tokenValue prefix.token |> equals "!"
        match prefix.right with
        | NameExpression name -> tokenValue name.token |> equals "a"
        | _ -> failwith "Expected NameExpression"
    | _ -> failwith "Expected PrefixExpression"
    ()

let fails f =
    try
        f() |> ignore
        None
    with e -> (Some(e))


[<Fact>]
let PrefixOperatorVsBinaryOperatorFails() =
    let run() =
        [ "!=" ]
        |> List.map toToken
        |> ParseExpr 0
        |> ignore

    let x = fails run
    if x = None then failwith "Expected Some(<Exception>)"
    else ()

[<Fact>]
let PrefixOperatorVsBinaryOperator() =
    let tokens = [ "a"; "!="; "b" ] |> List.map toToken
    match ParseExpr 0 tokens |> fst with
    | BinaryExpression e -> tokenValue e.token |> equals "!="
    | _ -> failwith "Expected "
    ()

[<Fact>]
let PrefixOperator() =
    let tokens = [ "!"; "a" ] |> List.map toToken
    match ParseExpr 0 tokens |> fst with
    | PrefixExpression e ->
        match e with
        | { token = ("!", _, 0, 0) } -> ()
        | x -> failwithf "Expected  (\"!\", _, 0, 0) instead of %A" x
    | e -> failwithf "Expected %A instead of %A" PrefixExpression e
    ()

[<Fact>]
let OddStart () =    
    match [ "!"; "a"; "!="; "b" ] // "!a!=b"
          |> List.map toToken
          |> ParseExpr 0
          |> fst with
    | PrefixExpression e ->
        match e with
        | { token = ("!", _, _, _) } -> ()
        | token -> failwithf "Expected %A instead of %A" ({ token = ("!") }) token
    | e -> failwithf "Expected %A instead of %A" PrefixExpression e
