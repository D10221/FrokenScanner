module MiniParser.Tests.Parsing

open Xunit

open MiniParser.Tests.Common
open MiniParser.Parsing.Expressions
open MiniParser.Parsing.Parser
open MiniParser.Parsing.Types
open MiniParser.Visiting
open MiniParser.Parsing.Error
open MiniParser.Lexing.Types

[<Fact>]
let Test1() =
    let chars = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
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
    let tokens =
        [ ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          ("+", OP, 0, 0)
          ("b", WORD, 0, 0)
          (")", OP, 0, 0)
          ("*", OP, 0, 0)
          ("c", WORD, 0, 0) ]

    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(((a + b)) * c)"

[<Fact>]
let GroupTest2() =
    let tokens =
        [ ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          (")", OP, 0, 0) ]

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
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
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
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
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
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map tokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a(a,a))"

[<Fact>]
let PrefixExpressionTest() =
    match [ ("!", OP, 0, 0)
            ("a", WORD, 0, 0) ]
          |> ParseExpr 0
          |> fst with
    | PrefixExpression prefix ->
        tokenValue prefix.Token |> equals "!"
        match prefix.Right with
        | NameExpression name -> tokenValue name.Token |> equals "a"
        | _ -> failwith "Expected NameExpression"
    | _ -> failwith "Expected PrefixExpression"
    ()

[<Fact>]
let PrefixFails() =
    try
        [ "!=" ]
        |> List.map (toTokenOf TokenType.OP)
        |> ParseExpr 0
        |> ignore
    with
    | :? ParseError as e when e.Token = Some("!=", TokenType.OP, 0, 0) -> ()        
    | e -> failwithf "Expected %A instead of %A" ParseError e

[<Fact>]
let PrefixOperatorVsBinaryOperator() =
    let tokens = [ "a"; "!="; "b" ] |> List.map (toTokenOf TokenType.WORD)
    match ParseExpr 0 tokens |> fst with
    | BinaryExpression e -> tokenValue e.Token |> equals "!="
    | _ -> failwith "Expected "    

[<Fact>]
let PrefixOperator() =    
    match ParseExpr 0 [ ("!", OP, 0, 0); ("a", WORD, 0, 0) ]  |> fst with
    | PrefixExpression e ->
        match e with
        | { Token = ("!", _, 0, 0) } -> ()
        | x -> failwithf "Expected  (\"!\", _, 0, 0) instead of %A" x
    | e -> failwithf "Expected %A instead of %A" PrefixExpression e

[<Fact>]
let OddStart() =
    match [ ("!", OP, 0, 0)
            ("a", WORD, 0, 0)
            ("!=", OP, 0, 0)
            ("b", WORD, 0, 0) ] // "!a!=b"
          |> ParseExpr 0
          |> fst with
    | PrefixExpression e ->
        match e with
        | { Token = ("!", _, _, _) } -> ()
        | token -> failwithf "Expected %A instead of %A" ({ Token = ("!") }) token
    | e -> failwithf "Expected %A instead of %A" PrefixExpression e
