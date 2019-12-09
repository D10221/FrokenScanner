module MiniParser.Tests.Parsing

open Xunit

open MiniParser.Tests.Common
open MiniParser.Expressions
open MiniParser.Parsing.Parser
open MiniParser.Visiting
open MiniParser.Parsing.Error
open MiniParser.Token
open MiniParser.Parse

[<Fact>]
let Test1() =
    let chars = [ "a"; "*"; "b"; "+"; "c"; "*"; "d"; "="; "e"; "/"; "f"; "-"; "g"; "/"; "h" ]
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
    let precedence = 0
    let (expr, _) = ParseExpr precedence tokens
    tokens
    |> List.map TokenValue
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
    |> List.map TokenValue
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
    |> List.map TokenValue
    |> List.fold (+) "",
    Visit expr
    |> clean
    |> equals "(a)"

[<Fact>]
let GroupTest3() =
    let tokens =
        [ ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          (")", OP, 0, 0)
          ("+", OP, 0, 0)
          ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          (")", OP, 0, 0) ]

    match ParseExpr 0 tokens |> fst with
    | BinaryExpression b ->
        match b.Left with
        | GroupExpression g ->
            match g.Right with
            | NameExpression n ->
                match n.Token with
                | ("a", _, _, _) -> ()
                | x -> failwithf "Bad token %A" x
            | x -> failwithf "Bad Expression %A" x
        | x -> failwithf "Bad Expression %A" x
        match b.Right with
        | GroupExpression g ->
            match g.Right with
            | NameExpression n ->
                match n.Token with
                | ("a", _, _, _) -> ()
                | x -> failwithf "Bad token %A" x
            | x -> failwithf "Bad Expression %A" x
        | x -> failwithf "Bad Expression %A" x
    | x -> failwithf "Bad Expression %A" x

[<Fact>]
let GroupTest4() =
    match ParseExpr 0
              [ ("(", TokenType.OP, 0, 0)
                ("a", TokenType.WORD, 0, 0)
                ("+", TokenType.OP, 0, 0)
                ("a", TokenType.WORD, 0, 0)
                (")", TokenType.OP, 0, 0) ] |> fst with
    | GroupExpression e ->
        match e.Right with
        | BinaryExpression b ->
            match b.Left with
            | NameExpression name -> 
                match name.Token with 
                | ("a", _, 0, 0 ) -> ()
                | t -> failwithf "Expected %A instead of %A" "a" t
            | e -> failwithf "Expected %A instead of %A" NameExpression e
            match b.Right with
            | NameExpression name -> 
                match name.Token with 
                | ("a", _, 0, 0 ) -> ()
                | t -> failwithf "Expected %A instead of %A" "a" t
            | e -> failwithf "Expected %A instead of %A" NameExpression e
        | e -> failwithf "Expected %A instead of %A" BinaryExpression e
    | e -> failwithf "Expected %A instead of %A" GroupExpression e

[<Fact>]
let GroupTest5() =    
    let tokens =
        [ ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          ("b", WORD, 0, 0)
          (")", OP, 0, 0) ]
    try
        ParseExpr 0 tokens |> ignore
    with
    | :? ParseError as e ->
        match e.Token with
        | Some(("b", WORD, 0, 0)) -> ()
        | token -> failwithf "Expected %A instead of %A" "b" token
    | e -> failwithf "Expected %A instead of %A" ParseError e

[<Fact>]
let GroupTest6() =    
    let tokens =
        [ ("(", OP, 0, 0)
          ("a", WORD, 0, 0)
          (",", WORD, 0, 0)
          ("a", WORD, 0, 0)
          (")", OP, 0, 0) ]
    try
        ParseExpr 0 tokens |> ignore
    with
    | :? ParseError as e ->
        match e.Token with
        | Some((",", _, _, _)) -> ()
        | token -> failwithf "Expected %A instead of %A" "," token
    | e -> failwithf "Expected %A instead of %A" ParseError e


[<Fact>]
let CallTest() =
    let chars = [ "a"; "("; ")" ]
    let tokens = chars |> List.map (toTokenOf TokenType.WORD)
    let (expr, _) = ParseExpr 0 tokens
    tokens
    |> List.map TokenValue
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
    |> List.map TokenValue
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
    |> List.map TokenValue
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
        TokenValue prefix.Token |> equals "!"
        match prefix.Right with
        | NameExpression name -> TokenValue name.Token |> equals "a"
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
    | BinaryExpression e -> TokenValue e.Token |> equals "!="
    | _ -> failwith "Expected "

[<Fact>]
let PrefixOperator() =
    match ParseExpr 0
              [ ("!", OP, 0, 0)
                ("a", WORD, 0, 0) ] |> fst with
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
            ("b", WORD, 0, 0) ]
          |> ParseExpr 0
          |> fst with
    | PrefixExpression e ->
        match e with
        | { Token = ("!", _, _, _) } -> ()
        | token -> failwithf "Expected %A instead of %A" ({ Token = ("!") }) token
    | e -> failwithf "Expected %A instead of %A" PrefixExpression e

[<Fact>]
let ParseStringTest() =
    "a * b\n"
    |> ParseString
    |> fst
    |> (fun x ->

    (Visit x)
    |> clean
    |> equals "(a * b)"

    match x with
    | BinaryExpression e ->
        TokenValue e.Token |> equals "*"

        match e.Left with
        | NameExpression name -> TokenValue name.Token |> equals "a"
        | _ -> failwith "expected a"

        match e.Right with
        | NameExpression name -> TokenValue name.Token |> equals "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
