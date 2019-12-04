module MiniParserTests

open MiniParser.Lexing.Scanner
open MiniParser.Parsing.Visitor
open Xunit
open System.Text.RegularExpressions
open MiniParser.Parsing.Types
open MiniParser.Parsing.Parser

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let Test1() =
    let input = "a * b\n"

    let scan input =
        [ for (token, tokenType) in Scan input do
            if tokenType <> "space" && tokenType <> "newline" then yield token ]

    let precedence = 0
    ParseExpr (scan (input.ToCharArray() |> Array.toList)) precedence
    |> fst
    |> (function
    | BinaryExpression e -> 
        equals e.token "*"
        
        match e.left with 
        | NameExpression name ->
            equals name.token "a"
        | _ -> failwith "expected a"

        match e.right with
        | NameExpression name ->
            equals name.token "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
