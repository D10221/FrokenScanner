module MiniParser.Tests.MiniParser

open Xunit
open MiniParser.Parsing.Expressions
open MiniParser.Visiting
open MiniParser.Parse
open MiniParser.Parsing.Types
open MiniParser.Tests.Common

[<Fact>]
let Test1() =
    "a * b\n"
    |> ParseString
    |> fst
    |> (fun x ->

    (Visit x)
    |> clean
    |> equals "(a * b)"

    match x with
    | BinaryExpression e ->
        tokenValue e.token |> equals  "*"

        match e.left with
        | NameExpression name -> tokenValue name.token |> equals "a"
        | _ -> failwith "expected a"

        match e.right with
        | NameExpression name -> tokenValue name.token |> equals "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
