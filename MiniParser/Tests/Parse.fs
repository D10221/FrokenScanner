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
        tokenValue e.Token |> equals  "*"

        match e.Left with
        | NameExpression name -> tokenValue name.Token |> equals "a"
        | _ -> failwith "expected a"

        match e.Right with
        | NameExpression name -> tokenValue name.Token |> equals "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
