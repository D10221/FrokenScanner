module MiniParser.Tests.MiniParser

open Xunit
open MiniParser.Token
open MiniParser.Expressions
open MiniParser.Visiting
open MiniParser.Parse
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
        TokenValue e.Token |> equals  "*"

        match e.Left with
        | NameExpression name -> TokenValue name.Token |> equals "a"
        | _ -> failwith "expected a"

        match e.Right with
        | NameExpression name -> TokenValue name.Token |> equals "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
