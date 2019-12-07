module MiniParser.Tests.MiniParser

open Xunit
open MiniParser.Parsing.Visitor
open MiniParser.Parsing.Types
open MiniParser.Parse
open MiniParser.Parsing.Token
open System.Text.RegularExpressions

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let clean input = Regex.Replace(input, "\"", "")


[<Fact>]
let Test1() =
    "a * b\n"
    |> ParseString
    |> fst
    |> (fun x ->

    (visit x)
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
