module MiniParserTests

open MiniParser
open MiniParser.Parsing.Visitor
open Xunit
open System.Text.RegularExpressions
open MiniParser.Parsing.Types

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let clean input = Regex.Replace(input, "\"", "")

[<Fact>]
let Test1() =
    "a * b\n" 
    |> parseString     
    |> fst
    |> (fun x ->

    (visit x)
    |> clean
    |> equals "(a * b)"

    match x with
    | BinaryExpression e ->
        equals e.token "*"

        match e.left with
        | NameExpression name -> equals name.token "a"
        | _ -> failwith "expected a"

        match e.right with
        | NameExpression name -> equals name.token "b"
        | _ -> failwith "expected b"

    | _ -> failwith "Expected BinaryExpression")
