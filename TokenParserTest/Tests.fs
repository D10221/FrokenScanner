module Tests

open Xunit
open Parser.StringsParser
open Parser.CharsParser
open Parser.Visitor
open Parser.Precedence
open System.Text.RegularExpressions

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let ``It Works With Strings``() =
    [ for c in "a*b+a*b" -> c |> sprintf "%c" ]
    |> StringsParser Precedence
    |> visitMany
    |> List.item 0
    |> (fun x -> Regex.Replace(x, "\"", ""))
    |> equals "((a * b) + (a * b))"

[<Fact>]
let ``It Works With Chars``() =
    [ for c in "a*b+a*b" -> c ]
    |> CharsParser()
    |> visitMany
    |> List.item 0
    |> (fun x -> Regex.Replace(x, "'", ""))
    |> equals "((a * b) + (a * b))"
