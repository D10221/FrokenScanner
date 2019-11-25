module Tests

open Xunit
open Parser.Parse
open Parser.Visitor
open System.Text.RegularExpressions

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let ``It Works With Strings``() =
    [ for c in "a*b+a*b" -> c |> sprintf "%c" ]
    |> StringsParser()
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
