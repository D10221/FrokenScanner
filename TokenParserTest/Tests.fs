module Tests

open Xunit
open TokenParser.Parse
open TokenParser.Visitor

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let ``It Works With Strings`` () =
    [ for c in "a*b+a*b" -> c.ToString() ]
    |> StringsParser()
    |> visitMany
    |> List.item 0
    |> equals "(('a' * 'b') + ('a' * 'b'))"

[<Fact>]
let ``It Works With Chars`` () =
    [ for c in "a*b+a*b" -> c ]
    |> CharsParser()
    |> visitMany
    |> List.item 0
    |> equals "(('a' * 'b') + ('a' * 'b'))"
