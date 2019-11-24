module Tests

open Xunit
open TokenParser.Parser
open TokenParser.Visitor

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

[<Fact>]
let ItWorks() =
    "a*b+a*b".ToCharArray()
    |> Array.toList    
    |> parse
    |> visitMany
    |> List.item 0
    |> equals "(('a' * 'b') + ('a' * 'b'))"
