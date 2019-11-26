module SplitAndParse.Tests

open SplitAndParse.Parse
open SplitAndParse.Visitor
open System.Text.RegularExpressions
open Xunit 

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b
    

[<Fact>]
let Test1 () = 
    let scan = TokenSplitter.Scan.Scan
    let parse = SplitAndParse.Parse.Parser()
    "a*b+a*b".ToCharArray()
    |> Array.toList
    |> scan
    |> List.map (fun x-> fst x)
    |> parse
    |> visitMany
    |> List.item 0
    |> (fun x -> Regex.Replace(x, "\"", ""))
    |> equals "((a * b) + (a * b))"