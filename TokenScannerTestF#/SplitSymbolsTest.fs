module SplitSymbolsTest

open Xunit
open TokenScanner.Symbols
open TokenScanner.Queue
open TokenScanner.Scanner

let toString = List.fold (fun a b -> a + ((Option.get b).ToString())) ""
let toStrings = List.map toString
let areEqual a b = (a, b) |> Assert.Equal

/// splits symbols space new-line identifiers numbers
/// imports Symbols.scanlets
/// doesn't parse
/// preserve column number ? 
let scan (input: string) = Queue input |> Scanner scanlets

[<Fact>]
let Splits() =
    let r = scan (".6") |> toStrings
    areEqual r.[0] "."
    areEqual r.[1] "6"
