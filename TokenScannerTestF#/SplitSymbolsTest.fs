module SplitSymbolsTest

open Xunit
open TokenScanner.Symbols
open TokenScanner.Queue
open TokenScanner.Scanner

let toString = List.fold (fun a b -> a + ((Option.get b).ToString())) ""
let toStrings = List.map toString
let areEqual a b = (a, b) |> Assert.Equal
let join = List.fold (+) ""

/// splits symbols space new-line identifiers numbers
/// imports Symbols.scanlets
/// doesn't parse
let scan (input: string) = Queue input |> Scanner scanlets

[<Fact>]
let Splits() =
    let r = scan (".6") |> toStrings
    areEqual r.[0] "."
    areEqual r.[1] "6"

[<Fact>]
let SplitsAll() =
    let scanned =
        scan symbols
        |> toStrings
        |> join
    //
    areEqual symbols scanned

[<Fact>] // TODO
let SplitsMore() =
    let extra = "123" + "abc" + "1.1" + " " + ".1" + "\n" + "\r\n"
    let scanned =
        scan <| symbols + extra
        |> toStrings
        |> join
    //
    areEqual symbols (scanned + extra)
