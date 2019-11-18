module TokenScanner.Scan

open Scanner
open Subject
open Queue
open Collector
open System

let toString acc (o: Option<char>) = acc + o.Value.ToString()
let toStrings = (List.fold (toString) "")

/// <summary>
/// Scans ...
/// </summary>
let Scan(input: string) =
    let observer = Subject<List<Option<char>>>()    
    use collector = collectFrom (observer)
    Queue input |> Scanner observer |> ignore
    collector.Value() |> List.map toStrings
