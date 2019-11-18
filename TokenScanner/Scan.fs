module TokenScanner.Scan

open Scanner
open Subject
open Queue
open Collector

let toString acc (o: Option<char>) = acc + o.Value.ToString()
let toStrings = (List.fold (toString) "")

/// <summary>
/// Scans ...
/// </summary>
let Scan(input: string) =
    let observer = Subject<List<Option<char>>>()
    let scan = Scanner observer    
    use collector = collectFrom(observer)
    input
    |> Queue
    |> scan    
    collector.Value() |> List.map toStrings
