module TokenScanner.Scan

open Scanner
open Queue

/// <summary>
/// Observable Scanner ...TODO
/// </summary>
let Scan(input: string) =
    Queue input |> Scanner

