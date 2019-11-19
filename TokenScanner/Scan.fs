module TokenScanner.Scan

open Scanner
open Queue
open TokenScanner.Scanlets

/// <summary>
/// Observable Scanner ...TODO
/// </summary>
let Scan(input: string) =
    Queue input |> Scanner scanlets

