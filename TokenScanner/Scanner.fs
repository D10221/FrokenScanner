module TokenScanner.Scanner

open TokenScanner.Scan
open TokenScanner.Queue

///<summary>
/// Ready to use scanner
///</summary>
let Scan args =
    args
    |> Queue
    ||> Scan
