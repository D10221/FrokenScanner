module TokenScanner.Scanner
open TokenScanner.Scan
open TokenScanner.Queue
    let Scan args =
        args
        |> Queue
        ||> Scan
