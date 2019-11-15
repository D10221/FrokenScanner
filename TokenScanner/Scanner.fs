namespace TokenScanner

open Queue
open Scan

module Scanner =

    let Scan args =
        args
        |> Queue
        ||> Scan
