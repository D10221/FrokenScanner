module TokenScanner.Scanner

open Types
open System

let isMatchFor x (entry: ScanletEntry) =
    let (matches, _) = entry
    matches x

let tryFindScanletFor token = List.tryFind (isMatchFor token)

let rec Scanner scanlets (queue: Queue) =
    // :recursor
    let takeNext scanlet = scanlet queue :: Scanner scanlets queue

    let peek () = queue (true) // advance ad return current

    match peek() with
    | None -> [] // end of stream
    | token -> 
        // find scanlet
        match scanlets |> tryFindScanletFor token with
        | None ->  // raise not found / not implemented !
            raise <| // ...
                NotImplementedException(
                        sprintf "Can't find Scanlet for '%c'" 
                            token.Value)
        | scanletEntry ->
            // process and recurse
            let (_, scanlet) = scanletEntry.Value
            scanlet token |> takeNext
 