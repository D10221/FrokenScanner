module TokenScanner.Scanner

open Types

let rec Scanner scanlets (queue: Queue) =
    // :recursor
    let takeNext scanlet = scanlet queue :: Scanner scanlets queue

    let peek() = queue (true) // advance ad return current

    match peek() with
    | None -> [] // end of stream
    | token ->
        // ScanletEntry: token -> queue -> []
        token
        |> scanlets token
        |> takeNext
 