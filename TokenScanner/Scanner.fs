module TokenScanner.Scanner

open Types

/// **Description**
/// Scans the queue
/// **Output Type**
///   * `Option<char> list list`
let rec Scanner (queue: Queue) =
    // :recursor
    let takeNext scanlet =        
        scanlet queue :: Scanner queue

    let nextToken = queue (true) // advance ad return current
    
    match Scanlets.find nextToken with
        | None -> []
        | scanlet -> Option.get scanlet |> takeNext
 