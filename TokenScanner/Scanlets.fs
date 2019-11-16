module TokenScanner.Scanlets

open Types

/// <summary>
/// Match Sequence always includes 1st
/// </summary>
let MatchSequenceScanlet (expected: List<char>) (next: Next) =
    let first = Some(expected.[0])

    let ret =
        [ for x in expected.[1..] do
            let peek = next (false)
            if (peek = Some(x)) then next (true) ]
    first :: ret

///<summary>
/// Matches peek with y or z. else or None with x
///</summary>
let TriadScanlet ((x, y, z): char * char * char) (next: Next) =
    match next (false) with
    | None -> [ Some(x) ]
    | some when some = Some(y) || some = Some(z) ->
        [ Some(x)
          next (true) ]
    | _ -> [ Some(x) ]

/// <summary>
/// Hard coded Triad scanlet: erasier to read, Scans for '+' or '+=' or '++'
/// </summary>
let PlusScanlet(next: Next) =
    let peek unit = next (false)
    let next unit = next (true)
    match peek() with
    | Some('=')
    | Some('+') ->
        ([ Some('+')
           next() ])
    // ..else
    | _ -> ([ Some('+') ])
