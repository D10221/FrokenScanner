module TokenScanner.Scanlets

open Types

/// <summary>
/// Scanlet: Match Sequence
/// </summary>
let TakeMany (expected: List<char>) (next: Next) =
    let ret =
        [ for x in expected do
            let peek = next (false)
            if (peek = Some(x)) then next (true) ]
    ret
///<summary>
/// Scanlet: Matches peek with y else or None with x
///</summary>
let TakeOne target y (next: Next) =
    let peek unit = next (false)
    let next unit = next (true)
    match peek() with
    | None -> [ target ]
    | some when some = Some(y) ->
        [ target
          next() ]
    | _ -> [ target ]

///<summary>
/// Scanlet: Matches peek with y or z. else or None with x
///</summary>
let TakeTwo target ((y, z): (char * char)) (next: Next) =
    match next (false) with
    | None -> [ target ]
    | some when some = Some(y) || some = Some(z) ->
        [ target
          next (true) ]
    | _ -> [ target ]

/// <summary>
/// Scanlet: Hard coded Triad scanlet: erasier to read, Scans for '+' or '+=' or '++'
/// </summary>
let Plus(next: Next) =
    let peek unit = next (false)
    let next unit = next (true)
    match peek() with
    | Some('=')
    | Some('+') ->
        ([ Some('+')
           next() ])
    // ..else
    | _ -> ([ Some('+') ])
/// <summary>
/// collect while token is of the matching kind
/// </summary>
let TakeWhile (isMatch: Option<char> -> bool) (next: Next) =
    let peek unit = next (false)
    let next unit = next (true)
    let mutable collected: option<char> list = []
    while (isMatch (peek())) do
        collected <- collected @ [ next() ]
    collected
///<sumary>
/// Scanlet of Scanlet: prepend x to scanlet result
///</sumary>
let StartWith x (scanlet: Scanlet) (next: Next) =
    let ret = x :: (scanlet next)
    ret
///<sumary>
/// Scanlet: satisfy scanlet signature
/// returns x
/// passthru
///</sumary>
let Take (x: Option<char>) (next: Next) = [ x ]
