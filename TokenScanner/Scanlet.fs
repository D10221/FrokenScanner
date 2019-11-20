module TokenScanner.Scanlet

open Types

/// <summary>
/// Scanlet: Match Sequence
/// </summary>
let TakeMany (expected: List<'a>) (next: Queue<'a>) =
    let ret =
        [ for x in expected do
            let peek = next (false)
            if (peek = Some(x)) then next (true) ]
    ret
///<summary>
/// Scanlet: Matches peek with y else or None with x
///</summary>
let TakeOne  y  (next: Queue<'a>) =
    let peek () = next (false)
    let next () = next (true)
    match peek() with
    | None -> [  ]
    | some when some = Some(y) ->
        [ 
          next() ]
    | _ -> [  ]
///<summary>
/// Scanlet: Matches peek with y or z. else or None with x
///</summary>
let TakeTwo  ((y, z): 'a * 'a) (next: Queue<'a>) =
    match next (false) with
    | None -> [  ]
    | some when some = Some(y) || some = Some(z) ->
        [ 
          next (true) ]
    | _ -> [  ]
/// <summary>
/// collect while token is of the matching kind
/// </summary>
let TakeWhile (isMatch: Option<'a> -> bool) (next: Queue<'a>) =
    let peek () = next (false)
    let next () = next (true)
    let mutable collected: option<'a> list = []
    while (isMatch (peek())) do
        collected <- collected @ [ next() ]
    collected
///<sumary>
/// Scanlet of Scanlet: prepend x to scanlet result
///</sumary>
let StartWith (scanlet: Scanlet<'a>) target (next: Queue<'a>) =
    let ret = target :: (scanlet next)
    ret
///<sumary>
/// Scanlet: satisfy scanlet signature
/// returns x
/// passthru
///</sumary>
let Take (x: Option<'a>) (next: Queue<'a>) = [ x ]
