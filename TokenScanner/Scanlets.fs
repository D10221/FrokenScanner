module TokenScanner.Scanlets

open Types
/// <summary>
/// Scan and Match subsequent expected options
/// </summary>
let ScannletForManyOptions (next: Next) (expected: List<Option<char>>) =
    [ for expect in expected do
        let incoming = next (false)
        if (incoming = expect) then
            ignore <| next (true)
            yield incoming ]

[<Literal>]
let Equals = '='
///<summary>
/// Starts with "Equals"('-') Scanlet
///
///</summary>
let Scanlet(next: Next) =  
    let x = Some(Equals)
    match next(false) with
    | Some('=')
    | Some('>') ->
        [ x
          next(true) ]
    // else
    | _ -> ([ x ])
