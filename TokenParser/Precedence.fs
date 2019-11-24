module TokenParser.Precedence

let getPrecedence x =
    match x with
    | '+' -> 1
    | '*' -> 2
    | _ -> 0