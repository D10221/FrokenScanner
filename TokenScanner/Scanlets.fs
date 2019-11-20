/// <summary>
/// Configuration
/// </summary>
module TokenScanner.Scanlets

open Scanlet
open Matching
open System

let whenSome f = Option.isSome |> And f
let isSome x y = y = Some x

let isSpace = isRegexMatch "[^\S\r\n]" |> checkValue |> whenSome 
let isDigit = isRegexMatch "\d" |> checkValue |> whenSome
let isDot = isSome '.'
let isLetter = isRegexMatch "[a-zA-Z]" |> checkValue |> whenSome
let isIdentifier = isRegexMatch "[a-zA-Z_\$#@]" |> checkValue |> whenSome

let scanlets some =
    match some with
    | None -> raise (ArgumentException("can't be None", "some"))
    | Some '=' -> TakeTwo('=', '>') |> StartWith 
    | Some '+' -> TakeTwo('=', '+') |> StartWith
    | Some '-' -> TakeTwo('=', '-') |> StartWith
    | Some '\n' -> Take
    | Some '\r' -> TakeOne '\n' |> StartWith
    | x when isSpace x -> TakeWhile isSpace  |> StartWith
    | x when isIdentifier x -> TakeWhile(isIdentifier |> Or isDigit) |> StartWith
    | x when isDigit x -> TakeWhile(Or isDigit isDot) |> StartWith
    | _ -> raise (NotImplementedException((sprintf "'%c' is Not Implemented" some.Value)))
