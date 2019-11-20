/// <summary>
/// Configuration
/// </summary>
module TokenScanner.Symbols

open Scanlet
open Matching
open System

let private whenSome f = Option.isSome |> And f

let private isSome x y = y = Some x

let private isSpace =
    isRegexMatch "[^\S\r\n]"
    |> checkValue
    |> whenSome

let private isDigit =
    isRegexMatch "\d"
    |> checkValue
    |> whenSome

let private isDot = isSome '.'

let private isLetter =
    isRegexMatch "[a-zA-Z]"
    |> checkValue
    |> whenSome

let private isIdentifier =
    isRegexMatch "[a-zA-Z_\$#@]"
    |> checkValue
    |> whenSome

let symbols = "[~`\!@#\$%\^\&\*\(\)-=_+\[\]\{\}\\\|;:'\",\<\.\>/\?]"

let private isSymbol =
    isRegexMatch symbols
    |> checkValue
    |> whenSome


/// **Description**
/// Split by symbol number identifier
let scanlets some =
    match some with
    | None -> raise (ArgumentException("can't be None", "some"))
    | some when some |> isSymbol -> Take
    | x when isSpace x -> TakeWhile isSpace |> StartWith
    | x when isIdentifier x ->
        TakeWhile
            (isIdentifier
             |> Or isDigit
             |> Or isDot)
        |> StartWith
    | x when isDigit x -> TakeWhile isDigit |> StartWith
    | _ -> raise (NotImplementedException((sprintf "'%c' is Not Implemented" some.Value)))
