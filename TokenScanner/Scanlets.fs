/// <summary>
/// Configuration
/// </summary>
module TokenScanner.Scanlets

open System.Text.RegularExpressions
open Scanlet

let isDigit (o: Option<char>) = o <> None && Regex("\d").IsMatch(o.Value.ToString())
let isWord (o: Option<char>) =
    o <> None && not (isDigit o) && // not checking for digit results in order sensitive match
    Regex("\w").IsMatch(o.Value.ToString())
let isWordOrDigit (peek: Option<char>) = isWord (peek) || isDigit (peek)
let isDot o = o <> None && o.Value = '.'
let isDigitOrDot o = o <> None && isDigit (o) || isDot (o)
let isSpace (o: Option<char>) =
    o <> None && o.Value <> '\n' && o.Value <> '\r' && Regex("\s").IsMatch(o.Value.ToString())

let private maybeLikeASerilizableLikeThis =
    [ [ "="; "="; ">" ]
      [ "+"; "="; "+" ]
      [ "-"; "="; "-" ]
      [ "\n" ]
      [ "\r"; "\n" ] ]
/// <summary>
/// Configuration
/// </summary>
let private scanlets =
    Map.ofList <| [ ('=', TakeTwo('=', '>') |> StartWith(Some('=')))
                    ('+', TakeTwo('=', '+') |> StartWith(Some('+')))
                    ('-', TakeTwo('=', '-') |> StartWith(Some('-')))
                    // TODO: list from json? or something like it
                    ('\n', Take(Some('\n')))
                    ('\r', TakeOne '\n' |> StartWith(Some('\r'))) ]

let findKey key =
    match scanlets.TryFind(key) with
    | None -> None
    | scanlet -> scanlet
    
/// <summary>
/// Scanlet Exists
/// </summary>
let exists (key: Option<char>) = Option.isSome key && scanlets.ContainsKey(key.Value)

/// <summary>
/// Find scanlet
/// </summary>
let find token =
    match token with
    | None -> None
    | x when exists x -> findKey x.Value
    | x when x |> isSpace ->
        TakeWhile isSpace
        |> StartWith x
        |> Some
    | x when x |> isWord ->
        TakeWhile isWordOrDigit
        |> StartWith x
        |> Some
    | x when x |> isDigit ->
        TakeWhile isDigitOrDot
        |> StartWith x
        |> Some
    // TODO a.a ? No, is parser's job
    // TODO .a  ? No, is parser's job
    // TODO .1 ?
    // TODO 1D ?
    // TODO 1x0 ?
    | x ->
        // TODO: raise NotFound
        (printf "found: default: %A \n" x)
        Take x |> Some
