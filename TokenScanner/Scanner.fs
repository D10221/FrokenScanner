module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types
open Scanlets

let isDigit (o: Option<char>) = o <> None && Regex("\d").IsMatch(o.Value.ToString())
let isWord (o: Option<char>) =
    o <> None && not (isDigit o) && // not checking for digit results in order sensitive match
    Regex("\w").IsMatch(o.Value.ToString())
let isWordOrDigit (peek: Option<char>) = isWord (peek) || isDigit (peek)
let isDot o = o <> None && o.Value = '.'
let isDigitOrDot o = o <> None && isDigit (o) || isDot (o)
let isSpace (o: Option<char>) =
    o <> None && o.Value <> '\n' && o.Value <> '\r' && Regex("\s").IsMatch(o.Value.ToString())

/// <summary>
/// Configuration
/// </summary>
let scanlets =
    Map.ofList <| [ ('=', TakeTwo('=', '>') |> StartWith(Some('=')))
                    ('+', TakeTwo('=', '+') |> StartWith(Some('+')))
                    ('-', TakeTwo('=', '-') |> StartWith(Some('-')))
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
    // TODO a.a ? No is parser job
    // TODO .a  ? No is parser job
    // TODO .1  ? No is parser job
    // TODO 1D ?
    // TODO 1x0 ?
    | x ->
        (printf "found: default: %A \n" x)
        Take x |> Some

///<summary>
/// emit: could be a Subject ?
/// it: could return Observable ?
/// it: can't yield
///<summary>
let rec Scanner (emit: Subscriber) (queue: Queue) =
    // :recursor
    let takeNext scanlet =
        scanlet queue |> emit
        Scanner emit queue
    //
    match find <| queue (true) with
    | None -> ()
    | scanlet -> Option.get scanlet |> takeNext

/// <summary>
/// Scans ...
/// </summary>
let Scan(input: string) =

    let mutable accumulator: List<string> = []

    let append more = accumulator <- accumulator @ [ more ]

    let toString acc (o: Option<char>) = acc + o.Value.ToString()

    let subscriber (payload: List<Option<char>>) =
        payload
        |> (List.fold (toString) "")
        |> append

    let scan = Scanner subscriber

    input
    |> Queue
    |> scan
    accumulator
// |> List.map(fun list-> List.fold(fun a-> fun b -> a + b.ToString()) "" list )
