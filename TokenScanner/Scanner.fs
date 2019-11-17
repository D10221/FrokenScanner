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
let isLF o = o <> None && o.Value = '\n'
let isCR o = o <> None && o.Value = '\r'
let isSpace (o: Option<char>) =
    o <> None && o.Value <> '\n' && o.Value <> '\r' && Regex("\s").IsMatch(o.Value.ToString())

///<summary>
/// It could be be an Observable / Subject ? 
/// It can't yeld
///<summary>
let rec Scanner (emit: Subscriber) (next: Next) =

    let peek() = next (false) // don't advance & return next

    /// recurse 
    let takeNext scanlet = 
        scanlet next |> emit
        Scanner emit next 

    let next() = next (true) // advance & return current

    match next() with
    | None -> () // end!/Complete
    | Some('=') as x -> TakeTwo x ('=', '>') |> takeNext
    | Some('+') as x -> TakeTwo x ('=', '+') |> takeNext
    | Some('-') as x -> TakeTwo x ('=', '-') |> takeNext
    | Some('\n') as x -> Take x |> takeNext
    | Some('\r') as x -> TakeOne x '\n' |> takeNext // match '\r' and maybe '\n'
    | x when isSpace (x) ->
        TakeWhile isSpace
        |> StartWith x
        |> takeNext
    | x when isWord (x) ->
        TakeWhile isWordOrDigit
        |> StartWith x
        |> takeNext
    | x when isDigit (x) ->
        TakeWhile isDigitOrDot
        |> StartWith x
        |> takeNext
    // TODO a.a
    // TODO .a
    // TODO .1
    // TODO 1D ?
    // TODO 1x0 ?
    | x ->
        (printf "found: default: %A \n" x)
        Take x |> takeNext

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
