module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types
open Scanlets

let isDigit (o: Option<char>) = o <> None && Regex("\d").IsMatch(o.Value.ToString())
let isWord (o: Option<char>) = 
    o <> None && 
        not (isDigit o) &&  // not checking for digit results in order sensitive match
            Regex("\w").IsMatch(o.Value.ToString())
let isDot o = o <> None && o.Value = '.'
let isDigitOrDot o = o <> None && isDigit (o) || isDot (o)
let isLF o = o <> None && o.Value = '\n'
let isCR o = o <> None && o.Value = '\r'
let isSpace (o: Option<char>) =
    o <> None && o.Value <> '\n' && o.Value <> '\r' && Regex("\s").IsMatch(o.Value.ToString())
let isWordOrDigit (peek: Option<char>) = isWord (peek) || isDigit (peek)

///<summary>
///
///<summary>
let rec Scanner (subscriber: Subscriber) (_next: Next) =

    let scan = fun () -> Scanner subscriber _next
    let peek = fun () -> _next (false) // don't advance & return next
    let next = fun () -> _next (true) // advance & return current

    match next() with
    | None -> () // end!
    | Some('=') ->
        // match: '=' or '==' or '=>'
        TriadScanlet ('=', '=', '>') _next |> subscriber
        ignore <| scan()
    | Some('+') ->
        // specific scanlet , hardcoded
        PlusScanlet _next |> subscriber
        ignore <| scan()
    | Some('-') as x ->
        // inline scanlet
        // match '-' or '-=' or '--'
        match peek() with
        | Some('=')
        | Some('-') ->
            subscriber
                ([ x
                   next() ])
        // else
        | _ -> subscriber ([ x ])
        // ... finally
        ignore <| scan()
    | Some('\n') as x ->
        subscriber ([ x ])
        ignore <| scan()
    | Some('\r') ->
        // match '\r' and maybe '\n'
        DualScanlet ('\r', '\n') _next |> subscriber
        ignore <| scan()
    | x when isSpace (x) ->
        // group spaces
        subscriber (x :: CollectWhileScanlet isSpace _next)
        ignore <| scan()
    | x when isWord (x) ->
        // starts with word, may contain right numbers or word
        subscriber (x :: CollectWhileScanlet isWordOrDigit _next)
        ignore <| scan()
    | x when isDigit (x) ->
        // TODO: IsDigit or is dot and digits, or digit and dot digits
        subscriber (x :: CollectWhileScanlet isDigitOrDot _next)
        ignore <| scan()
    // TODO a.a
    // TODO .a
    // TODO .1
    // TODO 1D ? 
    // TODO 1x0 ?
    | x ->
        (printf "found: default: %A \n" x)
        subscriber ([ x ])
        ignore <| scan()

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
