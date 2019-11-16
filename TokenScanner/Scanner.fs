module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types
open Scanlets

let isWord (o: Option<char>) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
let isDigit (o: Option<char>) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
let isSpace (o: Option<char>) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())

///<summary>
///
///<summary>
let rec Scanner (subscriber: Subscriber) (_next: Next) =
    let scan = fun () -> Scanner subscriber _next
    let peek = fun () -> _next (false) // don't advance & return next
    let next = fun () -> _next (true) // advance & return current
    let x = next()
    match x with
    | None -> () // end!
    | Some('=') ->
        // match: '=' or '==' or '=>'
        TriadScanlet ('=', '=', '>') _next |> subscriber
        ignore <| scan()
    | Some('+') -> 
        PlusScanlet _next |> subscriber
        ignore <| scan()
    | Some('-') ->
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
    | Some(' ') ->
        let mutable collected: option<char> list = []
        while (isSpace (peek())) do
            collected <- collected @ [ next() ]
        subscriber ((x :: collected))
        ignore <| scan()
    // Something else
    | _ when isSpace (x) ->
        let mutable collected: option<char> list = []
        while (isSpace (peek())) do
            collected <- collected @ [ next() ]
        subscriber (x :: collected)
        ignore <| scan()
    | _ when isWord (x) ->
        let mutable collected: option<char> list = [ x ]
        while (isWord (peek()) || isDigit (peek())) do
            collected <- collected @ [ next() ]
        subscriber (collected)
        ignore <| scan()
    | _ ->
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
