module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types

let isWord (o: Option<char>) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
let isDigit (o: Option<char>) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
let isSpace (o: Option<char>) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())

///<summary>
///
///<summary>
let rec Scanner (subscriber: List<Option<char>> -> unit) (_next: Next) (x: Option<char>) =
    let scan = fun () -> Scanner subscriber _next (_next (true))
    let peek = fun () -> _next (false) // don't advance & return next
    let next = fun () -> _next (true) // advance & return current
    match x with
    | Some('=') ->
        match peek() with
        | Some('=')
        | Some('>') ->
            ignore <| next()
            subscriber
                ([ x
                   next() ])
        | _ -> subscriber ([ x ])
        // ... finally
        ignore <| scan()
    | Some('+') ->
        match peek() with
        | Some('=')
        | Some('+') ->
            ignore <| next()
            subscriber
                ([ x
                   next() ])
        // ..else
        | _ -> subscriber ([ x ])
        // ... finally
        ignore <| scan()
    | Some('-') ->
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
    | None -> () // end!
    // is NOT None is Something else
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
/// Starts the Scanner
/// </summary>
let Starter(next: Next) = (next, next (true))

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
    |> (Queue)
    |> Starter
    ||> scan
    accumulator
// |> List.map(fun list-> List.fold(fun a-> fun b -> a + b.ToString()) "" list )
