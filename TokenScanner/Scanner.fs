module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types

let join a = fun b -> a + b

let f = printf

let isWord (o: char option) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
let isDigit (o: char option) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
let isSpace (o: char option) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())

///<summary>
/// Scan builder
///<summary>
let Scanner (_next: Next) (add: List<Option<char>> -> unit) =
    let rec scan (x: Option<char>) =
        let next = fun () -> _next (true)
        let peek = fun () -> _next (false)
        let scanNext = fun () -> scan (next())
        match x with
        | Some('=') ->
            match peek() with
            | Some('=')
            | Some('>') ->
                ignore <| next()
                add
                    ([ x
                       next() ])
            | _ -> add ([ x ])
            // ... finally
            scanNext()
        | Some('+') ->
            match peek() with
            | Some('=')
            | Some('+') ->
                ignore <| next()
                add
                    ([ x
                       next() ])
            | _ -> add ([ x ])
            // ... finally
            scanNext()
        | Some('-') ->
            match peek() with
            | Some('=')
            | Some('-') ->
                add
                    ([ x
                       next() ])
            | _ -> add ([ x ])
            // ... finally
            scanNext()
        | Some(' ') ->
            let mutable collected: char option list = []
            while (isSpace (peek())) do
                collected <- collected @ [ next() ]
            add (x :: collected)
            scanNext()
        | None -> () // end!
        // is NOT None is Something else
        | _ when isSpace (x) ->
            let mutable collected: char option list = []
            while (isSpace (peek())) do
                collected <- collected @ [ next() ]
            add (x :: collected)
            scanNext()
        | _ when isWord (x) ->
            let mutable collected: char option list = [ x ]
            while (isWord (peek()) || isDigit (peek())) do
                collected <- collected @ [ next() ]
            add (collected)
            scanNext()
        | _ ->
            (f "found: default: %A \n" x)
            add ([ x ])
            scanNext()
    // ... return scan function
    scan
/// <summary>
/// Built Scanner
/// <summary>
let Scan(input: string) =
    let next = Queue(input)
    let mutable out: string option list = []

    let add (results: List<Option<char>>) =
        // ...
        results
        |> List.map (fun o -> o.Value.ToString())
        |> List.reduce join
        |> (fun value -> out <- out @ [ Some(value) ])

    let scan = Scanner next add
    scan (next (true))
    out
