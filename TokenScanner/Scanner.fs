module TokenScanner.Scanner
open System.Text.RegularExpressions
open Queue
open Types

let join a = fun b -> a + b

let f = printf

let isWord (o: char option) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
let isDigit (o: char option) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
let isSpace (o: char option) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())


let Scanner() =
    let mutable out: string option list = []

    let add (results: List<Option<char>>) =        
        // ...
        results
        |> List.map (fun o -> o.Value.ToString())
        |> List.reduce join
        |> (fun(value)->  out <- out @ [ Some(value) ])

    let rec scan peek next x =
        let scanNext = fun () -> scan peek next (next())
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
    /// <summary>
    /// San function
    /// <summary>
    fun (input: string) ->
        let (peek, next) = Queue(input)
        scan peek next (next())
        out
