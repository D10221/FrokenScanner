namespace TokenScanner

module Scan = 

    open System
    open System.Text.RegularExpressions

    let toString (c: Char) = c.ToString()

    let join a = fun b -> a + b

    let f = printf

    let isWord (o: char option) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
    let isDigit (o: char option) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
    let isSpace (o: char option) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())


    let rec Scan (peek: unit -> char option) (next: unit -> char option) =
        let mutable out: string option list = []
        let append value =
            out <- out @ [ Some(value) ]
            ()
        let add (x: char) =
            f "Adding %c\n" x
            append (x.ToString())
        let addMany (list: char list) =
            f "Addmany %A\n" list
            list
            |> List.map (toString)
            |> List.reduce join
            |> append
        let postFix (x: char option) (op1: char option) (op2: char option) (incoming: char option) =
            if (incoming = op1) then
                f "incoming: %A from %A is %A [op1]\n" incoming x op1
                addMany ([ x.Value; op1.Value ])
                ignore <| next()
            elif (incoming = op2) then
                f "incoming: %A from %A is %A [op2]\n" incoming x op2
                addMany ([ x.Value; op2.Value ])
                ignore <| next()
            else
                add (x.Value)
                f "incoming: %A from %A is NOT op\n" incoming x
        let rec scan x =
            match x with
            | Some('=') ->
                let p = peek()
                (f "found: %A peeking: %A \n" x p)
                (Some('='), Some('=')) ||> postFix x p
                (scan (next()))
            | Some('+') ->
                let p = peek()
                (f "found: %A peeking: %A \n" x p)
                (Some('='), Some('=')) ||> postFix x p
                (scan (next()))
            | Some('-') ->
                let p = peek()
                (f "found: %A peeking: %A \n" x p)
                (Some('='), Some('=')) ||> postFix x p
                (scan (next()))
            | Some(' ') ->
                (f "found: %A \n" x)
                add (x.Value)
                scan (next())
            | None ->
                (f "found: NONE %A \n" x)
                ()
            | _ when isWord (x) ->
                (f "found: word: %A \n" x)
                let mutable collected: char option list = [ x ]
                while (isWord (peek()) || isDigit (peek())) do
                    collected <- collected @ [ next() ]
                addMany (collected |> List.map (fun o -> o.Value))
                scan (next())
            | _ ->
                (f "found: default: %A \n" x)
                add (x.Value)
                scan (next())            
        scan (next())
        out