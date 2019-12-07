module MiniParser.Lexing

module Tests =

    open System.Text.RegularExpressions

    let symbols = "`-=~!@#$%^&*()_+[]\\{}|;':\",./<>?"

    let private any list x =
        list
        |> List.tryFind (fun c -> c = x)
        <> None

    let isSpace = [ ' '; '\t' ] |> any

    let isSymbol =
        symbols.ToCharArray()
        |> Array.toList
        |> any

    let isRegexMatch pattern = Regex(pattern).IsMatch

    let isWord c = c.ToString() |> isRegexMatch "[a-zA-Z_$#@]"

    let isDigit c = c.ToString() |> isRegexMatch "\d"

    let isWordOrDigit x = isWord x || isDigit x

module Scanlets =

    let takeNextIf isMatch head tail =
        match tail with
        | [] -> ([ head ], [])
        | next :: nextTail ->
            match next with
            | y when isMatch y -> ([ head; y ], nextTail)
            | _ -> ([ head ], [])

    let private append head next tail = (head :: next, tail)

    let takeNextIfMatch y = takeNextIf (fun x1 -> x1 = y)

    let rec takeWhile isMatch head tail =
        match tail with
        | [] -> ([ head ], [])
        | next :: nextTail ->
            match next with
            | x when isMatch x ->
                (next, nextTail)
                ||> takeWhile isMatch
                ||> append head
            | _ -> ([ head ], tail)

module Scanner =

    open Tests
    open Scanlets

    let private concat = List.fold (fun a b -> a + b.ToString()) ""

    let peek isMatch tail =
        match tail with
        | [] -> false
        | next :: _ ->
            match next with
            | y when isMatch y -> true
            | _ -> false
    // ...
    let private getThird =
        function
        | [] -> 0
        | prev ->
            let (_, _, c, _, _) = List.head prev
            c
    /// <summary>
    /// Scan char list and split on
    /// Symbol,
    /// Digit,
    /// Word/Identifier,
    /// Spaces (grouped)
    /// New line
    /// </summary>
    let Scan chars =
        let rec scan lineNo colNo input =
            // recurse
            let loop (token, tokenType) tail =
                let isLine = tokenType = "newline"

                let nextLineNo =
                    lineNo + (if isLine then 1
                              else 0)

                let nextColNo =
                    if isLine then 0
                    else colNo + (String.length token)

                (token, tokenType, lineNo, colNo) :: scan nextLineNo nextColNo tail

            let append tokenType (c: char list) tail =
                let value = (c |> concat)
                ((value, tokenType), tail)

            match input with
            | [] -> [] // done
            | head :: tail ->
                match head with
                | x when x = '.' && peek isDigit tail ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c)
                    ||> append "number"
                    ||> loop
                | x when x |> isSymbol -> ((x.ToString(), "symbol"), tail) ||> loop
                | x when x |> isDigit ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c)
                    ||> append "number"
                    ||> loop
                | x when x |> isWord ->
                    (x, tail)
                    ||> takeWhile isWordOrDigit
                    ||> append "word"
                    ||> loop
                | x when x |> isSpace ->
                    (x, tail)
                    ||> takeWhile isSpace
                    ||> append "space"
                    ||> loop
                // ... newLine
                | '\n' as x -> ((x.ToString(), "newline"), tail) ||> loop
                // No match if next is not '\n'
                | '\r' as x when tail |> peek (fun c -> c = '\n') ->
                    (x, tail)
                    ||> takeNextIfMatch '\n'
                    ||> append "newline"
                    ||> loop
                // let it go thru until we decide what to do with it
                | '\r' as x -> ((x.ToString(), "newline"), tail) ||> loop
                // ...
                | x -> sprintf "'%A' is Not Implemented" x |> failwith
        scan 0 0 chars
