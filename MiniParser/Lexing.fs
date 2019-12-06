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
            let (_, _, c) = List.head prev
            c
    /// <summary>
    /// re Count
    /// </summary>
    let ReCount length =
        let rec reCount lineNo =
            function
            | [] -> []
            | (token, tokenType, tokenStart) :: tail ->
                let isLine = tokenType = "newline"

                let nextLineNo =
                    lineNo + (if isLine then 1
                              else 0)
                (token, tokenType, length - tokenStart, lineNo) :: reCount nextLineNo tail
        reCount 0

    /// <summary>
    /// Scan char list and split on
    /// Symbol,
    /// Digit,
    /// Word/Identifier,
    /// Spaces (grouped)
    /// New line
    /// return (tokenValue, tokenType, tokenStart(column))
    /// </summary>
    let rec private sCan input =
        // recurse
        let scan (head: string * string) tail =
            let prev = sCan tail
            let start = getThird prev
            let tokenStart = start + (fst head).Length
            // (tokenValue, tokenType, tokenStart(column))
            (fst head, snd head, tokenStart) :: prev

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
                ||> scan
            | x when x |> isSymbol -> ((x.ToString(), "symbol"), tail) ||> scan
            | x when x |> isDigit ->
                (x, tail)
                ||> takeWhile (fun c -> isDigit c || '.' = c)
                ||> append "number"
                ||> scan
            | x when x |> isWord ->
                (x, tail)
                ||> takeWhile isWordOrDigit
                ||> append "word"
                ||> scan
            | x when x |> isSpace ->
                (x, tail)
                ||> takeWhile isSpace
                ||> append "space"
                ||> scan
            // ... newLine
            | '\n' as x -> ((x.ToString(), "newline"), tail) ||> scan
            // No match if next is not '\n'
            | '\r' as x when tail |> peek (fun c -> c = '\n') ->
                (x, tail)
                ||> takeNextIfMatch '\n'
                ||> append "newline"
                ||> scan
            // let it go thru until we decide what to do with it
            | '\r' as x -> ((x.ToString(), "newline"), tail) ||> scan
            // ...
            | x -> sprintf "'%A' is Not Implemented" x |> failwith
    /// <summary>
    /// Scan char list and split on
    /// Symbol,
    /// Digit,
    /// Word/Identifier,
    /// Spaces (grouped)
    /// New line
    /// </summary>
    let Scan xxx = sCan xxx |> ReCount(List.length xxx)
