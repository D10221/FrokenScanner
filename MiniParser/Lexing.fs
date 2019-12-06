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

        let private append t (x: char list) xxx = (((x |> concat), t), xxx)

        let peek isMatch tail =
            match tail with
            | [] -> false
            | next :: _ ->
                match next with
                | y when isMatch y -> true
                | _ -> false

        /// <summary>
        /// Scan char list and split on
        /// Symbol,
        /// Digit,
        /// Word/Identifier,
        /// Spaces (grouped)
        /// New line
        /// </summary>
        let rec Scan input =
            // recurse
            let scan head tail = head :: Scan tail

            match input with
            | [] -> [] // done
            | head :: tail ->
                match head with
                | x when x = '.' && peek isDigit tail ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c )
                    ||> append "number"
                    ||> scan
                | x when x |> isSymbol -> ((x.ToString(), "symbol"), tail) ||> scan
                | x when x |> isDigit ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c )
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
