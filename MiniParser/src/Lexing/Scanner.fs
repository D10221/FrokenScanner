module MiniParser.Lexing.Scanner 
open MiniParser.Token
open Scanlets
open System.Text.RegularExpressions

let operators = "`-=~!@#$%^&*()_+[]\\{}|;':\",./<>?"

let private any list x =
    list
    |> List.tryFind (fun c -> c = x)
    <> None

let isSpace = [ ' '; '\t' ] |> any

let isOperator =
    operators.ToCharArray()
    |> Array.toList
    |> any

let isRegexMatch pattern = Regex(pattern).IsMatch

let isWord c = c.ToString() |> isRegexMatch "[a-zA-Z_$#@]"

let isDigit c = c.ToString() |> isRegexMatch "\d"

let isWordOrDigit x = isWord x || isDigit x
let private concat = List.fold (fun a b -> a + b.ToString()) ""

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
let Scan chars =
    let rec scan lineNo colNo input =
        // recurse
        let loop (token, tokenType) tail =
            let isLine = tokenType = TokenType.NLINE

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
                ||> append NUMBER
                ||> loop
            | x when x |> isOperator -> ((x.ToString(), OP), tail) ||> loop
            | x when x |> isDigit ->
                (x, tail)
                ||> takeWhile (fun c -> isDigit c || '.' = c)
                ||> append NUMBER
                ||> loop
            | x when x |> isWord ->
                (x, tail)
                ||> takeWhile isWordOrDigit
                ||> append WORD
                ||> loop
            | x when x |> isSpace ->
                (x, tail)
                ||> takeWhile isSpace
                ||> append SPACE
                ||> loop
            // ... newLine
            | '\n' as x -> ((x.ToString(), NLINE ), tail) ||> loop
            // No match if next is not '\n'
            | '\r' as x when tail |> peek (fun c -> c = '\n') ->
                (x, tail)
                ||> takeNextIfMatch '\n'
                ||> append NLINE
                ||> loop
            // let it go thru until we decide what to do with it
            | '\r' as x -> ((x.ToString(), NLINE), tail) ||> loop
            // ...
            | x -> sprintf "Operator '%A' is Not Implemented" x |> failwith
    scan 0 0 chars
