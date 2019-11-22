module TokenSplitter.Scan

open TokenSplitter.Test
open TokenSplitter.Scanlets

let private concat = List.fold (fun a b -> a + b.ToString()) ""

let private append t (x: char list) xxx = (((x |> concat), t), xxx)

let peek isMatch tail =
    match tail with
    | [] -> false
    | next :: _ ->
        match next with
        | y when isMatch y ->  true            
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
        | x when x |> isSymbol -> ((x.ToString(), "symbol"), tail) ||> scan
        | x when x |> isDigit ->
            (x, tail)
            ||> takeWhile isDigit
            ||> append "digit"
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
        | '\r' as x when tail |> peek (fun c-> c = '\n')  ->
            (x, tail)
            ||> takeNextIfMatch '\n'
            ||> append "newline"
            ||> scan
        // let it go thru until we decide what to do with it
        | '\r' as x -> ((x.ToString(), "newline"), tail) ||> scan
        // ...
        | x -> sprintf "'%A' is Not Implemented" x |> failwith
 