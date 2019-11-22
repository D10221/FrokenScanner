module TokenSplitter.Scan

open TokenSplitter.Test
open TokenSplitter.Scanlets

let private concat = List.fold (fun a b -> a + b.ToString()) ""

let private append x xxx = (x |> concat, xxx)

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
    let scan head tail = head.ToString() :: Scan tail

    match input with
    | [] -> [] // done
    | head :: tail ->
        match head with
        | x when x |> isSymbol -> (x, tail) ||> scan
        | x when x |> isDigit ->
            (x, tail)
            ||> takeWhile isDigit
            ||> append
            ||> scan
        | x when x |> isWord ->
            (x, tail)
            ||> takeWhile isWordOrDigit
            ||> append
            ||> scan
        | x when x |> isSpace ->
            (x, tail)
            ||> takeWhile isSpace
            ||> append
            ||> scan
        // ... newLine
        | '\n' -> scan head tail
        | '\r' as x ->
            (x, tail)
            ||> takeNextIfMatch '\n'
            ||> scan
        // ...
        | x -> sprintf "'%c' is Not Implemented" x |> failwith
 