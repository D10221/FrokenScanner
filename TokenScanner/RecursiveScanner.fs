module TokenScanner.RecursiveScanner

open System.Text.RegularExpressions

let symbols = "`-=~!@#$%^&*()_+[]\\{}|;':\",./<>?"

let any list x =
    list
    |> List.tryFind (fun c -> c = x)
    <> None

let private isSpace = [ ' '; '\t' ] |> any

let private isSymbol =
    symbols.ToCharArray()
    |> Array.toList
    |> any

let isRegexMatch pattern = Regex(pattern).IsMatch

let private isWord c = c.ToString() |> isRegexMatch "[a-zA-Z_$#@]"

let private isDigit c = c.ToString() |> isRegexMatch "\d"

let concat = List.fold (fun a b -> a + b.ToString()) ""

let takeNextIf isMatch tail =
    match tail with
    | [] -> ([], tail)
    | next :: nextTail ->
        match next with
        | y when isMatch y -> ([ y ], nextTail)
        | _ -> ([], tail)

let takeNextIfMatch y head tail =
    let (next, nextTail) = takeNextIf (fun x1 -> x1 = y) tail
    (head :: next |> concat, nextTail)

let rec takeWhile isMatch head tail =
    match tail with
    | [] -> (head :: [], [])
    | next :: nextTail ->
        match next with
        | x when isMatch x ->
            let (b, bbb) = (takeWhile isMatch next nextTail)
            (head :: b, bbb)
        | _ -> (head :: [], tail)

let rec Scan input =

    let scan head tail = 
        head.ToString() :: Scan tail

    match input with
    | [] -> []
    | head :: tail ->
        match head with
        | x when x |> isSymbol -> (x, tail) ||> scan
        | x when x |> isDigit -> (x, tail) ||> scan
        | x when x |> isWord ->
            let (x1, xxx) = takeWhile (fun x -> isWord x || isDigit x) x tail
            (x1 |> concat, xxx) ||> scan
        | x when x |> isSpace -> (x, tail) ||> scan
        // ... newLine
        | '\n' -> scan head tail
        | '\r' as x ->
            (x, tail)
            ||> takeNextIfMatch '\n'
            ||> scan
        // ...
        | x -> sprintf "'%c' is Not Implemented" x |> failwith
 