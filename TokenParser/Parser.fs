module TokenParser.Parser

open System.Text.RegularExpressions
open Expressions

let parsePrefix token tail =
    match token with
    | x when x.ToString() |> Regex("\w").IsMatch -> (nameExpr x, tail)
    | x when x.ToString() |> Regex("\d").IsMatch -> (numberExp x, tail)
    | x -> failwithf "Prefix: %A not implemented" x

let getPrecedence x =
    match x with
    | '+' -> 1
    | '*' -> 2
    | _ -> 0

let binaryParselet x parseNext left =
    let (right, tail) = parseNext (getPrecedence (x))
    (binaryExpr x left right, tail)

let getPostfixParselet token =
    match token with
    | '+'
    | '*' as x -> binaryParselet x
    | x -> failwithf "Postfix: %A not implemented" x

/// <summary>
/// replaces while loop
/// </summary>
let rec parseprecedence parseExpr precedence left queue =
    match queue with
    | [] -> (left, queue)
    | token :: tail ->
        let nextPrecedence = getPrecedence token
        if (nextPrecedence) < precedence then
            (left, queue)
        else
            let parseNext = parseExpr tail //
            let parselet = getPostfixParselet token parseNext
            let (left, rest) = parselet left
            parseprecedence parseExpr precedence left rest

let rec parseExpr queue precedence =
    match queue with
    | token :: tail ->
        let (left, rest) = parsePrefix token tail
        parseprecedence parseExpr precedence left rest
    | [] -> failwith "queue can't be empty"

let rec parse input =
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input 0
        let ret = exp :: parse tail
        ret
