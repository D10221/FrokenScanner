module TokenParser.Parselets

open System.Text.RegularExpressions
open Expressions
open Precedence

let parsePrefix token tail =
    match token with
    | x when x.ToString() |> Regex("\w").IsMatch -> (nameExpr x, tail)
    | x when x.ToString() |> Regex("\d").IsMatch -> (numberExp x, tail)
    | x -> failwithf "Prefix: %A not implemented" x

let binaryParselet x parseNext left =
    let (right, tail) = parseNext (getPrecedence (x))
    (binaryExpr x left right, tail)

let getPostfixParselet token =
    match token with
    | '+'
    | '*' as x -> binaryParselet x
    | x -> failwithf "Postfix: %A not implemented" x