module TokenParser.Parselets

open System.Text.RegularExpressions
open Expressions
open Precedence
open Peek
open Types

let nameExprParselet (parse: Parse) queue token =    
    // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
    let expr = nameExpr token
    (expr, queue)

let numberExprParselet (parse: Parse) queue token =
    // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
    let expr = nameExpr token
    (expr, queue)

let getPrefixParselet token =
    match token with
    | x when x.ToString() |> Regex("\w").IsMatch -> nameExprParselet
    | x when x.ToString() |> Regex("\d").IsMatch -> numberExprParselet
    | x -> failwithf "Prefix: %A not implemented" x

let binaryParselet (parse: Parse) queue token (left: Expr) =
    let myPrecedence = getPrecedence (token)
    let (right, tail) = parse queue (myPrecedence)
    let expr = binaryExpr token left right
    (expr, tail)

let getPostfixParselet token =
    match token with
    | '+'
    | '*' -> binaryParselet
    | x -> failwithf "Postfix: %A not implemented" x
