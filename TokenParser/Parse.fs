module TokenParser.Parse

open System.Text.RegularExpressions
open Parser
open Parselets

let getPrecedence x =
    match x with
    | '+' -> 1
    | '*' -> 2
    | _ -> 0

let getPostfixParselet token =
    match token with
    | '+' -> binaryParselet 1
    | '*' -> binaryParselet 2
    | x -> failwithf "Postfix: %A not implemented" x


let getPrefixParselet token =
    match token with
    | x when x.ToString() |> Regex("\w").IsMatch -> nameExprParselet
    | x when x.ToString() |> Regex("\d").IsMatch -> numberExprParselet
    | x -> failwithf "Prefix: %A not implemented" x


/// Entry Point
let rec parse input =
    let parseExpr = Parser getPrecedence getPrefixParselet getPostfixParselet
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input 0
        exp :: parse tail
 