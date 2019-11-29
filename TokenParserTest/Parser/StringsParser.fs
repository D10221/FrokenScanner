module Parser.StringsParser

open System.Text.RegularExpressions
open TokenParser.Parser
open TokenParser.Parselets
open Precedence

let StringsParser getPrecedence  =    
    ///
    let getPostfixParselet token =
        match token with
        | "+" -> binaryParselet (getPrecedence "+")
        | "*" -> binaryParselet (getPrecedence "*")
        | x -> failwithf "Postfix: %A not implemented" x
    ///
    let getPrefixParselet token =
        match token with
        | x when x.ToString() |> Regex("\w").IsMatch -> nameExprParselet
        | x when x.ToString() |> Regex("\d").IsMatch -> numberExprParselet
        | x -> failwithf "Prefix: %A not implemented" x
    /// Entry Point
    let rec parse input =
        let parseExpr = Parser Precedence getPrefixParselet getPostfixParselet
        match input with
        | [] -> []
        | _ ->
            let (exp, tail) = parseExpr input 0
            exp :: parse tail
    // 
    parse
///