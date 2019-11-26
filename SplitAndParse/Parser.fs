module SplitAndParse.Parse

open TokenParser.Parselets
open TokenParser.Parser
open System.Text.RegularExpressions

let Parser () =
    /// 
    let getPrecedence x =
        match x with
        | "+" -> 1
        | "*" -> 2
        | _ -> 0
    ///
    let getPostfixParselet token =
        match token with
        | "+" -> BinaryParselet 1
        | "*" -> BinaryParselet 2
        | x -> failwithf "Postfix: %A not implemented" x
    ///
    let getPrefixParselet token =
        match token with
        | x when x.ToString() |> Regex("\w").IsMatch -> NameExprParselet
        | x when x.ToString() |> Regex("\d").IsMatch -> NumberExprParselet
        | x -> failwithf "Prefix: %A not implemented" x
    /// Entry Point
    let rec parse input =
        let parseExpr = Parser getPrecedence getPrefixParselet getPostfixParselet
        match input with
        | [] -> []
        | _ ->
            let (exp, tail) = parseExpr input 0
            exp :: parse tail
    // 
    parse