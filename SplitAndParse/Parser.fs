module SplitAndParse.Parse

open TokenParser.Parselets
open TokenParser.Parser
open System.Text.RegularExpressions

let Parser() =
    // Broken precedence
    // https://docs.microsoft.com/en-us/sql/t-sql/language-elements/operator-precedence-transact-sql?view=sql-server-ver15
    // https://www.sqlite.org/lang_expr.html
    let getPrecedence x =
        match x with        
        | "~" -> 1 // (Bitwise NOT)
        | "*"
        | "/"
        | "%" -> 2        
        | "+" // (Positive) (Addition) (Concatenation) 
        | "-" // (Negative) (Substraction) 
        | "&" // (Bitwise AND)
        | "^" // (Bitwise Exclusive OR)
        | "|" -> 3// (Bitwise OR)        
        // (Comparison operators)
        | "==" // equals
        | ">"
        | "<"
        | ">="
        | "<="
        | "<>"
        | "!="
        | "!>"
        | "!>" ->  4
        | "NOT" -> 5
        | "&&"
        | "AND" -> 6
        | "ALL"
        | "ANY"
        | "BETWEEEN"
        | "IN"
        | "LIKE"
        | "OR"
        | "||"
        | "SOME" -> 7
        | "=" -> 8 // Assignment
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
