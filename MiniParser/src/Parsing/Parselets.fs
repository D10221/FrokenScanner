/// <summary>
///
/// </summary>
module MiniParser.Parsing.Parselets 

open System.Text.RegularExpressions

open MiniParser.Parsing.Expressions
open MiniParser.Parsing.Precedence
open MiniParser.Parsing.Q
open MiniParser.Parsing.Types
//
let GroupParselet parseExpr token tail =
    let terminal = (fun x -> (tokenValue x) = ")")
    //
    let (queue, rest) = collect terminal tail
    let (expr, unprocessed) = parseExpr 0 queue
    assert (List.isEmpty unprocessed)
    (GroupExpression
        { token = token
          right = expr }, rest)

let NotParselet parseExpr token tail =
    match tail with
    | [] -> failwith "Expected Expr<>"
    | _ ->
        let (right, rest) = parseExpr 0 tail
        (PrefixExpression
            { token = token
              right = right }, rest)
//
let PrefixParselet token =
    match tokenValue token with
    | x when Regex("^\w+$").IsMatch(x.ToString()) ->
        fun parseExp token tail -> (NameExpression { token = token }, tail)
    | x when Regex("^\d+$").IsMatch(x.ToString()) ->
        fun parseExp token tail -> (NumberExpression { token = token }, tail)
    | "!" -> NotParselet
    | "(" -> GroupParselet
    | x -> failwithf "'%A' Not a Prefix" x
//
let BinaryParselet left parseExpr token tail =
    let (right, rightTail) = parseExpr (Precedence <| tokenValue token) tail

    let expr =
        BinaryExpression
            { token = token
              left = left
              right = right }
    (expr, rightTail)
//
let CallParselet left parseExpr token tail =
    let isTerminal t = (tokenValue t) = ")"
    let isSeparator t = (tokenValue t) = ","
    // ... Can be empty
    if peek isTerminal tail then
        (CallExpression
            { token = token
              left = left
              right = [] }, List.tail tail)
    else
        let (queue, rest) = collect isTerminal tail
        // parse a,b,c as 1 then b then c
        let toProcess = splitBy isSeparator queue

        let many =
            toProcess
            |> List.map (fun xxx ->
                let (e, rrr) = parseExpr 0 xxx
                assert List.isEmpty rrr
                e)

        let expr =
            CallExpression
                { token = token
                  left = left
                  right = many }

        (expr, rest)

///  right asssociative, expression parselet
let Parselet x =
    match tokenValue x with
    | "~" -> failwithf "%A not Implemented"
    | "*"
    | "%"
    | "/"
    | "+"
    | "-"
    | "^"
    | "|"
    | ">"
    | "=="
    | "<"
    | ">="
    | "<="
    | "!="
    | "<>"
    | "!<"
    | "!>"
    | "&&"
    | "||"
    | "=" -> BinaryParselet
    | "(" -> CallParselet
    | _ -> failwithf "%A is Not Implemented" x