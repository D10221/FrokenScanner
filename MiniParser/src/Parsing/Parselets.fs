/// <summary>
///
/// </summary>
module MiniParser.Parsing.Parselets

open MiniParser.Token
open MiniParser.Expressions
open Precedence
open Q

let private eq x b = x = b
//
let GroupParselet terminal parseExpr token tail =
    let isTerminal = (TokenValue >> eq terminal)
    let (queue, rest) = collect isTerminal tail
    let (expr, unprocessed) = parseExpr 0 queue
    assert (List.isEmpty unprocessed)
    (GroupExpression
        { Token = token
          Right = expr }, rest)
//
let PrefixOperatorParselet parseExpr token tail =
    match tail with
    | [] -> failwith "Expected Expr<>"
    | _ ->
        let (right, rest) = parseExpr 0 tail
        (PrefixExpression
            { Token = token
              Right = right }, rest)
//
let NameParselet _parseExp token tail = (NameExpression { Token = token }, tail)
//
let NumberParselet _parseExp token tail = (NumberExpression { Token = token }, tail)
//
let StringParselet terminal _parseExpr token tail =
    let isTerminal = TokenValue >> eq terminal
    // ... Can be empty
    if peek isTerminal tail then
        (StringExpression
            { Token = token
              Right = List.Empty }, List.tail tail)
    else
        let (queue, rest) = collect isTerminal tail
        (StringExpression
            { Token = token
              Right = queue }, rest)
//
//
let PrefixParselet token =
    match token with
    | ("\'", _, _, _) -> Some(StringParselet  "\'")
    | (_, TokenType.WORD, _, _) -> Some(NameParselet)
    | (_, TokenType.NUMBER, _, _) -> Some(NumberParselet)
    | ("!", _, _, _) -> Some(PrefixOperatorParselet)
    | ("(", _, _, _) -> Some(GroupParselet ")")
    | (_, TokenType.NONE, _, _) -> None
    | _ -> None
//
let BinaryParselet left parseExpr token tail =
    let (right, rightTail) = parseExpr (Precedence <| TokenValue token) tail

    let expr =
        BinaryExpression
            { Token = token
              Left = left
              Right = right }
    (expr, rightTail)
//
let CallParselet left parseExpr token tail =
    let isTerminal = TokenValue >> eq ")"
    let isSeparator = TokenValue >> eq ","
    // ... Can be empty
    if peek isTerminal tail then
        (CallExpression
            { Token = token
              Left = left
              Right = [] }, List.tail tail)
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

        (CallExpression
            { Token = token
              Left = left
              Right = many }, rest)

///  right asssociative, expression parselet
let Parselet x =
    match TokenValue x with
    // | "~" -> ParseError ("~ is not Implemented", None, None) |> raise
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
    | "=" -> Some(BinaryParselet)
    | "(" -> Some(CallParselet)
    | _ -> None
