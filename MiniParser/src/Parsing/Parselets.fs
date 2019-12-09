/// <summary>
///
/// </summary>
module MiniParser.Parsing.Parselets

open MiniParser.Parsing.Expressions
open MiniParser.Parsing.Precedence
open MiniParser.Parsing.Q
open MiniParser.Parsing.Types
open MiniParser.Lexing.Types
//
let GroupParselet parseExpr token tail =
    let terminal = (fun x -> (tokenValue x) = ")")
    //
    let (queue, rest) = collect terminal tail
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
let NumberParselet parseExp token tail = (NumberExpression { Token = token }, tail)
//
let PrefixParselet token =
    match token with
    | (_, TokenType.NONE, _, _) -> None
    | (_, TokenType.WORD, _, _) -> Some(NameParselet)
    | (_, TokenType.NUMBER, _, _) -> Some(NumberParselet)
    | ("!", _, _, _) -> Some(PrefixOperatorParselet)
    | ("(", _, _, _) -> Some(GroupParselet)
    | _ -> None
//
let BinaryParselet left parseExpr token tail =
    let (right, rightTail) = parseExpr (Precedence <| tokenValue token) tail

    let expr =
        BinaryExpression
            { Token = token
              Left = left
              Right = right }
    (expr, rightTail)
//
let CallParselet left parseExpr token tail =
    let isTerminal t = (tokenValue t) = ")"
    let isSeparator t = (tokenValue t) = ","
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

        let expr =
            CallExpression
                { Token = token
                  Left = left
                  Right = many }

        (expr, rest)

///  right asssociative, expression parselet
let Parselet x =
    match tokenValue x with
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
