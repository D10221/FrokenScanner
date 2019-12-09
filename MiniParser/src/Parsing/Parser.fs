module MiniParser.Parsing.Parser

open MiniParser.Token
open Parselets
open Precedence
open Error
//
let rec ParseExpr precedence tokens =

    let parseInfix left infix infixTail =
        match (Parselet infix) with
        | None -> failwithf "%A is Not Implemented!" infix
        | Some(parselet) ->
            let parse = parselet left //get right parselet
            parse ParseExpr infix infixTail

    let isPrecedenceLower token =
        token
        |> TokenValue
        |> Precedence
        >= precedence

    match tokens with
    | token :: tail ->
        try
            /// <summary>
            ///  while precedence <= peek next precedence
            ///  parse right asssociative expression
            /// </summary>
            let rec doWhile test thenDo left leftTail =
                match leftTail with
                | [] -> (left, leftTail) //done
                | infix :: infixTail ->
                    if test infix then
                        // consume
                        thenDo left infix infixTail ||> doWhile test thenDo
                    // ...
                    else (left, leftTail)

            let prefixParselet = PrefixParselet token // get left parselet
            match prefixParselet with
            | None -> 
                ParseError(sprintf "'%A' Not a prefix" (TokenValue token), Some(token), None) |> raise
            | Some(parse) -> 
                parse ParseExpr token tail ||> doWhile isPrecedenceLower parseInfix

        with
        | :? ParseError as e -> 
            raise e
        | e -> 
            ParseError(sprintf "Error parsing %A" token, Some(token), Some(e)) |> raise
    | [] -> ParseError("", None, None) |> raise
