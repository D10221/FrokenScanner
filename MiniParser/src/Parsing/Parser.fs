module MiniParser.Parsing.Parser

open MiniParser.Parsing.Parselets
open MiniParser.Parsing.Precedence
open MiniParser.Parsing.Types
open MiniParser.Parsing.Error
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
        |> tokenValue
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
                ParseError(sprintf "'%A' Not a prefix" (tokenValue token), Some(token), None) |> raise
            | Some(parse) -> 
                parse ParseExpr token tail ||> doWhile isPrecedenceLower parseInfix

        with
        | :? ParseError as e -> 
            raise e
        | e -> 
            ParseError(sprintf "Error parsing %A" token, Some(token), Some(e)) |> raise
    | [] -> ParseError("", None, None) |> raise
