module MiniParser.Parsing.Parser

open MiniParser.Parsing.Parselets
open MiniParser.Parsing.Precedence
open MiniParser.Parsing.Types
//
let rec ParseExpr precedence tokens =
    match tokens with
    | token :: tail ->
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
        //
        let parse = PrefixParselet token

        let precedenceLower token =
            token
            |> tokenValue
            |> Precedence
            >= precedence

        let parseInfix left infix infixTail =
            let parse = (Parselet infix) left
            parse ParseExpr infix infixTail

        parse ParseExpr token tail ||> doWhile precedenceLower parseInfix
    | [] -> failwith "queue can't be empty" // avoid? (None, [])
