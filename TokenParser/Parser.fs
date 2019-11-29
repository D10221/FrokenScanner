module TokenParser.Parser

open Peek
open Types

let Parser getPrecedence getPrefixParselet getPostfixParselet =
    //
    let rec parseExpr queue precedence =
        match queue with
        | token :: tail ->
            /// <summary>
            ///  parse right asssociative expression
            /// </summary>
            let rec loopWhile test left leftTail =
                let loop = loopWhile test
                match leftTail with
                | [] -> (left, leftTail) //done
                | infix :: infixTail ->
                    if test infix then
                        let parselet = getPostfixParselet infix left
                        parselet parseExpr infixTail infix ||> loop
                    // ...
                    else
                        (left, leftTail)
            // ...
            let parselet: Parselet<'a> = getPrefixParselet token
            ///  while precedence <= peek next precedence
            let loop = loopWhile <| fun infix -> precedence <= getPrecedence infix
            (tail ,token ) ||> parselet parseExpr ||> loop
        | [] -> failwith "queue can't be empty" // avoid? (None, [])

    parseExpr
