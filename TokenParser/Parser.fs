module TokenParser.Parser

open Peek
open Types

let Parser getPrecedence getPrefixParselet getPostfixParselet =
    //
    let rec parseExpr queue precedence =
        match queue with
        | token :: tail ->
            /// <summary>
            ///  while precedence <= peek next precedence
            ///  parse right asssociative expression
            /// </summary>
            let rec loopWhile test left leftTail =
                match leftTail with
                | [] -> (left, leftTail) //done
                | infix :: infixTail ->
                    if test infix then
                        let parselet = getPostfixParselet infix left
                        let (left, rest) = parselet parseExpr infixTail infix
                        loopWhile test left rest
                    // ...
                    else
                        (left, leftTail)
            // ...
            let parselet: Parselet<'a> = getPrefixParselet token
            parselet parseExpr tail token 
            ||> loopWhile (fun infix -> precedence <= getPrecedence infix)
        | [] -> failwith "queue can't be empty" // avoid? (None, [])

    parseExpr
