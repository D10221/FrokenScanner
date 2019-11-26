module TokenParser.Parser

open Peek
open Types

let Parser getPrecedence getPrefixParselet getPostfixParselet = 
    /// <summary>
    /// replaces while loop
    /// </summary>
    let rec parsePostFix = fun  parseExpr precedence left queue  ->
        /// recurse
        let loop = parsePostFix parseExpr precedence
        // while next precedence higher than precedence
        match queue with
        | q when (fun nextToken -> getPrecedence nextToken <= precedence)
                 |> peekTest
                 <| q -> (left, queue) // break loop
        | [] -> (left, queue) // nothing else to process
        | token :: tail ->
            let parselet: Parselet<'a> = getPostfixParselet token left
            let (left, rest) = parselet  parseExpr tail token
            loop left rest

    ///
    let rec parseExpr  =                
        fun queue precedence ->
            match queue with
            | token :: tail ->
                let parselet: Parselet<'a> = getPrefixParselet token
                let (left, rest) = parselet parseExpr tail token
                // may return juts left or loop postfix/precedence
                (parsePostFix parseExpr) precedence left rest
            | [] -> failwith "queue can't be empty" // avoid? (None, [])
    
    parseExpr