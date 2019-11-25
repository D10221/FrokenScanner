module TokenParser.Parser

open Peek
open Precedence
open Parselets

let isNextPrecedenceLowerThan precedence = (fun nextToken -> getPrecedence nextToken < precedence) |> peekTest

/// <summary>
/// replaces while loop
/// </summary>
let rec parsePostFix parseExpr precedence left queue =
    /// recurse
    let loop = parsePostFix parseExpr precedence
    // .. moving this question out , will to force ask again 
    if queue |> isNextPrecedenceLowerThan precedence then
        (left, queue) // may not process!
    else
        // while next precedence higher than precedence
        match queue with
        | [] -> (left, queue) // nothing else to process
        | token :: tail ->            
            let parselet = getPostfixParselet token
            let (left, rest) = parselet left parseExpr tail token 
            loop left rest

let rec parseExpr queue precedence =
    match queue with
    | token :: tail ->
        let parselet = getPrefixParselet token 
        let (left, rest) = parselet parseExpr tail token
        // may return juts left or loop postfix/precedence 
        parsePostFix parseExpr precedence left rest
    | [] -> failwith "queue can't be empty" // avoid? (None, [])

let rec parse input =
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input 0
        exp :: parse tail        