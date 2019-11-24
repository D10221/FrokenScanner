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
    // .. 
    if queue |> isNextPrecedenceLowerThan precedence then
        (left, queue) // may not process!
    else
        // while next precedence higher than precedence
        match queue with
        | [] -> (left, queue) // nothing else to process
        | token :: tail ->
            let parseNext = parseExpr tail // don't send the tail down
            let parselet = getPostfixParselet token parseNext
            let (left, rest) = parselet left
            loop left rest

let rec parseExpr queue precedence =
    match queue with
    | token :: tail ->
        let (left, rest) = parsePrefix token tail
        parsePostFix parseExpr precedence left rest
    | [] -> failwith "queue can't be empty"

let rec parse input =
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input 0
        exp :: parse tail        