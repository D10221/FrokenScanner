module TokenParser.Parser

open Peek
open Precedence
open Parselets
open Types

let private isNextPrecedenceLowerThan precedence = (fun nextToken -> getPrecedence nextToken < precedence) |> peekTest

/// <summary>
/// replaces while loop
/// </summary>
let rec parsePostFix parseExpr precedence left queue =
    /// recurse
    let loop = parsePostFix parseExpr precedence
    // while next precedence higher than precedence
    match queue with
    | q when isNextPrecedenceLowerThan precedence q -> (left, queue) // may not process!
    | [] -> (left, queue) // nothing else to process
    | token :: tail ->
        let parselet = getPostfixParselet token
        let (left, rest) = parselet left parseExpr tail token
        loop left rest

///
let rec parseExpr: Parse =
    fun queue precedence ->
        match queue with
        | token :: tail ->
            let parselet = getPrefixParselet token
            let (left, rest) = parselet parseExpr tail token
            // may return juts left or loop postfix/precedence
            parsePostFix parseExpr precedence left rest
        | [] -> failwith "queue can't be empty" // avoid? (None, [])

/// Entry Point
let rec parse input =
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input 0
        exp :: parse tail
 