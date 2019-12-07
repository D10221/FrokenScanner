module MiniParser.Parsing

module Types =
    type Expr<'a> =
        | NameExpression of NameExpression<'a>
        | NumberExpression of NumberExpression<'a>
        | GroupExpression of GroupExpression<'a>
        | BinaryExpression of BynaryExpression<'a>
        | CallExpression of CallExpression<'a>
        | EmptyExpression of EmptyExpression<'a>
        | PrefixExpression of PrefixExpression<'a>

    and EmptyExpression<'a> =
        { token: 'a }

    //
    and BynaryExpression<'a> =
        { token: 'a
          left: Expr<'a>
          right: Expr<'a> }

    //
    and NumberExpression<'a> =
        { token: 'a }

    //
    and NameExpression<'a> =
        { token: 'a }

    //
    and GroupExpression<'a> =
        { token: 'a
          right: Expr<'a> }

    //
    and CallExpression<'a> =
        { token: 'a
          left: Expr<'a>
          right: Expr<'a> list }

    //
    and PrefixExpression<'a> =
        { token: 'a
          right: Expr<'a> }
    //
    type ParseExp<'a> = 'a list -> int -> (Expr<'a> * 'a list)
    //
    type Parselet<'a> = ParseExp<'a> -> 'a -> 'a list -> (Expr<'a> * 'a list)
    //
    type InfixParselet<'a> = Expr<'a> -> Parselet<'a>

module Token =
    //
    let tokenValue (tokenValue, _, _, _) = tokenValue        
    
module Precedence =
    // reversed Sql operator precedence
    let Precedence x =
        match x with
        | "("
        | "~" -> 8
        | "*"
        | "%"
        | "/" -> 7
        | "+"
        | "-"
        | "^"
        | "|" -> 6
        | ">"
        | "=="
        | "<"
        | ">="
        | "<="
        | "!="
        | "<>"
        | "!<"
        | "!>" -> 5
        | "!" -> 4
        | "&&" -> 3
        | "||" -> 2
        | "=" -> 1
        | _ -> 0
/// <summary>
///
/// </summary>
module Q =
    //
    let expect x =
        function
        | None -> failwithf "Expected %A" x
        | some when some.Value = x -> x //do nothing
        | y -> failwithf "Expected %A but found %A" x y
    //
    let peek f aList =
        match aList with
        | [] -> false
        | h :: _ -> f (h)
    //
    let rec collect isTerminal queue =
        match queue with
        | [] -> ([], [])
        | head :: tail ->
            if (isTerminal head) then
                ([], tail)
            else
                if List.isEmpty tail && not (isTerminal head) then failwithf "Expected terminal but found %A" head
                let (x, rest) = tail |> collect isTerminal
                (head :: x, rest)
    //
    let rec splitBy isSeparator =
        function
        | [] -> []
        | q ->
            let index = q |> List.tryFindIndex isSeparator
            match index with
            | None -> [ q ]
            | some ->
                let (p, p1) = (q |> List.splitAt (some.Value + 1))
                match p with
                | [] -> [ q ]
                | _ -> (p |> List.filter (isSeparator >> not)) :: splitBy isSeparator (p1)
/// <summary>
///
/// </summary>
module Parselets =
    open System.Text.RegularExpressions
    open Precedence
    open Types
    open Q
    open Token
    //
    let GroupParselet parseExpr token tail =
        let terminal = (fun x -> (tokenValue x) = ")")
        //
        let (queue, rest) = collect terminal tail
        let (expr, unprocessed) = parseExpr 0 queue
        assert (List.isEmpty unprocessed)
        (GroupExpression
            { token = token
              right = expr }, rest)

    let NotParselet parseExpr token tail =
        match tail with
        | [] -> failwith "Expected Expr<>"
        | _ ->
            let (right, rest) = parseExpr 0 tail
            (PrefixExpression
                { token = token
                  right = right }, rest)
    //
    let PrefixParselet token =
        match tokenValue token with
        | x when Regex("^\w+$").IsMatch(x.ToString()) ->
            fun parseExp token tail -> (NameExpression { token = token }, tail)
        | x when Regex("^\d+$").IsMatch(x.ToString()) ->
            fun parseExp token tail -> (NumberExpression { token = token }, tail)
        | "!" -> NotParselet
        | "(" -> GroupParselet
        | x -> failwithf "'%A' Not a Prefix" x
    //
    let BinaryParselet left parseExpr token tail =
        let (right, rightTail) = parseExpr (Precedence <| tokenValue token) tail

        let expr =
            BinaryExpression
                { token = token
                  left = left
                  right = right }
        (expr, rightTail)
    //
    let CallParselet left parseExpr token tail =
        let isTerminal t = (tokenValue t) = ")"
        let isSeparator t = (tokenValue t) = ","
        // ... Can be empty
        if peek isTerminal tail then
            (CallExpression
                { token = token
                  left = left
                  right = [] }, List.tail tail)
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
                    { token = token
                      left = left
                      right = many }

            (expr, rest)

    ///  right asssociative, expression parselet
    let Parselet x =
        match tokenValue x with
        | "~" -> failwithf "%A not Implemented"
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
        | "=" -> BinaryParselet
        | "(" -> CallParselet
        | _ -> failwithf "%A is Not Implemented" x
/// <summary>
///
/// </summary>
module Parser =
    open Parselets
    open Precedence
    open Token
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
            parse ParseExpr token tail
            ||> doWhile (fun token -> precedence <= (Precedence <| tokenValue token)) (fun left infix infixTail ->
                    let parse = (Parselet infix) left
                    parse ParseExpr infix infixTail)
        | [] -> failwith "queue can't be empty" // avoid? (None, [])
