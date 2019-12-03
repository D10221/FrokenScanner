module MiniParser

module Lexing =
    module Tests =

        open System.Text.RegularExpressions

        let symbols = "`-=~!@#$%^&*()_+[]\\{}|;':\",./<>?"

        let private any list x =
            list
            |> List.tryFind (fun c -> c = x)
            <> None

        let isSpace = [ ' '; '\t' ] |> any

        let isSymbol =
            symbols.ToCharArray()
            |> Array.toList
            |> any

        let isRegexMatch pattern = Regex(pattern).IsMatch

        let isWord c = c.ToString() |> isRegexMatch "[a-zA-Z_$#@]"

        let isDigit c = c.ToString() |> isRegexMatch "\d"

        let isWordOrDigit x = isWord x || isDigit x

    module Scanlets =

        let takeNextIf isMatch head tail =
            match tail with
            | [] -> ([ head ], [])
            | next :: nextTail ->
                match next with
                | y when isMatch y -> ([ head; y ], nextTail)
                | _ -> ([ head ], [])

        let private append head next tail = (head :: next, tail)

        let takeNextIfMatch y = takeNextIf (fun x1 -> x1 = y)

        let rec takeWhile isMatch head tail =
            match tail with
            | [] -> ([ head ], [])
            | next :: nextTail ->
                match next with
                | x when isMatch x ->
                    (next, nextTail)
                    ||> takeWhile isMatch
                    ||> append head
                | _ -> ([ head ], tail)

    module Scanner =

        open Tests
        open Scanlets

        let private concat = List.fold (fun a b -> a + b.ToString()) ""

        let private append t (x: char list) xxx = (((x |> concat), t), xxx)

        let peek isMatch tail =
            match tail with
            | [] -> false
            | next :: _ ->
                match next with
                | y when isMatch y -> true
                | _ -> false

        /// <summary>
        /// Scan char list and split on
        /// Symbol,
        /// Digit,
        /// Word/Identifier,
        /// Spaces (grouped)
        /// New line
        /// </summary>
        let rec Scan input =
            // recurse
            let scan head tail = head :: Scan tail

            match input with
            | [] -> [] // done
            | head :: tail ->
                match head with
                | x when x = '.' && peek isDigit tail ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c )
                    ||> append "number"
                    ||> scan
                | x when x |> isSymbol -> ((x.ToString(), "symbol"), tail) ||> scan
                | x when x |> isDigit ->
                    (x, tail)
                    ||> takeWhile (fun c -> isDigit c || '.' = c )
                    ||> append "number"
                    ||> scan
                | x when x |> isWord ->
                    (x, tail)
                    ||> takeWhile isWordOrDigit
                    ||> append "word"
                    ||> scan
                | x when x |> isSpace ->
                    (x, tail)
                    ||> takeWhile isSpace
                    ||> append "space"
                    ||> scan
                // ... newLine
                | '\n' as x -> ((x.ToString(), "newline"), tail) ||> scan
                // No match if next is not '\n'
                | '\r' as x when tail |> peek (fun c -> c = '\n') ->
                    (x, tail)
                    ||> takeNextIfMatch '\n'
                    ||> append "newline"
                    ||> scan
                // let it go thru until we decide what to do with it
                | '\r' as x -> ((x.ToString(), "newline"), tail) ||> scan
                // ...
                | x -> sprintf "'%A' is Not Implemented" x |> failwith

module Parsing =

    module Types =
        type Expr<'a> =
            | NameExpression of NameExpression<'a>
            | NumberExpression of NumberExpression<'a>
            | GroupExpression of GroupExpression<'a>
            | BinaryExpression of BynaryExpression<'a>
            | CallExpression of CallExpression<'a>
            | EmptyExpression of EmptyExpression<'a>

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
        type ParseExp<'a> = 'a list -> int -> (Expr<'a> * 'a list)
        //
        type Parselet<'a> = ParseExp<'a> -> 'a -> 'a list -> (Expr<'a> * 'a list)
        //
        type InfixParselet<'a> = Expr<'a> -> Parselet<'a>

    module Visitor =
        open Types

        let private reduceOrDefault f def x =
            match x with
            | [] -> def
            | _ -> x |> List.reduce f
        //
        let rec visit (expr: Expr<'a>) =
            match expr with
            | NameExpression e -> sprintf "%A" (e.token)
            | NumberExpression e -> sprintf "%A" (e.token)
            | GroupExpression e -> sprintf "(%A)" (visit e.right)
            | CallExpression e ->
                let left = visit (e.left)

                let right =
                    (e.right)
                    |> List.map visit
                    |> reduceOrDefault (fun a b -> a + "," + b) ""
                sprintf "(%s%A%s))" left (e.token) right
            | BinaryExpression e ->
                let left = visit (e.left)
                let right = visit (e.right)
                sprintf "(%s %A %s)" left (e.token) right
            | EmptyExpression _ -> ""
        //
        let rec visitMany (exprs: Expr<'a> list) =
            match exprs with
            | [] -> []
            | (expr :: tail) ->
                let visited = (visit expr)
                visited :: visitMany tail
    /// <summary>
    ///
    /// </summary>
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
        let testNext f aList =
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
        //
        let GroupParselet parseExpr token tail =
            let terminal = (fun x -> x = ")")
            //
            let (queue, rest) = collect terminal tail
            let (expr, unprocessed) = parseExpr queue 0
            assert (List.isEmpty unprocessed)
            (GroupExpression
                { token = token
                  right = expr }, rest)
        //
        let PrefixParselet token =
            match token with
            | x when Regex("^\w+$").IsMatch(x.ToString()) ->
                fun parseExp token tail -> (NameExpression { token = token }, tail)
            | x when Regex("^\d+$").IsMatch(x.ToString()) ->
                fun parseExp token tail -> (NumberExpression { token = token }, tail)
            | "(" -> GroupParselet
            | x -> failwithf "'%A' Not a Prefix" x
        //
        let BinaryParselet left parseExpr token tail =
            let (right, rightTail) = parseExpr tail (Precedence token)

            let expr =
                BinaryExpression
                    { token = token
                      left = left
                      right = right }
            (expr, rightTail)
        //
        let CallParselet left parseExpr token tail =
            let isTerminal t = t = ")"
            let isSeparator t = t = ","
            // ... Can be empty
            if testNext isTerminal tail then
                (CallExpression
                    { token = token
                      left = left
                      right = [] }, List.tail tail)
            else  // collect args, every arg can be many tokens as 1 expresion
                let (queue, rest) = collect isTerminal tail
                // parse a,b,c as 1 then b then c
                let toProcess = splitBy isSeparator queue

                let many =
                    toProcess
                    |> List.map (fun xxx ->
                        let (e, rrr) = parseExpr xxx 0
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
            match x with
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
            | "!"
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
        //
        let rec ParseExpr queue precedence =
            match queue with
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
                ||> doWhile (fun token -> precedence <= Precedence token) (fun left infix infixTail ->
                        let parse = (Parselet infix) left
                        parse ParseExpr infix infixTail)
            | [] -> failwith "queue can't be empty" // avoid? (None, [])
