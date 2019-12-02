module MiniParser

open System.Text.RegularExpressions

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
            |> List.fold (fun a b -> a + if b = "" then "" else "," + b) ""
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
//
let peek tail =
    match tail with
    | [] -> None
    | h :: _ -> Some(h)
//
let peekTest f tail =
    match peek tail with
    | None -> false
    | some -> f (some.Value)
//
let pop tail =
    match tail with
    | [] -> (None, [])
    | h :: t -> (Some(h), t)
//
let expect x =
    function
    | None -> failwithf "Expected %A" x
    | some when some.Value = x -> x //do nothing
    | y -> failwithf "Expected %A but found %A" x y
//
let expectNext f aList =
    match aList with
    | [] -> false
    | h :: _ -> f (h)
//
let rec collect terminal separator queue =
    match queue with
    | [] -> ([], [])
    | head :: tail ->
        if (terminal head) then
            ([], queue)
        else
            if List.isEmpty tail && not (expectNext terminal tail) then failwithf "Expected %A" terminal
            let (x, rest) =
                tail
                |> List.filter separator
                |> collect terminal separator
            (head :: x, rest)
//
let GroupParselet parseExpr token tail =
    let (queue, tail) = collect (fun x -> x = ")") (fun x -> true) tail
    // Consume ending, replace tail
    let (last, rest) = pop tail
    last
    |> expect ")"
    |> ignore
    let (expr, unprocessed) = parseExpr queue 0
    assert (List.isEmpty unprocessed)
    (GroupExpression
        { token = token
          right = expr }, rest)
//
let PrefixParselet token =
    match token with
    | x when Regex("^\w+$").IsMatch(x.ToString()) -> fun parseExp token tail -> (NameExpression { token = token }, tail)
    | x when Regex("^\d+$").IsMatch(x.ToString()) ->
        fun parseExp token tail -> (NumberExpression { token = token }, tail)
    | "(" -> GroupParselet
    | x -> failwithf "'%A' Not a Prefix" x
//
let binaryParselet left parseExpr token tail =
    let (right, rightTail) = parseExpr tail (Precedence token)

    let expr =
        BinaryExpression
            { token = token
              left = left
              right = right }
    (expr, rightTail)

let callParselet left parseExpr token tail =
    if peekTest (fun x -> x = ")") tail then
        (CallExpression
            { token = token
              left = left
              right = [] }, List.tail tail)
    else
        let (queue, rest) = collect (fun t -> t = ")") (fun t -> t <> ",") tail
        assert (List.item 0 rest = ")")

        // TODO: parse a,b,c
        let (right, unprocessed) = parseExpr queue 0
        assert (List.isEmpty unprocessed)
        let expr =
            CallExpression
                { token = token
                  left = left
                  right = [ right ] }
        (expr, List.tail rest)

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
    | "=" -> binaryParselet
    | "(" -> callParselet
    | _ -> failwithf "%A is Not Implemented" x
//
let rec parseExpr queue precedence =
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
        let (left, tail) = parse parseExpr token tail
        (left, tail)
        ||> doWhile (fun token -> precedence <= Precedence token) (fun left infix infixTail ->
                let parse = (Parselet infix) left
                parse parseExpr infix infixTail)
    | [] -> failwith "queue can't be empty" // avoid? (None, [])
