// Learn more about F# at http://fsharp.org

open System
open System.Text.RegularExpressions

type Expr<'a> = 
    |NameExpression of NameExpression<'a>
    |NumberExpression of NumberExpression<'a>
    |BinaryExpression of BynaryExpression<'a>
and BynaryExpression<'a> = {    
    token: 'a    
    left: Expr<'a>
    right: Expr<'a>
}
and NumberExpression<'a> = {
    token: 'a
}
and NameExpression<'a> = {
    token: 'a
}

let rec visit (expr: Expr<'a>) =
        match expr with
        | NameExpression e -> sprintf "%A" (e.token)
        | NumberExpression e -> sprintf "%A" (e.token)
        | BinaryExpression e ->
            let left = visit (e.left)
            let right = visit (e.right)
            sprintf "(%s %A %s)" left (e.token) right

let rec visitMany (exprs: Expr<'a> list) =    
    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (visit expr)
        visited :: visitMany tail

// reversed Sql operator precedence
let getPrecedence x =
        match x with
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

let getPrefix token =
        match token with 
        | x when Regex("\w+").IsMatch (x.ToString()) -> 
            NameExpression { token = token }            
        | x -> failwithf "'%A' Not a Prefix" x

let peek queue =
    match queue with
    | [] -> None
    | head:: _ -> Some(head)

let peekTest test queue  =
    match peek queue with
    | None -> false
    | some -> test (some.Value)


let rec parseExpr queue precedence =
        match queue with
        | token :: tail ->                        
            //...while precedence <= peek next precedence
            let rec loop left leftTail =
                match leftTail with
                | [] -> (left, leftTail)
                | infix :: infixTail ->    
                    let infixPrecedence = getPrecedence infix
                    if precedence <= infixPrecedence   then
                        let binaryParselet() =
                            let (right, rightTail) = parseExpr infixTail infixPrecedence
                            let expr =
                                BinaryExpression
                                    { token = infix
                                      left = left
                                      right = right }
                            (expr, rightTail)
                        // consume
                        let (left, rest) = binaryParselet()
                        loop left rest
                    // ...
                    else
                        (left, leftTail)
            //
            let left = getPrefix token
            loop left tail
        | [] -> failwith "queue can't be empty" // avoid? (None, [])

[<EntryPoint>]
let main argv =
    let input = ["a";"*";"b";"+";"c"]
    let precedence = 0 
    let (expr, _) = parseExpr input precedence
    (input|> List.fold (+) "" , visit expr)
    ||> printf "%A\n%A\n"
    0 // fin
