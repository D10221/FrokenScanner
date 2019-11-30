module MiniParser

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
let Precedence x =
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
        | x when Regex("\d+").IsMatch (x.ToString()) -> 
            NumberExpression { token = token }            
        | x -> failwithf "'%A' Not a Prefix" x

let binaryParselet left parseExpr tail token=
    let (right, rightTail) = parseExpr tail (Precedence token)
    let expr =
        BinaryExpression
            { token = token
              left = left
              right = right }
    (expr, rightTail)

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
                    if test infix  then                        
                        // consume                        
                        thenDo left infix infixTail||> doWhile test thenDo
                    // ...
                    else
                        (left, leftTail)
            //
            let left = getPrefix token
            (left, tail) 
            ||> doWhile (fun token -> precedence <= Precedence token) ( 
                fun left infix infixTail -> 
                    let parselet = (Parselet infix) left
                    parselet parseExpr infixTail infix
            )
        | [] -> failwith "queue can't be empty" // avoid? (None, [])