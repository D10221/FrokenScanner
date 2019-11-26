module TokenParser.Parselets

open Types

let NameExprParselet: Parselet<'a> =
    fun parser queue token ->
        // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
        let expr = NameExpression { token = token }
        (expr, queue)

let NumberExprParselet: Parselet<'a> =
    fun parser queue token ->
        // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
        let expr = NumberExpression { token = token }
        (expr, queue)

let BinaryParselet: int -> Expr<'a> -> Parselet<'a> =
    fun precedence left parse queue token ->        
        let (right, tail) = parse queue precedence

        let expr =
            BinaryExpression
                { token = token
                  left = left
                  right = right }
        (expr, tail)

