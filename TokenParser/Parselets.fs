module TokenParser.Parselets

open Types

let nameExprParselet: Parselet =
    fun parser queue token ->
        // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
        let expr = NameExpression { token = token }
        (expr, queue)

let numberExprParselet: Parselet =
    fun parser queue token ->
        // if peekTest (fun x -> x = ' ') queue then failwithf "next can't be %A" ' '
        let expr = NumberExpression { token = token }
        (expr, queue)

let binaryParselet: int -> Expr -> Parselet =
    fun precedence left parse queue token ->        
        let (right, tail) = parse queue precedence

        let expr =
            BinaryExpression
                { token = token
                  left = left
                  right = right }
        (expr, tail)

