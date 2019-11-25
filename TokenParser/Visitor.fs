module TokenParser.Visitor

open Types

let rec visitMany (exprs: Expr<'a> list) =

    let rec visit (expr: Expr<'a>) =
        match expr with
        | NameExpression e -> sprintf "'%s'" (e.token.ToString())
        | NumberExpression e -> sprintf "'%s'" (e.token.ToString())
        | BinaryExpression e ->
            let left = visit (e.left)
            let right = visit (e.right)
            sprintf "(%s %s %s)" left (e.token.ToString()) right

    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (visit expr)
        visited :: visitMany tail
