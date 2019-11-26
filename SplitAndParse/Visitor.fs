module SplitAndParse.Visitor

open TokenParser.Types

let rec visitMany (exprs: Expr<'a> list) =

    let rec visit (expr: Expr<'a>) =
        match expr with
        | NameExpression e -> sprintf "%A" (e.token)
        | NumberExpression e -> sprintf "%A" (e.token)
        | BinaryExpression e ->
            let left = visit (e.left)
            let right = visit (e.right)
            sprintf "(%s %A %s)" left (e.token) right

    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (visit expr)
        visited :: visitMany tail
