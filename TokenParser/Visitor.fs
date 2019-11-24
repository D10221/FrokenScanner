module TokenParser.Visitor

open Types

let rec visitMany (exprs: Expr list) =

    let vistiNameExpr token = sprintf "'%c'" token

    let visitBinaryExp token children =
        let visited = visitMany children
        let left = visited |> List.item 0

        let right =
            visited
            |> List.item 1
            |> sprintf "%s"
        sprintf "(%s %c %s)" left token right

    let visit (expr: Expr) =
        match expr with
        | (token, "nameExpr", children) -> vistiNameExpr token
        | (token, "binaryExpr", children) -> visitBinaryExp token (children :?> Expr list)
        | (token, tokenType, children) -> failwithf "%s Not implemented" tokenType

    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (visit expr)
        visited :: visitMany tail
