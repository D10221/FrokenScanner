module MiniParser.Visiting 

open Parsing.Types
open Parsing.Token

let private reduceOrDefault f def x =
    match x with
    | [] -> def
    | _ -> x |> List.reduce f
//
let rec visit expr =
    match expr with
    | NameExpression e -> sprintf "%A" (tokenValue e.token)
    | NumberExpression e -> sprintf "%A" (tokenValue e.token)
    | GroupExpression e -> sprintf "(%A)" (visit e.right)
    | CallExpression e ->
        let left = visit (e.left)

        let right =
            (e.right)
            |> List.map visit
            |> reduceOrDefault (fun a b -> a + "," + b) ""
        sprintf "(%s%A%s))" left (tokenValue e.token) right
    | BinaryExpression e ->
        let left = visit (e.left)
        let right = visit (e.right)
        sprintf "(%s %A %s)" left (tokenValue e.token) right
    | PrefixExpression e -> sprintf "%A (%A)" (tokenValue e.token) (visit e.right)
    | EmptyExpression _ -> ""
//
let rec visitMany exprs =
    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (visit expr)
        visited :: visitMany tail