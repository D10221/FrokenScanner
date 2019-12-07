module MiniParser.Visiting 

open Parsing.Types
open Parsing.Token

let private reduceOrDefault f def x =
    match x with
    | [] -> def
    | _ -> x |> List.reduce f
//
let rec Visit expr =
    match expr with
    | NameExpression e -> sprintf "%A" (tokenValue e.token)
    | NumberExpression e -> sprintf "%A" (tokenValue e.token)
    | GroupExpression e -> sprintf "(%A)" (Visit e.right)
    | CallExpression e ->
        let left = Visit (e.left)

        let right =
            (e.right)
            |> List.map Visit
            |> reduceOrDefault (fun a b -> a + "," + b) ""
        sprintf "(%s%A%s))" left (tokenValue e.token) right
    | BinaryExpression e ->
        let left = Visit (e.left)
        let right = Visit (e.right)
        sprintf "(%s %A %s)" left (tokenValue e.token) right
    | PrefixExpression e -> sprintf "%A (%A)" (tokenValue e.token) (Visit e.right)
    | EmptyExpression _ -> ""
//
let rec VisitMany exprs =
    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (Visit expr)
        visited :: VisitMany tail