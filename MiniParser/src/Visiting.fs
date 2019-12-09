module MiniParser.Visiting 

open Parsing.Expressions
open Parsing.Types

let private reduceOrDefault f def x =
    match x with
    | [] -> def
    | _ -> x |> List.reduce f
//
let rec Visit expr =
    match expr with
    | NameExpression e -> sprintf "%A" (tokenValue e.Token)
    | NumberExpression e -> sprintf "%A" (tokenValue e.Token)
    | GroupExpression e -> sprintf "(%A)" (Visit e.Right)
    | CallExpression e ->
        let left = Visit (e.Left)

        let right =
            (e.Right)
            |> List.map Visit
            |> reduceOrDefault (fun a b -> a + "," + b) ""
        sprintf "(%s%A%s))" left (tokenValue e.Token) right
    | BinaryExpression e ->
        let left = Visit (e.Left)
        let right = Visit (e.Right)
        sprintf "(%s %A %s)" left (tokenValue e.Token) right
    | PrefixExpression e -> sprintf "%A (%A)" (tokenValue e.Token) (Visit e.Right)    
//
let rec VisitMany exprs =
    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (Visit expr)
        visited :: VisitMany tail