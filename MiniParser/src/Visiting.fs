module MiniParser.Visiting 

open MiniParser.Token
open Expressions

let private reduceOrDefault f def x =
    match x with
    | [] -> def
    | _ -> x |> List.reduce f
//
let rec Visit expr =
    match expr with
    | NameExpression e -> sprintf "%A" (TokenValue e.Token)
    | NumberExpression e -> sprintf "%A" (TokenValue e.Token)
    | GroupExpression e -> sprintf "(%A)" (Visit e.Right)
    | CallExpression e ->
        let left = Visit (e.Left)

        let right =
            (e.Right)
            |> List.map Visit
            |> reduceOrDefault (fun a b -> a + "," + b) ""
        sprintf "(%s%A%s))" left (TokenValue e.Token) right
    | BinaryExpression e ->
        let left = Visit (e.Left)
        let right = Visit (e.Right)
        sprintf "(%s %A %s)" left (TokenValue e.Token) right
    | PrefixExpression e -> sprintf "%A (%A)" (TokenValue e.Token) (Visit e.Right)    
    | StringExpression e->  sprintf "%A (%A)" (TokenValue e.Token) (e.Right |> List.map TokenValue)
//
let rec VisitMany exprs =
    match exprs with
    | [] -> []
    | (expr :: tail) ->
        let visited = (Visit expr)
        visited :: VisitMany tail