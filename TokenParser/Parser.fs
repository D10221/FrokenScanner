module TokenParser.Parser

open System.Text.RegularExpressions

type Expr = char * string * obj

let prefixExpr x: Expr = (x, "prefix", [] :> obj)

let binaryExpr x left right: Expr = (x, "postfix", [ left; right] :> obj)

let peek test queue = 
    match queue with 
    |[] -> false
    |head:: _ -> 
        test head
        
let parsePrefix queue =
    match queue with
    | [] -> (None, [])
    | token :: tail ->
        match token with
        | x when x.ToString()
                 |> Regex("\w").IsMatch
                 || x.ToString() |> Regex("\d").IsMatch ->
            let expr = prefixExpr x
            (Some(expr), tail)
        | x -> failwithf "Prefix: %A not implemented" x

let rec parsePostfix parseExpr queue left =
    match queue with
    | [] -> (left, queue)
    | token :: tail ->
        match token with
        | '*'       
        | '+' as x ->
            let (right, rest) = parseExpr tail
            let expr = binaryExpr x left right
            if peek (fun x-> x = '+' || x = '*' ) rest then 
                parsePostfix parseExpr rest expr
            else
            (expr, rest)
        //| x -> failwithf "Postfix: %A not implemented" x
        | _ -> (left, queue)

let rec parseExpr queue =
    let (left, rest) = parsePrefix queue
    let (left, tail) = parsePostfix parseExpr rest left.Value
    (left, tail)

let rec parse input =
    match input with
    | [] -> []
    | _ ->
        let (exp, tail) = parseExpr input
        let ret = exp :: parse tail
        ret