module TokenParser.Parser

type Expr = char * string * obj

let prefixExpr x: Expr = (x, "prefix", [] :> obj)

let postfixExpr x left: Expr = (x, "postfix", left :> obj)

let parsePrefix queue =
    match queue with
    | [] -> (None, [])
    | token :: tail ->
        match token with
        | 'a'
        | 'b' as x ->
            let expr = prefixExpr x
            (Some(expr), tail)
        | x -> failwithf "Prefix: %A not implemented" x

let rec parse input =
    
    let rec parseExpr parsePostfix xxx = 
            let (left, rest) = parsePrefix xxx
            let (postfix, tail) = parsePostfix rest left
            (postfix, tail )

    let rec parsePostfix queue left =
            match queue with
            | [] -> (None, [])
            | token :: tail ->
                match token with
                | '+'
                | '-'
                | '=' as x ->
                    // Binary Expr. (left,right)
                    let (right, rest ) = parseExpr parsePostfix tail 
                    let expr = (postfixExpr x (left, right))
                    (Some(expr), rest)
                //| x -> failwithf "Postfix: %A not implemented" x
                | _ -> (left, queue)          

    match input with
    | [] -> []
    | _ ->
        let (exp , tail ) = parseExpr parsePostfix input
        exp :: parse tail
