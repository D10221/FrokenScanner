module TokenParser.Peek

let peek queue =
    match queue with
    | [] -> None
    | head:: _ -> Some(head)

let peekTest test queue  =
    match peek queue with
    | None -> false
    | some -> test (some.Value)