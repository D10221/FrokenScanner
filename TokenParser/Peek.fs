module TokenParser.Peek

let peek queue =
    match queue with
    | [] -> None
    | head:: _ -> Some(head)