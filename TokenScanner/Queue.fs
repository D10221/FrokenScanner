module TokenScanner.Queue

///<summary>
/// Creeates Peek And Next hold index state
///</summary>
let Queue(input: string): bool -> char option =
    let mutable index = -1
    let ok unit = (index + 1) < (input.Length)

    let peek unit =
        if ok() then Some(input.[index + 1])
        else None

    let fwd unit =
        if ok() then
            index <- index + 1
            Some(input.[index])
        else
            None

    let next consume =
        if consume then fwd()
        else peek()
    // ...
    next
