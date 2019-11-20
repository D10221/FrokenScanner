module TokenScanner.Queue

///<summary>
/// Creeates Peek And Next hold index state
///</summary>
let Queue(input: string) =
    let mutable index = -1
    let ok () = (index + 1) < (input.Length)

    let peek () =
        if ok() then Some(input.[index + 1])
        else None

    let fwd () =
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
