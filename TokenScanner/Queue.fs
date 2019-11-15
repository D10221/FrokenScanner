module TokenScanner.Queue

open Types
///<summary>
/// Creeates Peek And Next hold index state
///</summary>
let Queue: Queue =
    fun (input: string) ->
        printf "queue: %s\n" input
        let mutable index = -1
        // ...
        let ok = fun () -> (index + 1) < (input.Length)
        // ...
        let peek unit =
            if ok() then Some(input.[index + 1])
            else None
        // ...
        let fwd unit =
                if ok() then
                    index <- index + 1
                    Some(input.[index])
                else
                    None            
        // ...
        let next: Next =
            fun consume ->
                if consume then fwd()
                else peek()

        // ...
        next
