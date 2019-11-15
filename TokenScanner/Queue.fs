namespace TokenScanner

module Queue =

    let Queue (input: string) =
        printf "queue: %s\n" input
        let mutable index = -1

        let peek unit =
            let ok = (index + 1) < (input.Length)
            if ok then Some(input.[index + 1])
            else None

        let next unit =
            let ok = ((index + 1) < (input.Length))
            if ok then
                index <- index + 1
                Some(input.[index])
            else
                None

        (peek, next)
