module TokenSplitter.Scanlets

let takeNextIf isMatch tail =
    match tail with
    | [] -> ([], tail)
    | next :: nextTail ->
        match next with
        | y when isMatch y -> ([ y ], nextTail)
        | _ -> ([], tail)

let takeNextIfMatch y head tail =
    let concat = List.fold (fun a b -> a + b.ToString()) ""
    let (next, nextTail) = takeNextIf (fun x1 -> x1 = y) tail
    (head :: next |> concat, nextTail)

let rec takeWhile isMatch head tail =
    let append next tail = (head :: next, tail)
    let take = takeWhile isMatch
    match tail with
    | [] -> ([ head ], [])
    | next :: nextTail ->
        match next with
        | x when isMatch x ->
            (next, nextTail)
            ||> take
            ||> append
        | _ -> ([ head ], tail)