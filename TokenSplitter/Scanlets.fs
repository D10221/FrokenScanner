module TokenSplitter.Scanlets

let takeNextIf isMatch head tail =
    match tail with
    | [] -> ([ head ], [])
    | next :: nextTail ->
        match next with
        | y when isMatch y -> 
            ([ head ; y ], nextTail)
        | _ -> ([ head ], [])

let private append head next tail = (head :: next, tail)

let takeNextIfMatch y  = takeNextIf (fun x1 -> x1 = y) 

let rec takeWhile isMatch head tail =    
    match tail with
    | [] -> ([ head ], [])
    | next :: nextTail ->
        match next with
        | x when isMatch x ->
            (next, nextTail)
            ||> takeWhile isMatch
            ||> append head
        | _ -> ([ head ], tail)