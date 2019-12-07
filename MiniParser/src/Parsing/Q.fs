module MiniParser.Parsing.Q
//
let expect x =
    function
    | None -> failwithf "Expected %A" x
    | some when some.Value = x -> x //do nothing
    | y -> failwithf "Expected %A but found %A" x y
//
let peek f aList =
    match aList with
    | [] -> false
    | h :: _ -> f (h)
//
let rec collect isTerminal queue =
    match queue with
    | [] -> ([], [])
    | head :: tail ->
        if (isTerminal head) then
            ([], tail)
        else
            if List.isEmpty tail && not (isTerminal head) then failwithf "Expected terminal but found %A" head
            let (x, rest) = tail |> collect isTerminal
            (head :: x, rest)
//
let rec splitBy isSeparator =
    function
    | [] -> []
    | q ->
        let index = q |> List.tryFindIndex isSeparator
        match index with
        | None -> [ q ]
        | some ->
            let (p, p1) = (q |> List.splitAt (some.Value + 1))
            match p with
            | [] -> [ q ]
            | _ -> (p |> List.filter (isSeparator >> not)) :: splitBy isSeparator (p1)