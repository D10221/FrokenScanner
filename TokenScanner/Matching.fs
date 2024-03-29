module TokenScanner.Matching

open System.Text.RegularExpressions

let checkValue check (o: char option) =    
        match o with
        | None -> false
        | some -> check some.Value

let Not f c = not <| f (c)

let And a b x = a (x) && b (x)

let Or a b x = a (x) || b (x)

let toString (c: char) = c.ToString()

let isRegexMatch pattern (c: char) =
    c
    |> toString
    |> Regex(pattern).IsMatch
