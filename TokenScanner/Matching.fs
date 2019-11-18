module TokenScanner.Matching

open System.Text.RegularExpressions

let valueMatch matcher =
    fun (o: char option) ->
        match o with
        | None -> false
        | some -> matcher some.Value

let AsString f = fun c -> f (c.ToString())

let Not (f: 'a -> bool) c = not <| f (c)

let And a b x = a (x) && b (x)

let Or a b x = a (x) || b (x)

let equals x y = x = y

let toString (c: char) = c.ToString()

let regexMatch pattern (c: char) =
    c
    |> toString
    |> Regex(pattern).IsMatch

let isDigit = regexMatch "\d"

let isLetter = regexMatch "[a-zA-Z]"

let isSpace = regexMatch "\s"
 