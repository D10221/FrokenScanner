module TokenSplitter.Test

open System.Text.RegularExpressions

let symbols = "`-=~!@#$%^&*()_+[]\\{}|;':\",./<>?"

let any list x =
    list
    |> List.tryFind (fun c -> c = x)
    <> None

let isSpace = [ ' '; '\t' ] |> any

let isSymbol =
    symbols.ToCharArray()
    |> Array.toList
    |> any

let isRegexMatch pattern = Regex(pattern).IsMatch

let isWord c = c.ToString() |> isRegexMatch "[a-zA-Z_$#@]"
let isDigit c = c.ToString() |> isRegexMatch "\d"
let isWordOrDigit x = isWord x || isDigit x