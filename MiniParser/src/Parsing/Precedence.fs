module MiniParser.Parsing.Precedence
// reversed Sql operator precedence
let Precedence x =
    match x with
    | "("
    | "~" -> 8
    | "*"
    | "%"
    | "/" -> 7
    | "+"
    | "-"
    | "^"
    | "|" -> 6
    | ">"
    | "=="
    | "<"
    | ">="
    | "<="
    | "!="
    | "<>"
    | "!<"
    | "!>" -> 5
    | "!" -> 4
    | "&&" -> 3
    | "||" -> 2
    | "=" -> 1
    | _ -> 0