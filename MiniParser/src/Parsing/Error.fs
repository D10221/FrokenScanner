module MiniParser.Parsing.Error

open MiniParser.Lexing.Types

let private orNull =
    function
    | None -> null
    | some -> some.Value

type ParseError(message, token: (string * TokenType * int * int) option, inner: Option<exn>) =

    inherit System.Exception(message, (orNull inner))

    member val Token = token
