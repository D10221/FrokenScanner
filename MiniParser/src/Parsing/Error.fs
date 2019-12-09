module MiniParser.Parsing.Error

open MiniParser.Token

let private orNull =
    function
    | None -> null
    | some -> some.Value

type ParseError(message, token: (string * TokenType * int * int) option, inner: Option<exn>) =

    inherit System.Exception(message, (orNull inner))

    member val Token = token

let InfixNotImplemented infix =
    ParseError((sprintf "Infix '%A' is Not Implemented!" (TokenValue infix)), Some(infix), None)

let PrefixNotImplemented token =
    ParseError(sprintf "prefix '%A' is Not Implemented" (TokenValue token), Some(token), None)
