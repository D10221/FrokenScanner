module MiniParser.Parse
    
open MiniParser.Parsing.Parser
open MiniParser.Lexing.Scanner

let private scan input =
        [ for (token, tokenType, _lineNo, _colNo) in Scan input do
            if tokenType <> "space" && tokenType <> "newline" then yield token ]

let private precedence = 0

let Parse input = scan input |> ParseExpr precedence

let ParseString (input: string) = input.ToCharArray() |> Array.toList |> scan  |> ParseExpr precedence