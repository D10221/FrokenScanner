module MiniParser.Parse
    
open MiniParser.Parsing.Parser
open MiniParser.Lexing.Scanner

let private scan input =
        [ for (token, tokenType, lineNo, colNo) in Scan input do
            if tokenType <> "space" && tokenType <> "newline" then yield (token, tokenType, lineNo, colNo) ]

let private precedence = 0

let Parse input = scan input |> ParseExpr precedence

let ParseString (input: string) = input.ToCharArray() |> Array.toList |> scan  |> ParseExpr precedence