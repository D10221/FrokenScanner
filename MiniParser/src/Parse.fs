module MiniParser.Parse
    
open MiniParser.Parsing.Parser
open MiniParser.Lexing.Scanner
open MiniParser.Lexing.Types

let private scan input =
        [ for (token, tokenType, lineNo, colNo) in Scan input do
            if tokenType <> TokenType.SPACE && tokenType <> TokenType.NLINE then yield (token, tokenType, lineNo, colNo) ]

let private precedence = 0

let Parse input = scan input |> ParseExpr precedence

let ParseString (input: string) = input.ToCharArray() |> Array.toList |> scan  |> ParseExpr precedence