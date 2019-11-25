module TokenParser.Types

// token -> tokenType -> children
type Expr = char * string * obj

// queue -> precedence -> (Expr, tail)
type Parse = char list -> int -> (Expr * char list)

// parse -> queue -> token - Expr
type Parselet = Parse -> char list -> char -> (Expr * char list)

/// left -> Parselet
type PostfixParselet = Expr -> Parselet

