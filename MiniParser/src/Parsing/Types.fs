
module MiniParser.Parsing.Types

open MiniParser.Parsing.Expressions
//
type ParseExp<'a> = 'a list -> int -> (Expr<'a> * 'a list)
//
type Parselet<'a> = ParseExp<'a> -> 'a -> 'a list -> (Expr<'a> * 'a list)
//
type InfixParselet<'a> = Expr<'a> -> Parselet<'a>

