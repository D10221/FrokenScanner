module TokenParser.Types

type Expr<'a> = 
    |NameExpression of NameExpression<'a>
    |NumberExpression of NumberExpression<'a>
    |BinaryExpression of BynaryExpression<'a>
and BynaryExpression<'a> = {    
    token: 'a    
    left: Expr<'a>
    right: Expr<'a>
}
and NumberExpression<'a> = {
    token: 'a
}
and NameExpression<'a> = {
    token: 'a
}

// queue -> precedence -> (Expr, tail)
type Parse<'a> = 'a list -> int -> (Expr<'a> * List<'a>)

// parse -> queue -> token - Expr
type Parselet<'a> = Parse<'a> -> 'a list -> 'a -> (Expr<'a> * List<'a>)

/// left -> Parselet
type InfixParselet<'a> = Expr<'a> -> Parselet<'a>

