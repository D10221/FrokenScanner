module TokenParser.Types


type Expr = 
    |NameExpression of NameExpression 
    |NumberExpression of NumberExpression 
    |BinaryExpression of BynaryExpression
and BynaryExpression = {    
    token: char    
    left: Expr
    right: Expr
}
and NumberExpression = {
    token: char    
}
and NameExpression = {
    token: char    
}

// queue -> precedence -> (Expr, tail)
type Parse = char list -> int -> (Expr * List<char>)

// parse -> queue -> token - Expr
type Parselet = Parse -> char list -> char -> (Expr * List<char>)

/// left -> Parselet
type PostfixParselet = Expr -> Parselet

