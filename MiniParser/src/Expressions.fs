module MiniParser.Expressions

type Expr<'a> =
    | BinaryExpression of BynaryExpression<'a>
    | CallExpression of CallExpression<'a>
    | GroupExpression of GroupExpression<'a>
    | NameExpression of NameExpression<'a>
    | NumberExpression of NumberExpression<'a>
    | PrefixExpression of PrefixExpression<'a>
    | StringExpression of StringExpression<'a>
//
and BynaryExpression<'a> =
    { Token: 'a
      Left: Expr<'a>
      Right: Expr<'a> }
//
and NumberExpression<'a> =
    { Token: 'a }
//
and NameExpression<'a> =
    { Token: 'a }
//
and GroupExpression<'a> =
    { Token: 'a
      Right: Expr<'a> }
//
and StringExpression<'a> =
    { Token: 'a
      Right: 'a list }      
//
and CallExpression<'a> =
    { Token: 'a
      Left: Expr<'a>
      Right: Expr<'a> list }
//
and PrefixExpression<'a> =
    { Token: 'a
      Right: Expr<'a> }
