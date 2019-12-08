module MiniParser.Parsing.Expressions

type Expr<'a> =
    | BinaryExpression of BynaryExpression<'a>
    | CallExpression of CallExpression<'a>
    | GroupExpression of GroupExpression<'a>
    | NameExpression of NameExpression<'a>
    | NumberExpression of NumberExpression<'a>
    | PrefixExpression of PrefixExpression<'a>
//
and BynaryExpression<'a> =
    { token: 'a
      left: Expr<'a>
      right: Expr<'a> }
//
and NumberExpression<'a> =
    { token: 'a }
//
and NameExpression<'a> =
    { token: 'a }
//
and GroupExpression<'a> =
    { token: 'a
      right: Expr<'a> }
//
and CallExpression<'a> =
    { token: 'a
      left: Expr<'a>
      right: Expr<'a> list }
//
and PrefixExpression<'a> =
    { token: 'a
      right: Expr<'a> }
