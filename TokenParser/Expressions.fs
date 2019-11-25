module TokenParser.Expressions

open Types

let nameExpr x: Expr = (x, "nameExpr", [] :> obj)

let numberExp x: Expr = (x, "numberExp", [] :> obj)

let binaryExpr x (left: Expr) (right: Expr): Expr = (x, "binaryExpr", [ left; right ] :> obj)
