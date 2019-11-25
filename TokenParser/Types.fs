module TokenParser.Types

type Expr = char * string * obj

type Parse = char list -> int -> (Expr * char list)

