open System

[<Literal>]
let Bang = "!" 

[<Literal>]
let At = "@" 

[<Literal>]
let Hash = "#" 

[<Literal>]
let Dollar = "$" 

[<Literal>]
let Mod = "%" 

[<Literal>]
let Caret = "^" 


[<Literal>]
let Amper = "&" 

[<Literal>]
let ParenL = "(" 


[<Literal>]
let ParenR = ")" 


[<Literal>]
let Underscore = "_" 

[<Literal>]
let Plus =  "+" 


[<Literal>]
let Diere = "~"

[<Literal>]
let Minus = "-" 
 
[<Literal>]
let Eq = "=" 


[<Literal>] 
let CurlyL = "{" 

 
[<Literal>]
let CurlyR = "}"

[<Literal>]
let SqrL =  "[" 
[<Literal>] 
let SqrR = "]"
 
[<Literal>]
let Colon =  ":"


[<Literal>] 
let Dquote = "\"" 

[<Literal>] 
let Scolon = "" 


[<Literal>] 
let Squote = "\'" 

[<Literal>] 
let Lthan = "<" 


[<Literal>]
let Mthan = ">" 

[<Literal>] 
let Qtn = "?" 

[<Literal>] 
let Comma = "," 

[<Literal>]
let Dot = "." 

[<Literal>] 
let Slash = "/" 

let Symbols = [ 
        Bang;
        At; 
        Hash; 
        Dollar; 
        Mod; 
        Caret; 
        Amper; 
        ParenL; 
        ParenR; 
        Underscore ; 
        Plus; 
        Diere; 
        Minus; 
        Eq; 
        CurlyL; 
        CurlyR; 
        SqrL; 
        SqrR; 
        Colon; 
        Dquote; 
        Scolon; 
        Squote; 
        Lthan; 
        Mthan; 
        Qtn;
        Comma; 
        Dot; 
        Slash
]

let delimiters =
    [ '~'
      '!'
      '@'
      '#'
      '$'
      '%'
      '^'
      '&'
      '*'
      '('
      ')'
      //'_';
      '+'
      '`'
      '-'
      '='
      '['
      ']'
      '{'
      '}'
      '''
      ':'
      ','
      '.'
      '/'
      '<'
      '>'
      '?' ]

let composite =
    [ "^^"; "&&"; "++"; "->"; "<-"; "=>"; "<="; "]>"; "<["; "<{"; "}>"; ":="; ":>"; "<:"; "/*"; "*/"; "<<"; ">>" ]

let toString (c: Char) = c.ToString()

let symbols = composite @ (delimiters |> List.map (toString))