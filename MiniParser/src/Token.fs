module MiniParser.Token

type TokenType =
        | NONE
        | OP 
        | WORD 
        | NUMBER
        | SPACE 
        | NLINE 

let TokenValue (tokenValue, _, _, _) = tokenValue