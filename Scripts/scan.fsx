#load "symbols.fsx"
open Symbols

let scan peek fwd next x = 
    match x with
    |Bang -> 
        let y = peek(1)
        match y with 
        |Eq ->
            fwd(1)
            x + y
        |_ -> x
    |At -> x
    |Hash-> x
    |Dollar-> x 
    |Mod-> x 
    |Caret-> x 
    |Amper-> x 
    |ParenL-> x 
    |ParenR-> x 
    |Underscore -> x
    |Plus-> x 
    |Diere-> x 
    |Minus-> x 
    |Eq-> x 
    |CurlyL-> x 
    |CurlyR-> x 
    |SqrL-> x 
    |SqrR-> x 
    |Colon-> x 
    |Dquote-> x 
    |Scolon-> x 
    |Squote-> x 
    |Lthan-> x 
    |Mthan-> x 
    |Qtn-> x
    |Comma-> x 
    |Dot-> x 
    |Slash-> x
    |_ -> next()