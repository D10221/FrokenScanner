module MiniParser.Tests.Common

open System.Text.RegularExpressions
open MiniParser.Token

let equals a b =
    if a <> b then failwithf "Expected %A found %A" a b

let allEqual (xxx: 'a list) (yyy: 'a list) =
    if xxx.Length <> yyy.Length then failwithf "%A %A differ in Length" xxx yyy
    for i = 0 to xxx.Length - 1 do
        try
            let x = xxx.[i]
            let y = yyy.[i]
            equals x y
        with e -> failwithf "index: '%i' %s" i e.Message
    ()
    
let clean input = Regex.Replace(input, "\"", "")

let toToken x = (x, TokenType.NONE, 0, 0)

let toTokenOf tokenType x = (x, tokenType, 0, 0)
