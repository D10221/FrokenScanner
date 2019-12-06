module MiniParser.Tests.Lexing

open Xunit
open MiniParser.Lexing.Scanner

let equals (a: 'a) (b: 'a) =
    if a <> b then raise (System.Exception(sprintf "expected %A instead of %A" a b))
    else ()

[<Fact>]
let Test1() =
    let result = Scan [ '\r'; '\n' ]
    equals ("\r\n", "newline", 0) result.[0]

[<Fact>]
let Test2() =    
    let r = (Scan [ ' '; '\n' ])
    equals (" ", "space", 0) r.[0]
    equals ("\n", "newline", 1) (Scan [ ' '; '\n' ]).[1]

[<Fact>]
let Test3() =
    let result = Scan [ '\n'; ' ' ]
    equals ("\n", "newline", 0) result.[0]
    equals (" ", "space", 1) result.[1]

[<Fact>]
let Test4() =
    let result = Scan [ '\r'; '\r'; '\n'; '\r' ]
    equals ("\r", "newline", 0) result.[0]
    equals ("\r\n", "newline", 1) result.[1]
    equals ("\r", "newline", 3) result.[2]

[<Fact>]
let Test5() =
    let result = Scan [ '1'; '1'; 'a'; 'a' ]
    equals ("11", "number", 0) result.[0]
    equals ("aa", "word", 2) result.[1]

[<Fact>]
let Test6() =
    let result = Scan [ 'a'; 'B'; '1'; '@'; '$'; '#'; '_' ]
    equals ("aB1@$#_", "word", 0) result.[0]

[<Fact>]
let Test7() =
    let result = Scan [ '+'; 'a'; 'B'; '1'; '@'; '$'; '#'; '_'; '!' ]
    equals ("+", "symbol", 0) result.[0]
    equals ("aB1@$#_", "word", 1) result.[1]
    equals ("!", "symbol", 8) result.[2]

[<Fact>]
let Test8() =
    let result = Scan [ '1'; '.'; '1' ]
    equals ("1.1", "number", 0) result.[0]

[<Fact>]
let Test9() =
    let tokens = Scan [ '.'; '1' ]
    let (_, x, _) = tokens.Head
    equals x "number"

[<Fact>]
let Test10 () = 
    let (_, x, _) = (Scan [ '.'; 'a' ]).Head
    equals x "symbol"


[<Fact>]
let Test11() =
    let (_, x, _) = (Scan [ '.'; 'a' ]).Head
    equals x "symbol"
