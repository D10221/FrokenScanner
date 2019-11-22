module Tests

open Xunit
open TokenSplitter.Scan

let equals (a: 'a) (b: 'a) =
    if a <> b then raise (System.Exception(sprintf "expected %A insteadof %A" a b))
    else ()

[<Fact>]
let Test1() =
    let result = Scan [ '\r'; '\n' ]
    equals ("\r\n", "newline") result.[0]

[<Fact>]
let Test2() =
    let result = Scan [ ' '; '\n' ]
    equals (" ", "space") result.[0]
    equals ("\n", "newline") result.[1]

[<Fact>]
let Test3() =
    let result = Scan [ '\n'; ' ' ]
    equals ("\n", "newline") result.[0]
    equals (" ", "space") result.[1]

[<Fact>]
let Test4() =
    let result = Scan [ '\r'; '\r'; '\n'; '\r' ]
    equals ("\r", "newline") result.[0]
    equals ("\r\n", "newline") result.[1]
    equals ("\r", "newline") result.[2]

[<Fact>]
let Test5() =
    let result = Scan [ '1'; '1'; 'a'; 'a' ]
    equals ("11", "digit") result.[0]
    equals ("aa", "word") result.[1]

[<Fact>]
let Test6() =
    let result = Scan [ 'a'; 'B'; '1'; '@'; '$'; '#'; '_' ]
    equals ("aB1@$#_", "word") result.[0]

[<Fact>]
let Test7() =
    let result = Scan [ '+'; 'a'; 'B'; '1'; '@'; '$'; '#'; '_'; '!' ]
    equals ("+", "symbol") result.[0]
    equals ("aB1@$#_", "word") result.[1]
    equals ("!", "symbol") result.[2]
