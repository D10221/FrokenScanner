module Tests

open Xunit
open TokenSplitter.Scan

let equals x y = Assert.Equal(x, y)


[<Fact>]
let Test1() =
    let result = Scan [ '\r'; '\n' ]
    equals "\r\n" result.[0]

[<Fact>]
let Test2() =
    let result = Scan [ ' '; '\n' ]
    equals " " result.[0]
    equals "\n" result.[1]

[<Fact>]
let Test3() =
    let result = Scan [ '\n'; ' ' ]
    equals "\n" result.[0]
    equals " " result.[1]

[<Fact>]
let Test4() =
    let result = Scan [ '\r'; '\r'; '\n'; '\r' ]
    equals "\r" result.[0]
    equals "\r\n" result.[1]
    equals "\r" result.[2]

[<Fact>]
let Test5() =
    let result = Scan [ '1'; '1'; 'a'; 'a' ]
    equals "11" result.[0]
    equals "aa" result.[1]

[<Fact>]
let Test6() =
    let result = Scan [ 'a'; 'B'; '1'; '@'; '$'; '#'; '_' ]
    equals "aB1@$#_" result.[0]

[<Fact>]
let Test7() =
    let result = Scan [ '+'; 'a'; 'B'; '1'; '@'; '$'; '#'; '_'; '!' ]
    equals "+" result.[0]
    equals "aB1@$#_" result.[1]
    equals "!" result.[2]
