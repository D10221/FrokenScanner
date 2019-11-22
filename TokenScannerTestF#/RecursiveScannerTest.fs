module RecursiveScannerTest

open Xunit
open TokenScanner.RecursiveScanner

let equals x y = Assert.Equal(x, y)

[<Fact>]
let Test1() =
    let result = Scan [ '\r'; '\n' ]
    equals "\r\n" result.[0]

[<Fact>]
let Test2() =
    let result = Scan [ '\r'; '\r'; '\n'; '\r' ]
    equals "\r" result.[0]
    equals "\r\n" result.[1]
    equals "\r" result.[2]

[<Fact>]
let Test3() =
    let result = Scan [ '1'; 'a' ]
    equals "1" result.[0]
    equals "a" result.[1]

[<Fact>]
let Test4() =
    let result = Scan [ 'a'; 'B' ]
    equals "aB" result.[0]
