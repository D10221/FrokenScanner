module MiniParser.Tests.Lexing

open Xunit
open MiniParser.Token
open MiniParser.Lexing.Scanner
open MiniParser.Tests.Common

let lineNos = List.map (fun (_, _, lineNo, _) -> lineNo)

[<Fact>]
let Test1() =
    Scan [ '\r'; '\n' ]
    |> List.item 0
    |> equals ("\r\n", NLINE, 0, 0)

[<Fact>]
let Test2() =
    Scan [ ' '; '\n' ]
    |> List.item 0
    |> equals (" ", SPACE, 0, 0)
    Scan [ ' '; '\n' ]
    |> List.item 1
    |> equals ("\n", NLINE, 0, 1)

[<Fact>]
let Test3() =
    Scan [ '\n'; ' ' ]
    |> allEqual
        [ ("\n", NLINE, 0, 0)
          (" ", SPACE, 1, 0) ]

[<Fact>]
let Test4() =
    Scan [ '\r'; '\r'; '\n'; '\r' ]
    |> allEqual
        [ ("\r", NLINE, 0, 0)
          ("\r\n", NLINE, 1, 0)
          ("\r", NLINE, 2, 0) ]

[<Fact>]
let Test5() =
    let result = Scan [ '1'; '1'; 'a'; 'a' ]
    equals ("11", NUMBER, 0, 0) result.[0]
    equals ("aa", WORD, 0, 2) result.[1]

[<Fact>]
let Test6() =
    let result = Scan [ 'a'; 'B'; '1'; '@'; '$'; '#'; '_' ]
    equals ("aB1@$#_", WORD, 0, 0) result.[0]

[<Fact>]
let Test7() =
    let result = Scan [ '+'; 'a'; 'B'; '1'; '@'; '$'; '#'; '_'; '!' ]
    equals ("+", OP, 0, 0) result.[0]
    equals ("aB1@$#_", WORD, 0, 1) result.[1]
    equals ("!", OP, 0, 8) result.[2]

[<Fact>]
let Test8() =
    let result = Scan [ '1'; '.'; '1' ]
    equals ("1.1", NUMBER, 0, 0) result.[0]

[<Fact>]
let Test9() =
    let tokens = Scan [ '.'; '1' ]
    let (_, x, _, _) = tokens.Head
    equals x NUMBER

[<Fact>]
let Test10() =
    let (_, x, _, _) = (Scan [ '.'; 'a' ]).Head
    equals x OP

[<Fact>]
let Test11() =
    let (_, x, _, _) = (Scan [ '.'; 'a' ]).Head
    equals x OP

[<Fact>]
let LineCounts() =
    Scan [ 'a'; '\n'; 'a'; '\n'; 'a'; '\n' ]
    |> lineNos
    |> allEqual ([ 0; 0; 1; 1; 2; 2 ])    

[<Fact>]
let LineCounts2() =    
    Scan [ '\n'; '\n'; '\n' ]
    |> lineNos
    |> allEqual ([ 0; 1; 2 ])