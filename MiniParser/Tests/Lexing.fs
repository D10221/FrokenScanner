module MiniParser.Tests.Lexing

open Xunit
open MiniParser.Lexing.Scanner

let equals (a: 'a) (b: 'a) =
    if a <> b then raise (System.Exception(sprintf "expected %A instead of %A" a b))
    else ()

let allEqual (xxx: 'a list) (yyy: 'a list) =
    if xxx.Length <> yyy.Length then failwithf "%A %A differ in Length" xxx yyy
    for i = 0 to xxx.Length - 1 do
        try
            let x = xxx.[i]
            let y = yyy.[i]
            equals x y
        with e -> failwithf "index: '%i' %s" i e.Message
    ()

let lineNos = List.map (fun (_, _, lineNo, _) -> lineNo)

[<Fact>]
let Test1() =
    Scan [ '\r'; '\n' ]
    |> List.item 0
    |> equals ("\r\n", "newline", 0, 0)

[<Fact>]
let Test2() =
    Scan [ ' '; '\n' ]
    |> List.item 0
    |> equals (" ", "space", 0, 0)
    Scan [ ' '; '\n' ]
    |> List.item 1
    |> equals ("\n", "newline", 0, 1)

[<Fact>]
let Test3() =
    Scan [ '\n'; ' ' ]
    |> allEqual
        [ ("\n", "newline", 0, 0)
          (" ", "space", 1, 0) ]

[<Fact>]
let Test4() =
    Scan [ '\r'; '\r'; '\n'; '\r' ]
    |> allEqual
        [ ("\r", "newline", 0, 0)
          ("\r\n", "newline", 1, 0)
          ("\r", "newline", 2, 0) ]

[<Fact>]
let Test5() =
    let result = Scan [ '1'; '1'; 'a'; 'a' ]
    equals ("11", "number", 0, 0) result.[0]
    equals ("aa", "word", 0, 2) result.[1]

[<Fact>]
let Test6() =
    let result = Scan [ 'a'; 'B'; '1'; '@'; '$'; '#'; '_' ]
    equals ("aB1@$#_", "word", 0, 0) result.[0]

[<Fact>]
let Test7() =
    let result = Scan [ '+'; 'a'; 'B'; '1'; '@'; '$'; '#'; '_'; '!' ]
    equals ("+", "symbol", 0, 0) result.[0]
    equals ("aB1@$#_", "word", 0, 1) result.[1]
    equals ("!", "symbol", 0, 8) result.[2]

[<Fact>]
let Test8() =
    let result = Scan [ '1'; '.'; '1' ]
    equals ("1.1", "number", 0, 0) result.[0]

[<Fact>]
let Test9() =
    let tokens = Scan [ '.'; '1' ]
    let (_, x, _, _) = tokens.Head
    equals x "number"

[<Fact>]
let Test10() =
    let (_, x, _, _) = (Scan [ '.'; 'a' ]).Head
    equals x "symbol"

[<Fact>]
let Test11() =
    let (_, x, _, _) = (Scan [ '.'; 'a' ]).Head
    equals x "symbol"

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