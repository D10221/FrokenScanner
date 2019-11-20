module ScanletsTests

open Xunit
open TokenScanner.Matching
open TokenScanner.Scanlets

[<Fact>]
let Test1() =
    let isIdentifier =
        Option.isSome
        |> And
        <| (checkValue <| isRegexMatch ("[a-zA-Z_\$#@]"))

    Assert.True(isIdentifier (Some('a')))
    Assert.False(isIdentifier None)
    Assert.True(isIdentifier (Some('_')))
    Assert.False(isIdentifier (Some('1')))
    Assert.True(isIdentifier (Some('$')))

[<Fact>]
let Test2() = Assert.True(isSpace (Some ' '))

[<Fact>]
let Test3() = Assert.True(isSome '\n' <| Some('\n'))
