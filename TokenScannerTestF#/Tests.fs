module Tests

open System
open Xunit
open TokenScanner.Matching

[<Fact>]
let ``My test``() =
    let isIdentifier =
        Option.isSome
        |> And
        <| valueMatch isLetter
        |> Or
        <| valueMatch (equals '_' 
                        |> Or <| equals '$' 
                        |> Or <| equals '#' )
    Assert.True(isIdentifier (Some('a')))
    Assert.False(isIdentifier None)
    Assert.True(isIdentifier (Some('_')))
    Assert.False(isIdentifier (Some('1')))
    Assert.True(isIdentifier (Some('$')))
