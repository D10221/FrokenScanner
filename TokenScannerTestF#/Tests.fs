module Tests

open System
open Xunit
open TokenScanner.Matching
open TokenScanner.Subject
open TokenScanner.Collector

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

[<Fact>]
let ``CollectorTest`` () = 
    let subj = new Subject<_>()
    let obs = subj :> IObservable<_>
    let mutable values = List.Empty

    let next x = 
        (subj :> IObserver<_>).OnNext(x)
    
    let get ()= 
        use collector = collectFrom(obs)
        next('a')
        collector.Value()

    values <- get()

    Assert.NotNull(values.[0])