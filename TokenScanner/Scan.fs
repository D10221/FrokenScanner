module TokenScanner.Scan

open Scanner
open Subject
open Queue
open System

/// <summary>
/// Observable Scanner ...TODO
/// </summary>
let Scan(input: string) =
    {new IObservable<List<Option<char>>> with
        member this.Subscribe subscriber =
            let observer = Subject<List<Option<char>>>()
            let disposable = (observer :> IObservable<List<Option<char>>>).Subscribe(subscriber)
            Queue input |> Scanner observer |> ignore
            { new IDisposable with
                member this.Dispose() = disposable.Dispose() }}

