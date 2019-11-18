module TokenScanner.Collector

open System

type Collector<'T> (observable: IObservable<'T>) =

    let mutable values: List<'T> = List.Empty
    let observe value = values <- values @ [ value ]
    let subscription = observable.Subscribe(observe)
    let mutable disposed = false

    member this.Value() = values
    member this.Disposed() = disposed

    interface IDisposable with
        member this.Dispose() =
            disposed <- true
            subscription.Dispose()

let collectFrom (observable: IObservable<_>) = new Collector<_>(observable)