module TokenScanner.Subject

open System

type Subject<'T>() =
    let mutable observers: IObserver<'T> list = []

    let iter f = observers |> Seq.iter f

    let next value = iter (fun observer -> observer.OnNext(value))
    let error ex = iter (fun observer -> observer.OnError(ex))
    let completed() = iter (fun observer -> observer.OnCompleted())
    let remove observer = observers <- List.filter (fun o -> o <> observer) <| observers

    interface IObserver<'T> with
        member this.OnNext value = next value
        member this.OnError ex = error ex
        member this.OnCompleted() = completed()

    interface IObservable<'T> with
        member this.Subscribe(observer: IObserver<'T>) =
            observers <- observers @ [ observer ]
            { new IDisposable with
                member this.Dispose() = remove observer }
