module TokenScanner.Scanner

open Types
open System

///<summary>
/// it: could return Observable ?
/// it: can't yield
///<summary>
let rec Scanner (observer: IObserver<List<Option<char>>>)  (queue: Queue) =
    // :recursor
    let takeNext scanlet =
        observer.OnNext <| scanlet queue
        Scanner observer queue

    let nextToken = queue (true) // advance ad return current
    
    try
        match Scanlets.find nextToken with
        | None -> observer.OnCompleted()
        | scanlet -> Option.get scanlet |> takeNext
    with ex -> observer.OnError ex
 