module TokenScanner.Disposable

open System

type Disposable(dispose: unit -> unit) =    
    interface IDisposable with
        member this.Dispose() = dispose()