module Program 

open System

[<EntryPoint>]
let main argv =
    Tests.``It Works With Chars``()
    Tests.``It Works With Strings``()
    0 
