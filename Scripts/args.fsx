open System

let args =
    fun () ->
        let args = Environment.GetCommandLineArgs()
        [ for arg in [ for i = 2 to args.Length - 1 do
                          yield args.[i] ] do
            yield arg ]