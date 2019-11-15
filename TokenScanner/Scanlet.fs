module TokenScanner.Scanlet

open Types
/// <summary>
/// Scan and Match subsequent expected options
/// </summary>
let scannlet (next: Next) (expected: List<Option<char>>) =
    [ for expect in expected do
        let incoming = next(false)
        if (incoming = expect) then
            ignore <| next(true)
            yield incoming ]
