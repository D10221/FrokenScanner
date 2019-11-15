module TokenScanner.Scanlet
open Types
/// <summary>
/// Scan and Match subsequent expected options
/// </summary>
let scannlet (peek: Peek) (next: Next) (expected: List<Option<char>>) =
    [ for expect in expected do
        let incoming = peek()
        if (incoming = expect) then
            ignore <| next()
            yield incoming ]
