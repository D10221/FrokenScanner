/// <summary>
/// Configuration
/// </summary>
module TokenScanner.Scanlets

open System.Text.RegularExpressions
open Scanlet
open Matching
open Types

let isDigit (o: Option<char>) = o <> None && Regex("\d").IsMatch(o.Value.ToString())
let isWord (o: Option<char>) =
    o <> None && not (isDigit o) && // not checking for digit results in order sensitive match
    Regex("\w").IsMatch(o.Value.ToString())
let isWordOrDigit (peek: Option<char>) = isWord (peek) || isDigit (peek)
let isDot o = o <> None && o.Value = '.'
let isDigitOrDot o = o <> None && isDigit (o) || isDot (o)
let isSpace (o: Option<char>) =
    o <> None && o.Value <> '\n' && o.Value <> '\r' && Regex("\s").IsMatch(o.Value.ToString())

let xxx: ScanletEntry = valueMatch <| equals '=', TakeTwo('=', '>') |> StartWith
let xx1: ScanletEntry = valueMatch <| equals '+', TakeTwo('=', '+') |> StartWith
let xx2: ScanletEntry = valueMatch <| equals '-', TakeTwo('=', '-') |> StartWith
let xx3: ScanletEntry = valueMatch <| equals '\n', Take
let xx4: ScanletEntry = valueMatch <| equals '\r', TakeOne '\n' |> StartWith
let xx5: ScanletEntry = isSpace, TakeWhile isSpace |> StartWith
let xx6: ScanletEntry = isWord, TakeWhile isWordOrDigit |> StartWith
let xx7: ScanletEntry = isDigit, TakeWhile isDigitOrDot |> StartWith
/// <summary>
/// Configuration
/// </summary>
let scanlets: List<ScanletEntry> = [ xxx; xx1; xx2; xx3; xx4; xx5; xx6; xx7 ]
// TODO a.a ? No, is parser's job
// TODO .a  ? No, is parser's job
// TODO .1 ?
// TODO 1D ?
// TODO 1x0 ?