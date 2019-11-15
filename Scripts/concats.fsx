
let concatStrings (input: seq<string>) = Seq.fold(fun a -> fun b-> a + b) "" input

let concatChars (input: seq<char>) = Seq.fold(fun a -> fun b-> a + b.ToString()) "" input
