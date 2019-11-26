#!/usr/bin/env fsi

#r "bin/Debug/netcoreapp3.0/TokenSplitter.dll"
#r "bin/Debug/netcoreapp3.0/TokenParser.dll"
#r "bin/Debug/netcoreapp3.0/SplitAndParse.dll"

open SplitAndParse.Visitor

let scan = TokenSplitter.Scan.Scan
let parse = SplitAndParse.Parse.Parser()
"a*b+a*b".ToCharArray()
|> Array.toList
|> scan
|> List.map (fun x-> fst x)
|> parse
|> visitMany
|> printf "%A"
0