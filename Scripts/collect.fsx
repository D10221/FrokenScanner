#load "concats.fsx"
#load "slices.fsx"

open Concats
open Slices

let collect (pattern: string) (input: string) =     
    [ for p in pattern do  
        for  x in input do  
            if p = x then yield x]    

let rec collectAll (patterns: list<string>) (input: string) = 
    let mutable length = 0
    [for pat in patterns do 
        if input.Length - length >= 0  then 
            let sliced = input |> sliceString length  |> concatChars
            let r = collect pat sliced
            length <- length + r.Length
            yield r
        ]
