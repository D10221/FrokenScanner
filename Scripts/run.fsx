#load "concats.fsx"
#load "slices.fsx"
#load "symbols.fsx"
#load "collect.fsx"

open Concats
open Slices
open System
open Collect;

let equal (value: char ) (x: string) = value.ToString() = x

let f = sprintf


Environment.GetCommandLineArgs() |> slice 2 |> concatStrings |> collectAll ["123"; "abc"] |> printf "Done: %A\n"