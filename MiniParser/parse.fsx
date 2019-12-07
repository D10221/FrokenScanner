#!/usr/bin/env fsi
(* in linux/osx make fsi.exe an alias or a link of fsharpi *)
#r "bin/Debug/netcoreapp3.0/MiniParser.dll"
open MiniParser
open System

let parsed = [
    for x in (Environment.GetCommandLineArgs() |> List.ofArray |> List.skip 3) do
    yield Parse.ParseString x ]

parsed |> printfn "%A\n"