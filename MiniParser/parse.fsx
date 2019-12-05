#!/usr/bin/env fsi
(* in linux/osx make fsi.exe an alias or a link of fsharpi *)

#load "./MiniParser.fs"

open MiniParser
open System

let parsed = [
    for x in (Environment.GetCommandLineArgs() |> List.ofArray |> List.skip 3) do
    yield MiniParser.parseString x ]

parsed |> printfn "%A\n"