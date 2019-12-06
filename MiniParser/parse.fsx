#!/usr/bin/env fsi
(* in linux/osx make fsi.exe an alias or a link of fsharpi *)
#load "Lexing.fs"
#load "./Parsing.fs"
#load "./Parse.fs"

open MiniParser
open System

let parsed = [
    for x in (Environment.GetCommandLineArgs() |> List.ofArray |> List.skip 3) do
    yield MiniParser.Parse.ParseString x ]

parsed |> printfn "%A\n"