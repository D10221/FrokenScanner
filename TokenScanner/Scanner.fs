module TokenScanner.Scanner

open System.Text.RegularExpressions
open Queue
open Types

let isWord (o: Option<char>) = o <> None && Regex("\w+").IsMatch(o.Value.ToString())
let isDigit (o: Option<char>) = o <> None && Regex("\d+").IsMatch(o.Value.ToString())
let isSpace (o: Option<char>) = o <> None && Regex("\s+").IsMatch(o.Value.ToString())

let Result(list: List<Option<char>>) = 
    list
    |> (List.fold (fun acc o -> acc + o.Value.ToString()) "")    
    // |> (List.map (fun o -> o.Value))     

///<summary>
///
///<summary>
let rec Scanner (next: Next) (x: Option<char>) =
    [ match x with
      | Some('=') ->
          match next (false) with
          | Some('=')
          | Some('>') ->
              ignore <| next (true)
              yield Result(([ x;next (true) ]))
          | _ -> yield Result([ x ])
          // ... finally
          ignore <| Scanner next (next (true))
      | Some('+') ->
          match next (false) with
          | Some('=')
          | Some('+') ->
              ignore <| next (true)
              yield Result([ x;next (true) ])
          // ..else
          | _ -> 
            yield Result([ x ])
          // ... finally
          ignore <| Scanner next (next (true))
      | Some('-') ->
          match next (false) with
          | Some('=')
          | Some('-') ->
              yield Result([ x;next (true) ])
          // else
          | _ -> yield Result([ x ])
          // ... finally
          ignore <| Scanner next (next (true))
      | Some(' ') ->
          let mutable collected: option<char> list = []
          while (isSpace (next (false))) do
              collected <- collected @ [ next (true) ]
          yield Result((x :: collected))
          ignore <| Scanner next (next (true))
      | None -> () // end!
      // is NOT None is Something else
      | _ when isSpace (x) ->
          let mutable collected: option<char> list = []
          while (isSpace (next (false))) do
              collected <- collected @ [ next (true) ]
          yield Result(x :: collected)
          ignore <| Scanner next (next (true))
      | _ when isWord (x) ->
          let mutable collected: option<char> list = [ x ]
          while (isWord (next (false)) || isDigit (next (false))) do
              collected <- collected @ [ next (true) ]
          yield Result(collected)
          ignore <| Scanner next (next (true))
      | _ ->
          (printf "found: default: %A \n" x)
          yield Result([ x ])
          ignore <| Scanner next (next (true)) ]

/// <summary>
/// Starts the Scanner
/// </summary>
let Starter(next: Next) = (next, next (true))

/// <summary>
/// Scans ...
/// </summary>
let Scan(input: string) =
    input
    |> (Queue)
    |> Starter
    ||> Scanner
// |> List.map(fun list-> List.fold(fun a-> fun b -> a + b.ToString()) "" list )
