let slice (from: int) (input: seq<'A> ) = 
    let source = Seq.indexed input
    [ for (i, value) in source do if i > from then yield value]

let sliceList (from: int) (input: 'A list) = 
  [for i = from to input.Length - 1 do yield input.[i] ];

let sliceString (from: int) (input: string) = 
  [for i = from to input.Length - 1 do yield input.[i] ];