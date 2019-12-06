(*
    IMPLEMENT:ME!
*)
[<Literal>]
let N = '\n'
// ...
let input = [ 'a'; 'b'; 'c'; N; 'a'; 'b'; 'c'; N; 'a'; 'b'; 'c' ]
// ...
let countLines =
    let rec lines absolute lineNo colNo =
        function
        | [] -> []
        | c :: tail ->
            let isLine = c = N

            let nextAcc =
                if isLine then 0
                else colNo + 1
            let nextLineNo =
                lineNo + if isLine then 1
                         else 0

            let nextAbsolute = absolute + 1
            (c, absolute, lineNo, colNo) :: lines nextAbsolute nextLineNo nextAcc tail
    lines 0 0 0
// ...
let length = (List.length input)
// ...
input
|> countLines
|> (fun xxx ->
(printf "c\t|#\t|L\t|C\t\n"
 for (a, b, c, d) in xxx do
     printf "%A\t|%A\t|%A\t|%A\t\n" a b c d))
