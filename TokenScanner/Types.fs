module TokenScanner.Types

type Queue<'a> = bool -> 'a option

type Scanlet<'a> = (Queue<'a>) -> List<Option<char>>