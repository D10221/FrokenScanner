module TokenScanner.Types
/// <summarY>
/// increase index and return current
/// parameter is true then consume/move/advance else peek 
/// </summary>
type Queue = bool -> char option

type Subscriber = List<Option<char>> -> unit

type Scanlet = Queue -> List<Option<char>>