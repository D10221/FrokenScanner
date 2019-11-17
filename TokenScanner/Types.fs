module TokenScanner.Types
/// <summarY>
/// increase index and return current
/// parameter is consume? move?
/// </summary>
type Next = bool -> char option

type Subscriber = List<Option<char>> -> unit

type Scanlet = Next -> List<Option<char>>