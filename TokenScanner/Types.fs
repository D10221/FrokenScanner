module TokenScanner.Types
/// <summarY>
/// increase index and return current
/// parameter is consume? move?
/// </summary>
type Next = bool -> char option
///<summary>
/// Creeates Peek And Next, holds index state
///</summary>
type Queue =  string -> Next

type Subscriber = List<Option<char>> -> unit
