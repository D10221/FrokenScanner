module TokenScanner.Types
/// <summary>
/// Peeks, doesn't change index
///</summary>
type Peek = unit -> char option
/// <summarY>
/// increase index and return current
/// </summary>
type Next = unit -> char option
///<summary>
/// Creeates Peek And Next hold index state
///</summary>
type Queue =  string -> (Peek * Next)
