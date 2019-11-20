module TokenScanner.Types
/// <summarY>
/// increase index and return current
/// parameter is true then consume/move/advance else peek 
/// </summary>
type Queue = bool -> char option

type Scanlet =  Queue -> List<Option<char>>

type MatchToken = Option<char> -> bool

type ScanletEntry = Option<char> -> Scanlet
