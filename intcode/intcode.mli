module IntMap : Core.Map.S with type Key.t = int
type program = int IntMap.t

val parse : string -> (program, string) result 

val run : ?pc:int -> program -> program * int
