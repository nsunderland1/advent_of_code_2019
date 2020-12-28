module IntMap : Core.Map.S with type Key.t = int
type program = int IntMap.t

type 'in_ input = {
  istate: 'in_; 
  read: 'in_ -> int * 'in_;
}

type 'out output = {
  ostate: 'out;
  write: 'out -> int -> 'out; 
}

val stdin : unit input 
val stdout : unit output

val parse : string -> (program, string) result 

val run :
  ?pc: int ->
  input:'in_ input ->
  output:'out output ->
  program ->
  program * int
