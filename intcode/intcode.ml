open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

module IntMap = Map.Make(Int)
type program = int IntMap.t               (* Maps addresses to values *)
type mode = Position | Immediate          (* Parameter mode *)
type state = {
  program: int IntMap.t;
  pc: int;
  omodes: int;
}
type 'a t = St of (state -> ('a * state)) (* State monad *)

type binop = Add | Mul
type jmpcond = IfTrue | IfFalse
type comp = Lt | Eq
type opcode =
| Stop
| BinOp of binop
| Input
| Output
| Jump of jmpcond
| Compare of comp

let return x = St (fun s -> (x, s))

let (>>=) (St f) g = St (fun s0 ->
    let (a, s1) = f s0 in
    let (St h) = g a in
    let (b, s2) = h s1 in
    (b, s2)
  )

let (let*) = (>>=)

let next_value = St (fun ({program; pc; _} as state) ->
  (IntMap.find_exn program pc, {state with pc=pc+1}))

let to_mode = function
| 0 -> Position
| 1 -> Immediate
| _ -> failwith "Invalid parameter mode"

let next_mode = St (fun ({omodes; _} as state) -> (to_mode (omodes mod 10), {state with omodes=omodes/10}))
let update_omodes omodes = St (fun state -> ((), {state with omodes}))

let get_opcode = function
| 99 -> Stop
| 1 -> BinOp Add
| 2 -> BinOp Mul
| 3 -> Input
| 4 -> Output
| 5 -> Jump IfTrue
| 6 -> Jump IfFalse
| 7 -> Compare Lt
| 8 -> Compare Eq
| other -> failwith (Printf.sprintf "Invalid opcode %d" other)

let next_opcode =
  let* next = next_value in
  let* _ = update_omodes (next/100) in
  return (get_opcode (next % 100))

let expose_memory = St (fun state -> (state.program, state))
(* let get_pc = St (fun (program, Pc pc, o) -> (pc, (program, Pc pc, o))) *)
let set_pc loc = St (fun state -> ((), {state with pc=loc}))
let read_from_address addr =
  let* memory = expose_memory in
  return (IntMap.find_exn memory addr)

let read_param =
  let* next = next_value in
  let* mode = next_mode in
  match mode with
  | Position -> read_from_address next
  | Immediate -> return next

let write ~dest ~value = St (fun ({program; _} as state) ->
    ((), {state with program=IntMap.set program ~key:dest ~data:value})
  )

let parse line =
  let open Angstrom in
  let parse parser = parse_string ~consume:All parser line in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let signed_integer = 
    choice [
      char '-' *> integer >>| Int.neg;
      integer;
    ]
  in
  let values = sep_by1 (char ',') signed_integer in
  parse (
    values
    >>| List.mapi ~f:(fun i value -> (i, value))
    >>| IntMap.of_alist_exn
  )   
let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let intcode_binop_fn = function Add -> ( + ) | Mul -> ( * )
let jump_cond_fn = function IfTrue -> ((<>) 0) | IfFalse -> ((=) 0)
let compare_fn = function Lt -> (<) | Eq -> (=)

let step_intcode =
  let* opcode = next_opcode in
  match opcode with
  | Stop -> return `Stop 
  | BinOp op -> (
      let* a = read_param in
      let* b = read_param in
      let* res = next_value in
      let* _ = write ~dest:res ~value:(intcode_binop_fn op a b) in
      return `Continue 
  )
  | Input ->
      let input = Option.value_exn (In_channel.(input_line stdin)) |> Int.of_string in
      let* res = next_value in
      let* _ = write ~dest:res ~value:input in
      return `Continue
  | Output ->
      let* a = read_param in
      print_integer a;
      return `Continue
  | Jump cond ->
      let* test = read_param in
      let* loc = read_param in
      let* _ =
        if jump_cond_fn cond test then
          set_pc loc
        else
          return ()
      in
      return `Continue
  | Compare comp ->
      let* a = read_param in
      let* b = read_param in
      let* dest = next_value in
      let res = Bool.to_int (compare_fn comp a b) in
      let* _ = write ~dest ~value:res in
      return `Continue

let run ?(pc=0) program =
  let rec run_aux () =
    let* res = step_intcode in
    match res with `Stop -> return () | `Continue -> run_aux ()
  in
  let (St s) = run_aux () in
  let (_, {program=final_program; pc=final_pc; _}) = s {program; pc; omodes=0} in
  (final_program, final_pc)
