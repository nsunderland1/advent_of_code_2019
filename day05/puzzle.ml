open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

module IntMap = Map.Make(Int)

module IntCode = struct
  type pc = Pc of int
  type mode = Position | Immediate
  type omodes = Om of int
  type state = int IntMap.t * pc * omodes 
  type 'a t = St of (state -> ('a * state))

  type binop = Add | Mul
  type opcode =
  | Stop
  | BinOp of binop
  | Input
  | Output

  let return x = St (fun s -> (x, s))

  let (>>=) (St f) g = St (fun s0 ->
      let (a, s1) = f s0 in
      let (St h) = g a in
      let (b, s2) = h s1 in
      (b, s2)
    )
  
  let (let*) = (>>=)

  let next_value = St (fun (program, Pc pc, o) -> (IntMap.find_exn program pc, (program, Pc (pc+1), o)))

  let to_mode = function
  | 0 -> Position
  | 1 -> Immediate
  | _ -> failwith "Invalid parameter mode"

  let next_mode = St (fun (program, pc, Om omodes) -> (to_mode (omodes mod 10), (program, pc, Om (omodes / 10))))
  let update_omodes omodes = St (fun (program, pc, _) -> ((), (program, pc, Om omodes)))
  
  let get_opcode = function
  | 99 -> Stop
  | 1 -> BinOp Add
  | 2 -> BinOp Mul
  | 3 -> Input
  | 4 -> Output
  | other -> failwith (Printf.sprintf "Invalid opcode %d" other)

  let next_opcode =
    let* next = next_value in
    let* _ = update_omodes (next/100) in
    return (get_opcode (next % 100))

  let expose_memory = St (fun (program, pc, o) -> (program, (program, pc, o)))
  let get_pc = St (fun (program, Pc pc, o) -> (pc, (program, Pc pc, o)))
  let read_from_address addr =
    let* memory = expose_memory in
    return (IntMap.find_exn memory addr)
  
  let read_param =
    let* next = next_value in
    let* mode = next_mode in
    match mode with
    | Position -> read_from_address next
    | Immediate -> return next

  let write ~dest ~value = St (fun (program, pc, o) ->
      ((), (IntMap.set program ~key:dest ~data:value, pc, o))
    )
end

let parse_intcode line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let signed_integer = ( * ) <$> (option 1 (char '-' *> return (-1))) <*> integer in
  let values = sep_by1 (char ',') signed_integer in
  parse (values >>| List.mapi ~f:(fun i value -> (i, value)) >>| IntMap.of_alist_exn)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let intcode_binop_fn = function IntCode.Add -> ( + ) | Mul -> ( * )

let step_intcode =
  let open IntCode in
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

let rec run_intcode () =
  let open IntCode in
  let* res = step_intcode in
  match res with `Stop -> return () | `Continue -> run_intcode ()

let _ =
  let input = In_channel.read_all "input" |> String.strip in
  let intcode = parse_intcode input in
  let (IntCode.St s) = run_intcode () in
  let _ = s (intcode, Pc 0, Om 0) in
  ()
