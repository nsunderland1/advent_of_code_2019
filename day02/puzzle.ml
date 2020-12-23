open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

module IntMap = Map.Make(Int)

module IntCode = struct
  type state = int IntMap.t * int
  type 'a t = St of (state -> ('a * state))

  type binop = Add | Mul
  type opcode =
  | Stop
  | BinOp of binop
  let return x = St (fun s -> (x, s))

  let (>>=) (St f) g = St (fun s0 ->
      let (a, s1) = f s0 in
      let (St h) = g a in
      let (b, s2) = h s1 in
      (b, s2)
    )
  
  let (let*) = (>>=)

  let next_value = St (fun (program, pc) -> (IntMap.find_exn program pc, (program, pc+1)))
  
  let get_opcode = function
  | 99 -> Stop
  | 1 -> BinOp Add
  | 2 -> BinOp Mul
  | other -> failwith (Printf.sprintf "Invalid opcode %d" other)

  let next_opcode =
    let* next = next_value in
    return (get_opcode next)

  let expose_memory = St (fun (program, pc) -> (program, (program, pc)))
  let get_pc = St (fun (program, pc) -> (pc, (program, pc)))
  let read_from_address =
    let* next = next_value in
    let* memory = expose_memory in
    return (IntMap.find_exn memory next)

  let write ~dest ~value = St (fun (program, pc) ->
      ((), (IntMap.set program ~key:dest ~data:value, pc))
    )
end

let parse_intcode line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let values = sep_by1 (char ',') integer in
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
      let* a = read_from_address in
      let* b = read_from_address in
      let* res = next_value in
      let* _ = write ~dest:res ~value:(intcode_binop_fn op a b) in
      return `Continue 
  )

let rec run_intcode () =
  let open IntCode in
  let* res = step_intcode in
  (* let* program = expose_memory in
  let* pc = get_pc in
  IntMap.to_alist program
  |> List.map ~f:(fun (i, mem) ->
      if i = pc then
        Printf.sprintf "[[%d]]" mem
      else
        string_of_int mem
    )
  |> String.concat ~sep:","
  |> Out_channel.output_string stdout;
  Out_channel.output_string stdout "\n\n"; *)
  match res with `Stop -> return () | `Continue -> run_intcode ()

let _ =
  let input = In_channel.read_all "input" |> String.strip in
  let intcode = parse_intcode input in
  let (IntCode.St s) = run_intcode () in
  let guesses = List.init ~f:(fun i -> i) 100 in
  let (a, b) = List.find_map_exn ~f:(fun a ->
    List.find_map ~f:(fun b ->
      let guess_intcode = IntMap.set (IntMap.set intcode ~key:1 ~data:a) ~key:2 ~data:b in
      let (_, (program, _)) = s (guess_intcode, 0) in
      if IntMap.find_exn program 0 = 19690720 then Some (a, b) else None
    ) guesses
  ) guesses
  in
  print_integer (100 * a + b)
  (* let (_, (program, _)) = s (intcode, 0) in *)
  (* print_integer (IntMap.find_exn program 0) *)
