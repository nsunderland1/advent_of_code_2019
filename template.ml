open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let parse_line line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let lowercase =
    take_while1 (function 'a' .. 'z' -> true | _ -> false)
  in
  let uppercase =
    take_while1 (function 'A' .. 'Z' -> true | _ -> false)
  in
  let alpha = lowercase <|> uppercase in
  let is_whitespace = function ' ' | '\n' -> true | _ -> false in
  let whitespace = satisfy is_whitespace in
  parse _

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let _ =
  let lines = In_channel.read_lines "input" in
  let data = List.map ~f:parse_line lines in
  data  
