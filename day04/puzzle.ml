open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let parse_line line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  parse ((fun low high -> (low, high)) <$> integer <* char '-' <*> integer)

let check_doubled_digit num =
  (* let rec aux = function
  | [] | [_] -> false
  | a::b::tl -> a = b || aux (b::tl)
  in
  aux (num |> string_of_int |> String.to_list) *)
  let digits = num |> string_of_int |> String.to_list in
  let runs = List.group ~break:(<>) digits in
  List.exists ~f:(fun run -> List.length run = 2) runs

let check_increasing_digits num =
  let rec aux = function
  | [] | [_] -> true
  | a::b::tl -> a <= b && aux (b::tl)
  in
  aux (num |> string_of_int |> String.to_list |> List.map ~f:Char.to_string |> List.map ~f:int_of_string)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let _ =
  let (low, high) = In_channel.read_all "input" |> String.rstrip |> parse_line in
  let candidates = List.init (high-low+1) ~f:(fun i -> i+low) in
  let res =
    candidates
    |> List.filter ~f:check_increasing_digits
    |> List.filter ~f:check_doubled_digit
    |> List.length
  in
  print_integer res
