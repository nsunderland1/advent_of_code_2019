open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let parse_line line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let body =
    take_while1 (function 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)
  in
  let link = (fun inner outer -> (inner, outer)) <$> body <* char ')' <*> body in
  parse link
let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

module StringMap = Map.Make(String)

(* let count_orbits graph =
  let rec num_bodies_and_orbits body =
    match StringMap.find graph body with
    | None -> (1, 0)
    | Some children ->
        let counts = List.map ~f:num_bodies_and_orbits children in
        let bodies, orbits = List.unzip counts in
        let total_orbits = List.reduce_exn ~f:(+) orbits in
        let total_bodies = List.reduce_exn ~f:(+) bodies in
        (1 + total_bodies, total_orbits + total_bodies)
  in
  let (_, orbits) = num_bodies_and_orbits "COM" in
  orbits *)

let path_to graph target =
  let rec path_to_aux body =
    let (let*) o f = Option.bind ~f o in
    if body = target then
      Some []
    else
      let* children = StringMap.find graph body in
      let* subpath = List.find_map ~f:path_to_aux children in
      Some (body::subpath)
  in
  Option.value_exn (path_to_aux "COM")

let rec drop_common_prefixes = function
| [], [] -> [], []
| [], _ | _, [] -> failwith "Invalid"
| a::atl, b::btl ->
    if a = b then
      drop_common_prefixes (atl, btl)
    else
      (a::atl, b::btl)

let _ =
  let lines = In_channel.read_lines "input" in
  let data = List.map ~f:parse_line lines in
  let graph = StringMap.of_alist_multi data in
  let you = path_to graph "YOU" in
  let san = path_to graph "SAN" in
  let (aRest, bRest) = drop_common_prefixes (you, san) in
  print_integer (List.length aRest + List.length bRest)
  (* print_integer (count_orbits graph) *)
