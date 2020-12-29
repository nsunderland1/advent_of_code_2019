open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let width = 25
let height = 6

let parse_line line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let digit =
    satisfy (function '0' .. '9' -> true | _ -> false) >>| Char.to_string >>| Int.of_string
  in
  let row = count width digit in
  let layer = count height row in
  parse (many layer)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let count layer digit =
  layer |> List.map ~f:(List.count ~f:((=) digit)) |> List.reduce_exn ~f:(+)

let advance = function
| [] | []::_ -> failwith "We should never get here"
| [el]::tl -> (el, tl)
| (el::hd)::tl -> (el, hd::tl)

let merge_layers layers =
  let rec aux layers =
    if List.for_all ~f:List.is_empty layers then
      []
    else
      let (candidates, rest) = layers |> List.map ~f:advance |> List.unzip in
      let colour = List.find_exn ~f:(fun n -> n = 0 || n = 1) candidates in
      colour::(aux rest)
  in
  layers |> aux |> List.groupi ~break:(fun index _ _ -> index mod width = 0)

let _ =
  let input = In_channel.read_all "input" |> String.rstrip in
  let layers = parse_line input in
  let result = merge_layers layers in
  print_endline "";
  List.iter ~f:(fun row ->
      row |> List.map ~f:Int.to_string |> String.concat |> String.tr ~target:'0' ~replacement:' ' |> Out_channel.print_endline
    ) result
  (* let min = List.min_elt ~compare:(fun a b -> Int.compare (count a 0) (count b 0)) layers in
  let min = Option.value_exn min in
  print_integer ((count min 1) * (count min 2)) *)
