open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let rec calc_fuel mass =
  let fuel = mass / 3 - 2 in
  if fuel < 1 then
    0
  else
    fuel + calc_fuel fuel

let _ =
  In_channel.read_lines "input"
  |> List.map ~f:int_of_string
  |> List.map ~f:calc_fuel
  |> List.fold ~f:(+) ~init:0
  |> print_integer
