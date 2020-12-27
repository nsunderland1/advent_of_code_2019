open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let add_pos (x1, y1) (x2, y2) = (x1+x2, y1+y2)
let mul_pos n (x, y) = (n*x, n*y)

let parse_line line =
  let open Angstrom in
  let parse parser = Result.ok_or_failwith (parse_string ~consume:All parser line) in
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
  in
  let uppercase = satisfy (function 'A' .. 'Z' -> true | _ -> false) in
  let dir = uppercase >>|
    function
    | 'R' -> (1, 0) | 'L' -> (-1, 0)
    | 'U' -> (0, 1) | 'D' -> (0, -1)
    | _ -> failwith "Invalid direction"
  in
  let path = (fun dir num -> mul_pos num dir) <$> dir <*> integer in
  parse (sep_by1 (char ',') path)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

let manhattan_dist (x1, y1) (x2, y2) =
  abs (x1-x2) + abs(y1-y2)

let get_paths wires =
  let (_, paths) =
    List.fold ~f:(fun (pos, acc) wire ->
      let new_pos = add_pos pos wire in
      (new_pos, (pos, new_pos)::acc)
    ) ~init:((0, 0), []) wires 
  in
  List.rev paths

let calc_steps paths =
  let (_, paths_with_steps) =
    List.fold ~f:(fun (steps, acc) (p_start, p_end) ->
      let next_steps = manhattan_dist p_start p_end in   
      (steps + next_steps, (p_start, p_end, steps)::acc)
    ) ~init:(0, []) paths
  in
  List.rev paths_with_steps

let vert_and_horiz paths =
  List.partition_tf ~f:(fun ((startX, _), (endX, _), _) -> startX = endX) paths

let intersect vert horiz =
  let pairs = List.cartesian_product vert horiz in
  let intersections =
    List.filter_map ~f:(fun ((vStart, vEnd, vS), (hStart, hEnd, hS)) ->
      let vX = fst vStart in
      let vTop = max (snd vStart) (snd vEnd) in
      let vBot = min (snd vStart) (snd vEnd) in
      let hY = snd hStart in
      let hRight = max (fst hStart) (fst hEnd) in
      let hLeft = min (fst hStart) (fst hEnd) in
      if Int.between vX ~low:hLeft ~high:hRight && Int.between hY ~low:vBot ~high:vTop then (
        let pt = (vX, hY) in
          if pt = (0, 0) then 
            None
          else
            Some (pt, vS + (manhattan_dist vStart pt) + hS + (manhattan_dist hStart pt))
      )
      else
        None
    ) pairs
  in
  let best =
    List.min_elt ~compare:(fun (_, aSteps) (_, bSteps) -> Int.compare aSteps bSteps) intersections
  in
  let (_, best_steps) = Option.value_exn best in
  best_steps

let _ =
  let lines = In_channel.read_lines "input" in
  let paths =
    lines
    |> List.map ~f:parse_line
    |> List.map ~f:get_paths
    |> List.map ~f:calc_steps
    |> List.map ~f:vert_and_horiz
  in
  let (aVert, aHoriz) = List.hd_exn paths in
  let (bVert, bHoriz) = List.last_exn paths in
  print_integer (min (intersect aVert bHoriz) (intersect bVert aHoriz))
