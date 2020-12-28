open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let print_integer some_int =
  print_endline "";
  Out_channel.output_string stdout (string_of_int some_int)

(* Stolen from http://typeocaml.com/2015/05/05/permutation/ *)
let rm x l = List.filter ~f:((<>) x) l  
let rec permutations = function  
| [] -> []
| x::[] -> [[x]]
| l -> List.fold ~f:(fun acc x -> acc @ List.map ~f:(fun p -> x::p) (permutations (rm x l))) ~init:[] l

let input_from_list = function
| [] -> failwith "Ran out of input"
| hd::tl -> (hd, tl)

let output_int _ n = n

let _ =
  let source = In_channel.read_all "input" |> String.rstrip in
  let program = Result.ok_or_failwith (Intcode.parse source) in
  let settings = List.init 5 ~f:(fun n -> n) in
  let perms = permutations settings in
  let results = List.map ~f:(fun perm ->
    List.fold ~f:(fun input setting ->
      let input = Intcode.{
        istate = [setting;input];
        read = input_from_list;}
      in
      let output = Intcode.{
        ostate = 0;
        write = output_int;}
      in
      let (_, _, res) = Intcode.run ~input ~output program in 
      res
    ) ~init:0 perm
  ) perms
  in
  print_integer (Option.value_exn (List.max_elt ~compare:Int.compare results))
