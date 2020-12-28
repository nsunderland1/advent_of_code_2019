open Core;;
let (=) = Poly.(=)
let (<>) = Poly.(<>)

let _ =
  let source = In_channel.read_all "input" |> String.rstrip in
  let program = Result.ok_or_failwith (Intcode.parse source) in
  let _ = Intcode.run program in
  ()
