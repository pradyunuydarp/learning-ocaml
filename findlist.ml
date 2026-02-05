let rec findlist l a =
  match l with
  | [] -> false
  | hd :: tl ->
      if hd = a then true
      else findlist tl a

let l = [4;5;6]
let l2 = [4.5;5.5;6.6]

let main () =
  let p = (findlist l 5) in (print_endline (string_of_bool p));
  let q = (findlist l2 5.5) in (print_endline (string_of_bool q))

let _ = main()

