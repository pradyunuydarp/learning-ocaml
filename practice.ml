(* last element of a list: *)

let rec last l = 
        match l with 
        [] -> None
        | [ x ] -> Some x
        | _::tl -> last tl

let l = [3;4;5]
let () = 
  match last l with
  | Some x -> print_int x
  | None -> print_string "Empty list"
  ; print_newline ()

(* Last two elements *)

let rec last2l = function
        [] -> None
        | [x] -> Some [x]
        | [x;y] -> Some [x;y]
        | _:: tl -> last2l tl
;;
let l2 = [4;5;6]
let () = 
  match last2l l2 with
  | Some [x;y] -> print_string( string_of_int x ^ "," ^ string_of_int y)
  | Some [x] -> print_string(string_of_int x)
  | None -> print_string "Empty list"
  ; print_newline ()


