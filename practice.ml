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
(*let () = 
  match last2l l2 with
  | Some [x;y] -> print_string( string_of_int x ^ "," ^ string_of_int y)
  | Some [x] -> print_string(string_of_int x)
  | None -> print_string "Empty list"
  ; print_newline ()
*)

(* nth element of a list*)

let rec nth n = function
        []-> None
        | h::t -> if n = 0 then Some h
                        else nth (n-1) t

(*let() = (match nth 2 [1;2;3] with 
                        | Some p ->print_int(p)
                        | None -> print_string("Index Out of bounds")
);
print_newline();
print_int(List.nth [5;6;7] 2);
*)
(* Alternatively, you can use the List.nth function*)

(*reversing a list*)

let rev list =
        let rec accum revl l = 
                match l with
                [] -> None
                | h::t -> accum (h::revl) t
        in 
        accum [] list

(*let () = 
        match rev [3;4;5] with
                None-> print_string("empty list")
                | Some l -> print_int(List.nth l 2);
*)


