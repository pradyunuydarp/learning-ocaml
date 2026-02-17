type 'a tuple = 
  | Pair of 'a * 'a
  | Triple of 'a * 'a * 'a

let p = Pair(1, 2)
let q = Triple(1, 2, 3)

(* Variables are immutable by default. We bind the result of the match to 'r'. *)
let r = match p with
  | Pair (a, b) -> a + b
  | Triple _ -> 0

 
 (*  To match q: *)
   let r = match q with
     | Triple (x, y, z) -> r + x + y + z
     | Pair _ -> 0 

;;
print_int r;
print_newline ()
