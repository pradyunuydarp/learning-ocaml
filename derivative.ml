let h = 0.0001

let d f x = (f(x+.h) -. f(x)) /. h

let d2 f x = (d f (x+.h) -.  d f x) /. h

(* the above way doesnt support direct chaining *)

let der f = 
        (fun x -> ((f (x +. h)) -. (f x)) /. h)

let dder f = der(der f)

let () = print_float (dder (fun x -> 2. *. x) 1.)
