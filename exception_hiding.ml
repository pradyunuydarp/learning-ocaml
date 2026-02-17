let div  = (/.)

let idiv = (/)
(*these dont hide div by zero exception. In some cases we'd wanna hide it*)

let save f zero =
        (fun x y -> if y =  zero then zero else f x y)

let div = save div 0.
let idiv = save idiv 0

let () = print_int(idiv 3 0)
