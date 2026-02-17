let sumlist l = List.fold_left ( +. ) 0.0 l

let pass x =
        if x > 30.0 then true
        else false

let mean l = sumlist l /. float_of_int (List.length l)

(* Function to find the standard deviation in a list *)
let stddev l =  
        let u = mean l in
        let diffsqr = List.map (fun x -> (x -. u) *. (x -. u)) l in 
        let var = sumlist diffsqr /. float_of_int (List.length l) in 
        if var != 0.0 then sqrt var
        else 0.0

let student_stddev l = 
        let passed = List.filter pass l in
        stddev passed

let main () = 
        let l = [35.0;35.0;35.0] in 
        (*let l = [1.0] in *)
        let s = student_stddev l in
        print_float s;
        print_newline ()

let _ = main()
