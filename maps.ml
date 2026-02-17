let rec filter f = function (* non tail recursive implementation*)
                [] -> []
                | h::t -> let newt = (filter f t) in 
                        if f(h) then h::(newt) 
                        else (newt)
(*TODO: learn Continuation passing style*)

let mymap f l = (* Tail recursive implementation *)
        let rec mapiter f accl  = function 
                [] -> accl 
                | h::t -> let newaccl = (f h) :: accl  
                              in mapiter f newaccl t
        in
        List.rev (mapiter f [] l) 

let () = let l  = [4;5;6] 
        in print_int ( List.nth (mymap (fun x -> x*2) l) 2)
