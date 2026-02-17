(* Filter - List.filter alternate *)
let filter f l = 
        let rec applyfilter f acc = function
                []-> acc
                | h::t -> let newaccl = if f h then (h :: acc) else acc 
                in  (applyfilter f newaccl t) 
        in
       List.rev ( applyfilter f [] l )

(* left_fold - List.fold_left alternate *)

let left_fold f base l = 
        let rec apply_fold f acc = function
                []-> acc
                | h::t -> let newaccl = f acc  h 
                        in apply_fold f newaccl t
        in
        apply_fold f base l

(* right_fold - List.fold_right alternate *)

let right_fold f base l = 
        let rec apply_fold f acc = function
                []-> acc
                | h::t -> let newaccl = f acc  h 
                        in apply_fold f newaccl t
        in
        apply_fold f base (List.rev l)

(* reverse - List.rev alternate *)

let rev l = 
        let rec reverse acc = function
                [] -> acc
                | h::t -> let newaccl = h::acc
                        in reverse newaccl t
        in reverse [] l

(* append - List.append alternate *)

let append l x = 
        let rec appnd acc a = function 
                [] -> ( a :: acc )
                | h::t -> let newaccl = h::acc
                        in appnd newaccl a t 
        in 
        List.rev ( appnd [] x l )


let () = 
        let l = [3;4;5] in let l2 = append l 6 in let l3 = rev l2 in let l4 = filter (fun x -> if x mod 2 = 1 then true else false ) l3 in let v = left_fold ( * ) 1 l4 in print_int(v)
