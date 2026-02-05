type mylist = Empty
        | NonEmpty of int * mylist

let rec sumlist l = function
        Empty-> 0
        | NonEmpty(h,t) -> h + (sumlist t)


let l1 = NonEmpty(1,NonEmpty(2,Empty))

let main () = 
        let s = (sumlist l1)  in (print_int s)

let _ = main ()
