type 'a treenode=
        NullNode
        | Node of 'a * 'a treenode * 'a treenode
        (* Leaf = null or Node = val+left+right *)

let rec n t = 
        match t with
        NullNode -> 0
        | Node (v,l,r) -> 1 + n l + n r

let rec search t x =
        match t with
        NullNode -> false
        | Node (v,l,r) ->
                if v = x then true
                else (search l x) || (search r x)

let main () =
        let tree = Node(1,Node(2,NullNode,NullNode),Node(3,NullNode,NullNode)) in print_int(n tree);
        print_newline();
        print_string(string_of_bool(search tree 3));
        print_newline()

let _ = main()

