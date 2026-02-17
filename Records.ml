type name = {firstname: string; lastname: string}

let fullname name = name.firstname ^ " " ^ name.lastname

let offspring parent childname = {parent with firstname = childname}

let () = let raj = {firstname = "Raj"; lastname = "Kapoor"} in
         let rishi = offspring raj "Rishi" in
         print_endline (fullname rishi)
