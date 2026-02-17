let isodd x = 
        if x mod 2 = 1 then true
        else false

let pick_the_odds  = 
        List.filter isodd 
