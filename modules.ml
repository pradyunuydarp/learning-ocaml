module Calendar = struct
       
        type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

        type date = {day: int; month: month; year: int}
        
        type time = {hour: int; minute: int; second: int}
        
        let is_leap_year y = 
                (y mod 4 = 0 && y mod 100 <> 0) || (y mod 400 = 0)

        let days_in_month m y = 
                match m with
                | Jan | Mar | May | Jul | Aug | Oct | Dec -> 31
                | Apr | Jun | Sep | Nov -> 30
                | Feb -> if is_leap_year y then 29 else 28
        
        let next_month m = 
                match m with
                | Jan -> Feb
                | Feb -> Mar
                | Mar -> Apr
                | Apr -> May
                | May -> Jun
                | Jun -> Jul
                | Jul -> Aug
                | Aug -> Sep
                | Sep -> Oct
                | Oct -> Nov
                | Nov -> Dec
                | Dec -> Jan

        let next_date d =
                let days = days_in_month (d.month) d.year in
                if d.day < days then {d with day = d.day + 1}
                else if d.month <> Dec then {day = 1; month = next_month d.month; year = d.year}
                else {day = 1; month=Jan; year = d.year + 1}

        let print_date d =
                let month_str = match d.month with
                        | Jan -> "January"
                        | Feb -> "February"
                        | Mar -> "March"
                        | Apr -> "April"
                        | May -> "May"
                        | Jun -> "June"
                        | Jul -> "July"
                        | Aug -> "August"
                        | Sep -> "September"
                        | Oct -> "October"
                        | Nov -> "November"
                        | Dec -> "December" in
                Printf.sprintf "%d %s %d" d.day month_str d.year
end

let d = 
        {Calendar.day = 17; Calendar.month = Calendar.Feb ; Calendar.year = 2025}
let nd = Calendar.next_date d
let () = print_endline(Calendar.print_date nd)
