fun is_older (d1 : (int * int * int), d2 : (int * int * int)) =
    if #1 d1 > #1 d2
    then false
    else
        if #1 d1 = #1 d2 andalso #2 d1 > #2 d2
        then false
        else
            if #2 d1 = #2 d2 andalso #3 d1 > #3 d2
            then false
            else 
                if #3 d1 = #3 d2
                then false
                else true

fun number_in_month (dlist : (int * int * int) list, m : int) =
    if null dlist
    then 0
    else
         if  #2 (hd dlist) = m
         then number_in_month (tl dlist, m) + 1
         else number_in_month (tl dlist, m)

fun number_in_months (dlist : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (dlist, hd ms) + number_in_months (dlist, tl ms)


fun dates_in_month (dlist : (int * int * int) list, m : int) =
    if null dlist
    then []
    else
        if #2 (hd dlist) = m
        then (hd dlist)::dates_in_month (tl dlist, m)
        else dates_in_month (tl dlist, m)

fun dates_in_months (dlist : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else hd (dates_in_month (dlist, hd ms))::dates_in_months (dlist, tl ms)


fun get_nth (slist : string list, n : int) =
    if n = 1
    then hd slist
    else get_nth (tl slist, n - 1)

fun date_to_string (d : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth (months, #2 d) ^ " " ^ Int.toString (#3 d) ^ ", " ^ Int.toString (#1 d)
    end

fun number_before_reaching_sum (sum : int, ilist : int list) =
    if null ilist
    then 0
    else
        if sum - hd ilist > 0
        then number_before_reaching_sum (sum - hd ilist, tl ilist) + 1
        else number_before_reaching_sum (sum - hd ilist, tl ilist)

fun what_month (day : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day, months) + 1
    end

fun month_range (drange : (int * int)) =
    if #1 drange > #2 drange
    then []
    else
        let fun helper (i : int) =
            if i = #2 drange + 1
            then []
            else what_month (i)::helper (i + 1)
        in helper (#1 drange)
        end

fun oldest (dlist : (int * int * int) list) =
    if null dlist
    then NONE
    else
        let val old = oldest (tl dlist)
        in
            if isSome old andalso is_older (valOf(old), hd dlist)
            then old
            else SOME (hd dlist)
        end

fun number_in_months_challenge (dlist : (int * int * int) list, ms : int list) =
    let fun helper (ms2 : int list) = 
        if null ms2
        then 0
        else number_in_month (dlist, hd ms2) + helper (tl ms2)
    in helper (ms)
    end

fun dates_in_months_challenge (dlist : (int * int * int) list, ms : int list) =
    let fun helper (ms2 : int list) =
        if null ms2
        then []
        else hd (dates_in_month (dlist, hd ms2))::helper (tl ms2)
    in helper (ms)
    end

fun reasonable_date (d : (int * int * int)) =
    if #1 d <= 0
    then false
    else
        let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            if #2 d = 2
            then
                if (((#1 d) mod 400 = 0 orelse (#1 d) mod 4 = 0) andalso ((#1 d) mod 100 <> 0))
                then if (#3 d > 0 andalso #3 d <= 29) then true else false
                else if (#3 d > 0 andalso #3 d <= 28) then true else false
            else
                if #2 d >= 1 andalso #2 d <= 12
                then
                    let fun mon (n : int) =
                        let fun helper (nn : int, ss : int list) = 
                            if nn = 1
                            then hd ss
                            else helper (nn - 1, tl ss)
                        in
                            helper (n, months)]
                        end
                    in if (#3 d > 0 andalso #3 d <= mon (#2 d)) then true else false
                    end
                else false
        end