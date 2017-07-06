fun is_older (d1 : (int * int * int), d2 : (int * int * int)) = (* d means date *)
    if #1 d1 < #1 d2
    then true
    else
        if #1 d1 = #1 d2
        then 
            if #2 d1 < #2 d2
            then true
            else
                if #2 d1 = #2 d2
                then
                    if #3 d1 < #3 d2 then true else false
                else false
        else false

fun number_in_month (dateList : (int * int * int) list, month : int) =
    if null dateList
    then 0
    else
         if  #2 (hd dateList) = month
         then number_in_month (tl dateList, month) + 1
         else number_in_month (tl dateList, month)

fun number_in_months (dateList : (int * int * int) list, monthList : int list) =
    if null monthList
    then 0
    else number_in_month (dateList, hd monthList) + number_in_months (dateList, tl monthList)

fun dates_in_month (dateList : (int * int * int) list, month : int) =
    if null dateList
    then []
    else
        if #2 (hd dateList) = month
        then (hd dateList) :: dates_in_month (tl dateList, month)
        else dates_in_month (tl dateList, month)

fun dates_in_months (dateList : (int * int * int) list, monthList : int list) =
    let fun append (xs : (int * int * int) list, ys : (int * int * int) list) =
        if null xs
        then ys
        else (hd xs) :: append (tl xs, ys)
    in
        if null monthList
        then []
        else append ((dates_in_month (dateList, hd monthList)), dates_in_months (dateList, tl monthList))
    end

fun get_nth (strList : string list, n : int) =
    if n = 1
    then hd strList
    else get_nth (tl strList, n - 1)

fun date_to_string (date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

fun number_before_reaching_sum (sum : int, numList : int list) =
    if null numList
    then 0
    else
        if sum - hd numList > 0
        then number_before_reaching_sum (sum - hd numList, tl numList) + 1
        else number_before_reaching_sum (sum - hd numList, tl numList)

fun what_month (day : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum (day, months) + 1
    end

fun month_range (dayRange : (int * int)) =
    if #1 dayRange > #2 dayRange
    then []
    else
        let fun helper (i : int) =
            if i = #2 dayRange + 1
            then []
            else what_month (i)::helper (i + 1)
        in helper (#1 dayRange)
        end

fun oldest (dateList : (int * int * int) list) =
    if null dateList
    then NONE
    else    
        let val old = oldest (tl dateList)
        in
            if isSome old andalso is_older (valOf(old), hd dateList)
            then old
            else SOME (hd dateList)
        end

(* optional *)
(* a helper function for both the function "number_in_months_challenge" and "dates_in_months_challenge" 
in order to remove the repeated elements insides the int list containing months *)
fun remove_repeated_elements(xs: int list) = 
    let
        fun check(xs: int list, item: int) =
            if null xs
            then false
            else if hd xs = item
            then true
            else check (tl xs, item)
        fun go_through_list(xs: int list) =
            if null xs
            then []
            else if check(tl xs, hd xs)
                then go_through_list(tl xs)
                else hd xs :: go_through_list(tl xs)
    in
        go_through_list(xs)
    end

fun number_in_months_challenge (dateList : (int * int * int) list, monthList : int list) =
    let fun helper (mlist : int list) = 
        if null mlist
        then 0
        else number_in_month (dateList, hd mlist) + helper (tl mlist)
    in helper (remove_repeated_elements (monthList))
    end

fun dates_in_months_challenge (dateList : (int * int * int) list, monthList : int list) =
    let fun helper (mlist : int list) =
        let fun append (xs : (int * int * int) list, ys : (int * int * int) list) =
            if null xs
            then ys
            else (hd xs) :: append (tl xs, ys)
        in
            if null mlist
            then []
            else append ((dates_in_month (dateList, hd mlist)), helper (tl mlist))
        end
    in helper (remove_repeated_elements (monthList))
    end

fun reasonable_date (date : (int * int * int)) =
    if #1 date <= 0
    then false
    else
        let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            if #2 date = 2
            then
                if (((#1 date) mod 400 = 0 orelse (#1 date) mod 4 = 0) andalso ((#1 date) mod 100 <> 0))
                then if (#3 date >= 1 andalso #3 date <= 29) then true else false
                else if (#3 date >= 1 andalso #3 date <= 28) then true else false
            else
                if #2 date >= 1 andalso #2 date <= 12
                then
                    let fun mon (n : int) = (* for calculating the last day of the n^th month 
                    e.g. 3 means March so that comes the result 31(the third element within list "month"*)
                        let fun helper (nn : int, ss : int list) = 
                            if nn = 1
                            then hd ss
                            else helper (nn - 1, tl ss)
                        in
                            helper (n, months)
                        end
                    in if (#3 date >= 1 andalso #3 date <= mon (#2 date)) then true else false
                    end
                else false
        end