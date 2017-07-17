fun is_older(first_date : (int * int * int), second_date : (int * int * int)) =
  if #1 first_date <> #1 second_date
  then #1 first_date < #1 second_date
  else if #2 first_date <> #2 second_date
  then #2 first_date < #2 second_date
  else #3 first_date < #3 second_date

fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if #2(hd dates) = month
  then 1 + number_in_month(tl dates, month)
  else 0 + number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if #2(hd dates) = month
  then hd dates::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(items : string list, nth : int) =
  if nth = 1
  then hd items
  else get_nth(tl items, nth - 1)

fun date_to_string(date : (int * int * int)) =
  let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum(sum : int, numbers : int list) =
  let val new_sum = sum - (hd numbers)
  in
    if new_sum <= 0
    then 0
    else 1 + number_before_reaching_sum(new_sum, tl numbers)
  end

fun what_month(day : int) =
  let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_months) + 1
  end

fun month_range(first_day : int, second_day : int) =
  if first_day > second_day
  then []
  else what_month(first_day)::month_range(first_day + 1, second_day)

fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else
  let val tl_oldest = oldest(tl dates)
  in if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
    then tl_oldest
    else SOME (hd dates)
  end

fun in_list(number : int, num_list : int list) =
  if null num_list
  then false
  else if ((hd num_list) = number)
  then true
  else in_list(number, tl num_list)

fun unique_list(num_list : int list) =
  if null num_list
  then []
  else if in_list(hd num_list, tl num_list)
  then unique_list(tl num_list)
  else hd num_list::unique_list(tl num_list)

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
  let val unique_months = unique_list(months)
  in
    number_in_months(dates, unique_months)
  end

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
  let val unique_months = unique_list(months)
  in
    dates_in_months(dates, unique_months)
  end

fun is_leap_year(year : int) =
  (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)

fun get_nth_alpha(items : 'a list, nth : int) =
  if nth = 1
  then hd items
  else get_nth_alpha(tl items, nth - 1)

fun reasonable_date(date : (int * int * int)) =
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    if (year < 1 orelse month < 1 orelse day < 1)
    then false
    else if (month > 12)
    then false
    else if (day > get_nth_alpha(days_in_months, month) andalso (not(is_leap_year(year)) orelse month <> 2 orelse day <> 29))
    then false
    else true
  end