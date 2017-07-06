use "hw1.sml";


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)


val t1 = is_older ((1,1,1), (1,1,1)) = false
val t2 = is_older ((1,1,1), (1,1,2)) = true
val t3 = is_older ((1,1,2), (1,1,1)) = false
val t4 = is_older ((1,2,1), (1,2,1)) = false
val t5 = is_older ((1,2,1), (1,2,2)) = true
val t6 = is_older ((1,3,6), (1,5,9)) = true
val t7 = is_older ((23,41,1), (5,244,3242642)) = false
val t8 = is_older ((13,16,99), (134,34,1)) = true
val t9 = is_older ((11,22,33), (12,1,1)) = true
val t10 = is_older ((1,2,3), (5,2,3)) = true
val t11 = is_older ((5,2,3), (5,2,2)) = false

val t12 = oldest ([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]) = SOME (5,2,2)

val rt1 = reasonable_date (~1, 2, 3) = false
val rt2 = reasonable_date (2012, 5, 2) = true
val rt3 = reasonable_date (2017, 7, 6) = true
val rt4 = reasonable_date (2011, 2, 29) = false
val rt5 = reasonable_date (2012, 2, 29) = true
val rt6 = reasonable_date (2013, 0, 11) = false
val rt7 = reasonable_date (2011, 2, 0) = false
val rt8 = reasonable_date (0, 0, 0) = false
val rt9 = reasonable_date (1, ~5, 3) = false
val rt10 = reasonable_date (2, 5, ~99) = false
val rt11 = reasonable_date (1, 2,213123) = false

val t13 = dates_in_month ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], 1) = []
val t14 = dates_in_months ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [1]) = []
val t14 = dates_in_months ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [2]) = [(1,2,25),(3,2,28),(1,2,27),(1,2,25)]
val t15 = dates_in_months ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [2,5]) = [(1,2,25), (3,2,28), (1,2,27), (1,2,25), (3,5,26)]

val t16 = number_in_months_challenge ([(1,2,25)], [2,2])


val t20 = dates_in_months_challenge ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [1,1,1,1,1])
val t21 = dates_in_months_challenge ([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [2,2,2])