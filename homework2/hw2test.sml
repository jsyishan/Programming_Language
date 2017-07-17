(* Homework2 Simple Test *)
use "hw2.sml";
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val t12 = all_except_option ("string", ["strig", "stru", "string"]) = SOME ["strig", "stru"]
val t13 = all_except_option ("string", ["strig", "stru", "string", "s"]) = SOME ["strig", "stru", "s"]
val t14 = all_except_option ("string", ["strig", "stru", "str"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val t22 = get_substitutions1 ([["foo"],["there", "foo"]], "foo") = ["there"]
val t23 = get_substitutions1 ([["foo"],["there", "foo", "zxc"]], "foo") = ["there", "zxc"]
val t24 = get_substitutions1 ([["foo"],["foo", "tt", "yy"]], "foo") = ["tt", "yy"]
val t25 = get_substitutions1 ([["foo"],["foo"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val t32 = get_substitutions2 ([["foo"],["there", "foo"]], "foo") = ["there"]
val t33 = get_substitutions2 ([["foo"],["there", "foo", "zxc"]], "foo") = ["there", "zxc"]
val t34 = get_substitutions2 ([["foo"],["foo", "tt", "yy"]], "foo") = ["tt", "yy"]
val t35 = get_substitutions2 ([["foo"],["foo"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val t42 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Betty", middle="W", last="Smith"}) =
	    [{first="Betty", last="Smith", middle="W"}, {first="Elizabeth", last="Smith", middle="W"}]
val t43 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Hentai", middle="W", last="Smith"}) =
	    [{first="Hentai", last="Smith", middle="W"}]
         
val test5 = card_color (Clubs, Num 2) = Black
val t52 = card_color (Spades, Num 2) = Black
val t53 = card_color (Spades, Num 4) = Black
val t54 = card_color (Spades, Ace) = Black
val t55 = card_color (Hearts, Jack) = Red
val t56 = card_color (Diamonds, Queen) = Red

val test6 = card_value (Clubs, Num 2) = 2
val t62= card_value (Clubs, Num 3) = 3
val t63= card_value (Clubs, Num 6) = 6
val t64= card_value (Hearts, Num 8) = 8
val t65= card_value (Clubs, Ace) = 11
val t66= card_value (Clubs, King) = 10
val t67= card_value (Clubs, Queen) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val t72 = remove_card ([(Hearts, Ace), (Hearts, Num 4), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)], (Hearts, Ace), IllegalMove) = 
[(Hearts, Num 4), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)]
val t73 = remove_card ([(Hearts, Ace), (Hearts, Num 4), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)], (Hearts, Num 4), IllegalMove) =
[(Hearts, Ace), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)]
val t74 = remove_card ([(Hearts, Ace), (Hearts, Num 4), (Hearts, Jack), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)], (Hearts, Jack), IllegalMove) =
[(Hearts, Ace), (Hearts, Num 4), (Hearts, Jack), (Clubs,Ace), (Spades,Num 6)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val t82 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val t83 = all_same_color [(Hearts, Ace), (Spades, Jack)] = false
val t84 = all_same_color [(Hearts, Ace), (Spades,Num 7)] = false
val t85 = all_same_color [(Hearts, Ace), (Clubs,Ace)] = false
val t86 = all_same_color [(Clubs, Ace), (Spades, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val t92 = sum_cards [(Clubs, Num 2),(Clubs, Num 3)] = 5
val t93 = sum_cards [(Clubs, Num 4),(Clubs, Num 3)] = 7
val t94 = sum_cards [(Hearts, Num 7),(Clubs, Num 2)] = 9
val t95 = sum_cards [(Spades, Ace),(Clubs, Num 3)] = 14
val t96 = sum_cards [(Clubs, Jack),(Clubs, Queen)] = 20
val t97 = sum_cards [(Clubs, King),(Diamonds, Ace)] = 21
val t98 = sum_cards [(Clubs, Num 5),(Clubs, Num 4)] = 9
val t99 = sum_cards [(Clubs, Num 2),(Clubs, Num 3),(Clubs, Num 3),(Clubs, Num 7)] = 15

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)], 10) = 4
val t102 = score ([(Hearts, Num 2),(Clubs, Num 4), (Clubs, Ace)], 10) = 21
val t103 = score ([(Hearts, Num 2),(Clubs, Num 4), (Spades, Num 5)], 10) = 3
val t104 = score ([(Hearts, Num 2),(Clubs, Num 4), (Diamonds, Num 5)], 20) = 9
val t105 = score ([(Hearts, Num 2),(Hearts, Num 4), (Diamonds, Ace)], 19) = 1


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val t112 = officiate ([(Hearts, Num 2),(Clubs, Num 4), (Diamonds, Num 6), (Clubs, Ace)],[Draw, Draw, Draw], 15) = 3
val t113 = officiate ([(Hearts, Num 2),(Clubs, Num 4), (Diamonds, Num 6), (Clubs, Ace)],[Draw, Draw, Draw, Draw], 15) = 24
val t114 = officiate ([(Hearts, Num 2),(Hearts, Num 4), (Diamonds, Num 6), (Hearts, Ace)],[Draw, Draw, Draw, Draw], 15) = 12
val t115 = officiate ([(Hearts, Num 2),(Hearts, Num 3), (Diamonds, Num 6), (Hearts, Ace)],[Draw, Draw, Draw], 15) = 2

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
 
