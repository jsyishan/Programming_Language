(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, []) = NONE
|   all_except_option (str, x :: xs') =
        case same_string (str, x) of
            true => SOME xs'
        |   false => case all_except_option (str, xs') of
                            NONE => NONE
                        |   SOME y => SOME (x :: y)

fun get_substitutions1 (strLL, s) =
    case strLL of
        [] => []
    |   x :: xs' => 
            case all_except_option (s, x) of
                NONE => get_substitutions1 (xs', s)
            |   SOME ys' => ys' @ get_substitutions1 (xs', s)
                    
fun get_substitutions2 (strLL, s) =
    let fun aux (strLL', strL) =
        case strLL' of
            [] => strL
        |   x :: xs' =>
                case all_except_option (s, x) of
                    NONE => aux (xs', strL)
                |   SOME ys' => aux (xs', strL @ ys')
    in  aux (strLL, [])
    end

fun similar_names (strLL, name : {first : string, middle : string, last : string}) =
    let val {first = f, middle = m, last = l} = name
        fun helper (strL) =
            case strL of
                [] => []
            |   x :: xs' => {first = x, middle = m, last = l} :: helper (xs')
    in name :: helper (get_substitutions1 (strLL, f))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (color, _) =
    case color of
        Spades => Black
    |   Clubs => Black
    |   _ => Red

fun card_value (_, value) =
    case value of
        Ace => 11
    |   Num n => n
    |   _ => 10

fun remove_card (cs : card list, c : card, e : exn) =
    let fun aux ([]) = raise e
        |   aux (x :: xs') = 
            case x=c of
                true => xs'
            |   false => x :: aux (xs')
    in aux (cs)
    end

fun all_same_color cs =
    case cs of
        [] => true
    |   _ :: [] => true
    |   head :: (neck :: rest) => (card_color (head) = card_color (neck) andalso all_same_color (neck :: rest))

fun sum_cards (cardList) =
    let fun aux (cs, sum) =
        case cs of
            [] => sum
        |   x :: xs' => aux (xs', sum + card_value (x))
    in aux (cardList, 0)
    end

fun score (cs, goal) =
    let val sum = sum_cards (cs)
        fun cmp () = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in if all_same_color (cs) then cmp () div 2 else cmp ()
    end

(* fun officiate (cs : card list, ms : move list, goal : int) =
    let fun aux (heldCardList : card list, cardList : card list, moveList : move list) =
        case moveList of
            [] => heldCardList
        |   x :: xs' =>
             case x of
                Draw => if case cardList of 
                               [] => true
                           |   y :: ys' =>
                                case y of
                                    
    in score (aux ([], cs, ms), goal)
    end *)
fun officiate (cs, ms, goal) =
    let fun aux (heldCardList, _, []) = heldCardList
        |   aux (heldCardList, [], _) = heldCardList
        |   aux (heldCardList, cardList, moveList) =
                case moveList of
                    m :: ms' =>
                        case m of
                            Discard c => aux (remove_card (heldCardList, c, IllegalMove), cardList, ms')
                        |   Draw =>
                                case cardList of
                                    c' :: cs' =>
                                        if sum_cards heldCardList > goal 
                                        then heldCardList
                                        else aux (c' :: heldCardList, cs', ms')
    in score (aux ([], cs, ms), goal)
    end

 (* optional 
fun sum_cards_challenge (cardList) =
    let fun aux (cs, sum) =
        case cs of
            [] => sum
        |   x :: xs' => aux (xs', sum + card_value (x))
    in aux (cardList, 0)
    end

fun score_challenge (cs, goal) =  *)
