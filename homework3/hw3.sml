(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals strList = List.filter (fn x => Char.isUpper (String.sub (x, 0))) strList

fun longest_string1 strList = foldl (fn (acc, x) => if String.size (acc) > String.size (x) then acc else x) "" strList

fun longest_string2 strList = foldl (fn (acc, x) => if String.size (acc) >= String.size (x) then acc else x) "" strList

fun longest_string_helper f = foldl (fn (acc, x) => if f (String.size (acc), String.size (x)) then acc else x) ""

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f elementList =
	case elementList of
		[] => raise NoAnswer
	|	x :: xs' =>
			case f x of
				SOME v => v
			|	_ => first_answer f xs'

fun all_answers f elementList =
	let fun aux lst acc =
		case lst of
			[] => acc
		|	x :: xs' =>
			case f x of
				NONE => raise NoAnswer
			|	SOME v => (aux xs' (v @ acc))
	in SOME (aux elementList []) handle NoAnswer => NONE
	end

val count_wildcards = g (fn _ => 1) (fn x => 0) 

val count_wild_and_variable_lengths = g (fn x => 1) String.size 

fun count_some_var (str, p) = g (fn x => 0) (fn x => if x = str then 1 else 0) p

fun check_pat p =
	let fun all_strings_of_variables ptn =
		case ptn of
			Variable v => [v]
		|	TupleP t => foldl (fn (p, i) => (all_strings_of_variables p) @ i) [] t
		|	ConstructorP (s1, ptn) => all_strings_of_variables ptn
		|	_ => []

		fun is_unique lst =
			case lst of
				[] => true
			|	x :: xs' => if List.exists (fn y => x = y) xs' then false else is_unique (xs')
	in	is_unique (all_strings_of_variables p)
	end

fun match (v, p) = 
	case p of
		Wildcard => SOME []
	|	Variable x => SOME [(x, v)]
	|	UnitP => (
		case v of
			Unit => SOME []
		|	_ => NONE)
	|	ConstP cp => (
		case v of
			Const c => if cp = c then SOME [] else NONE
		|	_ => NONE)
	|	TupleP ps => (
		case v of
			Tuple ts => (all_answers (match) (ListPair.zipEq (ts, ps)) handle UnequalLengths => NONE)
		|	_ => NONE)
	|	ConstructorP (s1, ptn) => (
		case v of
			Constructor (s2, value) => if s1 = s2 then match (value, ptn) else NONE
		|	_ => NONE)

fun first_match v ptnList = SOME (first_answer (fn x=> match (v, x)) ptnList) handle NoAnswer => NONE