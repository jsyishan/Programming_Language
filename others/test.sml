(* signature DIGIT1 = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end

signature DIGIT2 = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
end

signature DIGIT3 = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val test : digit -> unit
end

signature DIGIT4 = 
sig
type digit
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end

signature DIGIT5 = 
sig
type digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end

structure Digit :> DIGIT5 =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end *)
 
(* fun mystery f = fn xs =>
    let
        fun g xs =
           case xs of
               [] => NONE
             | x::xs' => if f x then SOME x else g xs'
    in
        case xs of
            [] => NONE
          | x::xs' => if f x then g xs' else mystery f xs'
    end 

fun mystery1 f xs =
    let
        fun g xs =
           case xs of
               [] => NONE
             | x::xs' => if f x then SOME x else g xs'
    in
        case xs of
            [] => NONE
          | x::xs' => if f x then g xs' else mystery f xs'
    end  *)
(* 
 signature COUNTER1 =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end

signature COUNTER2 =
sig
    type t = int
    val newCounter : int -> t
    val first_larger : t * t -> bool
end

signature COUNTER3 =
sig
    type t
    val newCounter : int -> int
    val increment : t -> t
    val first_larger : t * t -> bool
end

signature COUNTER4 =
sig
    type t
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end

structure NoNegativeCounter :> COUNTER3 = 
struct

exception InvariantViolated

type t = int

fun newCounter i = if i <= 0 then 1 else i

fun increment i = i + 1

fun first_larger (i1,i2) =
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0

end

open NoNegativeCounter  *)