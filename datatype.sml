(*

1. Type aliasing
2. Datatypes

*)


datatype Day = Sun
	|Mon
	|Tue
	|Wed
	|Thur
	|Fri
	|Sat
	     
	 


val x = Sun

fun isHoliday Sun = true
  | isHoliday Sat = true
  | isHoliday _  = false



(* Options *)

fun head (x::_) = x

(*val x =  head []*) (* Causes a error - A runtime error *)

	      (* headSafe: 'a list -> 'a option *)

	      (* datatype 'a option = SOME of a
	                              | NONE 
	       *)

datatype 'a Option = Some of 'a
	| None

fun headSafe (x :: _) = SOME x
  | headSafe _  = NONE 

		      
		      
