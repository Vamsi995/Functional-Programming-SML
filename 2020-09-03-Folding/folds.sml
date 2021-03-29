(*
1. Define the functions foldr and foldl using the pattern matching for list.
*)

fun foldl f s [] = s
  | foldl f s (x::xs) = foldl f (f (x,s)) xs


fun foldr f s [] = s
  | foldr f s (x::xs) = f(x,foldr f s xs)

(*
2. Without using pattern, define the function sum : int list -> int that computes the sum of a list of integers.
*)

fun sum arg = foldr (op +) 0 arg

(*
3. Instead of using explicit recursion, define the following library function in terms of either foldr or foldl which ever is convenient. For the documentation of these library function, read the documentation of the List structure

partition : ('a -> bool) -> 'a list -> 'a list * 'a list

map : ('a -> 'b) -> 'a list -> 'b list.

reverse : 'a list -> 'a list

nth : 'a list * int -> 'a option.

*)

fun spartition f (x,(u,v)) = if f x = true
			 then
			    (x :: u, v)
			 else
			     (u, x :: v)
fun partition f arg = foldl (spartition f) ([],[]) arg



fun smap f (x,ys) = f x :: ys
fun map f arg = foldr (smap f) [] arg



fun reverse arg = foldl (op ::) [] arg



datatype 'a Find = LookingFor of int
		  |Found of 'a




fun snth (element,(LookingFor (n))) = if n = 0
				    then
					Found element
				    else
					LookingFor (n-1)
  | snth (element,Found x) = Found x

fun getEl (Found ele) = SOME ele
  | getEl _ = NONE


fun nth ([],_) = NONE
  | nth (x::xs, t) = if t < 0
		     then
			 NONE
		     else if t = 0
		     then
			 SOME x
		     else
			 getEl (foldl (snth) (LookingFor (t-1)) (xs))
