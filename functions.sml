(*
Functions
*)


(* Identity Function *)

fun identity x = x (* Polymorphic function*)
		   
val temp = identity 10
val str = identity "hey"
		   
fun integerIdentity (x : int) = x		   (* Specifying the types of arguments *)

(* SML supports only single argument functions *)

fun curry f x y = f (x,y)
fun uncurry f (x,y) = f x y
			

(* Empty Function *)

fun empty [] = true
  | empty _ = false 
			
(*val res = empty [1,2,3]*)
			
(* Length Function *)

fun len [] = 0
  | len (_::xs) = 1 + len xs 

(*val res = len [1,2,3]*)


(* Map function in sml *)

fun map f [] = []
  | map f (x::xs) = x : map f xs 

			  
			
(* Append function *)
fun append [] x = x :: []
  | append (y::ys) x = y :: append ys x

(*val res = append [2,3,4] 3*)
		 


(* Concat function *)

val x = [2,3,4] @ [3,4] (* For concatenating two lists *)

	(* concat: 'a list -> 'a list -> 'a list *)

fun concat (x::xs) ys = x :: concat xs ys
  | concat [] ys = ys

		
