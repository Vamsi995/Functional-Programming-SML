
(*
1.
Define a type expr to capture this abstract syntax using ML data types with variables represented as strings type var = string datatype expr = ...
*)

datatype expr = var of string
	      | fnappln of expr * expr
	      | fnabst of string * expr

(*
2.
Write a function fresh : string list -> string which will produce a fresh variable name, i.e. given xs : string list, the strin fresh xs will be different from all the strings in xs.
Hint: Use Cantors diagonalisation
*)

fun max_len [] = ""
  | max_len (x::xs) = let val element = max_len xs
    in
	if size x > size element
	then
	    x
	else
	    element
   end

fun fresh ys = "A" ^ (max_len ys)

(*
3.
Consider an expression like fn x => x y. Here x is a bound variable because it occurs in the shadow of the binder fn x => ... However, y is a free variable. Write a functions free : expr -> var list to compute the list of free and all the Hint: First write down a formal definition for what is the set of free variable
*)


fun exists (el: string) [] = false
  | exists (el: string) (x::xs) = if (el = x)
				  then
				      true
				  else
				      exists el xs

fun set_union xs [] = xs
  | set_union [] xs = xs
  | set_union (x::xs) ys = if exists x ys
		     then
			set_union xs ys
		     else
			 x :: (set_union xs ys)

fun set_dif xs [] = xs
  | set_dif [] xs = []
  | set_dif (x::xs) ys = if exists x ys
			 then
			     set_dif xs ys
			 else
			     x :: set_dif xs ys


fun free (fnabst (x, xs)) = set_dif (free xs) [x]
	| free (var (x)) = if exists x [] = true
				then
				    []
				else
				    [x]

	| free (fnappln (x, y)) = set_union (free x) (free y)



(*
4.
Write a function subst : var * expr -> expr -> expr where subst (x,N) M substitutes all free occurrence of x in M with N. In mathematical notation it is written as M [x:=N].
*)


fun subst (x,N) (fnappln (m,n)) = (fnappln ((subst (x,N) m), (subst (x,N) n)))
  | subst (x,N) (var (y)) = if x = y
			   then
			       N
			   else
			       (var (y))

  | subst (x,N) (fnabst (m, n)) = if x = m
				  then
				      (fnabst (m, n))
				  else
				      (fnabst(m, (subst (x,N) n)))
