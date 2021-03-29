
datatype lam = V of string
             | A of lam * lam
             | L of string * lam


datatype hlam = HV   of string
              | HA   of hlam * hlam
              | HL   of hlam -> hlam



(*
1. Define the substitution function subst : string * hlam -> hlam -> hlam. The expression subst ("x",u) e should replace every free occurance of the variable "x" in e with u. The tricky case is how does one handle the HL f case. HINT subst (x,u) (HL f) is HL fp where fp inp is essentially f inp but for substituting x by u.
*)


fun subst (x,u) (HV (e)) = if x = e
			 then
			     u
			 else
			     (HV (e))

  | subst (x,u) (HA (e1,e2)) = (HA ((subst (x,u) e1),(subst (x,u) e2)))
  | subst (x,u) (HL f) = let
                              fun fp t = subst (x,u) (f t)
			 in
			     (HL (fp))
			 end

(*
2. Notice that the substitution did not have to worry about the variable bound by the lambda and is simpler than the one defined for standard lambda calculus representation. However building a lambda term is difficult. Define the function abstract : string -> hlam -> hlam that essentially builds the HOAS term for λ x . M, i.e. if mh : hlam is the HOAS representation of M, then abstract "x" mh should give the HOAS representation for λ x . M. HINT The HOAS for λ x . M is given HL f, where f is that function that takes a HOAS nh : hlam and gives the HOAS term obtained by substituting "x" with nh in mh. Use part 2 to complete the assignment.
*)


fun abstract x mh = let
                         fun f nh = subst (x,nh) mh
                    in
			(HL (f))
                    end


(*
3. Define a function freshen : hlam -> string which will generate a string that does not occur free in its input. For example, for something like HL (fn t => HA (t , HV "x")) it can generate any string but the string "x". If we have a way to compute the free variables of hlam one can use that. However, the freshen is easier. free : hlam -> list string. Write a helper function freeP : hlam -> list string such that for all t : hlam freeP t ∩ FV(t) = ∅. Idea is when one sees FV(HAb f) ⊂ f (HV "x").
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



fun freeP (HV (e)) = [e]
  | freeP (HA (e1,e2)) = set_union (freeP e1) (freeP e2)
  | freeP (HL f) = freeP (f (HV "x"))


fun freshen t = fresh (freeP t)








(*
4. Give a conversion functions hoas : lam -> hlam syntax : hlam -> lam. In both cases handling the constructor associated with with lambda abstraction is difficult.
*)



fun hoas (V (e)) = (HV (e))
  | hoas (A (e1,e2)) = (HA (hoas e1, hoas e2))
  | hoas (L (x,M)) =  (abstract x (hoas M))



fun syntax (HV (e)) = (V (e))
  | syntax (HA (e1,e2)) = (A (syntax e1, syntax e2))
  | syntax (HL f) = let

      val u = freshen (HL f)
  in
      (L (u, syntax (f (HV u))))
  end
