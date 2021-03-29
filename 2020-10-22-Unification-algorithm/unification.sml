
signature SIG = sig

    type symbol

    val arity : symbol -> int

    structure Ord: ORD_KEY where type ord_key = symbol

end


signature VAR = sig

    type var

    structure Ord: ORD_KEY where type ord_key = var

end




functor Term (structure S:SIG
	      structure V:VAR) = struct


datatype term = variable of V.var |
		sym_ap of S.symbol * term list



fun app (s:S.symbol) (l:term list) = if List.length l = S.arity s
				     then
					 SOME (sym_ap (s,l))
                                     else
                                         NONE





fun occurs (variable x, t: V.var) = if V.Ord.compare(x,t)=EQUAL
				    then
					true
				    else
					false

  | occurs (sym_ap (sym,[]), t: V.var) = false


  | occurs (sym_ap (sym,(y::ys)), t: V.var) = if occurs (y,t) = false
					      then
						 occurs (sym_ap (sym, ys), t)
					      else
						  true



fun retVar (variable x, t: V.var) = if V.Ord.compare(x,t) = EQUAL
				    then
					true
				    else
					false
  | retVar (sym_ap (sym, _), t:V.var) = false


end



functor Telescope (structure S:SIG
		   structure V:VAR) = struct


structure terms = Term (structure S = S; structure V = V)


structure telescope = RedBlackMapFn (V.Ord)


fun subst (x: V.var) (terms.variable (v)) (tel) = if V.Ord.compare(x, v) = EQUAL
							       then
								   false
							       else
								   let
								       val v_term = telescope.find(tel, v)
								       fun findvar (SOME ter) = subst x ter tel
									 | findvar NONE = true
								   in
								       findvar v_term
								   end

  | subst (x: V.var) (terms.sym_ap (sym, termlist)) (tel) = let

                                                               fun checkterm (y::ys) = if subst x y tel
			                                                               then
				                                                          checkterm ys
			                                                               else
				                                                           false
                                                                   | checkterm [] = true
                                                              in
                                                                 checkterm termlist
                                                              end



fun insertItem (x: V.var) (terms.variable (v)) (tel) = if V.Ord.compare(x,v) = EQUAL
						      then
							  SOME tel
						      else
							  if subst x (terms.variable v) tel
							  then
							      SOME (telescope.insert(tel,x,terms.variable v))
							  else
							      NONE
  | insertItem (x: V.var) (terms.sym_ap (sym, termslist)) tel  = if subst x (terms.sym_ap (sym,termslist)) tel
							     then
								 SOME (telescope.insert(tel,x,(terms.sym_ap (sym, termslist))))
							     else
								 NONE

end


(*
1. unify : telescope -> (term, term) -> option telescope. Given a telescope σ and terms s and t, it computes an extension σ₀ of σ (if possible) that unifies s[σ] and t[σ]. Recall our notation that s[σ] is the term obtained by substituting all the variable assignments of σ in s.
*)

(*
2. unifyList : telescope -> (term, term) list -> option telescope. This function simultaneously unifies each of the terms under the given telescope.
*)

functor Unify (structure S:SIG
	       structure V:VAR
	      ) = struct

structure teles = Telescope(structure S = S; structure V = V)

structure terms = teles.terms

fun unify tel (terms.variable v1, terms.variable v2) = let

    val map_v1 = teles.telescope.find(tel,v1)
    val map_v2 = teles.telescope.find(tel,v2)
    fun unifystep (SOME x, SOME y) = unify tel (x,y)
      | unifystep (SOME x, NONE) = teles.insertItem v2 x tel
      | unifystep (NONE, SOME y) = teles.insertItem v1 y tel
      | unifystep (NONE, NONE) = teles.insertItem v1 (terms.variable v2) tel

in
    unifystep (map_v1,map_v2)

end

  | unify tel (terms.variable v, terms.sym_ap (sym, termslist)) = let

      val map_v = teles.telescope.find(tel,v)
      val temp = terms.sym_ap (sym, termslist)

      fun unifystep (SOME x) = unify tel (x, temp)
	| unifystep NONE = teles.insertItem v temp tel
  in
      unifystep map_v

  end

  | unify tel (terms.sym_ap (sym, termslist), terms.variable v) = let

      val t1 = terms.sym_ap (sym,termslist)
      val t2 = terms.variable v

  in
      unify tel (t2,t1)
  end
  | unify tel (terms.sym_ap (sym, termslist), terms.sym_ap (sym1, termslist1)) = let

      fun listConcat (x::xs, y::ys) = (x,y) :: listConcat (xs,ys)
	| listConcat (_,[]) = []
	| listConcat ([],_) = []

  in

      if S.arity sym = S.arity sym1
      then
	  unifyList tel (listConcat (termslist,termslist1))
      else
	  NONE
  end

and unifyList tel (x::xs) = let

    val tel_v = unify tel x
    fun checktel (SOME t)= unifyList t xs
      | checktel NONE = NONE

in
    checktel tel_v
end
  | unifyList tel [] = SOME tel


end
