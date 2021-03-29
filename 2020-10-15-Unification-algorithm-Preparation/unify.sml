(*
1.A signature as we defined above is a set, in implementations we capture it as a type. Define a ML signature that captures a signature as we just mentioned.
``` signature SIG = sig

end

```

The contents of this signature should be a type that captures the symbols and function that compute the arity given the function. We also need comparisons on functional symbols so inside the signature we need a structure Ord : ORD_KEY on the symbol type. You will need to assert here that the type Ord.ord_key and the symbol type should be the same.

As an example, someone who wants to use your unification algorithm for say Peano arithmetic will define the following structure and use it with the Term functor given below.

structure Peano : SIG where datatype symbol = Zero | Succ fun arity Zero = 0 | arity Succ = 1 end

*)



signature SIG = sig

    type symbol

    val arity : symbol -> int

    structure Ord: ORD_KEY where type ord_key = symbol

end




(*
2. Define a similar signature VAR to capture variables. Essentially we only need a ORD structure on the variable type but it is good to have a separate signature here so that latter on we can add more things if required.
*)


signature VAR = sig

    type var

    structure Ord: ORD_KEY where type ord_key = var

end

(*
3. Define the Term functor that takes as arguments two structures (S : SIG) and V : VAR and builds a type that captures terms over the signature S.symbol and variable set V.var. Write the helper functions like occurs : term * V.t -> bool that checks whether the variable occurs in the term or not.
*)


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


(*
4. Use the ORD_MAP structure to implement an efficient data structure for telescopes. We maintain the telescope as a ORD_MAP data structure and the problem to solve is to figure out of a new equation (xᵢ ≡ tᵢ) can be added to a given map. This finite map keeps track of the variable assignment, i.e. the map uses the variable as a key and the term associated with it as the value.

x₁ ≡ t₁, .... xᵢ ≡ tᵢ ... xₙ ≡ tₙ then the map will have n keys x₁,....,xₙ with t₁,...,tₙ as the associated values.
*)



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
