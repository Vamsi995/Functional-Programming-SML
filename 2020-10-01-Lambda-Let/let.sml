(*
1. Define abstract syntax for λ-let and λ-letrec as a SML datatype.

*)

type var = string


datatype expr = Var of var
	      | fnappln of expr * expr
	      | fnabst of var * expr



datatype lmlet = letvar of var
    | letr of var*lmlet*lmlet
    | letabst of var*lmlet
    | letappln of lmlet*lmlet



datatype lmletrec =  letrecvar of var
    | letrec of var*lmletrec*lmletrec
    | letrecabst of var*lmletrec
    | letrecappln of lmletrec*lmletrec


(*
2. Write the conversion from these language to that of the plain lambda calculus as described in the class. Write the conversion process using two functions unletrec : λ-letrec -> λ-let and unlet : λ-let -> λ-calculus

*)


fun unlet (exp: lmlet): expr = case exp of letvar (x) => Var(x)
    | letr (expr1, expr2, expr3) => fnappln (fnabst (expr1, unlet expr3), unlet expr2)
    | letabst (expr1, expr2) => fnabst (expr1, unlet expr2)
    | letappln (expr1, expr2) => fnappln (unlet expr1, unlet expr2)


fun unletrec (exp: lmletrec): lmlet = case exp of letrecvar (x) => letvar (x)
    | letrec (expr1, expr2, expr3) => letr (expr1, unletrec expr2, unletrec expr3)
    | letrecabst (expr1, expr2) => letabst (expr1, unletrec expr2)
    | letrecappln (expr1, expr2) => letappln (unletrec expr1, unletrec expr2)
