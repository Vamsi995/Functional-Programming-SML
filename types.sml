(*
Types

- int
- bool
- string
- real
- list -> []
- unit -> ()


*)


val x = 10
val hello = "hello"
val y = (12,"str")
val z = [4,5,6]
	    

val x = 3.5
val y = 2

(*Error as no operations can be performed with integers and reals -> no implicit conversion*)
(*val z = x * y*)

	    
		

(*fun average x y = 1/2 * (x + y)*)  (* Throws an error as we are trying to multiply integer with reals*)



fun average x y = 1.0/2.0 * (x+y)
				
		
