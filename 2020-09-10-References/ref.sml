(*
1. Define a Counter structure that has an internal ref cell and exposes the three functions, incr : unit -> unit, set : int -> unit, get : unit -> int. The outside world should not have any other access to the counter. Hint: You will have to define a signature say COUNTER and restrict he signature of your Counter structure appropriately.

*)

signature COUNTER = sig


    val get : unit -> int

    val incr : unit -> unit

    val decr : unit -> unit

end


structure Counter : COUNTER = struct

val num = ref 0
fun set x = num := x
fun get () = !num
fun incr () = (num := !num + 1)
fun decr () = (num := !num - 1)

end

(*

2. What if your program requires two or more counter ? Instead of a plain Counter structure define a MkCounter functor which creates a structure of the previous kind. This way you can have multiple counters.


*)


functor MkCounter () : COUNTER = struct

val num = ref 0
fun set x = num := x
fun get () = !num
fun incr () = (num := !num + 1)
fun decr () = (num := !num - 1)

end
