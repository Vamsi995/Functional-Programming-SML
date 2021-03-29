(*

1. Write the tri-variate versions of curry and uncurry functionsWrite the tri-variate (i.e. 3 variable versions of curry and uncurry functions. First step is to write down the type (in the comments)..

curry : ('a * 'b * 'c -> 'd) -> 'a -> ('b -> ('c -> 'd))
uncurry: ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd

*)

fun curry f x y z = f (x,y,z)
fun uncurry f (x,y,z) = f x y z



(*

2. Write the functions fst : 'a * 'b -> 'a and snd : 'a * 'b -> 'b that project a tuple into its components.

*)

fun fst (u,v) = u
fun snd (u,v) = v



(*

3. Write the length function for lists length : 'a list -> int.

*)

fun len [] = 0
  | len (x::xs) = 1 + len xs


(*

4. Write the reverse : 'a list -> 'a list function. Be careful to not make it O(n^2).

*)

fun acc [] temp = temp
  | acc (x::xs) temp = acc xs (x::temp)

fun rev xs = acc xs []


(*

5. Write a function to compute the nth element in the Fibonacci sequence fib : int -> int. Be careful for the obvious version is exponential.

*)

fun fib 1 = 0
  | fib 2 = 1
  | fib n = fib (n-1) + fib (n-2)
