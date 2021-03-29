



(*
1. For lists define the function map : ('a -> 'b) -> 'a list -> 'b list. The semantics of map is that it applies the given function on all the elements of the list, i.e. map f [x1, x2,....,xn] = [f x1, f x2, ... f xn]
*)

fun map f [] = []
  | map f (x :: xs) = x :: map f xs


(*
2. Define the data type 'a tree that captures a binary tree.
*)


datatype 'a tree = Null
		 | Node of 'a * 'a tree * 'a tree


(*
3. Can you write a function treemap analogues to map for list ? First write its type and then complete its definition.
*)

(* treemap: ('a -> 'b) -> 'a tree -> 'a tree *)

fun treemap f Null = Null
  | treemap f (Node (x, t1, t2)) =  Node (f x, treemap f t1, treemap f t2)



(*

4. Define the in-order, pre-order and post-order traversal of the binary tree returning the list of nodes in the given order. First write down the type of the function(s) and then go about defining them.

*)

fun concat (x::xs) ys = x :: concat xs ys
  | concat [] ys = ys


(* inorder: 'a tree -> 'a list *)
fun inorder Null = []
  | inorder (Node (x, t1, t2)) = (inorder t1 @ [x]) @ inorder t2

(* preorder: 'a tree -> 'a list *)
fun preorder Null = []
  | preorder (Node (x, t1, t2)) = ([x] @ preorder t1) @ preorder t2

(* preorder: 'a tree -> 'a list *)
fun postorder Null = []
  | postorder (Node (x, t1, t2)) = concat (concat (postorder t1) (postorder t2)) [x]

(*
5. Define the rotate clockwise function for binary trees. Pictorially this rotation function is defined as the following. If the left subtree of the root is null then rotation is identity operation.
*)

fun clockRotate Null = Null
  | clockRotate (Node (x,Node(y,t3,t4),t2)) = Node (y,t3,Node (x,t4,t2))
  | clockRotate (Node (x, Null, t2)) = Node (x, Null, t2)
