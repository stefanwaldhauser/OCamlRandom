(* Ocaml adopts eager evaluation by default, 
but it is possible to simulate lazy evaluation/call-by-need*)

(* ==================================== *)
(* SIMULATING LAZY EVALUATION IN OCAML*)
(* ==================================== *)

(* Lazy Lists Implementations*)

type 'a lazy_list =  Cons of ('a * (unit -> 'a lazy_list))

(* from_lazy n creates the "infinite" list of natrual numbers startin at n *)
let rec from_lazy n = Cons(n,(fun () -> from_lazy (n+1))) (*[n;n+1;n+1;...]*)

(* take n returns the first n elements of a lazy list *)
let take n ll = 
    let rec aux n (Cons(a,tl_f)) acc = if n = 0 then acc 
                                       else aux (n-1) (tl_f ()) (a::acc)
    in
    (aux n ll []) |> List.rev

(*filter_lazy takes a predicate function and a lazy list. it returns a new lazy
  list that contains only those elements that satisfy the given predicatee *)
let rec filter_lazy p (Cons(a,tl_f)) =  
                      if p a then Cons(a,(fun () -> filter_lazy p (tl_f ())))  
                      else filter_lazy p (tl_f ())
                      
(* ========================================================================== *)

(* Binary Tree Implementations *)
type 'a tree = Empty 
| Node of 'a * 'a tree * 'a tree


(* Lazy Tree Implementations*)
type 'a ltree = 
  LNode of 'a * 
 (unit -> 'a ltree) * 
 (unit -> 'a ltree)

(* generators*)
(* constructs an infinite tree 
   where all nodes of the nth layer store the 
   value r + n. 
   We consider the root as layer 0, so the root stores value r*)
let rec layer_tree r = LNode(r, 
(fun () -> layer_tree (r+1)), 
(fun () -> layer_tree (r+1)))

(* constructs a tree with root (0,0) 
and for every node with pair (n, d), 
the left child stores (n, d+1) 
and the right child stores (n + 1, d) *)
let rational_tree () = 
    let rec aux (n,d) = LNode((n,d), 
    (fun () -> aux (n,(d+1))), 
    (fun () -> aux (n+1,d )))
    in
    aux (0,0)

(* functions *)
(* returns the top n layers of the given infinite tree 
as a finite binary tree *)
let rec top n (LNode(a, llt_f, rlt_f)) = 
    if n = 0 then Empty
    else let lt = top (n-1) (llt_f ())
         in
         let rt = top (n-1) (rlt_f ())
         in
         Node(a, lt, rt)

(* maps all elements of the tree using the given function*)
let rec map f (LNode(a,llt_f,rlt_f)) = 
    LNode((f a),
    ( fun () -> map f (llt_f ()) ), 
    (fun () -> map f (rlt_f ()) ) )

(* returns the infinite subtree rooted at a node that satisfies the given predicate *)
let find f lt =
  let rec bfs queue =
    match queue with 
    | [] -> failwith "unreachable"
    | LNode (a, llt_f, rlt_f)::qs -> 
        if f a 
        then LNode (a, llt_f, rlt_f)
        else bfs (qs @ [(llt_f ())] @ [(rlt_f ())])
  in
  bfs [lt]

