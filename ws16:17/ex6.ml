module type Graph = sig
    type node
    type graph
    val successors : graph -> node -> node list
end

module BinaryTree = struct
    type node = int
    type graph = | Empty
                 | Node of graph * node * graph
    
    let rec successors g n  = match g with 
                 | Empty -> failwith "Node not found in graph!"
                 | Node(gl,n',gr) -> if n = n' then match (gl,gr) with 
                                        | (Empty, Empty) -> []
                                        | ( (Node(_,ln,_)), Empty) -> [ln]
                                        | (Empty, (Node(_,rn,_))) -> [rn]
                                        | ( (Node(_,ln,_)), (Node(_,rn,_)) ) -> [ln; rn]
                                     else if n < n'
                                        then successors gl n
                                     else 
                                        successors gr n
end

(* playing around with BinaryTree Definiton*)
open BinaryTree
(* TestTree*)
let ex_t = Node(Node(Empty,10,Node(Empty,15,Empty)), 20, Node(Empty,30,Empty))

module GraphImpl = struct
    type node = int
    type graph = (node * node list) list 
    let rec successors g n = match List.assoc_opt n g with 
                             | None -> failwith "Node not found in graph!"
                             | Some(nl) -> nl
end

(* playing around with GraphImpl *)
open GraphImpl
(* TestGraph*)
let g = [(1,[2;3;4]);(2,[5]);(3,[1]);(4,[]);(5,[])]

module MakeGraphSearch (G:Graph)   = struct

(* dfs fold implementation with type G.graph -> G.node -> ('a -> G.node -> 'a ) -> 'a -> 'a *) 
	let dfs g f start_node start_value= 
		let rec aux visited result s = match s with 
		    | [] -> result (* base case : stack is empty *)
		    | n::rest_of_s -> if List.mem n visited  
                              then aux visited result rest_of_s
                              else let new_visited = n::visited
                                   in 
                                   let new_result = f result n
                                   in
                                   let new_stack = (G.successors g n)@rest_of_s
                           in
                           aux new_visited new_result new_stack
		in aux [] start_value [start_node]
end

(* playing around with MakeGraphSearch *)
module TreeDfsFold = MakeGraphSearch(BinaryTree)
let sumTree = TreeDfsFold.dfs ex_t (+) 20 0
let _ = TreeDfsFold.dfs ex_t (fun _ n -> Printf.printf "%d \n" n) 20 ()

module GraphDfsFold = MakeGraphSearch(GraphImpl)
let sumGraph = GraphDfsFold.dfs g (+) 1 0
let _ = GraphDfsFold.dfs g (fun _ n -> Printf.printf "%d \n" n) 1 ()
