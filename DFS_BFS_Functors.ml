(* ======================================== GRAPH MODULE ========================================== *)
(* GRAPH MODULE TYPE*)
module type Graph = sig
        type node
        type graph
        val successors : graph -> node -> node list
end
(* AdjacentList Implementation*)
module GraphImplAdjacentList : (Graph with type node = int and type graph = (int*int list) list) = struct
	type node = int (* nodes are identified by a unique id*)
	type graph = (node*node list) list 
	let successors g n = g |> List.assoc n
end

(* ======================================== DFS | BFS FUNCTORS ==================================== *)
module MakeBFSFold (G: Graph) = struct
	let fold g f start_node start_value= 
		let rec aux visited result q = match q with 
		    | [] -> result (* base case : queue is empty *)
		    | n::rest_of_q -> if List.mem n visited  
                              then aux visited result rest_of_q 
                              else let new_visited = n::visited
                                   in 
                                   let new_result = f result n
                                   in
                                   let new_queue = rest_of_q@(G.successors g n) (* only thing changed from dfs *)
                           in
                           aux new_visited new_result new_queue
		in aux [] start_value [start_node]
end
module MakeDFSFold (G: Graph) = struct
	let fold g f start_node start_value= 
		let rec aux visited result s = match s with 
		    | [] -> result (* base case : stack is empty *)
		    | n::rest_of_s -> if List.mem n visited  
                              then aux visited result rest_of_s
                              else let new_visited = n::visited
                                   in 
                                   let new_result = f result n
                                   in
                                   let new_stack = (G.successors g n)@rest_of_s (* only thing changed from bfs *)
                           in
                           aux new_visited new_result new_stack
		in aux [] start_value [start_node]
end