type 'a tree = Node of 'a * 'a tree list

(*            
                                  .─.                                     
                                 (10 )                                    
                                  `─'                                     
                                   ▲                                      
                                   │                                      
         ┌──────────────────┬──────┴─────────┬─────────────┐              
         │                  │                │             │              
         │                  │                │             │              
         │                  │                │             │              
        .─.                .─.              .─.           .─.             
       ( 5 )              (20 )            ( 4 )         (15 )            
        `─'                `─'              `─'           `─'             
         ▲                                                 ▲              
  ┌──────┴──────┐                                ┌─────────┴┬──────────┐  
  │             │                                │          │          │  
 .─.           .─.                              .─.        .─.        .─. 
(12 )         ( 8 )                            ( 9 )      (30 )      (13 )
 `─'           `─'                              `─'        `─'        `─'  *)

let sub_tree_five = Node(5,[(Node(12,[]));Node(8,[])])
let sub_tree_fifteen = Node(15,[(Node (9,[]));(Node(30,[]));(Node(13,[]))])
let ex_tree = Node(10,[sub_tree_five;(Node(20,[]));(Node(4,[])); sub_tree_fifteen])


let rec dfs_fold f acc (Node(a,c_l)) = match c_l with
			| [] -> f acc a
			| c_l -> c_l |> List.fold_left (fun acc ct -> dfs_fold f acc ct ) (f acc a)