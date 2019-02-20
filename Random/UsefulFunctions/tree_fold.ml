(* binary tree*)
type 'a tree = | Empty 
               | Node of 'a * 'a tree *  'a tree

let ex_tree = Node(5, (   Node(1,   (Node(0,Empty,Empty))  ,   (Node(3,Empty,Empty))) )   , (Node(10,Empty,Empty))     );;  



(* pre_order fold *)
let pre_fold f acc t = 
  let rec aux t acc = match t with
        | Empty -> acc
        | Node(a,lt,rt) -> let acc_one = f acc a
                           in
                           let acc_two = aux lt acc_one
                           in
                           aux rt acc_two
  in
  aux t acc

(* in_order fold*)
let in_fold f acc t = 
  let rec aux t acc = match t with
        | Empty -> acc
        | Node(a,lt,rt) -> let acc_one = aux lt acc
                           in
                           let acc_two = f acc_one a
                           in
                           aux rt acc_two
  in
  aux t acc

(* post_order fold *)
let post_fold f acc t = 
  let rec aux t acc = match t with
        | Empty -> acc
        | Node(a,lt,rt) -> let acc_one = aux lt acc
                           in
                           let acc_two = aux rt acc_one
                           in
                           f acc_two a
  in
  aux t acc

(* level_order fold*)

let unfold ~init ~f =
    let rec loop state acc =
      match f state with
      | None -> List.rev acc
      | Some (x, new_state) -> loop new_state (x :: acc)
    in
    loop init []

let level_order_fun tree =
  unfold ~init:[tree] ~f:(fun state ->
    match state with
    | [] -> None
    | h :: t ->
      match h with
      | Empty -> None
      | Node (x, Empty, Empty) -> Some (x, t)
      | Node (x, c, Empty)
      | Node (x, Empty, c) -> Some (x, t @ [c])
      | Node (x, a, b) -> Some (x, t @ [a;b]))

let level_fold f acc t = 
  let level_order_l = level_order_fun t
  in
  level_order_l |> List.fold_left f acc



(* unnormal fold add3 *)
let rec tf f b t = match t with Empty -> b 
                  | Node (x, l, r) -> f (tf f b l) x (tf f b r)



(* tree arbitrary number of children*)
type 'a tree = Node of 'a * 'a tree list

let rec dfs_fold f acc (Node(a,c_l)) = match c_l with
			| [] -> f acc a
			| c_l -> c_l |> List.fold_left (fun acc ct -> dfs_fold f acc ct ) (f acc a)