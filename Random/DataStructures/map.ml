module type PrintableType = sig
	type t
	val to_string : t -> string
end

module type OrderedPrintableType = sig 
	include PrintableType
	val compare : t -> t -> int
end

module type Map = sig
	type key
	type value
	type t

	val empty : t
	val set : key -> value -> t -> t
	val get : key -> t -> value
	val get_opt : key -> t -> value option
	val to_string : t -> string
end

module MakeBTreeMap (K:OrderedPrintableType) (V:PrintableType) : (Map with type key = K.t and type value = V.t)  = struct
	type key = K.t (* exposed *)
	type value = V.t (* exposed *)

	type t = | Empty (* hidden *)
			 | Node of key * value * t * t

	let empty = Empty

	let rec set k v t = match t with
						| Empty -> Node(k,v,Empty,Empty)
						| Node(k',v',lt,rt) -> let diff = K.compare k k' 
											   in 
											   if diff = 0 then Node(k,v,lt,rt)
											   else if diff < 0 then Node(k',v',(set k v lt),rt)
											   else Node(k',v',lt,(set k v rt)) 

	let rec get k t = match t with
					  	| Empty -> raise Not_found
					  	| Node(k',v',lt,rt) -> let diff = K.compare k k'
					  						   in
					  						   if diff = 0 then v'
					  						   else if diff < 0 then get k lt
					  						   else get k rt

	let rec get_opt k t = try
							let v = get k t
							in
							Some(v)
						  with Not_found -> None

	let rec tree_fold f acc t = match t with
		| Empty -> acc
		| Node(k,v,lt,rt) -> let acc_one = f acc (k,v)
							 in
							 let acc_two = tree_fold f acc_one lt
						     in
						     tree_fold f acc_two rt
						 	

	let to_string t = 
		let k_v_l = tree_fold (fun acc (k,v) -> (k,v)::acc) [] t
		in
		let s_l = k_v_l |> List.map (fun (k,v) -> Printf.sprintf "| %s -> %s |" (K.to_string k) (V.to_string v) )
		in
		s_l |> List.fold_left (fun acc s -> s ^ acc) ""

end


module IntType: OrderedPrintableType with type t = int = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end

module StringType : PrintableType with type t = string = struct
  type t = string
  let to_string s = "\"" ^ s ^ "\""
end


module IntStringMap = MakeBTreeMap(IntType)(StringType)
module IntIntMap = MakeBTreeMap(IntType)(IntType)


let values = [2;4;6;7;8;31]

let m = List.fold_left (fun m k -> IntStringMap.set k (string_of_int (k * k)) m) IntStringMap.empty values
let _ = print_endline (IntStringMap.to_string m)