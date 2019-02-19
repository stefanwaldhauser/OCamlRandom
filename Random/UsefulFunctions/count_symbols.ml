let rec explode s = if s = "" then [] else (String.sub s 0 1)::(explode (String.sub s 1 ( (String.length s) -1)))
let count_symbols s =
	let char_l = explode s |> List.sort Pervasives.compare
	in	
	if char_l = [] then [] 
	else let result  = List.fold_left (fun acc c -> let (curr_c,curr_count) = List.hd acc in
									  if curr_c = c then (curr_c,(curr_count+1)):: (List.tl acc)
									  else (c,1)::acc
									  ) [(List.hd char_l,1)] (List.tl char_l)
		 in
		 result |> List.sort (fun (_,countA) (_,countB) -> countA - countB)
		 

let count_symbols ?(cmp = compare) (lis) = 
  let fr cmp l = 
    let rec enum acc c = function
      | []  -> acc
      | [x]  -> (x, c)::acc
      | a:: (b::xs as t)  -> (
          if cmp a b = 0 then enum acc (c + 1) t else enum ((a, c + 1)::acc) 1 t)
    in enum [] 1 l |> List.rev 
  in 
  List.sort cmp lis |> fr cmp;;

(*test*)
count_symbols ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
(**result = [("a", 7); ("b", 2); ("c", 3); ("d", 2); ("e", 4)]*)
