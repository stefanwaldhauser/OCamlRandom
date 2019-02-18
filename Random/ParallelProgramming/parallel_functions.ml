(* make a parallel version out of a unary function *)
let par_unary f =
	let f' input_l =
		let in_ch_l = input_l |> List.map (fun i -> (i,(Event.new_channel ())))
		in
		let thread_f (i,ch) = 
			let r = f i 
			in
			Event.sync(Event.send ch r)
		in
		let _ = in_ch_l |> List.map (fun (i,ch) -> Thread.create thread_f (i,ch))
		in
		in_ch_l |> List.map (fun (_,ch) -> Event.sync(Event.receive ch))
	in
	f'

(* make a parallel version out of a binary function *)
let par_binary f =
	let f' input_l_a input_l_b =
		if List.length input_l_a <> List.length input_l_b then failwith "lists have different lengths"
		else  
		let a_b_ch_list = List.map2 (fun a b -> (a,b, (Event.new_channel ())) ) input_l_a input_l_b
		in
		let thread_f (a,b,ch) =
			let r = f a b
			in
			Event.sync(Event.send ch r)
		in
		let ch_l = a_b_ch_list |> List.map (fun (a,b,ch) -> let _ = Thread.create thread_f (a,b,ch)
															in
															ch )
		in
		ch_l |> List.map (fun ch -> Event.sync(Event.receive ch))
	in
	f'