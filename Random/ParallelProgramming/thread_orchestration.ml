type thread_comm_messages   = | DONE_PRINTING
			                  | NEED_TO_PRINT_MORE
			                  | GO
			                  
let spawn_counter n = 
	let comm_ch = Event.new_channel ()
	in
	let rec thread_f (curr,n) = 
		let _ = Event.sync(Event.receive comm_ch) (* wait for main thread signal*)
		in
		if  curr > n then Event.sync(Event.send comm_ch DONE_PRINTING)
		else 
			let s =  Printf.sprintf "Thread ID: %d: %d" (Thread.id(Thread.self ())) curr
			in 
			let _ = print_endline s
			in
			let _ = Event.sync(Event.send comm_ch NEED_TO_PRINT_MORE)
			in
			thread_f ((curr+1),n)
	in
	let _ = Thread.create thread_f (0,n)
	in
	comm_ch

let run_counters m n = 
	let comm_ch_l = List.init m (fun _ -> spawn_counter n)
	in
	let rec aux comm_ch_l = match comm_ch_l with
		| [] -> ()
		| ch::chls -> let _ = Event.sync(Event.send ch GO)
					  in
					  match Event.sync(Event.receive ch) with
					  | DONE_PRINTING -> aux chls
					  | NEED_TO_PRINT_MORE -> aux (chls@[ch])
					  | _ -> failwith "unreachable"
	in
	aux comm_ch_l






