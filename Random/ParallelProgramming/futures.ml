module Future = struct
	type 'a t = 'a Event.channel
	type 'a fut_res = | ResultWrapper of 'a
				      | ExceptionWrapper of exn
				      
	let create f a = 
		let fut_ch = Event.new_channel ()
		in
		let rec thread_f () = 
			try 
				let r = f a 
				in
				Event.sync(Event.send fut_ch (ResultWrapper r) )
			with e ->  
				Event.sync(Event.send fut_ch (ExceptionWrapper e) )
		in
		let _ = Thread.create thread_f ()
		in
		fut_ch
	
	let get fut_ch = match Event.sync(Event.receive fut_ch) with
					 | ResultWrapper(r) -> r
					 | ExceptionWrapper(e) -> raise e
					 
					 
	let _then g fut_ch = 
		let fut_ch_two = Event.new_channel ()
		in
		let thread_f () = try
						  	let x = Event.sync(Event.receive fut_ch)
						  	in
						  	match x with
						  	| ResultWrapper(r1) -> let r2 = g r1
						  						   in
						  						   Event.sync(Event.send fut_ch_two (ResultWrapper r2 ))
						    | ExceptionWrapper(e1) -> Event.sync(Event.send fut_ch_two (ExceptionWrapper e1))  
						  with e2 -> Event.sync(Event.send fut_ch (ExceptionWrapper e2) )
		in
		let _ = Thread.create thread_f ()
		in
		fut_ch_two
	
	let when_any fut_ch_l =
		let fut_ch = Event.new_channel ()
		in
		let event_l = fut_ch_l |> List.map Event.receive
		in
		let thread_f () = let x = Event.select event_l
						  in
						  Event.sync(Event.send fut_ch x)
		in
		let _ = Thread.create thread_f ()
		in
		fut_ch
	
	let when_all fut_ch_l = 
		let fut_ch = Event.new_channel ()
		in
		let thread_f () = 
			let x = fut_ch_l |> List.map (fun ch -> Event.sync(Event.receive ch))
			in
			let contains_ex = x |> List.find_opt (fun r -> match r with | ResultWrapper _ -> false 
					                                                    | ExceptionWrapper _ -> true)
			in
			match contains_ex with
			| Some(ExceptionWrapper(e)) -> Event.sync(Event.send fut_ch (ExceptionWrapper e))
			| None -> let rl = x |> List.map (fun y -> match y with 
													   | ResultWrapper r ->  r)
					  in
					  Event.sync(Event.send fut_ch (ResultWrapper rl ))
		in
		let _ = Thread.create thread_f ()
		in
		fut_ch
		
		

  (* additional stuff *)
  
  let memoize existing_fut_ch =
    let fut_ch = Event. new_channel () 
    in
    let thread_f () =
      let r = Event.sync (Event.receive existing_fut_ch) 
      in
      let rec send_cont () =
        let _ = Event.sync (Event.send fut_ch r)
        in
        send_cont ()
      in
      send_cont ()
    in
    let _ = Thread.create thread_f () in
    fut_ch

  let result_to receiver_ch fut_ch =
    let thread_f () =
      match Event.sync (Event.receive fut_ch) with
      | ResultWrapper r -> Event.sync (Event.send receiver_ch r)
      | ExceptionWrapper e -> raise e
    in
    let _ = Thread.create thread_f () 
    in
    ()

  let get_opt fut_ch = Event.poll (Event.receive fut_ch)
				
end

(* playing around *)

open Future

let my_future  = create (fun a -> a+ 1) 1
let my_future2 = create (fun a -> a+ 2) 1
let my_future3 = create (fun a -> a+ 3) 1
let my_future4 = create (fun a -> a+ 4) 1

let future_l = [my_future;my_future2;my_future3;my_future4]

let r = future_l |> when_all |> get


















