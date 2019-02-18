type blog = string list
type user = string
type pass = string
type message = | Post of user * pass * string
			   | Read of user * blog Event.channel
type t = message Event.channel

(* SERVER *)
let start_server auth_l = 
	let init_u_b_l = auth_l |> List.map (fun (u,_) -> (u,[]))
	in
	let req_channel = Event.new_channel ()
	in
	let rec thread_f u_b_l = match Event.sync(Event.receive req_channel) with 
							 | Read(u,reply_ch) ->  if List.mem_assoc u u_b_l then
													   let b = List.assoc u u_b_l
													   in
													   let _ = Event.sync(Event.send reply_ch b)
													   in
													   thread_f u_b_l					 						
													else 
														let _ = Event.sync(Event.send reply_ch [])
														in
														thread_f u_b_l
							 | Post(u,pw,p) ->      if List.mem (u,pw) auth_l then 
														let new_u_b_l = u_b_l |> List.map (fun (user,blog) -> if user = u then (user,(blog@[p])) else (user,blog))
														in 
														thread_f new_u_b_l		
													else thread_f u_b_l
	in
	let _ = Thread.create thread_f init_u_b_l
	in
	req_channel

(* CLIENT *)
let post req_ch u pw p = 
	let post_req = Post(u,pw,p)
	in
	Event.sync(Event.send req_ch post_req)
	

let read req_ch u =
	let reply_ch = Event.new_channel ()
	in
	let read_req = Read(u,reply_ch)
	in
	let _ = Event.sync(Event.send req_ch read_req)
	in
	Event.sync(Event.receive reply_ch);;
	


(* Playing around with it *) 
let s = start_server [("userA", "passA"); ("userB", "passB")] 
in 
post s "userB" "passB" "Welcome to my OCaml blog."; 
post s "userA" "passA" "My name is A and I'm starting my own blog!"; 
post s "userB" "12345" "I am a hacker attacking B's blog now!"; 
post s "userB" "passB" "You can have threads in OCaml!"; 
read s "userB";;
			   
			   