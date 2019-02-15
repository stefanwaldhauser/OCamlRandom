(* 
┌──────────────────────────────────────────────────────────────────────────────────────────────┐
│                 ____                 _ _      _   __  __                                     │
│                |  _ \ __ _ _ __ __ _| | | ___| | |  \/  | __ _ _ __      _                   │
│                | |_) / _` | '__/ _` | | |/ _ \ | | |\/| |/ _` | '_ \   _| |_                 │
│                |  __/ (_| | | | (_| | | |  __/ | | |  | | (_| | |_) | |_   _|                │
│                |_|   \__,_|_|  \__,_|_|_|\___|_| |_|  |_|\__,_| .__/    |_|                  │
│                                                               |_|                            │
│                                ____          _                                               │
│                               |  _ \ ___  __| |_   _  ___ ___                                │
│                               | |_) / _ \/ _` | | | |/ __/ _ \                               │
│                               |  _ <  __/ (_| | |_| | (_|  __/                               │
│                               |_| \_\___|\__,_|\__,_|\___\___|                               │
│                                                                                              │
│                 __        ______    _  __     ___ _____   _______  __  _____                 │
│                 \ \      / / ___|  / |/ /_   / / |___  | | ____\ \/ / |___  |                │
│                  \ \ /\ / /\___ \  | | '_ \ / /| |  / /  |  _|  \  /     / /                 │
│                   \ V  V /  ___) | | | (_) / / | | / /   | |___ /  \    / /                  │
│                    \_/\_/  |____/  |_|\___/_/  |_|/_/    |_____/_/\_\  /_/                   │
│                                                                                              │
└──────────────────────────────────────────────────────────────────────────────────────────────┘
 *)


(* val pmap : ('a -> 'b) -> 'a list -> 'b event list *)
let pmap f el_l = 
    let el_ch_l = 
        el_l |> List.map (fun e ->  (e, (Event.new_channel ()))) 
    in
    let thread_f (el,ch) = 
            Event.sync(Event.send ch (f el))
    in
    let _ = el_ch_l |> List.iter (fun (el,ch) -> let _ = Thread.create thread_f (el,ch) 
                                                  in () )
    in
    el_ch_l |> List.map (fun (_,ch) -> Event.receive ch) (* not syncing! *)

(* val preduce : ('a -> 'a -> 'c ) -> 'a -> 'a -> 'c event *)
let preduce g x y  = 
    let ch = Event.new_channel ()
    in
    let thread_f () = Event.sync(Event.send ch (g x y))
    in
    let _ = Thread.create thread_f ()
    in
    Event.receive ch  (* not syncing! *)


(* val reduce_list ('a -> 'a -> 'a) -> 'a event list -> 'a *)
let reduce_list g event_list =
    let rec aux event_list no_of_finished_events = 
        let diff = (List.length event_list) - no_of_finished_events
        in
        if diff < 1 then failwith "error" (* will only happen with an empty list which we can ignore according to exercise *)
        else if diff = 1 then List.hd event_list
        else 
         let a = Event.select event_list 
         in
         let b = Event.select event_list 
         in
         let new_event = preduce g a b 
         in
         aux (new_event::event_list) (no_of_finished_events + 2)
    in
    aux event_list 0

(* val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a list -> 'b *)
let map_reduce f g l = 
    let pmap_event_list = pmap f l
    in
    let reduce_res_event = reduce_list g pmap_event_list
    in
    Event.sync reduce_res_event


(* playing around with it*)
let my_test_list = [1]
let map_f a = a*a
let reduce_f a b = a + b
let result = map_reduce map_f reduce_f my_test_list