(* 
┌──────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                                                              │
│                                                                                              │
│                                                                                              │
│                      ____ ____  ____      ____                                               │
│                     |  _ \___ \|  _ \    / ___|  ___ _ ____   _____ _ __                     │
│                     | |_) |__) | |_) |___\___ \ / _ \ '__\ \ / / _ \ '__|                    │
│                     |  __// __/|  __/_____|__) |  __/ |   \ V /  __/ |                       │
│                     |_|  |_____|_|       |____/ \___|_|    \_/ \___|_|                       │
│                                                                                              │
│  __        ______    _  __     ___ _____   _______  __   ___    ____      _        _         │
│  \ \      / / ___|  / |/ /_   / / |___  | | ____\ \/ /  ( _ )  |  _ \ ___| |_ __ _| | _____  │
│   \ \ /\ / /\___ \  | | '_ \ / /| |  / /  |  _|  \  /   / _ \  | |_) / _ \ __/ _` | |/ / _ \ │
│    \ V  V /  ___) | | | (_) / / | | / /   | |___ /  \  | (_) | |  _ <  __/ || (_| |   <  __/ │
│     \_/\_/  |____/  |_|\___/_/  |_|/_/    |_____/_/\_\  \___/  |_| \_\___|\__\__,_|_|\_\___| │
│                                                                                              │
│                                                                                              │
│                                                                                              │
│                                                                                              │
└──────────────────────────────────────────────────────────────────────────────────────────────┘ 
*)



type ('a,'b) t = Publish of 'a * 'b Event.channel 
               | Request of 'a * 'b Event.event option Event.channel

(* server function *)
let broker () = 
    let client_req_channel = Event.new_channel ()
    in
    let rec thread_f key_sendch_l = match Event.sync(Event.receive client_req_channel) with
                    | Publish(k,send_ch) -> 
                                           let new_key_sendch_l = (k,send_ch)::key_sendch_l
                                           in
                                           thread_f new_key_sendch_l
                    | Request(k,reply_ch) -> 
                                           match List.assoc_opt k key_sendch_l with
                                               | None -> Event.sync(Event.send reply_ch None) 
                                                         ;
                                                         thread_f key_sendch_l
                                               | Some(send_ch) ->  let receive_event = Event.receive send_ch 
                                                                   in
                                                                   Event.sync(Event.send reply_ch (Some(receive_event)))
                                                                   ;
                                                                   thread_f key_sendch_l                 
    in 
    let _ = Thread.create thread_f []
    in
    client_req_channel


(* client functions *)
let publish client_req_channel k v =
  let send_ch = Event.new_channel ()
  in
  let rec thread_f () = Event.sync(Event.send send_ch v); thread_f ()
  in
  let _ = Thread.create thread_f ()
  in
  let publish_req = Publish(k,send_ch)
  in
  Event.sync(Event.send client_req_channel publish_req)

let request client_req_channel k =
    let reply_ch = Event.new_channel ()
    in
    let request_req = Request(k,reply_ch)
    in
    let _ = Event.sync(Event.send client_req_channel request_req)
    in
    match Event.sync(Event.receive reply_ch) with
    | None -> None
    | Some(receive_event) ->  let res = Event.sync receive_event
                              in
                              Some(res)


(* playing around with it *)

let broker = broker ()
let _ = publish broker 1 "hello"
let _ = publish broker 2 "world"
let test = request broker 1
let test2 = request broker 2;;




