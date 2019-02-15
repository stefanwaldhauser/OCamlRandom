(* 
┌──────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                                                              │
│                                                                                              │
│                                                                                              │
│                   _____ _                               _ _____     _                        │
│                  | ____(_)_ __  ___   ______      _____(_)_   _| __(_) ___                   │
│                  |  _| | | '_ \/ __| |_  /\ \ /\ / / _ \ | | || '__| |/ _ \                  │
│                  | |___| | | | \__ \_ / /  \ V  V /  __/ |_| || |  | |  __/                  │
│                  |_____|_|_| |_|___( )___|  \_/\_/ \___|_( )_||_|  |_|\___|                  │
│                                    |/                    |/                                  │
│                __        ______    _  __     ___ _____   _______  __  ____                   │
│                \ \      / / ___|  / |/ /_   / / |___  | | ____\ \/ / | ___|                  │
│                 \ \ /\ / /\___ \  | | '_ \ / /| |  / /  |  _|  \  /  |___ \                  │
│                  \ V  V /  ___) | | | (_) / / | | / /   | |___ /  \   ___) |                 │
│                   \_/\_/  |____/  |_|\___/_/  |_|/_/    |_____/_/\_\ |____/                  │
│                                                                                              │
│                                                                                              │
│                                                                                              │
│                                                                                              │
└──────────────────────────────────────────────────────────────────────────────────────────────┘
 *)

type trie = Node of bool * (char * trie) list

let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []


(* val insert: string -> trie -> trie *)
let insert word t  = 
    let cl = explode  word
    in
    let rec aux cl (Node(is_w,c_t_l)) = match cl with
    | [] -> Node(true,c_t_l)
    | c::cs -> match List.assoc_opt c c_t_l with
               | None -> let new_t = aux cs (Node(false,[])) (* recursively create the complete new tree structure we have to attach to the current trie*)
                         in
                         let new_c_t_l = (c,new_t)::c_t_l  (* we attach the new trie to the current one by adding it to the c_t_l*)
                         in
                         Node(is_w,new_c_t_l) (* return our updated trie *)
                | Some(t) ->let repl_t = aux cs t (* recursively create the complete replacement tree structure we have to replace in the current trie*)
                            in
                            let new_c_t_l = (c,repl_t)::(c_t_l |> List.remove_assoc c) (* replace old tree trie with replacement one *)
                            in
                            Node(is_w,new_c_t_l) (* return our updated trie *)
    in
    if cl = [] 
    then t else aux cl t

(* playing around with insert *)

let rot = (Node(true,[])) (* rot node *)
let ro = Node (false, [('t',rot)]) (* ro node *)
let r  = Node(false,[('o', ro     )]) (* r node *)
let ex_trie = Node(false,[('r',r)])

let trie_containing_rotor = insert "rotor" ex_trie
let trie_containing_rotor_and_blut = insert "blut" trie_containing_rotor




let suffix_char s c = s ^ String.make 1 c 

(* this is a wrong attempt as it only creates all leaf words. It is interesting nonetheless *)
(* let rec collectWords trie = match trie with
    | Node(is_w,[]) -> [""]  | Node(is_w,c_t_l) -> c_t_l |> List.map (fun (c,t) -> List.map (fun s -> prefix_char s c) (collectWords t)) |> List.flatten *)


(* correct version*)
let collect_words trie  =
    let rec aux trie current_word words = match trie with 
    | Node(_,[]) -> current_word :: words
    | Node(true, c_t_l) -> c_t_l |> List.fold_left (fun words_of_prev_t (c,t) -> aux t (suffix_char current_word c) words_of_prev_t) (current_word::words)
    | Node(false, c_t_l) -> c_t_l |> List.fold_left (fun words_of_prev_t (c,t) -> aux t (suffix_char current_word c) words_of_prev_t) words
in
aux trie "" []

(* val merge: trie -> trie -> trie *)
let merge trieA trieB = 
    let allWordsOfA = collect_words trieA
    in
    allWordsOfA |> List.fold_left (fun prev_trie word -> insert word prev_trie) trieB


(* playing around with merge *)

let rot = (Node(true,[])) (* rot node *)
let ro = Node (false, [('t',rot)]) (* ro node *)
let r  = Node(false,[('o', ro     )]) (* r node *)
let ex_trie = Node(false,[('r',r)])

let tree_containing_rot_banane = insert "banane" ex_trie
let tree_containing_rot_rakete = insert "rakete" ex_trie

let merged_tree = merge tree_containing_rot_banane tree_containing_rot_rakete






