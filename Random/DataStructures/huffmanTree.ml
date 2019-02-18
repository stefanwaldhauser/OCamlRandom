(* BASED ON : http://cl-informatik.uibk.ac.at/teaching/ws16/fp/material/fpln.pdf *)

(* Step 1: Analyzing the Sample*)

(* The idea for counting the number of occurrences of 
symbols in the sample is to ﬁrst sort the input (hence equal 
symbols are one after another), 
then count the lengths of segments of equal symbols,
 and ﬁnally return a list of symbol-weight pairs (sorted by weights) 
 where the weight of a symbol is the number of its 
 occurrences in the sample. *)

(* ==================================== *)
(* Additional Functions for Lst*)
(* ==================================== *)
let concat list_of_lists = List.fold_left (fun acc l -> acc@l) [] list_of_lists

(* take_while p xs results in the longest preﬁx of xs such that all elements satisfy p,*)
let rec take_while p l = match l with 
| [] -> []
| x::xs -> if p x then x::take_while p xs else []

(* drop_while p xs results in the list obtained from xs by 
removing elements at the front until p is no longer satisﬁed *)
let rec drop_while p l = match l with
| [] -> []
| x::xs -> if p x then drop_while p xs else x::xs

(* fst contains the elements that have been removed using snd*)
let span p xs = (take_while p xs, drop_while p xs)

(* until repeatedly applies a given function until some condition becomes true*)
let rec until p f x =
if p x then x else until p f (f x)

(* Counting the lengths of segments consisting of equal symbols,
resulting in a list of pairs where the second component is a symbol 
and the ﬁrst one the number of occurrences of this symbol in the sample*)

let rec collate l = match l with 
| [] -> [] 
| h::tl -> let (h_eq_prefix,rest) = span (fun x -> x = h) (h::tl) 
                in
                (List.length h_eq_prefix, h) :: collate rest

(* generate (frequency,symbol) list and then sort this list again using frequency*)
let sample l = List.sort Pervasives.compare l |> collate |> List.sort Pervasives.compare

(* ==================================== *)
(* Building the Huffman Tree*)
(* ==================================== *)

(* In a Huﬀman tree leaves and all other nodes carry diﬀerent kinds of information.
 The value of a leaf is a certain character plus the weight for this character 
 whereas each non-leaf node only needs to store the sum of the weights of its two subtrees. 

To model that, the type *)
type 'a btree = Empty | Node of ('a btree * 'a * 'a btree)
type node_content = (int * char option)

(*)
Thus leaf nodes will have values of the form (w, Some c) and non-leaf nodes values of the form (w, None). 
The type of Huﬀman trees is therefore deﬁned by
*)

type t = node_content btree

let rec insert compare v  t = match t with
    | Empty -> Node(Empty,v,Empty) 
    | Node(l,w,r) -> if compare v w < 1 
                    then Node(insert compare v l,w,r) 
                    else Node(l,w,insert compare v r)

(* In outline, the procedure for building a Huﬀman tree is ﬁrst to convert the given frequency list into a list of trees, 
and then repeatedly to combine two trees with lightest weights until just one tree remains.
----> therefore the rarest symbols will have the longest path from teh root*)

(* returns the weight of a given huffman tree*)
let weight ht = match ht with
    | Node(_,(weight,_),_) -> weight
    | _ -> failwith "empty tree"

(* given a list of hts sorted by weight and a ht this function inserts the ht in the correct position*)
let insert ht list_of_hts_sorted =
    let (smaller_eq,rest) = span (fun ht' -> weight ht' <= weight ht) list_of_hts_sorted 
    in
    smaller_eq@(ht::rest) 

let combine_first_two_trees hts = match hts with 
| first_ht::second_ht::rest -> let total_weight = weight first_ht + weight second_ht
                               in
                               let combined_tree = Node(first_ht, (total_weight,None), second_ht)
                               in
                               insert combined_tree rest
| _ -> failwith "Length has to be greater at least two"

(* building the huffman tree*)
let make_leaf_node (weight,character) = Node(Empty,(weight,Some character),Empty)
let is_singleton l = List.length l = 1

let make_huffman_tree input_string =
    let leaf_only_hts = List.map make_leaf_node (sample input_string) (* list of leaf only hts order ascending by weight, so RAREST
    symbols are in the beginning*)
    in
    until is_singleton combine_first_two_trees leaf_only_hts |> List.hd


(* ==================================== *)
(* Encoding and Decoding*)
(* ==================================== *)

(* In a Huﬀman tree it is easy (and fast) to ﬁnd a character !given a bit sequence! (decoding), however, 
for compressing (i.e., encoding) data, the opposite operation is needed, 
namely ﬁnding a bit sequence for a given character. For this purpose a code table is generated out of the Huﬀman tree. *)

type bits = int list
type table = (char * bits) list

(* The function table takes a Huﬀman tree as input and generates a code table. 
The helper function performs a top to bottom construction, 
i.e., it builds the code when descending in the tree 
and when arriving at a leaf the code (for a single character) is already computed.*)

let table t = 
    let rec tab current_code ht = match ht with
        | Node(Empty,(_,Some v),Empty) -> [(v,current_code)] (* base case: hit leaf node*) 
        | Node(l,_,r) -> let codes_of_left_side = tab (current_code@[0]) l 
                        in
                        let codes_of_right_side = tab (current_code@[1]) r (* inner node*)
                        in
                        codes_of_left_side @ codes_of_right_side
        | _ -> failwith "the Huffman tree is empty" in tab [] t

(* To consult the table the function lookup is used *)
let rec lookup tab c = match tab with
    | ((v,code)::tl) -> if v = c then code else lookup tl c 
    | _ -> failwith "not found"

let encode t list_of_strings =
    let tab = table t 
    in 
    concat(List.map (lookup tab) list_of_strings)

let rec decode_char (ht,bit_sequence) = match (ht,bit_sequence) with
    | (Node(Empty,(_,Some c),Empty), bit_sequence_without_c) -> (c,bit_sequence) (* leaf node*)
    | (Node(lht,_,_),0::rest_of_bit_sequence) -> decode_char (lht,rest_of_bit_sequence) 
    | (Node(_,_,rht),1::rest_of_bit_sequence) -> decode_char (rht,rest_of_bit_sequence) 
    | _ -> failwith "empty tree"


let rec decode ht bit_sequence = match bit_sequence with
| [] -> [] 
| bit_sequence -> let (char,bit_sequence_without_c) = decode_char (ht,bit_sequence) 
                  in
                  char::decode ht bit_sequence_without_c