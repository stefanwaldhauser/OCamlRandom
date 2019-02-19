(**===== 6 ====*)
module type Machine = sig
  type instr
  type state
  val start : int -> state
  val exec : state -> instr -> state
  val get_result : state -> int
end

(*6.1*)

type sinstr = Constant of int | Add | Scale2 of int

module StackMaschine: Machine 
  with type instr = sinstr and type state = int list = struct 
  type instr = sinstr
  type state = int list
  let start i = [i]
  let get_result = List.hd
  let exec s = function 
    | Constant (c) -> c::s
    | Add -> (match s with a::b::xs -> a + b ::xs | _ -> failwith "fail")
    | Scale2 (fac) ->  (match s with a::b::xs -> fac * a :: fac * b:: xs| _ -> failwith "fail")
end

(*6.2*)
type reg = Ra | Rb | Rc
type rm_state = { ra : int; rb : int; rc : int }

let set (r:reg) (v:int) (s:rm_state): rm_state =
  match r with 
  | Ra -> {s with ra = v}
  | Rb -> {s with rb = v}
  | Rc -> {s with rc = v}

let get r s : int =
  match r with 
    Ra -> s.ra
  | Rb -> s.rb
  | Rc -> s.rc

type rinstr = MoveReg of reg * reg | MoveConst of reg * int | Add of reg * reg

module RegisterMachine: Machine with type instr = rinstr and type state = rm_state= struct
  type instr = rinstr
  type state = rm_state
  let start i = {ra = 0; rb = 0; rc = i}
  let get_result st = get Rc st
  let exec st = function 
    | MoveConst (r, i) -> set r i st
    | MoveReg (r1, r2) -> set r1 (get r2 st) st
    | Add (r1, r2) -> set r1 (get r1 st + get r2 st) st
end 

(*6.3*)
module MakeProgramEvaluator (M : Machine) = struct
  type statement = Instr of M.instr
                 | Ite of statement list * statement list
  type state = M.state
  let rec execute s stmts = 
    let rec f s = function
      | Instr (i) -> M.exec s i
      | Ite (t, e) -> (if M.get_result s <> 0 then 
      List.fold_left f s t else List.fold_left f s e)
    in List.fold_left f s stmts
  let run prog input = execute (M.start input) prog
end
