(* Advanced Pattern Matching *)
(* [p1 | p2 | ... | pn]: an "or" pattern *)
(* [(p : t)]: a pattern with an explicit type annotation *)
(* [c]: any constant *)
(* ['ch1'..'ch2']: ch means a character literal here. 'A'..'Z means any uppercase letter' *)
(* [p when e]: matches [p] iff. [e] evaluates to [true] *)

(* Pattern Matching with Let *)
let c, d = (1, 2) in
c * d

(* Pattern Matching with Functions *)
(* [let f p1 ... pn = e1 in e2]   function as part of let expression *)
(* [let f p1 ... pn = e]          function definition at toplevel *)
(* [fun p1 ... pn -> e]           anonymous function *)

type mon = { name : string; hp : int }

let get_hp m = match m with { name = n; hp = h } -> h
let get_hp m = match m with { name = _; hp = h } -> h
let get_hp m = match m with { name; hp } -> hp
let get_hp m = match m with { hp } -> hp
let get_hp m = m.hp
let fst (x, _) = x
let snd (_, y) = y
let thrd t = match t with x, y, z -> z

let thrd t =
  let x, y, z = t in
  z

let thrd t =
  let _, _, z = t in
  z

let thrd (_, _, z) = z
