type ptype = TNormal | TFire | TWater
type mon = { name : string; hp : int; ptype : ptype }

let c = { name = "Charmander"; hp = 39; ptype = TFire }

let print_mon m =
  Printf.printf "%s %d %s\n%!" m.name m.hp
    (match m.ptype with
    | TNormal -> "normal"
    | TFire -> "fire"
    | TWater -> "water")

let _ = print_mon c

let _ =
  Printf.printf "%d\n%!" (match c with { name = n; hp = h; ptype = t } -> h)

(* Sugar!! *)
let _ = Printf.printf "%d\n%!" (match c with { name; hp; ptype } -> hp)

(* Record Copy *)
let c' = { c with name = "Charmeleon" }

(* Desugar.. *)
let c'' = { name = "Charmeleon"; hp = c.hp; ptype = c.ptype }
let _ = print_mon c'
let _ = print_mon c''
