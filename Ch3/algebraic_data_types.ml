type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
type ptype = TNormal | TFire | TWater
type peff = ENormal | ENotVery | Esuper

(* Variants that carry data *)
type point = float * float
type shape = Point of point | Circle of point * float | Rect of point * point

let area = function
  | Point _ -> 0.0
  | Circle (_, r) -> 3.14 *. r *. r
  | Rect ((x1, y1), (x2, y2)) ->
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      w *. h

let center = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) -> ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)

let (sh : shape) = Point (2.0, 2.0)

(* Recursive variants *)
type int_list = Nil | Cons of int * int_list

let lst3 = Cons (3, Nil)
let lst123 = Cons (1, Cons (2, lst3))
let rec sum = function Nil -> 0 | Cons (h, t) -> h + sum t
let rec length = function Nil -> 0 | Cons (_, t) -> 1 + length t
let empty = function Nil -> true | Cons _ -> false

type node = { value : int; next : mylist }
and mylist = Nil | Node of node

(* Parameterized variants *)
type 'a mlist = Nil | Cons of 'a * 'a mlist

let lst3 = Cons (3, Nil)
let lst_hi = Cons ("hi", Nil)
let rec length = function Nil -> 0 | Cons (_, t) -> 1 + length t

type ('a, 'b) pair = { first : 'a; sencod : 'b }

(* Polymorphic variants *)
(* type fin_or_inf = Finite of int | Infinite *)
(* let f = function 0 -> Infinite | n -> Finite n *)
let f = function 0 -> `Inifnite | n -> `Finite n

(* built-in variants *)
type 'a blist = [] | ( :: ) of 'a * 'a blist

let empty_list = []
let one = 1 :: []
let two = ( :: ) (1, ( :: ) (2, []))
let thr = [ 1; 2; 3 ]

type 'a option = None | Some of 'a

let nil = None
let value = Some 42
