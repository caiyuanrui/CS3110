let rec a = 1 :: a
let rec from n = n :: from (n + 1)

(* let nats = from 0 *)
(* stackoveflow *)

(* In the definition of a recursive value, we are not permitted to use a value before it is finished being defined. *)
(* let rec nats = 0 :: List.map (fun x -> x + 1) nats *)

(* use function to delay evaluation *)
type 'a seq = Cons of 'a * (unit -> 'a seq)

let rec from n = Cons (n, fun () -> from (n + 1))

(** [hd s] is the head of [s]. *)
let hd (Cons (h, _)) = h

(** [tl s] is the tail of [s]. *)
let tl (Cons (_, t)) = t ()

(** [take n s] is the list of the first [n] elements of [s]. *)
let rec take n s = if n = 0 then [] else hd s :: take (n - 1) (tl s)

(** [drop n s] is all but the first [n] elements of [s]. *)
let rec drop n s = if n = 0 then s else drop (n - 1) (tl s)

let nats = from 0
let zero2ten = take 10 nats

let _ =
  List.fold_left (fun acc e -> acc ^ " -> " ^ string_of_int e) "head" zero2ten
  |> print_endline

(** [square <a; b; c; ...>] is [<a * a; b * b; c * c; ...]. *)
let rec square (Cons (h, t)) = Cons (h * h, fun () -> square (t ()))

(** [sum <a1; a2; a3; ...> <b1; b2; b3; ...>] is
    [<a1 + b1; a2 + b2; a3 + b3; ...>]. *)
let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1 + h2, fun () -> sum (t1 ()) (t2 ()))

(** [map f <a; b; c; ...>] is [<f a; f b; f c; ...>]. *)
let rec map f (Cons (h, t)) = Cons (f h, fun () -> map f (t ()))

(** [map2 f <a1; b1; c1;...> <a2; b2; c2; ...>] is
    [<f a1 b1; f a2 b2; f a3 b3; ...>]. *)
let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, fun () -> map2 f (t1 ()) (t2 ()))

let square' = map (fun x -> x * x)
let sum' = map2 (fun x y -> x + y)
let rec nats = Cons (0, fun () -> map (fun x -> x + 1) nats)
let times = ref 0

(** not efficient, evaluate from the beginning everytime *)
let rec fibs =
  Cons
    ( 1,
      fun () ->
        ignore (times := !times + 1);
        Cons (1, fun () -> sum fibs (tl fibs)) )

let _ =
  "fibs (cal " ^ string_of_int !times ^ "times): "
  ^ List.fold_left
      (fun acc e -> acc ^ " -> " ^ string_of_int e)
      "head" (take 10 fibs)
  |> print_endline
