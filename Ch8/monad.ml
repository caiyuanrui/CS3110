module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* 1. Maybe Monad *)

(** [return x] has trival effects. *)
let return x = Some x

let ( >>= ) x op = match x with None -> None | Some x -> op x

(*  - take x and extract from it the value a,
    - then take y and extract from it b,
    - then use a and b to construct a return value. *)

let ( + ) x y =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( + ) a b)

let ( - ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( - ) a b)

let ( * ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( * ) a b)

let ( / ) x y =
  x >>= fun a ->
  y >>= fun b -> if b = 0 then None else return (Stdlib.( / ) a b)

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) x f = match x with None -> None | Some x -> f x
end

(* retore these arithmatic operations *)
let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )

(* 2. Writer Monda *)
let inc = ( + ) 1
let dec = ( - ) 1
let ( >> ) f g x = x |> f |> g
let id x = inc >> dec
let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)
let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)
let dec_log_upgraded (x, s) = (x - 1, Printf.sprintf "%s Called dec on %i; " s x)
let id x = x |> inc_log |> dec_log_upgraded

(* helper1 *)
let log name f = fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

(* helper2 *)
let loggable name f =
 fun (x, s1) ->
  let y, s2 = log name f x in
  (y, s1 ^ s2)

let inc' = loggable "inc" inc
let dec' = loggable "dec" dec
let id' = inc' >> dec'

(* helper3 *)
let e x = (x, "")
let _ = id' @@ e 1 |> Stdlib.snd |> print_endline

module Writer : Monad = struct
  type 'a t = 'a * string

  let return x : 'a t = (x, "")

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
    let x, s1 = m in
    let y, s2 = f x in
    (y, s1 ^ s2)
end

(* Monad Laws *)
(* 1. [return x >>= f] should behave the same as [f x] *)
(* 2. [m >>= return] behaves the same as [m] *)
(* 3. [(m >>= f) >>= g] behaves the same as [m >>= (fun x -> f x >>= g)] *)
(* [return] is a left and right identity of [>>=], and [>>=] is associative *)
(* Law 1 says that having the trivial effect on a value, then binding a function on it, is the same as just calling the function on the value. *)
(* Law 2 says that binding on the trivial effect is the same as just not having the effect.  *)
(* Law 3 says that bind sequences effects correctly *)

module type MonadExt = sig
  include Monad

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end

module MonadExtImpl : MonadExt = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) x f = match x with None -> None | Some x' -> f x'
  let compose f g x = f x >>= fun y -> g y
  let ( >=> ) = compose
end

(* Using the compose operator, there is a much cleaner formulation of the monad laws:

Law 1: [return >=> f] behaves the same as [f]. (left identity)

Law 2: [f >=> return] behaves the same as [f]. (right identity)

Law 3: [(f >=> g) >=> h] behaves the same as [f >=> (g >=> h)]. (associativity) *)
