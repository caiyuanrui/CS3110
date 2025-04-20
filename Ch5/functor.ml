module type X = sig
  val x : int
end

module IncX (M : X) = struct
  let y = M.x + 1
end

(** Anonymous functor *)
module IncX' =
functor
  (M : X)
  ->
  struct
    let y = M.x + 1
  end

module A : X = struct
  let x = 0
end

module B = IncX (A)

let _ = A.x |> print_int |> print_newline
let _ = B.y |> print_int |> print_newline

module type T = sig
  type t

  val x : t
end

module Pair1 (M : T) = struct
  let p = (M.x, 1)
end

module type P1 = functor (M : T) -> sig
  val p : M.t * int
end

module Pair2 : P1 =
functor
  (M : T)
  ->
  struct
    let p = (M.x, 1)
  end

module P0 = Pair1 (struct
  type t = int

  let x = 1
end)

module PA = Pair1 (struct
  type t = char

  let x = 'a'
end)
