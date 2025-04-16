module type Stack = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val peek : 'a t -> 'a
  val size : 'a t -> int
end

module ListStack : Stack = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let pop = function [] -> raise Empty | h :: t -> t
  let peek = function [] -> raise Empty | h :: t -> h
  let size = List.length
end

module CustomStack : Stack = struct
  type 'a entry = { top : 'a; rest : 'a t; size : int }
  and 'a t = S of 'a entry option

  exception Empty

  let empty = S None
  let is_empty = function S None -> true | _ -> false
  let size = function S None -> 0 | S (Some { size }) -> size
  let push x s = S (Some { top = x; rest = s; size = size s + 1 })
  let peek = function S None -> raise Empty | S (Some { top }) -> top
  let pop = function S None -> raise Empty | S (Some { top; rest }) -> rest
end
