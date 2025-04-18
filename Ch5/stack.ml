module type Stack = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t option
  val peek : 'a t -> 'a option
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end

module ListStack : Stack = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push = List.cons
  let peek = function [] -> None | h :: _ -> Some h
  let pop = function [] -> None | _ :: t -> Some t
  let size = List.length
  let to_list = Fun.id
end

(* Option.map aka fmap *)
let ( >>| ) opt f = match opt with None -> None | Some x -> Some (f x)

(* Option.bind *)
let ( >>= ) opt f = match opt with None -> None | Some x -> f x

let _ =
  ListStack.(empty |> push 1 |> pop >>| push 2 >>= pop >>| push 3 >>| to_list)
